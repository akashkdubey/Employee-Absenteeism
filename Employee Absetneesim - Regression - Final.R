rm(list = ls())

#Getting Current working directory.
getwd()

#Setting working directory
setwd('/Users/akash/Desktop/Project 2 -Employe Absenteism')

getwd()

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','readxl')

lapply(x, require, character.only = TRUE)


############################################# READING DATA #####################################################
library("readxl")
data <- read_excel('Absenteeism_at_work_Project.xls')

#Getting the dimensions of data
dim(data)


# Fetting Structure Of data
str(data)


#Retrieving Column names of train and test data.
colnames(data)


#Removing Spaces between the column names
names(data) =  gsub(" ", "_", names(data))


############ Distribution pf Target Variable ##############

# For train data
library(ggplot2)
pl = ggplot(data ,aes(x = Absenteeism_time_in_hours)) + ggtitle("Absenteeism_time_in_hours")
print(pl + geom_bar(fill = 'blue'))

########################################## Missing Value Analysis ##############################################

missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]



### Imputing Missing Values
require(DMwR)
cnames = colnames(data)
newdata = data.frame()
newdata = data.frame(data)
data = knnImputation(newdata, k = 3)
sum(is.na(data))
anyNA(data)


########################################### Data Visusalisation ####################################################

#### Box-Plot for data
boxplot(data[,c("ID", "Reason_for_absence","Month_of_absence","Day_of_the_week","Seasons","Transportation_expense","Distance_from_Residence_to_Work")])
boxplot(data[,c("Service_time", "Age","Work_load_Average.day","Hit_target","Disciplinary_failure" ,"Education" ,"Son")])
boxplot(data[,c("Social_drinker","Social_smoker","Pet", "Weight", "Height", "Body_mass_index","Absenteeism_time_in_hours")])

#### KDE plot 
library("kdensity")
plot(density(data$ID))
plot(density(data$Reason_for_absence))
plot(density(data$Month_of_absence))
plot(density(data$Day_of_the_week))
plot(density(data$Seasons))
plot(density(data$Transportation_expense))
plot(density(data$Distance_from_Residence_to_Work))
plot(density(data$Service_time))
plot(density(data$Age))
plot(density(data$Work_load_Average.day))
plot(density(data$Hit_target))
plot(density(data$Absenteeism_time_in_hours))
plot(density(data$Body_mass_index))
plot(density(data$Height))
plot(density(data$Weight))
plot(density(data$Pet))
plot(density(data$Social_smoker))
plot(density(data$Social_drinker))
plot(density(data$Son))
plot(density(data$Education))
plot(density(data$Disciplinary_failure))

## Normality Check
qqnorm(data$ID)
qqnorm(data$Reason_for_absence)
qqnorm(data$Month_of_absence)
qqnorm(data$Day_of_the_week)
qqnorm(data$Seasons)
qqnorm(data$Transportation_expense)
qqnorm(data$Distance_from_Residence_to_Work)
qqnorm(data$Service_time)
qqnorm(data$Age)
qqnorm(data$Work_load_Average.day)
qqnorm(data$Hit_target)
qqnorm(data$Absenteeism_time_in_hours)
qqnorm(data$Body_mass_index)
qqnorm(data$Height)
qqnorm(data$Weight)
qqnorm(data$Pet)
qqnorm(data$Social_drinker)
qqnorm(data$Social_smoker)
qqnorm(data$Son)
qqnorm(data$Education)
qqnorm(data$Disciplinary_failure)


#### PCA Visualisation
library(ggfortify)
autoplot(prcomp(data), data = data, colour = 'Absenteeism_time_in_hours')



###################### Outlier Analysis #######################

cnames =c('Service_time', 'Age', 'Work_load_Average.day', 'Transportation_expense','Hit_target', 'Height', 'Absenteeism_time_in_hours', 'Weight')
df = data

#Replace all outliers with NA and impute
for(i in cnames)
{
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  #print(length(val))
  df[,i][df[,i] %in% val] = NA
}

# Imputing missing values
df = knnImputation(df,k=3)
data = df
anyNA(data)


################################# CREATING DUMMIES FOR CATEGORICAL VARIABLES ####################################
data = fastDummies::dummy_cols(data , select_columns = "Seasons" , remove_first_dummy = TRUE)
data = fastDummies::dummy_cols(data , select_columns = "Month_of_absence" , remove_first_dummy = TRUE)
data = fastDummies::dummy_cols(data , select_columns = "Day_of_the_week" , remove_first_dummy = TRUE)
data = fastDummies::dummy_cols(data , select_columns = "Reason_for_absence" , remove_first_dummy = TRUE)
data = fastDummies::dummy_cols(data , select_columns = "ID" , remove_first_dummy = TRUE)
data = fastDummies::dummy_cols(data , select_columns = "Education" , remove_first_dummy = TRUE)
data = fastDummies::dummy_cols(data , select_columns = "Pet" , remove_first_dummy = TRUE)
data = fastDummies::dummy_cols(data , select_columns = "Son" , remove_first_dummy = TRUE)

knitr::kable(data)

# Deleting the columns for which dummies are created
data = subset(data, select = -c(Seasons,Month_of_absence, Day_of_the_week,Reason_for_absence,ID, Education, Pet, Son ))


############################################ Scaling the data #######################################

cnames1 =c('Service_time', 'Age', 'Work_load_Average.day', 'Transportation_expense','Hit_target', 'Height', 'Weight')

# Normalization
for(i in cnames1)
{
  print(i)
  data[,i] = (data[,i] - min(data[,i]))/(max(data[,i])-min(data[,i]))
}

########################################## Feature Selection ######################################

##### Using Correlation plot

library(corrgram)
corrgram(data[,cnames] , order =F, upper.panel = panel.pie , text.panel = panel.txt , main = "Correlation Plot")

# Weight seems to be correlated, so Deleting weight from the data

data = subset(data , select = -c(Weight))




##### Splitting the data into train and test
n = nrow(data)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train = data[trainIndex ,]
test = data[-trainIndex ,]

X_train = subset(train,select = -c(Absenteeism_time_in_hours))
y_train = subset(train,select = c(Absenteeism_time_in_hours))

X_test = subset(test,select = -c(Absenteeism_time_in_hours))
y_test = subset(test,select = c(Absenteeism_time_in_hours))

##### Using PCA

#principal component analysis
prin_comp = prcomp(X_train)

#compute standard deviation of each principal component
std_dev = prin_comp$sdev

#compute variance
pr_var = std_dev^2

#proportion of variance explained
prop_varex = pr_var/sum(pr_var)

#cdf plot for principle components
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
X_train.data = data.frame( prin_comp$x)

# From the above plot selecting 45 components since it explains almost 95+ % data variance
X_train.data =X_train.data[,1:45]

#transform test into PCA
X_test.data = predict(prin_comp, newdata = X_test)
X_test.data = as.data.frame(X_test.data)

#select the first 45 components
X_test.data=X_test.data[,1:45]




######################################## Machine learning model##########################################

X_train.data$Absenteeism_time_in_hours = paste(y_train$Absenteeism_time_in_hours)
X_test.data$Absenteeism_time_in_hours = paste(y_test$Absenteeism_time_in_hours)


library(mlbench)
#### KNN
#Develop Model on training data
fit_LR = knnreg(Absenteeism_time_in_hours ~ ., data = X_train.data)
#Lets predict for testing data
pred_LR_test = predict(fit_LR,X_test.data)
# Results 
print(postResample(pred = pred_LR_test, obs =y_test$Absenteeism_time_in_hours))



###### Multiple Linear Regression
#Develop Model on training data
set.seed(100)
#Develop Model on training data
fit_LR = lm(Absenteeism_time_in_hours ~ ., data = X_train.data)
#Lets predict for testing data
pred_LR_test = predict(fit_LR,X_test.data)
# Results 
print(postResample(pred = pred_LR_test, obs =y_test$Absenteeism_time_in_hours))




###### SVM
#Develop Model on training data
fit_LR = svm(Absenteeism_time_in_hours ~ ., data = X_train.data)
#Lets predict for testing data
pred_LR_test = predict(fit_LR,X_test.data)
# Results 
print(postResample(pred = pred_LR_test, obs =y_test$Absenteeism_time_in_hours))




###### Decision Tree

#Develop Model on training data
#Develop Model on training data
fit_DT = rpart(Absenteeism_time_in_hours ~., data = X_train.data, method = 'anova')
pred_DT_test = predict(fit_DT,X_test.data)
# Results
print(postResample(pred = pred_DT_test, obs = y_test$Absenteeism_time_in_hours))



###### GBDT

#Develop Model on training data
fit_GBDT = gbm(Absenteeism_time_in_hours~., data = X_train.data, n.trees = 500, interaction.depth = 2)
#Lets predict for testing data
pred_GBDT_test = predict(fit_GBDT,X_test.data, n.trees = 500)
# For testing data 
print(postResample(pred = pred_GBDT_test, obs = y_test$Absenteeism_time_in_hours))





###### Random Forest
#Develop Model on training data
fit_DT = randomForest(Absenteeism_time_in_hours ~., data = X_train.data)
pred_DT_test = predict(fit_DT,X_test.data)
# Results
print(postResample(pred = pred_DT_test, obs = y_test$Absenteeism_time_in_hours))










