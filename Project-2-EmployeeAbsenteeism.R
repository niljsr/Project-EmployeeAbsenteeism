rm(list =ls())
getwd()
setwd("G:/edwisor")
install.packages("readxl")
library(readxl)
data = read_excel("G:/edwisor/data2_new.xls")
data_new = data[,-(1)]
class(data$Absenteeism)
data(AirPassengers)
View(AirPassengers)
data_numerical = sapply(data, is.numeric)
data_missing = sapply(data,is.na)
data_missing

install.packages("MissMech")
library(MissMech)
has_na <- sapply(data_new, function(x) any(is.na(x)))
TestMCARNormality(data_new[has_na])
TestMCARNormality(data_new[has_na],method="Auto")

#Since p value is 0.056 ~ 0.05 .So we have no concrete evidence to say data MCAR
# Data is MAR

# imputing NA in missing observations
data_new[data_new == "?"] <- NA

# Using MICE for Missing value Imputation

install.packages("mice")
library(mice)
md.pattern(data_miss)
class(completeData)
tempData <- mice(data_new,m=5,maxit=50,seed=500)
summary(tempData)


completeData <- complete(tempData,2)
data_missing = sapply(completeData,is.na)
data_missing
# complete data is the dataset with no missing values

# Outlier Analysis

cnames = colnames(completeData)

# to detect outliers in the data
for(i in cnames)
{
  print(i)
  val = completeData[,i][completeData[,i] %in% boxplot.stats(completeData[,i],coef=1.5)$out]
  
  print(val)

  
}


nrow(completeData)

# Box Plot for Variables

boxplot(completeData$Reason, main="Reason for absence") # no outliers
boxplot(completeData$Month, main ="Month") # no outliers
boxplot(completeData$Transportationexpense, main =" Transportation Expense") # one outlier
boxplot(completeData$Distance, main= 'Distance')
boxplot(completeData$Servicetime, main ='Service Time') # one outlier
boxplot(completeData$Age, main ='Age') # one Outlier
boxplot(completeData$WorkloadAverage, main ='Average Work')
boxplot(completeData$Bodymassindex, main ='Bodymassindex')
boxplot(completeData$Height, main ='Height')
boxplot(completeData$Weight, main ='Weight')
boxplot(completeData$Pet, main ='Pet')
boxplot(completeData$Socialsmoker, main ='Socialsmoker')
boxplot(completeData$Son, main ='Son')
boxplot(completeData$Education, main ='Education')
boxplot(completeData$Hittarget, main ='Hit Target')
boxplot(completeData$WorkloadAverage, main ='Average Work')
boxplot(completeData$Absenteeism, main ="Absenteeism") # 10 Outliers

# Most of yhe Outliers not removed or imputed as all seemed important except Absenteeism and Service time

# Removing Outliers from Absenteeism and Service Time Variables

for(i in (1:740)) {
  
  if(completeData_final[i,19] > 39) {
    rows_not_keep <- i
    completeData_final <- completeData_final[-(i),]   
    print(i)
  }
  
}

for(i in (1:711)) {
  
  if(completeData_final[i,7] > 24) {
    rows_not_keep <- i
    completeData_final <- completeData_final[-(i),]   
    print(i)
  }

}
completeData$Day <- data_new$Day
completeData$Season <- data_new$Seasons

# Feature Selection
# correlation matrix

data_numeric = completeData[,(5:20)]
data_numeric = data_numeric[,-(7:8)]
data_numeric = data_numeric[,-(8:9)]


install.packages("corrgram")
library(corrgram)

corrgram(data_numeric, order=F ,upper.panel = panel.pie, text.panel = panel.txt, main='correlation plot')

install.packages("caret")
library(caret)
corelationmatrix <- cor(data_numeric)

print(corelationmatrix)

# find attributes taht are highly correlated (ideally >0.75)

highlycorrelated <- findCorrelation(corelationmatrix, cutoff = 0.7)

print(highlycorrelated)

# based on Correlation matrix we will remove 'Weight' variable from numeric variables


# Categorical variables

data_categorical = completeData[,-(5:10)]
data_categorical = data_categorical[,-(7)]
data_categorical = data_categorical[,-(9:12)]

cname2 = colnames(data_categorical)

summary(aov(data_categorical$Absenteeism ~ data_categorical$Disciplinaryfailure))
# No significant differnce between means of two groups

summary(aov(data_categorical$Absenteeism ~ data_categorical$Socialdrinker))
# significant difference between means of two groups

summary(aov(data_categorical$Absenteeism ~ data_categorical$Socialsmoker))
# no significant difference between means of two groups

# Removing 'Weight' variable from the data set.

completeData = subset(completeData, select = -c(Weight))


# Model Deployment
# Random Forest Regressor



View(completeData_final)

data_factor = sapply(data, is.factor)
data_factor
install.packages("randomForest")
install.packages("inTrees")
library(randomForest)
library(inTrees)
# separating training and test sets
train_index=sample(1:nrow(completeData_final),0.7 *nrow(completeData_final))

train = completeData_final[train_index,]
test = completeData_final[-train_index,]
Random_Forest = randomForest( Absenteeism ~ .,data= train, importance =TRUE, ntree =500, mtry = floor(sqrt(ncol(train) - 1)))
print(Random_Forest)
# Extract Rules from random Forest
# transform RF object into an inTrees format

treeList = RF2List(Random_Forest)

#Extract Rules
exec = extractRules(treeList, train[,-19])

# visualize some rules
exec[1:2,]

# make rules more readable
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]

# Get Rule Metrics(to predict the final value as per the rules)

ruleMetric = getRuleMetric(exec,train[,-19], train$Absenteeism)
ruleMetric[1:3,]

# Evaluate the model
# Predict the test data using RF Model

RF_Predictions = predict(Random_Forest, test[,-20])
RF_Predictions


# importance/significance of variables
importance(Random_Forest,type =1)

# Mean Absolute Percentage Error
mape <- function(y , yhat) {
  mean(abs((y- yhat)/y))
}

# Error Metrics
install.packages("miscTools")
library(miscTools)
r2 <- rSquared(test$Absenteeism, test$Absenteeism - predict(Random_Forest, test[,-19]))
r2

# Mean Square Error

mse <- mean((test$Absenteeism - predict(Random_Forest, test[,-19]))^2)
mse

#  Multiple Linear Regression

# fitting the train data into regression model
lm_model = lm(Absenteeism ~., data = train)

# summary of the model

summary(lm_model)

# predicting the test data

predict_data = predict(lm_model,test[,1:19])
# Error metrics
# Calculate MAPE
# calculate MSE
mape <- function(y , yhat) {
  mean(abs((y- yhat)/y))*100
}
mape(test[,9], predict_data)

mse <- mean((test$Absenteeism - predict(lm_model,test[,1:19]))^2)
mse

# Loss Calculation

total =0
sum =0
for(i in (1:212)) {
  
  # nof days
  
  if(test[i,2] == test[(i+1),2]) {

  a <- RF_Predictions[i]/24
  work = a * test[i,9]
  
  total = total +work
  
  }
  else {
    sum = sum + work
    a =0
    work=0
    print(sum)
  }
}
final = sum %/% 12

print(final)
