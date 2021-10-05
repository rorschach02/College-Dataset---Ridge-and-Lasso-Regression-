#########################################################
## Created By : Aniket Maheshwari
## Date: 09/30/2021
#########################################################

##Clear the environment 
rm(list = ls())


## First we will set the directory of the R script 
setwd("C:/Users/anike/Desktop/Sem 1/EAS 506 Statistical Data Mining/Homework/Homework 2")


library(leaps)
library(MASS)
library(ISLR)
library(lattice)
library(ggplot2)
library(corrplot)
library(car)
library(caret)
library(coefplot)
library(glmnet)

### Importing Data ###

College_data = College 
?College
### looking Into Data ####
dim(College_data) 
str(College_data)
summary(College_data)
class(College_data)

### Checking if dataset has any null values ##

indx <- apply(College_data, 2, function(x) any(is.na(x)))
indx

# source : https://discuss.analyticsvidhya.com/t/how-can-i-check-whether-my-data-frame-contains-na-inf-values-in-some-column-or-not-in-r/1647
## So none of the columns had null value 
?College
head(College_data , 10)


## Let's Normalize the dataset ####
normalize <- function(x) {
  (x -min(x)) / (max(x) - min(x))
  
}

college_data_norm <- as.data.frame(lapply(College_data[2:18], normalize))
head(college_data_norm , 4)
full_dataset <- cbind(College_data,college_data_norm )
full_dataset <- subset(full_dataset , select = -c(2:18))
head(full_dataset ,1)

# source :  https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function


##### Splitting Train and test data #####
## Now because Private is a bivariate categorical variable we want equal parts in both testing and training data so we use Stratified Sampling

set.seed(1)
trainIndex <- createDataPartition(full_dataset$Private, p = 0.70,list = FALSE,times = 1)
train_data <- full_dataset[trainIndex,]
test_data <- full_dataset[-trainIndex,]
dim(train_data)
dim(test_data)
# source : https://www.listendata.com/2015/02/splitting-data-into-training-and-test.html


## Linear Regression ## 
linear_model <- lm(Apps~., data=train_data)
linear_model_summary <- summary(linear_model)
linear_model_summary

# RSS came out to be : 0.02267 
linear_model_predict <- predict(linear_model , test_data , type="response")

MSE_linear_model <- mean((test_data$Apps - linear_model_predict )^2) ## Mean Squared Error for test set 
MSE_linear_model ## MSE is 0.0003986848

###### Ridge Regression #####
## Before we start with ridge regression, we need to split test and training data according to glmnet() method

## Splitting ## 
set.seed(1)
train.x <- model.matrix(Apps~., train_data ) [,-1]
train.y <-  train_data$Apps
test.x <- model.matrix(Apps~.,test_data) [,-1]
test.y <- test_data$Apps



## Cross - Validation ## 
## Now we need to know the best penalty variable (lambda) before we put it in ridge regression glmnet() method

set.seed (1)
cross_valid_output <- cv.glmnet(train.x,train.y,alpha =0)
plot(cross_valid_output)

bestlam <- cross_valid_output$lambda.min
bestlam #Bestlam : 0.008117708

#performing ridge regression 
ridge_analysis <- glmnet (train.x, train.y, alpha = 0,lambda = bestlam)

ridge_analysis_summary <- summary(ridge_analysis)
ridge_analysis_summary


## Fitting the model to test set ##
ridge.pred <- predict(ridge_analysis , s = bestlam ,newx = test.x ) ##Test
ridge.pred

## MSE error 
MSE_ridge_regression <- mean((ridge.pred - test.y )^2)
MSE_ridge_regression ## MSE is 0.0004046323

## RSS error 
RSS_ridge_regression <- sum((ridge.pred - test.y)^2)
RSS_ridge_regression ##RSS is 0.09387468

##### Lasso - Regression #####
set.seed(1)
cross_valid_output_lasso <- cv.glmnet(train.x,train.y,alpha =1)
plot(cross_valid_output_lasso)

bestlam_lasso <- cross_valid_output_lasso$lambda.min
bestlam_lasso

#performing lasso regression 
lasso_analysis <- glmnet (train.x, train.y, alpha = 1,lambda = bestlam_lasso)

lasso_analysis_summary <- summary(lasso_analysis)
lasso_analysis_summary
lasso_analysis$beta

## Fitting the model to test set ##
lasso.pred <- predict(lasso_analysis, s = bestlam_lasso , newx = test.x )
lasso.pred

##MSE error 
MSE_lasso_regression <- mean((lasso.pred - test.y )^2)
MSE_lasso_regression ## MSE error is 0.0003982019

## RSS error 
RSS_lasso_regression <- sum((lasso.pred - test.y)^2)
RSS_lasso_regression ## RSS error is 0.09238284

## non- zero coefficient
coef(lasso_analysis) # This don't show any coefficient with value 0. Let's try something else

outcome <- coef(lasso_analysis, s=bestlam_lasso)
outcome
outcome[outcome[,1]!=0,]








