
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL building ------##
##----------------------------------------------------------------------------------##


##--- Step 1: Clear environment variables ------------------------------------------##
rm(list=ls(all=TRUE))

##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##

##setwd

##__________________________________________________________________________________##
getwd()
setwd("C:/Users/LEGION/Documents")
##--- Step 3: Read the data from the csv file --------------------------------------##
cars_data = read.csv(file="Toyota_SimpleReg.csv",header = T)
names(cars_data)
str(cars_data)
summary(cars_data)
##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop any id,model attribute(s):
cars_data=cars_data[,-c(1,2)]
## Summary of the data and look for any missing values:
str(cars_data)
summary(cars_data)

## Correlation and Covariance between the attributes:
cov(cars_data)
plot(cars_data$Age_06_15,cars_data$Price)
plot(cars_data$Age_06_15,cars_data$Price,xlab = "age of the car",ylab="price in($)",pch=18,col="red")

#Describe how the covarainace and correlation coefficients 
cor(cars_data)
cor(cars_data$Age_06_15,cars_data$Price)

#Do the attributes have a good enough correlation coefficient to support linear regres  sion model building?

##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (80:20) ratio
rows=seq(1,nrow(cars_data),1)
set.seed(123)
trainRows=sample(rows,(70* nrow(cars_data))/100)
cars_train=cars_data[trainRows,]
cars_test=cars_data[-trainRows,]

trainRows1=sample(rows,(80* nrow(cars_data))/100)
cars_train1=cars_data[trainRows,]
cars_test1=cars_data[-trainRows,]

trainRows2=sample(rows,(90* nrow(cars_data))/100)
cars_train2=cars_data[trainRows,]
cars_test2=cars_data[-trainRows,]
##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##
LinReg = lm(Price ~ Age_06_15, data= cars_train)
LinReg
coefficients(LinReg)

LinReg1 = lm(Price ~ Age_06_15, data= cars_train1)
LinReg1
coefficients(LinReg1)

LinReg2 = lm(Price ~ Age_06_15, data= cars_train2)
LinReg2
coefficients(LinReg2)

## Summary of model:
summary(LinReg)
plot(LinReg$residuals)
summary(LinReg)
#Extract the intercept coefficient from the linear regression model


#Extract the residual values


##__________________________________________________________________________________##


##--- Step 7: Check for validity of linear regression assumptions ------------------##
#HINT: plot the 4 graphs to check. Write your comments
par(nfrow = c(2,2))
plot(LinReg)
par(nfrow=c(1,1))
##__________________________________________________________________________________##


##--- Step 8: Predict on testdata --------------------------------------------------##
test_predicition=predict(LinReg,cars_data)
test_predicition
plot(test_predicition)
test_actual=cars_test$Price
plot(test_actual)
##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##
library(DMwR)

#Error verification on train data
regr.eval(cars_train$Price,LinReg$fitted.values)
plot(regr.eval(cars_train$Price,LinReg$fitted.values))

#Error verification on test data
regr.eval(test_actual,test_predicition)


##__________________________________________________________________________________##


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
#Find the confidence and prediction intervals and plot them for the WHOLE dataset
Conf_Pred = data.frame(predict(LinReg,cars_test,interval="confidence",levell=0.95))
Pred_Pred = data.frame(predict(LinReg,cars_test,interval="prediction",levell=0.95))
str(Conf_Pred)
summary(Conf_Pred)
#data visualization
plot(cars_test$Age_06_15,cars_test$Price,xlab = "age of the car ",ylab="price in($)")

points(cars_test$Age_06_15,Conf_Pred$fit,type="l",col="green",lwd=2)
points(cars_test$Age_06_15,Conf_Pred$lwr,pch="-",col="red",lwd=4)
points(cars_test$Age_06_15,Conf_Pred$upr,pch="-",col="red",lwd=4)
points(cars_test$Age_06_15,Pred_Pred$lwr,pch="-",col="blue",lwd=4)
points(cars_test$Age_06_15,Pred_Pred$upr,pch="-",col="blue",lwd=4)

##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##