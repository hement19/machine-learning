#Remove warnings
options(warn=-1)

#Reading libraries
library(caTools)
library(car)
library(DAAG)
library(ROCR)

#Removing env variables
rm(list=ls(all=TRUE))

#Setting working directory
getwd()
setwd("C:/Users/Ben Roshan/Documents")

#Reading csv files
flierresponse=read.csv(file='FlierResponse.csv',header=T)
framingham=read.csv(file='framingham.csv',header=T)

str(flierresponse)
summary(flierresponse)
summary(framingham)
flierresponse$Response=as.factor(flierresponse$Response)
framingham <- na.omit(framingham) 
#Random split the data into training and testing sets
set.seed(1000)
split=sample.split(framingham$TenYearCHD,SplitRatio = 0.70)

train=subset(framingham,split==TRUE)
test=subset(framingham,split==FALSE)

#Logistic regression model
framinghamlog=glm(TenYearCHD~.,data=train,family = binomial)
summary(framinghamlog)
car::vif(framinghamlog)

#Accuracy on training set
predictTrain=predict(framinghamlog,type="response",newdata=train)
predictTrain

#Confusion matrix with threshold of 0.5
table(train$TenYearCHD, predictTrain>0.5)

#Model metrics
accuracy=(3082+51)/(3082+506+19+51)
accuracy
precision=(2170)/(2170+357)
precision
sensitivity_recall=(2170)/(2170+9)
sensitivity_recall
specificity=(30)/(30+357)
specificity

#Accuracy on test set
predictTest=predict(framinghamlog,type="response",newdata=test)
predictTest

#Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest>0.5)
table(test$TenYearCHD, predictTest>0.9)
table(test$TenYearCHD, predictTest>0.7)
table(test$TenYearCHD, predictTest>0.3)
table(test$TenYearCHD, predictTest>0.1)

#Model metrics
accuracy=(915+12)/(915+12+158+7)
accuracy
precision=(915)/(915+158)
precision
sensitivity_recall=(915)/(915+7)
sensitivity_recall
specificity=(12)/(12+158)
specificity

#Checking AIC
#AIC should be minimum
summary(framinghamlog)

summary(test)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
