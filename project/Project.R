#Clear the working environment
rm(list=ls())

#Set the working environment path
setwd("/home/nikos/Documents/Rprojects/THL311")


source("preprocessing.R")
source("costFunction.R")
source("sigmoid.R")
source("predictlog.R")
source("boxplotnumeric.R")

#install.packages("ROCR")
library(ROCR)
#install.packages("glmnet")
library(glmnet)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("reshape2")
library(reshape2)


train=read.csv("adult.csv", header=F, col.names=c("age", "type_employer", "fnlwgt", "education",
                                                   "education_num","marital", "occupation", "relationship", "race","sex",
                                                   "capital_gain", "capital_loss", "hr_per_week","country", "over50k"), stringsAsFactors = FALSE)

#print boxplot for all numeric variables
boxplotnumeric(train)

#clean and structure data for the training dataset
train=preproccesing(train)

#Independent variables
X <- as.matrix(train[,c(1:(ncol(train)-1)) ])

#Add ones to X (the interceptor term theta_0)
X <- cbind(rep(1,nrow(X)),X)

#Dependent variable
Y <- as.matrix(train$over50k)


test=read.csv("adultest.csv", header=F, col.names=c("age", "type_employer", "fnlwgt", "education",
                                                    "education_num","marital", "occupation", "relationship", "race","sex",
                                                    "capital_gain", "capital_loss", "hr_per_week","country", "over50k"), stringsAsFactors = FALSE)
#Do the same for the test datase
test=preproccesing(test)

Xtest <- as.matrix(test[,c(1:(ncol(test)-1)) ])
Xtest <- cbind(rep(1,nrow(Xtest)),Xtest)
Ytest <- as.matrix(test$over50k)


#create and initialize vector with theta parameters equal to zero
initial_theta <- rep(0,ncol(X))

#Perform optimization to minize cost function and get the parameters
#Plug the costFunction and grad functions with theta parameters initialized to zero
optimRes <- optim(par = initial_theta, fn = costFunction(X,Y), gr = grad(X,Y), 
                                  method="BFGS", control = list(maxit = 500))

#Compute confusion matrixes for trained and test set
trainedtheta=optimRes$par
conf.matrix.train=matrix(predictlog(X,Y,trainedtheta), ncol=2)
conf.matrix.test=matrix(predictlog(Xtest,Ytest,trainedtheta), ncol=2)

#Compute accuracies(train, test and baselines)
accuracy.train=(conf.matrix.train[1]+conf.matrix.train[4])/nrow(train)
accuracy.test=(conf.matrix.test[1]+conf.matrix.test[4])/nrow(test)
baseline.accuracy=(conf.matrix.test[1]+conf.matrix.test[3])/nrow(test)

#calculate predictions on the test set
predictions=sigmoid(Xtest%*%trainedtheta)

#Compute the area under curve (ROC)
ROCRpred = prediction(predictions, test$over50k)
auc=as.numeric(performance(ROCRpred, "auc")@y.values)

#plot the roc curve (colorizing the threshod)
perf_logistic = performance(ROCRpred, "tpr", "fpr")
plot(perf_logistic,colorize=TRUE,  print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

#Compare results with R's built-in function
adultglm =glmnet( X ,Y, family="binomial")                                   #alpha=1(default)-> penalize using L1 norm (lasso regression)
predictionsglm= predict.glmnet(adultglm, Xtest, s=0.0001, type="response")   #use regularization to prevent overfit
conf.matrix.glm=matrix(table(test$over50k, predictionsglm>0.5))

accuracy.glm=(conf.matrix.glm[1]+conf.matrix.glm[4])/nrow(test)

#the difference of accuracy betwwen the 2 methods
deviation=accuracy.test-accuracy.glm

cat(sprintf('Baseline accuracy: %f\n', baseline.accuracy))
cat(sprintf('Prediction accuracy for the train set: %f\n', accuracy.train))
cat(sprintf('Prediction accuracy for the test set: %f\n', accuracy.test))
cat(sprintf('Prediction accuracy for the glmnet library : %f\n', accuracy.glm))
cat(sprintf('Area of curve (AUC) : %f\n', auc))




