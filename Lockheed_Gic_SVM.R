# Lockheed: Predicting Gic using failure modes with supporting vector regression
# Author: Hao Zhong

# Reference: https://www.svm-tutorial.com/2014/10/support-vector-regression-r/

# Packages
library(rstudioapi)
library(e1071)

# Initialize environment and path (when restarting RStudio)
# rm(list = ls())
setwd(dirname(getActiveDocumentContext()$path))
getwd()

# 
# Data Import
# 
mydata <- read.csv("Fake Data DOE_IV 1_24_2017.csv", header = TRUE, stringsAsFactors = FALSE)
doe.parameters <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", "Surface.Preperation", "Contaminate.Type")
failure.modes <- c("X.Laminate.1", "X.Interfacial.1", "X.Cohesive.1")
mydata <- mydata[, c(failure.modes, "Gic")]
str(mydata)
summary(mydata)

#
# Linear regression
# 
formula <- "Gic ~ X.Laminate.1 * X.Interfacial.1"
lm.model <- lm(formula = formula, data = mydata)
summary(lm.model)
rmse <- function(error){
  sqrt(mean(error^2))
}
error <- lm.model$residuals 
predictionRMSE <- rmse(error)
predictionRMSE
# 
# We know now that the RMSE of our linear regression model is 3.227293 
# Let's try to improve it with SVR

# 
# SVM Regression
#

# Actually, only use 2 of the 3 failure modes will be proper as well as enough
svm.model <- svm(Gic ~ X.Laminate.1 * X.Interfacial.1,  data = mydata)
summary(svm.model)
predictedY <- predict(svm.model, mydata)
error <- mydata$Gic - predictedY
svrPredictionRMSE <- rmse(error)
svrPredictionRMSE
# 3.290993

# Tuning the SVR model
tuneResult <- tune(svm, Gic ~ X.Laminate.1 * X.Interfacial.1,  data = mydata,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(0:10))
)
plot(tuneResult)
print(tuneResult)
print(sqrt(tuneResult$best.performance))
# best performance: MSE = 10.68438, RMSE = 3.268698 epsilon 0.8 cost 1

# Further tuning
tuneResult <- tune(svm, Gic ~ X.Laminate.1 * X.Interfacial.1,  data = mydata,
                   ranges = list(epsilon = seq(0.8,0.9,0.001), cost = 1)
) 
plot(tuneResult)
print(tuneResult)
# best performance: MSE = 10.58998, RMSE = 3.254225 epsilon 0.855 cost 1

# Best model
tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, mydata) 
error <- mydata$Gic - tunedModelY  
tunedModelRMSE <- rmse(error)
tunedModelRMSE


# From doe.4:
# 

# model 4.1: support vector machine (linear kernel)
svm.model <- svm(Failure.Modes ~ ., data=train, kernel="linear")
svm.predict<-predict(svm.model, newdata = test[,-23], type = "class")
table(test$Failure.Modes, svm.predict)

# model 4.2: support vector machine (sigmoid kernel)
svm.model <- svm(Failure.Modes ~ ., data=train, kernel="sigmoid")
svm.predict<-predict(svm.model, newdata = test[,-23], type = "class")
table(test$Failure.Modes, svm.predict)

# model 4.3: support vector machine (polynomial kernel)
svm.model <- svm(Failure.Modes ~ ., data=train, kernel="polynomial")
svm.predict<-predict(svm.model, newdata = test[,-23], type = "class")
table(test$Failure.Modes, svm.predict)

# model 4.4: support vector machine (radial basis kernel)
svm.model <- svm(Failure.Modes ~ ., data=train, kernel="radial")
svm.predict<-predict(svm.model, newdata = test[,-23], type = "class")
table(test$Failure.Modes, svm.predict)