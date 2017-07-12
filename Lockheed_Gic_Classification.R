# Lockheed: Classification of Gic
# Author: Hao Zhong
# Date: 6/16/2017
# References:
#   http://www.statmethods.net/advstats/cart.html
#   ftp://centos.ustc.edu.cn/CRAN/web/packages/rpart/vignettes/longintro.pdf
#   

# Packages
library(rstudioapi)
library(ggplot2)
library(tree)
library(rpart)
library(party)
library(randomForest)
library(e1071)
library(caret)

# Initialize environment and path (when restarting RStudio)
rm(list = ls())
setwd(dirname(getActiveDocumentContext()$path))
getwd()

# 
# Data Import
#

# Import raw data from file
filename <- "Fake Data DOE_IV 1_24_2017.csv"
mydata.all <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

# Rename Surface.Preperation to Surface.Preparation
names(mydata.all)[names(mydata.all) == 'Surface.Preperation'] <- 'Surface.Preparation'

# Define groups of variables
doe.parameters <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", "Surface.Preparation", "Contaminate.Type")
doe.nums <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")
doe.cats <- c("Surface.Preparation", "Contaminate.Type")
failure.modes <- c("X.Laminate.1", "X.Interfacial.1", "X.Cohesive.1")

# Correct variable types
mydata <- mydata.all[, c(doe.parameters, "Gic")]
mydata$Adhesive.Out.Time <- abs(as.numeric(mydata$Adhesive.Out.Time))
mydata$Prep..to.Bond.Time <- abs(as.numeric(mydata$Prep..to.Bond.Time))
mydata$Contamination.Amount <- as.numeric(mydata$Contamination.Amount)
mydata <- na.omit(mydata)

# (Optional) Fix Adhesive.Out.Time with a few user-defined start timestamps
start.timestamps <- sort(c(132218, 381434, 513652, Inf))
for (i in 1:length(mydata$Adhesive.Out.Time)){
  for (j in 1:(length(start.timestamps)-1)){
    if (mydata$Adhesive.Out.Time[i] >= start.timestamps[j] & mydata$Adhesive.Out.Time[i] < start.timestamps[j+1]){
      mydata$Adhesive.Out.Time[i] <- mydata$Adhesive.Out.Time[i] - start.timestamps[j]
    }
  }
}
summary(mydata)


# Creating a categorical variable representing Gic levels
# 2 levels: Low/High
km2 <- kmeans(mydata$Gic, centers = c(min(mydata$Gic), max(mydata$Gic)))
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = as.factor(km2$cluster)))
# 3 levels: Low/Medium/High
km3 <- kmeans(mydata$Gic, centers = c(min(mydata$Gic), mean(mydata$Gic), max(mydata$Gic)))
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = as.factor(km3$cluster)))

# Use 2 levels
Gic.Level <- km2$cluster
Gic.Level[Gic.Level == 1] <- "Low"
Gic.Level[Gic.Level == 2] <- "High"
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Gic.Level))
mydata$Gic <- Gic.Level

# Or use 3 levels
Gic.Level <- km3$cluster
Gic.Level[Gic.Level == 1] <- "Low"
Gic.Level[Gic.Level == 2] <- "Medium"
Gic.Level[Gic.Level == 3] <- "High"
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Gic.Level))
mydata$Gic <- Gic.Level


################################################################################
# Decision Tree and Random Forests for CLASSIFICATION
################################################################################

# http://www.di.fc.ul.pt/~jpn/r/tree/tree.html#random-forests

#
# 1. Regression Trees
# 

tree.model <- ctree(Gic ~ Contamination.Amount +  Adhesive.Out.Time, data = mydata)
plot(mydata$Contamination.Amount, mydata$Adhesive.Out.Time, col = mydata$Gic, pch=20, xlab="Sepal.Length",ylab="Petal.Length")
partition.tree(tree.model, ordvars=c("Contamination.Amount","Adhesive.Out.Time"), add=TRUE)

tree.model <- tree(Gic ~ ., data = mydata)
tree.model <- tree(Sepal.Width ~ Sepal.Length +  Petal.Length, data = iris)
tree.model <- tree(Sepal.Width ~ ., data = iris)
plot(tree.model)
text(tree.model, cex=.75)

plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Sepal.Width, pch=20, xlab="Sepal.Length",ylab="Petal.Length")
partition.tree(tree.model, ordvars=c("Sepal.Length","Petal.Length"), add=TRUE)

summary(tree.model)


#
# 1.0 library(tree)
# 



# 
# 1.1 RPART tree
# 

# grow tree 
fit <- rpart(Gic ~ ., method="class", data=mydata)

# output
print(fit)  # The print function gives an abbreviated output
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree for Gic", cex.main = 1)
text(fit, use.n = TRUE, all = TRUE, cex = 0.7)
# create attractive postscript plot of tree 
post(fit, file = "tree.ps", title = "Classification Tree for Gic")

# cross-validation error helps to to find the optimal cp
printcp(fit) # display the results on complexity parameter 
plotcp(fit) # visualize cross-validation results 
# Typically, you will want to select a tree size that minimizes the cross-validated error
cp.optimal <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
cp.optimal

# prune the tree 
pruned.fit<- prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
summary(pruned.fit)

# plot the pruned tree 
plot(pruned.fit, uniform=TRUE, main="Pruned Classification Tree for Gic")
text(pruned.fit, use.n=TRUE, all=TRUE, cex=.8)
post(pruned.fit, file = "ptree.ps", title = "Pruned Classification Tree for Gic")

# 
# 1.2 ctree
# 
# References:
#   https://cran.r-project.org/web/packages/partykit/vignettes/ctree.pdf

# ctree requires either numerics or factors
mydata.CharAsFactor <- mydata
mydata.CharAsFactor$Surface.Preparation <- as.factor(mydata.CharAsFactor$Surface.Preparation)
mydata.CharAsFactor$Contaminate.Type <- as.factor(mydata.CharAsFactor$Contaminate.Type)

# Conditional Inference Tree for Gic
fit <- ctree(Gic ~ ., data=mydata, )
plot(fit, main="Conditional Inference Tree for Gic")
table(ctree.pred, ctree.predict)

#
# 2. Random forests
#

#
# 2.1 randomForest
#

# Random Forest prediction of Gic data
fit <- randomForest(Gic ~ ., data=mydata, importance = TRUE)
print(fit) # view results 
importance(fit) # importance of each predictor

# Breitman and Cutler:
# (https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr)
# In random forests, there is no need for cross-validation or a separate test 
# set to get an unbiased estimate of the test set error. It is estimated 
# internally, during the run ... Each tree is constructed using a different 
# bootstrap sample from the original data ... This has proven to be unbiased in 
# many tests.

#
# 2.2 cforest
# 

# cforest requires either numerics or factors
mydata.CharAsFactor <- mydata
mydata.CharAsFactor$Surface.Preparation <- as.factor(mydata.CharAsFactor$Surface.Preparation)
mydata.CharAsFactor$Contaminate.Type <- as.factor(mydata.CharAsFactor$Contaminate.Type)

cf.fit <- cforest(Gic ~ ., data = mydata.CharAsFactor, controls = cforest_unbiased())
print(cf.fit)
plot(cf.fit)

tr <- party:::prettytree(cf.fit@ensemble[[1]], names(cf.fit@data@get("input")))  
#tr

plot(new("BinaryTree", tree=tr, data=cf.fit@data, responses=cf.fit@responses))

# 
# 3. Support Vector Machine
# 
# To understand confusionMatrix outout:
#   https://artax.karlin.mff.cuni.cz/r-help/library/caret/html/confusionMatrix.html


svm.fit <- svm(Gic ~ ., data = mydata)
summary(svm.fit)
table(svm.fit$fitted, mydata$Gic)
confusionMatrix(svm.fit$fitted, mydata$Gic)

# Tuning
svm.tune <- tune(svm, Gic ~ ., data = mydata, ranges=list(cost=2^(-1:8), epsilon=c(0.05,0,2)))
print(svm.tune)
plot(svm.tune)
svm.tune$best.parameters

# Tuned result
svm.fit.tuned <- svm.tune$best.model
summary(svm.fit.tuned)
confusionMatrix(svm.fit.tuned$fitted, mydata$Gic)






# From doe4_Hao

# kmeans
km1 <- kmeans(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount")], centers = 6)
scatterplot3d(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount")],
              color = km1$cluster, 
              pch = as.numeric(mydata$Contaminate.Type))
plot3d(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount")],
       col = km1$cluster, 
       pch = as.numeric(mydata$Contaminate.Type))

km2 <- kmeans(mydata[, c("Prep..to.Bond.Time", "Contamination.Amount")], centers = 6)
plot(mydata[, c("Prep..to.Bond.Time", "Contamination.Amount")], 
     col = km2$cluster, pch = as.numeric(mydata$y))

km3 <- kmeans(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount")], centers = 6)
scatterplot3d(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount")], color = km1$cluster)
plot3d(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount")], col = km1$cluster)


#library(mclust)
install.packages("mclust")
library(mclust)
fit <- Mclust(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount", "Contaminate.Type", "Surface.Preparation")])
plot(fit) # plot results 
summary(fit)



# From doe4_Hao


#########################################################################
#########################################################################

## Construct training and testing subsets
set.seed(8341)
samp <- sample(nrow(mydata), 0.6 * nrow(mydata))
mydata.train <- mydata[samp, ]
mydata.test <- mydata[-samp, ]

## Make formulas
# y ~ Adhesive.Out.Time + Prep..to.Bond.Time + Surface.Preparation + Contaminate.Type + Contamination.Amount
formula.1 <- as.formula(paste("y", paste(independent.variables.1, collapse = " + "), sep = " ~ "))
# y ~ Adhesive.Out.Time + Prep..to.Bond.Time + Contamination.Amount
formula.2 <- as.formula(paste("y", paste(independent.variables.2, collapse = " + "), sep = " ~ "))
# y ~ Prep..to.Bond.Time + Surface.Preparation + Contaminate.Type + Contamination.Amount
formula.3 <- as.formula(paste("y", paste(independent.variables.3, collapse = " + "), sep = " ~ "))

## Random Forest (randomForest)
#  Model used: formula.1 - all 5 variables
set.seed(3605)
mydata.rf.1 <- randomForest(formula.1, data = mydata.train)
varImp(mydata.rf.1); varImpPlot(mydata.rf.1)
plot(mydata.rf.1); legend("top", colnames(mydata.rf.1$err.rate), col=1:4, cex=0.8, fill=1:4)
mydata.pred <- predict(mydata.rf.1, newdata = mydata.test)
confusionMatrix(data = mydata.pred, reference = mydata.test$y)

## Conditional importance forest (cforest)
#  Model used: formula.1 - all 5 variables
set.seed(555)
mydata.cforest.1 <- cforest(formula.1, data = mydata.train, control = cforest_unbiased(ntree = 500, mtry = 2), )
varImp(mydata.cforest.1); varImpPlot(mydata.cforest.1)
mydata.cforest.pred.1 <- predict(mydata.cforest.1, newdata = mydata.test)
confusionMatrix(data = mydata.cforest.pred.1, reference = mydata.test$y)

## Random Forest (randomForest) without Categorical Var
#  Model used: formula.2 - only the 3 numerical variables
set.seed(3605)
mydata.rf.2 <- randomForest(formula.2, data = mydata.train)
varImp(mydata.rf.2); varImpPlot(mydata.rf.2)
plot(mydata.rf.2); legend("top", colnames(mydata.rf.2$err.rate), col=1:4, cex=0.8, fill=1:4)
mydata.pred.2 <- predict(mydata.rf.2, newdata = mydata.test)
show(formula.2)
confusionMatrix(data = mydata.pred.2, reference = mydata.test$y)

## Decision tree (ctree)
#  Model used: formula.1 - all 5 variables

set.seed(3345)
mydata.ctree.1 <- ctree(formula.1, data = mydata.train, controls=cforest_control(mtry=2, mincriterion=0))
plot(mydata.ctree.1, type="simple")
confusionMatrix(predict(mydata.ctree.1, newdata = mydata.test), reference = mydata.test$y)





#
# SVM
# 
# 


# Construct training and testing subsets
set.seed(8341)
samp <- sample(nrow(mydata), 0.6 * nrow(mydata))
mydata.train <- mydata[samp, c("Adhesive.Out.Time", "Prep..to.Bond.Time","Contamination.Amount", "y")]
mydata.test <- mydata[-samp, c("Adhesive.Out.Time", "Prep..to.Bond.Time","Contamination.Amount", "y")]
#
mydata.LowAOT.HighPBT.LowAmt <- mydata[which(mydata$Adhesive.Out.Time.Level=="LowAOT" & mydata$Prep..to.Bond.Time>=2 & mydata$Contamination.Amount.Level=="LowAmt"), ]
# 
library(kernlab)
svm.model <- ksvm(y ~ Adhesive.Out.Time + Prep..to.Bond.Time + Contamination.Amount, data = mydata.train)
print(svm.model)
# plot(svm.model, data = mydata.train)
svm.pred <- predict(svm.model, mydata.test)
plot(svm.pred)
table(svm.pred, mydata.test$y)
#




### Construct training and testing subsets
set.seed(8341)
samp <- sample(nrow(mydata), 0.6 * nrow(mydata))
mydata.train <- mydata[samp, ]
mydata.test <- mydata[-samp, ]

## Make formula for random forest
# Add + sign between exploratory variables
var.names <- paste(independent.variables, collapse = " + ")
# Add response variable y and convert to a formula object
rf.formula <- as.formula(paste("y", var.names, sep = " ~ "))

## Model: Random Forest
set.seed(3605)
mydata.rf <- randomForest(rf.formula, data = mydata.train)
plot(mydata.rf, main="Error vs. Number of Trees")
legend("top", colnames(mydata.rf$err.rate), col=1:4, cex=0.8, fill=1:4)
mydata.pred <- predict(mydata.rf, newdata = mydata.test)
confusionMatrix(data = mydata.pred, reference = mydata.test$y)

