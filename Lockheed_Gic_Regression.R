# Lockheed: Regression on Gic
# Author: Hao Zhong

# (CAUTION: This will clean up the whole workspace)
# Initialize environment and path (when restarting RStudio)
library(rstudioapi)
rm(list = ls())
setwd(dirname(getActiveDocumentContext()$path))
getwd()

# Load packages
library(ggplot2)
library(rpart)
library(party)
library(randomForest)

# 
# Data Import and Preparation
# 

# Use with caution as this will automatically clean up the whole workspace and
# redo all the data import and preparation steps. If you feel unsure, please open 
# this file and run it manually.
source("Lockheed.R")

################################################################################
# Decision Tree and Random Forests
################################################################################

# 
# Decision tree: rpart
# http://www.statmethods.net/advstats/cart.html
# 

# grow tree 
tree.fit <- rpart(Gic ~ ., method = "anova", data=mydata)

printcp(tree.fit) # display the results 
plotcp(tree.fit) # visualize cross-validation results 
summary(tree.fit) # detailed summary of splits

# plot tree 
plot(tree.fit, uniform=TRUE, main="Regression Tree for Gic")
text(tree.fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(tree.fit, file = "tree.ps", title = "Regression Tree for Gic")

actual <- mydata$Gic
tree.pred <- predict(fit, mydata)
tree.resid <- tree.pred - actual

# RMSE
RMSE <- sqrt(mean(tree.resid^2))
RMSE

# Prediction plot
plot(actual, tree.pred, xlim = c(0, 15), ylim = c(0, 15))
abline(0, 1)
segments(actual, actual, actual, tree.pred, col="red")

# Residuals plot
plot(actual, tree.resid)
abline(0, 0)
segments(actual, 0, actual, tree.resid, col="red")

# Use ggplot for residuals plot
g <- ggplot(data = NULL, aes(x = actual, y = tree.pred)) + geom_point(shape = 1) + 
  geom_segment(aes(xend = actual, yend = actual), alpha = 0.1) + 
  geom_abline(intercept = 0, slope = 1, alpha = 0.5) +
  theme_bw()
g

# 
# Conditional Inference Tree: ctree
# 

# cforest requires either numerics or factors
mydata.CharAsFactor <- mydata
mydata.CharAsFactor$Surface.Preparation <- as.factor(mydata.CharAsFactor$Surface.Preparation)
mydata.CharAsFactor$Contaminate.Type <- as.factor(mydata.CharAsFactor$Contaminate.Type)

ctree.fit <- ctree(Gic ~ ., data = mydata.CharAsFactor, controls = ctree_control(maxdepth = 3))
plot(ctree.fit, main="Conditional Inference Tree for Gic")
ctree.pred <- predict(ctree.fit, mydata.CharAsFactor)
ctree.resid <- ctree.pred - actual

# Prediction plot
plot(actual, ctree.pred, xlim = c(0, 15), ylim = c(0, 15))
abline(0, 1)
segments(actual, actual, actual, ctree.pred, col="red")

# Residuals plot
plot(actual, ctree.resid)
abline(0, 0)
segments(actual, 0, actual, ctree.resid, col="red")

# 
# Random Forest prediction of Gic
# 

mydata.dummy <- cbind(dummy(mydata, int = TRUE), mydata[, c(doe.nums, "Gic")])
mydata.dummy.standard <- cbind(dummy(mydata, int = TRUE, ), scale(mydata[, doe.nums]), mydata["Gic"])
mydata.dummy.fullrank <- as.data.frame(model.matrix(~., mydata)[, -1]) # Won't work because it has spaces in some variable names

fit <- randomForest(Gic ~ .,   data = mydata.dummy.fullrank)
print(fit) # view results 
importance(fit) # importance of each predictor
varImpPlot(fit)
plot(fit, main="Error vs. Number of Trees")

# On Surface.Preparation
# grow tree 
fit <- rpart(Surface.Preparation ~ ., method = "class", data=mydata)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree for Surface.Preparation")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "tree.ps", title = "Classification Tree for Surface.Preparation", pretty = 3)
