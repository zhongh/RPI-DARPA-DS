## INSTRUCTIONS
#  - FIRST, set working directory - where this code and data file locate
#  - First make sure the packages below are properly installed in your R environment
#  - Read in the data file using either the xlsx or the csv reader provided
#  - Run rest of the code line by line to calculate and display the results

## Set working directory (please configure to your setting)
setwd("/Users/Hao/Desktop/RPI-DARPA-DS/")

## Configure necessary packages
install.packages("ipred")
install.packages("party")
install.packages("randomForest")
install.packages("e1071")
install.packages("ggplot2")
install.packages("caret")
install.packages("scatterplot3d")
library(scatterplot3d)
library(ipred)
library(class)
library(party)
library(randomForest)
library(e1071)
library(caret)

## Read data from file

#  If reading from XLSX file, use:
# library(xlsx)
# doe4 <- read.xlsx(file = "", sheetIndex = 1, header = TRUE, as.data.frame = TRUE, stringsAsFactors = FALSE, colClasses = "character")

#  If reading from CSV file, use:
doe4 <- read.csv("Fake Data DOE_IV 1_24_2017.csv", header = TRUE, stringsAsFactors = FALSE)

## Data preparation - CHOOSE ONE METHODS BELOW (currently II)

#  METHOD I (Abandoned -> go to method II)- NA's treated as values
mydata <- doe4
# Adhesive.Out.Time
mydata$Adhesive.Out.Time <- as.numeric(mydata$Adhesive.Out.Time)
mydata$Adhesive.Out.Time[is.na(mydata$Adhesive.Out.Time)] <- mean(mydata$Adhesive.Out.Time, na.rm = TRUE)
# Prep..to.Bond.Time
mydata$Prep..to.Bond.Time <- as.numeric(mydata$Prep..to.Bond.Time)
mydata$Prep..to.Bond.Time[is.na(mydata$Prep..to.Bond.Time)] <- mean(mydata$Prep..to.Bond.Time, na.rm = TRUE)
# Surface.Preperation
mydata$Surface.Preperation[is.na(mydata$Surface.Preperation)] <- "None"
mydata$Surface.Preperation <- as.factor(mydata$Surface.Preperation)
# Contaminate.Type
mydata$Contaminate.Type[is.na(mydata$Contaminate.Type)] <- "None"
mydata$Contaminate.Type <- as.factor(mydata$Contaminate.Type)
# Contamination.Amount
mydata$Contamination.Amount <- as.numeric(mydata$Contamination.Amount)
mydata$Contamination.Amount[is.na(mydata$Contamination.Amount)] <- as.numeric(0)
# Gic -> y = 'Low' or 'High'
mydata$y <- ifelse(mydata$Gic < 5, 'Low', 'High')
mydata$y <- as.factor(mydata$y)

#  Method II - NA's omited
mydata <- doe4
mydata$Adhesive.Out.Time <- as.numeric(mydata$Adhesive.Out.Time)
mydata$Prep..to.Bond.Time <- as.numeric(mydata$Prep..to.Bond.Time)
mydata$Surface.Preperation <- as.factor(mydata$Surface.Preperation)
mydata$Contaminate.Type <- as.factor(mydata$Contaminate.Type)
mydata$Contamination.Amount <- as.numeric(mydata$Contamination.Amount)
# Gic -> y = 'Low' or 'High'
mydata$y <- as.factor(ifelse(mydata$Gic < 5, 'Low', 'High'))
# Omit NA's
mydata <- na.omit(mydata)

##########################################################################
##########################################################################

## Define a few lists of variables of interests for the convenience of further use
# All 5 variables
independent.variables.1 <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Surface.Preperation", "Contaminate.Type", "Contamination.Amount")
# Numerical variables only
independent.variables.2 <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")
# Without the adhesive out time
independent.variables.3 <- c("Prep..to.Bond.Time", "Surface.Preperation", "Contaminate.Type", "Contamination.Amount")
# Categorical variables only
independent.variables.4 <- c("Surface.Preperation", "Contaminate.Type")
independent.variables.5 <- c("Surface.Preperation", "Contaminate.Type", "y")

## A quick exploration
str(mydata)
str(mydata[independent.variables.1])
summary(mydata)
summary(mydata[independent.variables.1])
## Some special treatments*******
# Fix the negative values in Prep..to.Bond.Time
mydata$Prep..to.Bond.Time <- abs(mydata$Prep..to.Bond.Time)
str(mydata)
summary(mydata)
X <- mydata[independent.variables.1]
attach(mydata)

##########################################################################
##########################################################################

## Data exploration

library(lattice)
library(ggplot2)

## Plot

plot(mydata[, c(independent.variables.1, "Gic")])
plot(mydata[, c(independent.variables.2, "Gic")])
# THIS BELOW might be a better one: (Adhesive.Out.Time seems to be not very helpful):
plot(mydata[, c(independent.variables.3, "Gic")])

# Look at every variable
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time))
mydata$Adhesive.Out.Time.Level <- as.factor(ifelse(mydata$Adhesive.Out.Time < 440000, 'Low', 'High'))
detach(mydata); attach(mydata);
ggplot(mydata[which(Adhesive.Out.Time.Level == "Low"), ]) + geom_histogram(aes(x = Adhesive.Out.Time))
ggplot(mydata[which(Adhesive.Out.Time.Level == "High"), ]) + geom_histogram(aes(x = Adhesive.Out.Time))
ggplot(mydata[which(Adhesive.Out.Time.Level == "Low"), ]) + geom_density(aes(x = Adhesive.Out.Time))
ggplot(mydata[which(Adhesive.Out.Time.Level == "High"), ]) + geom_density(aes(x = Adhesive.Out.Time))


table(Contaminate.Type)
table(Surface.Preperation)
table(Contaminate.Type, Surface.Preperation)
ggplot(mydata, aes(x = Contamination.Amount, fill = Contaminate.Type)) + geom_histogram()
# grouped by Contamination Type
ggplot(mydata, aes(x = Contamination.Amount, fill = y)) + geom_histogram() + facet_wrap(~ Contaminate.Type + Surface.Preperation)
ggplot(mydata, aes(x = Contamination.Amount, y = Gic, col = Gic)) + geom_jitter() + facet_wrap(~Contaminate.Type)
# Grouped by Surface.Preparation

# ???
ggplot(mydata, aes(x = Surface.Preperation, fill = y)) + geom_bar() + facet_wrap(~Contaminate.Type) + ggtitle("Surface Preparation by Contaminate Type")
ggplot(mydata, aes(x = Surface.Preperation, fill = y)) + geom_bar() + facet_wrap(~Contaminate.Type) + ggtitle("Surface Preparation by Contaminate Type")

ggplot(mydata, aes(x = Surface.Preperation, y = Gic, fill = Contaminate.Type)) + geom_violin() + facet_wrap(~Contaminate.Type) 

# 1-D Density plot
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preperation, alpha = 0.5)) + facet_wrap(~Contaminate.Type) + ggtitle("Gic Density Plot by Contaminate Type")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type, alpha = 0.5)) + facet_wrap(~Surface.Preperation) + ggtitle("Gic Density Plot by Surface Preparation")
# 2-D Density plot
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Contamination.Amount)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Adhesive.Out.Time)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Prep..to.Bond.Time)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Adhesive.Out.Time, y = Contamination.Amount)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Prep..to.Bond.Time, y = Contamination.Amount)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")



## xyplot
xyplot(mydata$Gic ~ mydata$Contamination.Amount | mydata$Surface.Preperation * mydata$Contaminate.Type)

## (+) https://www.youtube.com/watch?v=Od8gfNOOS9o
X <- mydata[c(independent.variables.2, "Gic")]
X <- subset(mydata, select = -c(Surface.Preperation, Contaminate.Type, y))
summary(X)
cov(X)  
pcal <- princomp(X, scores = TRUE, cor = TRUE)
summary(pcal)
loadings(pcal)
plot(pcal)
screeplot(pcal, type = "line", main = "Screen plot")


## Histogram 
# Just want to see the overall distribution, if any small number of outliers, etc
library()
hist(mydata$Gic)
hist(mydata$Adhesive.Out.Time)
hist(mydata$Contamination.Amount)
hist(mydata$Prep..to.Bond.Time)

## Scatterplot and clusters
scatterplot3d(mydata[, c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")], color = as.numeric(mydata$Contaminate.Type), pch = as.numeric(mydata$Surface.Preperation))
scatterplot3d(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount")], color = as.numeric(mydata$Contaminate.Type), pch = as.numeric(mydata$Surface.Preperation))

## (+) Interactive 3D plot
install.packages("rgl")
library(rgl)
plot3d(cbind(Adhesive.Out.Time, Prep..to.Bond.Time, Contamination.Amount), col = as.numeric(y), size = 10)
plot3d(cbind(X.Cohesive.1, X.Interfacial.1, X.Laminate.1), col = Gic)

## (+) Factor analysis
factanal(mydata[, c(independent.variables.2, "Gic")], factors = 1)


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
fit <- Mclust(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount", "Contaminate.Type", "Surface.Preperation")])
plot(fit) # plot results 
summary(fit)

#########################################################################
#########################################################################

## Construct training and testing subsets
set.seed(8341)
samp <- sample(nrow(mydata), 0.6 * nrow(mydata))
mydata.train <- mydata[samp, ]
mydata.test <- mydata[-samp, ]

## Make formulas
# y ~ Adhesive.Out.Time + Prep..to.Bond.Time + Surface.Preperation + Contaminate.Type + Contamination.Amount
formula.1 <- as.formula(paste("y", paste(independent.variables.1, collapse = " + "), sep = " ~ "))
# y ~ Adhesive.Out.Time + Prep..to.Bond.Time + Contamination.Amount
formula.2 <- as.formula(paste("y", paste(independent.variables.2, collapse = " + "), sep = " ~ "))
# y ~ Prep..to.Bond.Time + Surface.Preperation + Contaminate.Type + Contamination.Amount
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


