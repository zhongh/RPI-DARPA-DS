## General Instructions
#  - FIRST, set working directory - where this code and the data file(s) locate
#  - Make sure the packages below are properly installed in your R environment
#  - Read in the data file using either the xlsx or the csv reader provided
#  - Run rest of the code line by line to calculate and display the results

## Set working directory (please configure the path to your setting)
setwd("/Users/Hao/Projects/RPI-DARPA-DS/")

## Install necessary packages
# install.packages("ipred")
# install.packages("party")
# install.packages("randomForest")
# install.packages("e1071")
# install.packages("ggplot2")
# install.packages("caret")
# install.packages("scatterplot3d")
# install.packages("kernlab")

## 1. Data Import and Cleaning

# If reading from XLSX file, use:
# library(xlsx)
# doe4 <- read.xlsx(file = "", sheetIndex = 1, header = TRUE, as.data.frame = TRUE, stringsAsFactors = FALSE, colClasses = "character")

# If reading from CSV file, use:
doe4 <- read.csv("Fake Data DOE_IV 1_24_2017.csv", header = TRUE, stringsAsFactors = FALSE)

## Data preparation - CHOOSE ONE METHODS BELOW (currently II)
mydata <- doe4

#  METHOD I (Abandoned -> go to method II)- NA's treated as values
# # Adhesive.Out.Time
# mydata$Adhesive.Out.Time <- as.numeric(mydata$Adhesive.Out.Time)
# mydata$Adhesive.Out.Time[is.na(mydata$Adhesive.Out.Time)] <- mean(mydata$Adhesive.Out.Time, na.rm = TRUE)
# # Prep..to.Bond.Time
# mydata$Prep..to.Bond.Time <- as.numeric(mydata$Prep..to.Bond.Time)
# mydata$Prep..to.Bond.Time[is.na(mydata$Prep..to.Bond.Time)] <- mean(mydata$Prep..to.Bond.Time, na.rm = TRUE)
# # Surface.Preperation
# mydata$Surface.Preperation[is.na(mydata$Surface.Preperation)] <- "None"
# mydata$Surface.Preperation <- as.factor(mydata$Surface.Preperation)
# # Contaminate.Type
# mydata$Contaminate.Type[is.na(mydata$Contaminate.Type)] <- "None"
# mydata$Contaminate.Type <- as.factor(mydata$Contaminate.Type)
# # Contamination.Amount
# mydata$Contamination.Amount <- as.numeric(mydata$Contamination.Amount)
# mydata$Contamination.Amount[is.na(mydata$Contamination.Amount)] <- as.numeric(0)
# # Gic -> y = 'Low' or 'High'
# mydata$y <- ifelse(mydata$Gic < 5, 'Low', 'High')
# mydata$y <- as.factor(mydata$y)

#  Method II - NA's omited
mydata$Adhesive.Out.Time <- as.numeric(mydata$Adhesive.Out.Time)
mydata$Prep..to.Bond.Time <- as.numeric(mydata$Prep..to.Bond.Time)
mydata$Surface.Preperation <- as.factor(mydata$Surface.Preperation)
mydata$Contaminate.Type <- as.factor(mydata$Contaminate.Type)
mydata$Contamination.Amount <- as.numeric(mydata$Contamination.Amount)
# Gic -> y = 'Low' or 'High'
mydata$y <- as.factor(ifelse(mydata$Gic < 5, 'Low', 'High'))
# Omit NA's
mydata <- na.omit(mydata)


# Define a few lists of variables of interests for further use
# All 5 variables
independent.variables.1 <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Surface.Preperation", "Contaminate.Type", "Contamination.Amount")
# Numerical variables only
independent.variables.2 <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")
# Without the adhesive out time
independent.variables.3 <- c("Prep..to.Bond.Time", "Surface.Preperation", "Contaminate.Type", "Contamination.Amount")
# Categorical variables only
independent.variables.4 <- c("Surface.Preperation", "Contaminate.Type")
independent.variables.5 <- c("Surface.Preperation", "Contaminate.Type", "y")


# Further examine the data and some special treatments
# Use str() and summary() to look at variable types and summary statistics; 
str(mydata[, c(independent.variables.1, "Gic", "y")])
summary(mydata[, c(independent.variables.1, "Gic", "y")])
# Use head() or tail() to take a peep at the data
head(mydata[, c(independent.variables.1, "Gic", "y")])
tail(mydata[, c(independent.variables.1, "Gic", "y")])
# (*) FIX weird negative values observed in Prep..to.Bond.Time
mydata$Prep..to.Bond.Time <- abs(mydata$Prep..to.Bond.Time)

## Take a look again
str(mydata[c(independent.variables.1, "Gic", "y")])
summary(mydata[c(independent.variables.1, "Gic", "y")])




#
## 2. Data exploration
#
library(lattice)
library(ggplot2)

# With dataset "attached", a variable can be access by its own name
attach(mydata)

# Use table() to build a table of the counts at each combination of the factors
table(mydata$y, mydata$Surface.Preperation)
table(mydata$y, mydata$Contaminate.Type)
table(mydata$Contaminate.Type, mydata$Surface.Preperation)

# X <- mydata[independent.variables.1]

# Plot independent variables and Gic (y) against each other
plot(mydata[, c(independent.variables.1, "Gic")])   
plot(mydata[, c(independent.variables.2, "Gic")]) # Without 2 categorical variables
plot(mydata[, c(independent.variables.4, "Gic")]) # Without 3 numerical variables
plot(mydata[, c(independent.variables.5, "Gic")]) # Without 3 numerical variables

#
# Take a look at the distribution of each variables (mainly using ggplot2)
#

# Categorical variables: Use barcharts to look at the distributions
ggplot(mydata) + geom_bar(aes(x = Surface.Preperation))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type))
# Represent y (High/Low Gic) as colored sections in the bars
ggplot(mydata) + geom_bar(aes(x = Surface.Preperation, fill = y))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = y))
# Add facet_wrap() features to generate subgraphs by another factor
ggplot(mydata) + geom_bar(aes(x = Surface.Preperation, fill = y)) + facet_wrap(~ Contaminate.Type) + ggtitle("Surface Preparation Barplot by Contaminate Type")
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = y)) + facet_wrap(~ Surface.Preperation) + ggtitle("Contaminate Type Barplot by Surface Preparatio")
# Use Surface.Preperation or Contaminae.Type as colored sections in each bar
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Surface.Preperation))
ggplot(mydata) + geom_bar(aes(x = Surface.Preperation, fill = Contaminate.Type))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Surface.Preperation)) + facet_wrap(~ y) + ggtitle("Contaminate Type Barplot by High/Low Gic")
ggplot(mydata) + geom_bar(aes(x = Surface.Preperation, fill = Contaminate.Type)) + facet_wrap(~ y) + ggtitle("Surface Preparation Barplot by High/Low Gic")

# Comment:
# From domain expert, also inspired by our intuition from the categorical variables
# on experiment design, it appears reasonable that we perform some data analytics
# later for each combination of Surface.Preperation and Contaminae.Type, especially
# those analysis that are more for numerical variables.

# Numerical variables: Use histograms

# Adhesive.Out.Time
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preperation), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preperation), bins = 30) +
  facet_wrap(~ Contaminate.Type)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), bins = 30) +
  facet_wrap(~ Surface.Preperation)
# Create 2 Adhesive.Out.Time levels with threshold at 440000 (may be different for real data)
mydata$Adhesive.Out.Time.Level <- as.factor(ifelse(mydata$Adhesive.Out.Time < 440000, 'LowAOT', 'HighAOT'))
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time), bins = 30) + 
  facet_wrap(~ Adhesive.Out.Time.Level, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), bins = 20) + 
  facet_wrap(~ Adhesive.Out.Time.Level, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preperation), bins = 20) + 
  facet_wrap(~ Adhesive.Out.Time.Level, scales = "free")

# Comment:
# How are Adhesive.Out.Time, Prep..to.Bond.Time, Contamination.Amount related to 
# Surface.Preperation and Contaminae.Type?
# Are Adhesive.Out.Time, Prep..to.Bond.Time, and Contamination.Amount also
# designed values?

# Prep..to.Bond.Time
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Surface.Preperation), bins = 30)
ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Surface.Preperation), alpha = 0.5)
# Note that the values in the leftmost bin are around 1.4, not zeros.
# Separate the plots by another factor
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), bins = 30) +
  facet_wrap(~ Surface.Preperation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Surface.Preperation), bins = 30) +
  facet_wrap(~ Contaminate.Type, scales = "free")
ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), alpha = 0.5) +
  facet_wrap(~ Surface.Preperation, scales = "free")
ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Surface.Preperation), alpha = 0.5) +
  facet_wrap(~ Contaminate.Type, scales = "free")

# Comment:
# Again, it is intuitive that Surface.Preperation and Contaminae.Type are
# designed values.

# Contamination.Amount
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preperation), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type), bins = 30)
# Try separate Contamination.Amount at a threshold of 5000
mydata$Contamination.Amount.Level <- as.factor(ifelse(mydata$Contamination.Amount < 5000, 'LowAmt', 'HighAmt'))
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preperation)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Surface.Preperation), alpha = 0.5) + facet_wrap(~ Contamination.Amount.Level, scales = "free")

# 3 numerical variables faceted by both Contaminate.Type and Surface.Preperation
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = y)) + facet_wrap(~ Contaminate.Type + Surface.Preperation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = y)) + facet_wrap(~ Contaminate.Type + Surface.Preperation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = y)) + facet_wrap(~ Contaminate.Type + Surface.Preperation, scales = "free")

# 3 numerical variables vs Gic (and y)
#
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Gic, col = Gic)) + facet_wrap(~ Contaminate.Type + Surface.Preperation, scales = "free")
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Gic, col = y)) + facet_wrap(~ Contaminate.Type + Surface.Preperation, scales = "free")
#
ggplot(mydata) + geom_jitter(aes(x = Adhesive.Out.Time, y = Gic, col = Gic)) + facet_wrap(~ Contaminate.Type + Surface.Preperation, scales = "free")
ggplot(mydata) + geom_jitter(aes(x = Adhesive.Out.Time, y = Gic, col = y)) + facet_wrap(~ Contaminate.Type + Surface.Preperation, scales = "free")
#
ggplot(mydata) + geom_jitter(aes(x = Prep..to.Bond.Time, y = Gic, col = Gic)) + facet_wrap(~ Contaminate.Type + Surface.Preperation, scales = "free")
ggplot(mydata) + geom_jitter(aes(x = Prep..to.Bond.Time, y = Gic, col = y)) + facet_wrap(~ Contaminate.Type + Surface.Preperation, scales = "free")

# Comment:
# No obvious trend observed between Gic and those 3 variables.
# Further investigation needed, since we know this is not the case...

# Gic (to be continued here)
# Histograms
ggplot(mydata) + geom_histogram(aes(x = Gic), bins = 30) + ggtitle("Gic Histogram for All")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preperation), bins = 30) + ggtitle("Gic Histogram for Surface.Preperation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + ggtitle("Gic Histogram for Contaminate.Type")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Adhesive.Out.Time.Level), bins = 30) + ggtitle("Gic Histogram for Surface.Preperation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contamination.Amount.Level), bins = 30) + ggtitle("Gic Histogram for Contaminate.Type")
#
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preperation), bins = 30) + facet_wrap(~Contaminate.Type) + ggtitle("Gic Histogram by Contaminate Type")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + facet_wrap(~Surface.Preperation) + ggtitle("Gic Histogram by Surface Preparation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + facet_wrap(~Contamination.Amount.Level) + ggtitle("Gic Histogram by Surface Preparation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preperation), bins = 30) + facet_wrap(~Contamination.Amount.Level) + ggtitle("Gic Histogram by Surface Preparation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + facet_wrap(~Adhesive.Out.Time.Level) + ggtitle("Gic Histogram by Surface Preparation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preperation), bins = 30) + facet_wrap(~Adhesive.Out.Time.Level) + ggtitle("Gic Histogram by Surface Preparation")
#
# 1-D Density plot
ggplot(mydata) + geom_density(aes(x = Gic), alpha = 0.5) + ggtitle("Gic Density for All")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preperation), alpha = 0.5) + ggtitle("Gic Density for Surface.Preperation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5) + ggtitle("Gic Density for Contaminate.Type")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Adhesive.Out.Time.Level), alpha = 0.5) + ggtitle("Gic Density for Surface.Preperation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contamination.Amount.Level), alpha = 0.5) + ggtitle("Gic Density for Contaminate.Type")
#
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preperation, alpha = 0.5)) + facet_wrap(~Contaminate.Type) + ggtitle("Gic Density Plot by Contaminate Type")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type, alpha = 0.5)) + facet_wrap(~Surface.Preperation) + ggtitle("Gic Density Plot by Surface Preparation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~Contamination.Amount.Level) + ggtitle("Gic Density by Surface Preparation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preperation), alpha = 0.5) + facet_wrap(~Contamination.Amount.Level) + ggtitle("Gic Density by Surface Preparation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~Adhesive.Out.Time.Level) + ggtitle("Gic Density by Surface Preparation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preperation), alpha = 0.5) + facet_wrap(~Adhesive.Out.Time.Level) + ggtitle("Gic Density by Surface Preparation")
#
# 2-D Density plot
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Contamination.Amount)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Adhesive.Out.Time)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Prep..to.Bond.Time)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Adhesive.Out.Time, y = Contamination.Amount)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Prep..to.Bond.Time, y = Contamination.Amount)) + facet_wrap(~Surface.Preperation) + ggtitle("2-D Density Plot by Surface Preparation")
# which is another representation of a 2-D jitter plots (scatterplots)

#
#
# SVM
# 
# 
# Plots
# 
# x = Adhesive.Out.Time, y = Prep..to.Bond.Time
ggplot(mydata) + geom_jitter(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time, color = y))
ggplot(mydata) + geom_jitter(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time, color = y)) + facet_wrap(~ Adhesive.Out.Time.Level, scales = "free")
#
# x = Adhesive.Out.Time, y = Contamination.Amount
ggplot(mydata) + geom_jitter(aes(x = Adhesive.Out.Time, y = Contamination.Amount, color = y))
ggplot(mydata) + geom_jitter(aes(x = Adhesive.Out.Time, y = Contamination.Amount, color = y)) + facet_wrap(~ Adhesive.Out.Time.Level, scales = "free")
#
# x = Contamination.Amount, y = Prep..to.Bond.Time
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Prep..to.Bond.Time, color = y)) 
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Prep..to.Bond.Time, color = y)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Prep..to.Bond.Time, color = y)) + facet_wrap(~ Surface.Preperation * Contaminate.Type, scales = "free")
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Prep..to.Bond.Time, color = Gic)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Prep..to.Bond.Time, color = Gic)) + facet_wrap(~ Surface.Preperation * Contaminate.Type, scales = "free")
#
library(rgl)
plot3d(cbind(Adhesive.Out.Time, Prep..to.Bond.Time, Contamination.Amount), col = as.numeric(mydata.train$y))
plot3d(cbind(mydata[which(mydata$Adhesive.Out.Time.Level=="LowAOT"), ]$Adhesive.Out.Time, 
             mydata[which(mydata$Adhesive.Out.Time.Level=="LowAOT"), ]$Prep..to.Bond.Time, 
             mydata[which(mydata$Adhesive.Out.Time.Level=="LowAOT"), ]$Contamination.Amount), 
       col = as.numeric(mydata[which(mydata$Adhesive.Out.Time.Level=="LowAOT"), ]$y), 
       size = 20,
       xlab = "Adhesive Out Time", ylab = "Prep to Bond Time", zlab = "Contamination Amount",
       main = "y ~ Adhesive.Out.Time + Prep..to.Bond.Time + Contamination.Amount (at Low AOT")
plot3d(cbind(mydata[which(mydata$Adhesive.Out.Time.Level=="HighAOT"), ]$Adhesive.Out.Time, 
             mydata[which(mydata$Adhesive.Out.Time.Level=="HighAOT"), ]$Prep..to.Bond.Time, 
             mydata[which(mydata$Adhesive.Out.Time.Level=="HighAOT"), ]$Contamination.Amount), 
       col = as.numeric(mydata[which(mydata$Adhesive.Out.Time.Level=="HighAOT"), ]$y), 
       size = 20,
       xlab = "Adhesive Out Time", ylab = "Prep to Bond Time", zlab = "Contamination Amount",
       main = "y ~ Adhesive.Out.Time + Prep..to.Bond.Time + Contamination.Amount (at High AOT")
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




