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
install.packages("randomForest")
install.packages("e1071")
install.packages("ggplot2")
install.packages("GGally")
install.packages("reshape2")
install.packages("caret") # confusionMatrix
# install.packages("scatterplot3d")
install.packages("kernlab")

## 1. Data Import and Cleaning

# If reading from XLSX file, use:
# library(xlsx)
# mydata <- read.xlsx(file = "", sheetIndex = 1, header = TRUE, as.data.frame = TRUE, stringsAsFactors = FALSE, colClasses = "character")

# If reading from CSV file, use:
mydata <- read.csv("Fake Data DOE_IV 1_24_2017.csv", header = TRUE, stringsAsFactors = FALSE)


# Take a look
# 
# Use str() and summary() to look at variable types and summary statistics; 
str(mydata[c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", "Surface.Preperation", "Contaminate.Type", "Gic")])
summary(mydata[c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", "Surface.Preperation", "Contaminate.Type", "Gic")])
# 
# Use head() or tail() to take a peep at the data
head(mydata[, c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", "Surface.Preperation", "Contaminate.Type", "Gic")])
tail(mydata[, c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", "Surface.Preperation", "Contaminate.Type", "Gic")])


## Data preparation - CHOOSE ONE METHODS BELOW (currently II)

#  METHOD I (Abandoned -> go to method II)- NA's treated as values
# # Adhesive.Out.Time
# mydata$Adhesive.Out.Time <- as.numeric(mydata$Adhesive.Out.Time)
# mydata$Adhesive.Out.Time[is.na(mydata$Adhesive.Out.Time)] <- mean(mydata$Adhesive.Out.Time, na.rm = TRUE)
# # Prep..to.Bond.Time
# mydata$Prep..to.Bond.Time <- as.numeric(mydata$Prep..to.Bond.Time)
# mydata$Prep..to.Bond.Time[is.na(mydata$Prep..to.Bond.Time)] <- mean(mydata$Prep..to.Bond.Time, na.rm = TRUE)
# # Surface.Preparation
# mydata$Surface.Preparation[is.na(mydata$Surface.Preparation)] <- "None"
# mydata$Surface.Preparation <- as.factor(mydata$Surface.Preparation)
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
mydata$Surface.Preparation <- as.factor(mydata$Surface.Preparation)
mydata$Contaminate.Type <- as.factor(mydata$Contaminate.Type)
mydata$Contamination.Amount <- as.numeric(mydata$Contamination.Amount)
# Gic -> y = 'Low' or 'High'
mydata$y <- as.factor(ifelse(mydata$Gic < 5, 'Low', 'High'))
# Omit NA's
mydata <- na.omit(mydata)
# Add a duplicate column of Surface.Preparation with correct column name
mydata$Surface.Preparation <- mydata$Surface.Preparation


# Define a few lists of variables of interests for further use
# All 5 variables
independent.variables.1 <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Surface.Preparation", "Contaminate.Type", "Contamination.Amount")
# Numerical variables only
independent.variables.2 <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")
# Without the adhesive out time
independent.variables.3 <- c("Prep..to.Bond.Time", "Surface.Preparation", "Contaminate.Type", "Contamination.Amount")
# Categorical variables only
independent.variables.4 <- c("Surface.Preparation", "Contaminate.Type")
independent.variables.5 <- c("Surface.Preparation", "Contaminate.Type", "y")



#
## 2. Data exploration
#
library(lattice)
library(ggplot2)

# With dataset "attached", a variable can be access by its own name
attach(mydata)

# Use table() to build a table of the counts at each combination of the factors
table(mydata$y, mydata$Surface.Preparation)
table(mydata$y, mydata$Contaminate.Type)
table(mydata$Contaminate.Type, mydata$Surface.Preparation)

# Plot independent variables and Gic (y) against each other
plot(mydata[, c(independent.variables.all, "Gic")])   
plot(mydata[, c(independent.variables.cat, "Gic")]) # Without 2 categorical variables
plot(mydata[, c(independent.variables.num, "Gic")]) # Without 3 numerical variables
plot(mydata[, c(independent.variables.num, "Gic", "y")]) # Without 3 numerical variables

#
# Take a look at the distribution of each variables (mainly using ggplot2)
#

# Categorical variables
# 
# Use barcharts to look at the distributions (counts of each category)
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type))
# 
# Use color to further visualize how each bar is made up by another factor
# 
# y (High/Low Gic) as colored sections in the bars 
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = y))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = y))
# Contaminate.Type or Surface.Preparation as colored sections in the bars 
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Contaminate.Type))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Surface.Preparation))
#
# We also want to visualize how each bar is made up by another numerical variable. In this case, use violin plot.
#
# On Gic
ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Gic, fill = Surface.Preparation))
ggplot(mydata) + geom_violin(aes(x = Contaminate.Type, y = Gic, fill = Contaminate.Type))
#
# On other 3 independent numerical variables (No pretty results)
# ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Contamination.Amount, fill = Surface.Preparation))
# ggplot(mydata) + geom_violin(aes(x = Contaminate.Type, y = Contamination.Amount, fill = Contaminate.Type))
# ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Adhesive.Out.Time, fill = Surface.Preparation))
# ggplot(mydata) + geom_violin(aes(x = Contaminate.Type, y = Adhesive.Out.Time, fill = Contaminate.Type))
# ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Prep..to.Bond.Time, fill = Surface.Preparation))
# ggplot(mydata) + geom_violin(aes(x = Contaminate.Type, y = Prep..to.Bond.Time, fill = Contaminate.Type))

# Multiple facets by another factor
# 
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = y)) + facet_wrap(~ Contaminate.Type) + ggtitle("Surface Preparation by Contaminate Type")
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = y)) + facet_wrap(~ Surface.Preparation) + ggtitle("Contaminate Type by Surface Preparation")
# 
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Surface.Preparation)) + facet_wrap(~ y) + ggtitle("Contaminate Type by High/Low Gic")
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Contaminate.Type)) + facet_wrap(~ y) + ggtitle("Surface Preparation  by High/Low Gic")
#
ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Gic, fill = Surface.Preparation)) + facet_wrap(~ Contaminate.Type) + ggtitle("Surface Preparation vs Gic, by Contaminate Type")
ggplot(mydata) + geom_violin(aes(x = Contaminate.Type, y = Gic, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation) + ggtitle("Contaminate Type vs Gic, by Surface Preparation")

# Workflow Summary:
# 1) Barplot on one categorical variable
# 2) Represent how each bar is made up by another variable
#     - second variable is factor: add fill color in barplot
#     - second variable is numerical: as the y-axis in violin plot
# 3) Make multiple facets by a 3rd (even a 4th, if it makes sense yet still tidy) variable (factor)

# Comment:
# From domain expert, also inspired by our intuition from the categorical variables
# on experiment design, it appears reasonable that we perform some data analytics
# later for each combination of Surface.Preparation and Contaminae.Type, especially
# those analysis that are more for numerical variables.

# Numerical variables: Use histograms
# 
# For numerical variables we start with histogram instead of barplot. We may also
# use a density plot to visualize the distribution implied by the histogram, 
# although this could be intuitively misleading if abused.
# 
# Adhesive.Out.Time
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = y), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preparation), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = y), bins = 30) + facet_wrap(~ Contaminate.Type)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = y), bins = 30) + facet_wrap(~ Surface.Preparation)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = y), bins = 30) + facet_wrap(~ Surface.Preparation * Contaminate.Type)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type * y)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation * y)


# (+++) To be added after looking at the plot on real data)
# 
# Create 2 Adhesive.Out.Time levels with threshold at 440000 (may be different for real data)
mydata$Adhesive.Out.Time.Level <- as.factor(ifelse(mydata$Adhesive.Out.Time < 440000, 'LowAOT', 'HighAOT'))
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time), bins = 30) + 
  facet_wrap(~ Adhesive.Out.Time.Level, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), bins = 20) + 
  facet_wrap(~ Adhesive.Out.Time.Level, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preparation), bins = 20) + 
  facet_wrap(~ Adhesive.Out.Time.Level, scales = "free")
#
#
# Comment:
# How are Adhesive.Out.Time, Prep..to.Bond.Time, Contamination.Amount related to 
# Surface.Preparation and Contaminae.Type?
# Are Adhesive.Out.Time, Prep..to.Bond.Time, and Contamination.Amount also
# designed values?


# Prep..to.Bond.Time
# 
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = y), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation), bins = 30)
ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = y), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation), alpha = 0.5)
#
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = y), bins = 30) + facet_wrap(~ Surface.Preparation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = y), bins = 30) + facet_wrap(~ Contaminate.Type, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = y), bins = 30) + facet_wrap(~ Surface.Preparation * Contaminate.Type, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation * y, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type * y, scales = "free")
#
# ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = y), alpha = 0.5) + facet_wrap(~ Surface.Preparation, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = y), alpha = 0.5) + facet_wrap(~ Contaminate.Type, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = y), alpha = 0.5) + facet_wrap(~ Surface.Preparation * Contaminate.Type, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~ Surface.Preparation, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation), alpha = 0.5) + facet_wrap(~ Contaminate.Type, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~ Surface.Preparation * y, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation), alpha = 0.5) + facet_wrap(~ Contaminate.Type * y, scales = "free")
#
#
# Comment:
# Again, it is intuitive that Surface.Preparation and Contaminae.Type are
# designed values.


# Contamination.Amount
# 
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = y), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preparation), bins = 30)
ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = y), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Contaminate.Type), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Surface.Preparation), alpha = 0.5)
#
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = y), bins = 30) + facet_wrap(~ Surface.Preparation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = y), bins = 30) + facet_wrap(~ Contaminate.Type, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = y), bins = 30) + facet_wrap(~ Surface.Preparation * Contaminate.Type, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation * y, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type * y, scales = "free")
#
# ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = y), alpha = 0.5) + facet_wrap(~ Surface.Preparation, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = y), alpha = 0.5) + facet_wrap(~ Contaminate.Type, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = y), alpha = 0.5) + facet_wrap(~ Surface.Preparation * Contaminate.Type, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~ Surface.Preparation, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Surface.Preparation), alpha = 0.5) + facet_wrap(~ Contaminate.Type, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~ Surface.Preparation * y, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Surface.Preparation), alpha = 0.5) + facet_wrap(~ Contaminate.Type * y, scales = "free")
#
#
# (+++) To be added after looking at the plot on real data)
#
# Try separate Contamination.Amount at a threshold of 5000
mydata$Contamination.Amount.Level <- as.factor(ifelse(mydata$Contamination.Amount < 5000, 'LowAmt', 'HighAmt'))
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preparation)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Surface.Preparation), alpha = 0.5) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
#

# Gic
# 
ggplot(mydata) + geom_histogram(aes(x = Gic), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation), bins = 30)
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preparation), alpha = 0.5)
#
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation * y, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type * y, scales = "free")
#
# ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~ Surface.Preparation, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preparation), alpha = 0.5) + facet_wrap(~ Contaminate.Type, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~ Surface.Preparation * y, scales = "free")
# ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preparation), alpha = 0.5) + facet_wrap(~ Contaminate.Type * y, scales = "free")
#

# 3 numerical variables faceted by both Contaminate.Type and Surface.Preparation
# 
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = y), bins = 30) + facet_wrap(~ Contaminate.Type + Surface.Preparation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = y), bins = 30) + facet_wrap(~ Contaminate.Type + Surface.Preparation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = y), bins = 30) + facet_wrap(~ Contaminate.Type + Surface.Preparation, scales = "free")
#
# 3 numerical variables vs Gic (and y)
#
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Gic, col = y)) + facet_wrap(~ Contaminate.Type + Surface.Preparation, scales = "free")
ggplot(mydata) + geom_jitter(aes(x = Adhesive.Out.Time, y = Gic, col = y)) + facet_wrap(~ Contaminate.Type + Surface.Preparation, scales = "free")
ggplot(mydata) + geom_jitter(aes(x = Prep..to.Bond.Time, y = Gic, col = y)) + facet_wrap(~ Contaminate.Type + Surface.Preparation, scales = "free")
#
# 3 numerical variables' correlation in different groups
#
library(GGally)
library(reshape2)
#
#
ggcorr(mydata)
ggcorr(mydata[, independent.variables.num])
# ggplot(data = melt(cor(mydata[, independent.variables.num])), aes(x=Var1, y=Var2, fill=value)) + geom_tile()
#
# Correlation in each subset combining Suraface Prep and Contamination Type
#
for (i in levels(mydata$Surface.Preparation)) {
  for (j in levels(mydata$Contaminate.Type)) {
    cat(paste("\nSurface.Preparation =", i, "and Contaminate.Type =", j, ":\n"))
    print(cor(mydata[which(mydata$Surface.Preparation==i & mydata$Contaminate.Type==j), independent.variables.num]))
    # print(ggplot(data = melt(cor(mydata[which(mydata$Surface.Preparation==i & mydata$Contaminate.Type==j), independent.variables.num])), aes(x=Var1, y=Var2, fill=value)) + geom_tile())
    print(ggcorr(mydata[which(mydata$Surface.Preparation==i & mydata$Contaminate.Type==j), independent.variables.num]))
    }
}

# Comment: (+++) To be updated upon seeing patterns on real data!
# No obvious trend observed between Gic and those 3 variables.
# Further investigation needed, since we know this is not the case...


# (+++) Might not be added - basically reflects the jitter plot
# Gic (to be continued here)
# Histograms
ggplot(mydata) + geom_histogram(aes(x = Gic), bins = 30) + ggtitle("Gic Histogram for All")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation), bins = 30) + ggtitle("Gic Histogram for Surface.Preparation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + ggtitle("Gic Histogram for Contaminate.Type")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Adhesive.Out.Time.Level), bins = 30) + ggtitle("Gic Histogram for Surface.Preparation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contamination.Amount.Level), bins = 30) + ggtitle("Gic Histogram for Contaminate.Type")
#
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation), bins = 30) + facet_wrap(~Contaminate.Type) + ggtitle("Gic Histogram by Contaminate Type")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + facet_wrap(~Surface.Preparation) + ggtitle("Gic Histogram by Surface Preparation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + facet_wrap(~Contamination.Amount.Level) + ggtitle("Gic Histogram by Surface Preparation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation), bins = 30) + facet_wrap(~Contamination.Amount.Level) + ggtitle("Gic Histogram by Surface Preparation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + facet_wrap(~Adhesive.Out.Time.Level) + ggtitle("Gic Histogram by Surface Preparation")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation), bins = 30) + facet_wrap(~Adhesive.Out.Time.Level) + ggtitle("Gic Histogram by Surface Preparation")
#
# 1-D Density plot
ggplot(mydata) + geom_density(aes(x = Gic), alpha = 0.5) + ggtitle("Gic Density for All")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preparation), alpha = 0.5) + ggtitle("Gic Density for Surface.Preparation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5) + ggtitle("Gic Density for Contaminate.Type")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Adhesive.Out.Time.Level), alpha = 0.5) + ggtitle("Gic Density for Surface.Preparation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contamination.Amount.Level), alpha = 0.5) + ggtitle("Gic Density for Contaminate.Type")
#
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preparation, alpha = 0.5)) + facet_wrap(~Contaminate.Type) + ggtitle("Gic Density Plot by Contaminate Type")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type, alpha = 0.5)) + facet_wrap(~Surface.Preparation) + ggtitle("Gic Density Plot by Surface Preparation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~Contamination.Amount.Level) + ggtitle("Gic Density by Surface Preparation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preparation), alpha = 0.5) + facet_wrap(~Contamination.Amount.Level) + ggtitle("Gic Density by Surface Preparation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~Adhesive.Out.Time.Level) + ggtitle("Gic Density by Surface Preparation")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preparation), alpha = 0.5) + facet_wrap(~Adhesive.Out.Time.Level) + ggtitle("Gic Density by Surface Preparation")
#
# 2-D Density plot
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Contamination.Amount)) + facet_wrap(~Surface.Preparation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Adhesive.Out.Time)) + facet_wrap(~Surface.Preparation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Prep..to.Bond.Time)) + facet_wrap(~Surface.Preparation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Adhesive.Out.Time, y = Contamination.Amount)) + facet_wrap(~Surface.Preparation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time)) + facet_wrap(~Surface.Preparation) + ggtitle("2-D Density Plot by Surface Preparation")
ggplot(mydata) + geom_density_2d(aes(x = Prep..to.Bond.Time, y = Contamination.Amount)) + facet_wrap(~Surface.Preparation) + ggtitle("2-D Density Plot by Surface Preparation")
# which is another representation of a 2-D jitter plots (scatterplots)
#
## (++++) Plots That could be used when having information on levels in num vars
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
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Prep..to.Bond.Time, color = y)) + facet_wrap(~ Surface.Preparation * Contaminate.Type, scales = "free")
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Prep..to.Bond.Time, color = Gic)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_jitter(aes(x = Contamination.Amount, y = Prep..to.Bond.Time, color = Gic)) + facet_wrap(~ Surface.Preparation * Contaminate.Type, scales = "free")
#



#
# 3D visualizations
#
library(rgl)
#
plot3d(cbind(mydata$Adhesive.Out.Time, mydata$Prep..to.Bond.Time, mydata$Contamination.Amount), col = as.numeric(mydata$Gic))


install.packages("plot3D")
install.packages("plot3Drgl")
library(scatterplot3d)
library(plot3D)
library(plot3Drgl)
#
scatterplot3d(x = mydata$Adhesive.Out.Time, y= mydata$Prep..to.Bond.Time, z = mydata$Contamination.Amount, color = as.numeric(mydata$Surface.Preparation))
legend("topright", levels(mydata$Surface.Preparation), pch = 16, col = as.numeric(mydata$Surface.Preparation))
#
scatter3Drgl(x = mydata$Adhesive.Out.Time, y= mydata$Prep..to.Bond.Time, z = mydata$Contamination.Amount, col = as.numeric(mydata$Surface.Preparation))
scatter3D(x = mydata$Adhesive.Out.Time, y= mydata$Prep..to.Bond.Time, z = mydata$Contamination.Amount, col = as.numeric(mydata$Surface.Preparation))


plot3d(cbind(mydata$Adhesive.Out.Time, mydata$Prep..to.Bond.Time, mydata$Contamination.Amount), col = as.numeric(mydata$Gic))
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






## (+) https://www.youtube.com/watch?v=Od8gfNOOS9o
X <- mydata[c(independent.variables.2, "Gic")]
X <- subset(mydata, select = -c(Surface.Preparation, Contaminate.Type, y))
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
scatterplot3d(mydata[, c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")], color = as.numeric(mydata$Contaminate.Type), pch = as.numeric(mydata$Surface.Preparation))
scatterplot3d(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount")], color = as.numeric(mydata$Contaminate.Type), pch = as.numeric(mydata$Surface.Preparation))

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
fit <- Mclust(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount", "Contaminate.Type", "Surface.Preparation")])
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


