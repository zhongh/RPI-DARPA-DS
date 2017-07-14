# Lockheed: Data Preparation
# Author: Hao Zhong

# (CAUTION: This will clean up the whole workspace)
# Initialize environment and path (when restarting RStudio)
library(rstudioapi)
rm(list = ls())
# setwd(dirname(getActiveDocumentContext()$path))
getwd()

# Import raw data from file
mydata.all <- read.csv("data/Lockheed.csv", header = TRUE, stringsAsFactors = FALSE)

# Rename Surface.Preperation to Surface.Preparation
names(mydata.all)[names(mydata.all) == 'Surface.Preperation'] <- 'Surface.Preparation'

# Define groups of variables for convenience in further use
doe.all <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", 
                    "Surface.Preparation", "Contaminate.Type")
doe.num <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")
doe.cat <- c("Surface.Preparation", "Contaminate.Type")
failure.modes <- c("X.Laminate.1", "X.Interfacial.1", "X.Cohesive.1")

# Correct variable types
mydata.all$Adhesive.Out.Time <- abs(as.numeric(mydata.all$Adhesive.Out.Time))
mydata.all$Prep..to.Bond.Time <- abs(as.numeric(mydata.all$Prep..to.Bond.Time))
mydata.all$Contamination.Amount <- as.numeric(mydata.all$Contamination.Amount)
mydata.all$Gic <- as.numeric(mydata.all$Gic)
mydata.all$Surface.Preparation <- as.factor(mydata.all$Surface.Preparation)
mydata.all$Contaminate.Type <- as.factor(mydata.all$Contaminate.Type)

# Subset to 'mydata' which contains only needed variables
mydata <- mydata.all[, c(doe.all, "Gic")]

# Omit NA's: 508 rows of data
mydata <- na.omit(mydata)

# Create a new dataframe where Gic is a categorical response variable 
# using 5 as the cutoff value
Gic.Level <- as.factor(ifelse(mydata$Gic >= 5, "High", "Low"))

# (Optional) Fix Adhesive.Out.Time with a few user-defined start timestamps
# start.timestamps <- sort(c(132218, 381434, 513652, Inf))
# for (i in 1:length(mydata$Adhesive.Out.Time)){
#   for (j in 1:(length(start.timestamps)-1)){
#     if (mydata$Adhesive.Out.Time[i] >= start.timestamps[j] & mydata$Adhesive.Out.Time[i] < start.timestamps[j+1]){
#       mydata$Adhesive.Out.Time[i] <- mydata$Adhesive.Out.Time[i] - start.timestamps[j]
#     }
#   }
# }
# summary(mydata)