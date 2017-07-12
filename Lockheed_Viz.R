# Lockheed: Data Exploration, Summary, and Visualizations
# Author: Hao Zhong

# Load packages
library(rstudioapi)
library(ggplot2)
library(GGally)

# 
# Data Import and Preparation
# 

# Use with caution as this will automatically clean up the whole workspace and
# redo all the data import and preparation steps. If you feel unsure, please open 
# this file and run it manually.
source("Lockheed.R")

# 
# Data Exploration
# 

# Use table() to build a table of the counts at each combination of the factors
# table(y, mydata$Surface.Preparation)
# table(y, mydata$Contaminate.Type)
table(mydata$Contaminate.Type, mydata$Surface.Preparation)

# Parallel coordinates plot
doe.nums <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")
doe.cats <- c("Surface.Preparation", "Contaminate.Type")
mydata.scaled <- cbind(scale(mydata[, doe.nums]), mydata[, c(doe.cats, "Gic")])
ggparcoord(mydata.scaled, columns = c(1,2,3,6), groupColumn = "Contaminate.Type", scale = 'globalminmax') + facet_wrap(~ Surface.Preparation)
ggparcoord(mydata.scaled, columns = c(1,2,3,6), groupColumn = "Surface.Preparation", scale = 'globalminmax') + facet_wrap(~ Contaminate.Type)
# Option 2:
# http://www.buildingwidgets.com/blog/2015/1/30/week-04-interactive-parallel-coordinates-1
# devtools::install_github("timelyportfolio/parcoords")
library(parcoords)
parcoords(mydata, rownames = FALSE, brushMode = "2D-strums", queue = T, color = list(colorBy="Surface.Preparation", colorScale=htmlwidgets::JS('d3.scale.category10()')))
