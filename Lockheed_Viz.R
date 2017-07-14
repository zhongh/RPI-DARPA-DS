# Lockheed: Data Exploration, Summary, and Visualizations
# Author: Hao Zhong

# Load packages
library(rstudioapi)
library(ggplot2)
library(GGally)

# Set working directory to wherever this file locates
# setwd(dirname(getActiveDocumentContext()$path))
getwd()

# Use with caution as this will automatically clean up the whole workspace and
# redo all the data import and preparation steps. If you feel unsure, please open 
# this file and run it manually.
source("Lockheed.R")

################################################################################

# 
# Data Exploration and Visualization
# 

# Build a contingency table of the counts at each combination of factor levels
table(mydata$Contaminate.Type, mydata$Surface.Preparation)

# Categorical variables: barplots

ggplot(mydata) + geom_bar(aes(x = Surface.Preparation))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type))

# Use stacked barplots to further visualize how each bar is consisted of another factor
# To do this, use the "fill" property
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Contaminate.Type))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Surface.Preparation))
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Gic.Level))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Gic.Level))

# Use violin or box plot to visualize how each bar is consisted of another numerical variable
ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Gic, fill = Surface.Preparation))
ggplot(mydata) + geom_violin(aes(x = Contaminate.Type, y = Gic, fill = Contaminate.Type))
ggplot(mydata) + geom_boxplot(aes(x = Surface.Preparation, y = Gic, fill = Surface.Preparation))
ggplot(mydata) + geom_boxplot(aes(x = Contaminate.Type, y = Gic, fill = Contaminate.Type))

# Facetting by another factor 
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Gic.Level)) + facet_wrap(~ Contaminate.Type) + ggtitle("Surface Preparation by Contaminate Type")
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Gic.Level)) + facet_wrap(~ Surface.Preparation) + ggtitle("Contaminate Type by Surface Preparation")
ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Gic, fill = Surface.Preparation)) + facet_wrap(~ Contaminate.Type) + ggtitle("Surface Preparation vs Gic, by Contaminate Type")
ggplot(mydata) + geom_violin(aes(x = Contaminate.Type, y = Gic, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation) + ggtitle("Contaminate Type vs Gic, by Surface Preparation")

# What we have done above:
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

# For numerical variables we start with histogram instead of barplot. We may also
# use a density plot to visualize the distribution implied by the histogram, 
# although this could be intuitively misleading if abused.
 
# Adhesive.Out.Time
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Gic.Level), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preparation), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Gic.Level), bins = 30) + facet_wrap(~ Contaminate.Type)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Gic.Level), bins = 30) + facet_wrap(~ Surface.Preparation)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type)
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation)

# Create 2 Adhesive.Out.Time levels with threshold at 440000 (may be different for real data)
mydata$Adhesive.Out.Time.Level <- as.factor(ifelse(mydata$Adhesive.Out.Time < 440000, 'LowAOT', 'HighAOT'))
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), bins = 20) + 
  facet_wrap(~ Adhesive.Out.Time.Level, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preparation), bins = 20) + 
  facet_wrap(~ Adhesive.Out.Time.Level, scales = "free")

# Comment:
# How are Adhesive.Out.Time, Prep..to.Bond.Time, Contamination.Amount related to 
# Surface.Preparation and Contaminae.Type?
# Are Adhesive.Out.Time, Prep..to.Bond.Time, and Contamination.Amount also
# designed values?

# Prep..to.Bond.Time
# 
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type, scales = "free")

# Comment:
# Again, it is intuitive that Surface.Preparation and Contaminae.Type are
# designed values.

# Contamination.Amount
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preparation), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type, scales = "free")

# (+++) To be added after looking at the plot on real data)
#
# Try separate Contamination.Amount at a threshold of 5000
mydata$Contamination.Amount.Level <- as.factor(ifelse(mydata$Contamination.Amount < 5000, 'Low Amt', 'High Amt'))
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preparation)) + facet_wrap(~ Contamination.Amount.Level, scales = "free")

# Gic
ggplot(mydata) + geom_histogram(aes(x = Gic), bins = 30)
#
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30)
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation), bins = 30)
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preparation), alpha = 0.5)
#
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type), bins = 30) + facet_wrap(~ Surface.Preparation, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation), bins = 30) + facet_wrap(~ Contaminate.Type, scales = "free")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5) + facet_wrap(~ Surface.Preparation, scales = "free")
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preparation), alpha = 0.5) + facet_wrap(~ Contaminate.Type, scales = "free")

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





# library(GGally)
# library(reshape2)
# #
# #
# ggcorr(mydata) + facet_wrap(~ Contaminate.Type)
# # ggplot(data = melt(cor(mydata[, independent.variables.num])), aes(x=Var1, y=Var2, fill=value)) + geom_tile()
# #
# # Correlation in each subset combining Suraface Prep and Contamination Type
# #
# for (i in levels(mydata$Surface.Preparation)) {
#   for (j in levels(mydata$Contaminate.Type)) {
#     cat(paste("\nSurface.Preparation =", i, "and Contaminate.Type =", j, ":\n"))
#     print(cor(mydata[which(mydata$Surface.Preparation==i & mydata$Contaminate.Type==j), doe.num]))
#     # print(ggplot(data = melt(cor(mydata[which(mydata$Surface.Preparation==i & mydata$Contaminate.Type==j), independent.variables.num])), aes(x=Var1, y=Var2, fill=value)) + geom_tile())
#     print(ggcorr(mydata[which(mydata$Surface.Preparation==i & mydata$Contaminate.Type==j), doe.num]))
#   }
# }



################################################################################

# 
# Pairwise scatterplots
#  

# Use plot()
plot(mydata)   

# Use GGally:ggpairs
library(GGally)
# References:
# http://dkhramov.dp.ua/Comp.PlotMultidimensionalData#.WWepucbMyu4
# https://www.rdocumentation.org/packages/GGally/versions/1.3.1/topics/ggpairs#Details

# "400000" is a cutoff Adhesive.Out.Time value to subset the fake data
ggpairs(data = mydata[which(mydata$Adhesive.Out.Time < 400000), ], 
        title = "Pairwise plots of DoE variables and Gic (Using GGally:ggpairs)",
        upper = list(
          continuous = "cor",
          combo = "box",
          discrete = "facetbar"
        ),
        lower = list(
          continuous = "points",
          combo = "box_no_facet",
          discrete = "ratio"
        ),
        diag = list(
          continuous = "barDiag" 
        ),
        mapping = aes(color = Contaminate.Type)
)

# Include only 1 categorical variable for clarity: Contaminate.Type
ggpairs(data = mydata[which(mydata$Adhesive.Out.Time < 400000), ],
        columns = c(doe.num, "Contaminate.Type", "Gic"),
        title = "Pairwise plots of DoE variables and Gic (Using GGally:ggpairs)",
        upper = list(
          continuous = "cor",
          combo = "facethist",
          discrete = "facetbar"
        ),
        lower = list(
          continuous = "points",
          combo = "box",
          discrete = "ratio"
        ),
        diag = list(
          continuous = "barDiag" 
        ),
        mapping = aes(color = Contaminate.Type)
)

# Include only 1 categorical variable for clarity: Surface.Preparation
ggpairs(data = mydata[which(mydata$Adhesive.Out.Time < 400000), ],
        columns = c(doe.num, "Surface.Preparation", "Gic"),
        title = "Pairwise plots of DoE variables and Gic (Using GGally:ggpairs)",
        upper = list(
          continuous = "cor",
          combo = "facethist",
          discrete = "facetbar"
        ),
        lower = list(
          continuous = "points",
          combo = "box",
          discrete = "ratio"
        ),
        diag = list(
          continuous = "barDiag" 
        ),
        mapping = aes(color = Surface.Preparation)
)

# Other options include pairs(), lattice::splom, etc

################################################################################

# Build a contingency table of the counts at each combination of factor levels
table(mydata$Contaminate.Type, mydata$Surface.Preparation)









################################################################################

# 
# Radial plot
# 

# library(plotrix) 
# radial.plot(scale(mydata[, c(doe.num, "Gic")]),                  # данные
#             labels=names(mydata[, c(doe.num, "Gic")]), # подписи к осям
#             rp.type="p",           # тип линии ("p" - полигон)
#             lwd="1",               # толщина линии
#             line.col=mydata[, 4],       # цвет линии (номер цвета в палитре)
#             rad.col="lightblue"    # цвет осей
# )

################################################################################

# 
# Andrews plot
# 

# install.packages("andrews")
# library(andrews) 
# andrews(mydata, clr = 4)
# andrews(mydata, type = 4, clr = 5)

################################################################################

# 
# Parallel coordinates plot
# 

# Use GGally::ggparcoord
# mydata.scaled <- cbind(scale(mydata[, doe.num]), mydata[, c(doe.cat, "Gic")])
# ggparcoord(mydata.scaled, columns = c(1,2,3,6), groupColumn = "Contaminate.Type", shadeBox = "red",
#            scale = 'globalminmax', boxplot = TRUE, alphaLines = .5) + 
#   facet_wrap(~ Surface.Preparation)

# Use parcoords::parcoords
# http://www.buildingwidgets.com/blog/2015/1/30/week-04-interactive-parallel-coordinates-1
# devtools::install_github("timelyportfolio/parcoords")
# library(parcoords)
# parcoords(mydata, rownames = FALSE, 
#           color = list(
#             colorBy = "Surface.Preparation", 
#             colorScale = htmlwidgets::JS('d3.scale.category10()')
#             )
#           )

