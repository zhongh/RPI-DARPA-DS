# Lockheed: Data Exploration and Visualization
# Author: Hao Zhong

# Install packages (commented out)
# install.packages("ggplot2")
# install.packages("GGally")

# Set working directory to wherever this file locates
library(rstudioapi)
# setwd(dirname(getActiveDocumentContext()$path))
getwd()

# Use with caution as this will automatically clean up the whole workspace and
# redo all the data import and preparation steps. If you feel unsure, please run 
# this file "Lockheed.R" manually and suit to your purpose.
source("Lockheed.R")

################################################################################

library(ggplot2)

# Add `Gic.Level` to `mydata` for convenience in using `ggplot2::facet_wrap`
mydata <- cbind(mydata, Gic.Level)

# 
# 1.1 Barplot: One Categorical Variable (`ggplot2::geom_bar`)
# 

# Define the categorical variable by `aes(x = ...)`
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type))

# Use `aes(fill = ...)` to color each bar by its own category
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Surface.Preparation))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Contaminate.Type))

# Use `ggplot2::facet_wrap` to facet into multiple plots by another factor 
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Surface.Preparation)) + facet_wrap(~ Contaminate.Type)
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation)

# 
# 1.2 Stacked Barplot: Categorical vs Categorical (`geom_bar` w/ aes(fill = ...))
# 

ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Contaminate.Type))
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Surface.Preparation))
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Gic.Level))

# Facetting by another factor (`facet_wrap`)
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Contaminate.Type)) + facet_wrap(~ Contaminate.Type, ncol = 2)
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Contaminate.Type)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Surface.Preparation)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_bar(aes(x = Contaminate.Type, fill = Surface.Preparation)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Gic.Level)) + facet_wrap(~ Contaminate.Type)
ggplot(mydata) + geom_bar(aes(x = Surface.Preparation, fill = Gic.Level)) + facet_wrap(~ Gic.Level)
# Note that `ncol = k` sets the facet grids in k column(s).
# For other usages/ please go to `?facet_wrap`

# 
# 1.3 Boxplot: Categorical vs Numerical (`ggplot2::geom_boxplot`)
# 

# `fill = NULL`
ggplot(mydata) + geom_boxplot(aes(x = Gic.Level, y = Adhesive.Out.Time, fill = NULL))
ggplot(mydata) + geom_boxplot(aes(x = Gic.Level, y = Prep..to.Bond.Time, fill = NULL))
ggplot(mydata) + geom_boxplot(aes(x = Gic.Level, y = Contamination.Amount, fill = NULL))

# `fill` same as `x`
ggplot(mydata) + geom_boxplot(aes(x = Contaminate.Type, y = Adhesive.Out.Time, fill = Contaminate.Type))
ggplot(mydata) + geom_boxplot(aes(x = Contaminate.Type, y = Prep..to.Bond.Time, fill = Contaminate.Type))
ggplot(mydata) + geom_boxplot(aes(x = Contaminate.Type, y = Contamination.Amount, fill = Contaminate.Type))
ggplot(mydata) + geom_boxplot(aes(x = Contaminate.Type, y = Gic, fill = Contaminate.Type))
# Consider facetting using another categorical variable different from `x`
ggplot(mydata) + geom_boxplot(aes(x = Contaminate.Type, y = Adhesive.Out.Time, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_boxplot(aes(x = Contaminate.Type, y = Prep..to.Bond.Time, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_boxplot(aes(x = Contaminate.Type, y = Contamination.Amount, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_boxplot(aes(x = Contaminate.Type, y = Gic, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, ncol = 1)

# `fill` different from `x`
ggplot(mydata) + geom_boxplot(aes(x = Surface.Preparation, y = Adhesive.Out.Time, fill = Gic.Level))
ggplot(mydata) + geom_boxplot(aes(x = Surface.Preparation, y = Prep..to.Bond.Time, fill = Gic.Level))
ggplot(mydata) + geom_boxplot(aes(x = Surface.Preparation, y = Contamination.Amount, fill = Gic.Level))
ggplot(mydata) + geom_boxplot(aes(x = Surface.Preparation, y = Gic, fill = Gic.Level))
# Consider facetting using another categorical variable different from `x`
# In this case it avoids a overly dense plot
ggplot(mydata) + geom_boxplot(aes(x = Surface.Preparation, y = Adhesive.Out.Time, fill = Gic.Level)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_boxplot(aes(x = Surface.Preparation, y = Prep..to.Bond.Time, fill = Gic.Level)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_boxplot(aes(x = Surface.Preparation, y = Contamination.Amount, fill = Gic.Level)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_boxplot(aes(x = Surface.Preparation, y = Gic, fill = Gic.Level)) + facet_wrap(~ Gic.Level, ncol = 1)

# 
# 1.3.* Violin Plot: Categorical vs Numerical (`geom_violin`)
# 

# Violin plot (using `ggplot2::geom_violin`) can be seen as a "smoothed" version
# of box plot, however not recommended in this case. Its syntax is identical to
# `geom_boxplot` therefore you can easily change from a boxplot to a violin plot.
# 
# Examples:
ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Adhesive.Out.Time, fill = Surface.Preparation))
ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Prep..to.Bond.Time, fill = Surface.Preparation))
ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Contamination.Amount, fill = Surface.Preparation))
ggplot(mydata) + geom_violin(aes(x = Surface.Preparation, y = Gic, fill = Surface.Preparation))

#
# 2.1 Histogram: One Numerical Variable (`ggplot2::geom_histogram`)
# 

ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time)) 
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time)) 
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount)) 
ggplot(mydata) + geom_histogram(aes(x = Gic)) 

# Use `facet_wrap` to facet into multiple plots by another factor 
# Facet by Contaminate.Type
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time)) + facet_wrap(~ Contaminate.Type, ncol = 2)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time)) + facet_wrap(~ Contaminate.Type, ncol = 2)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount)) + facet_wrap(~ Contaminate.Type, ncol = 2)
ggplot(mydata) + geom_histogram(aes(x = Gic)) + facet_wrap(~ Contaminate.Type, ncol = 2)
# Facet by Surface.Preparation
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Gic)) + facet_wrap(~ Surface.Preparation, ncol = 1)
# Facet by Gic.Level
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Gic)) + facet_wrap(~ Gic.Level, ncol = 1)

#
# 2.1.* Density Plot: One Numerical Variable (`geom_density`)
# 

# Basically a smoothed version of histogram, just as violin plot is to boxplot.
# Examples:
ggplot(mydata) + geom_density(aes(x = Contamination.Amount)) 
ggplot(mydata) + geom_density(aes(x = Gic)) 
ggplot(mydata) + geom_density(aes(x = Contamination.Amount)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_density(aes(x = Gic)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_density(aes(x = Contamination.Amount)) + facet_wrap(~ Gic.Level, ncol = 1)

#
# 2.2 Stacked Histogram: Numerical vs Categorical (`geom_histogram` + `aes(fill = ...)`)
# 

# fill = Surface.Preparation
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preparation))
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation))
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preparation))
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation))
# fill = Contaminate.Type
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type))
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type))
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type))
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type))
# fill = Gic.Level
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Gic.Level))
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Gic.Level))
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Gic.Level))
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Gic.Level))

# Facetting by another factor (`facet_wrap`)
# fill = Surface.Preparation, facet by Contaminate.Type
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preparation)) + facet_wrap(~ Contaminate.Type, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation)) + facet_wrap(~ Contaminate.Type, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preparation)) + facet_wrap(~ Contaminate.Type, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation)) + facet_wrap(~ Contaminate.Type, ncol = 1)
# fill = Surface.Preparation, facet by Gic.Level
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Surface.Preparation)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Surface.Preparation)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Surface.Preparation)) + facet_wrap(~ Gic.Level, ncol = 1)
# fill = Contaminate.Type, facet by Surface.Preparation
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, ncol = 1)
# fill = Contaminate.Type, facet by Gic.Level
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type)) + facet_wrap(~ Gic.Level, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type)) + facet_wrap(~ Gic.Level, ncol = 1)
# fill = Gic.Level, facet by Contaminate.Type
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Gic.Level)) + facet_wrap(~ Contaminate.Type, ncol = 1) 
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Gic.Level)) + facet_wrap(~ Contaminate.Type, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Gic.Level)) + facet_wrap(~ Contaminate.Type, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Gic.Level)) + facet_wrap(~ Contaminate.Type, ncol = 1)
# fill = Gic.Level, facet by  Surface.Preparation
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Gic.Level)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Gic.Level)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Gic.Level)) + facet_wrap(~ Surface.Preparation, ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Gic.Level)) + facet_wrap(~ Surface.Preparation, ncol = 1)

#
# 2.2.* Overlapping Density Plots: Numerical vs Categorical (`geom_density` + `aes(fill = ...)`)
# 

# fill = Surface.Preparation
ggplot(mydata) + geom_density(aes(x = Adhesive.Out.Time, fill = Surface.Preparation), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Surface.Preparation), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Surface.Preparation), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Gic, fill = Surface.Preparation), alpha = 0.5)
# fill = Contaminate.Type
ggplot(mydata) + geom_density(aes(x = Adhesive.Out.Time, fill = Contaminate.Type), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Contaminate.Type), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Gic, fill = Contaminate.Type), alpha = 0.5)
# fill = Gic.Level
ggplot(mydata) + geom_density(aes(x = Adhesive.Out.Time, fill = Gic.Level), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Prep..to.Bond.Time, fill = Gic.Level), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Contamination.Amount, fill = Gic.Level), alpha = 0.5)
ggplot(mydata) + geom_density(aes(x = Gic, fill = Gic.Level), alpha = 0.5)

#
# 2.3 Scatterplot: Numerical vs Numerical (`geom_point`)
# 

# Exploring Gic vs Contamination.Amount

# Basic scatterplot: Gic vs Contamination.Amount
ggplot(mydata) + geom_point(aes(x = Gic, y = Contamination.Amount))
# Color points by a factor
ggplot(mydata) + geom_point(aes(x = Gic, y = Contamination.Amount, col = Contaminate.Type))
ggplot(mydata) + geom_point(aes(x = Gic, y = Contamination.Amount, col = Surface.Preparation))
# Separate into multiple facets by a factor
ggplot(mydata) + geom_point(aes(x = Gic, y = Contamination.Amount, col = Contaminate.Type)) + facet_wrap(~ Surface.Preparation)
ggplot(mydata) + geom_point(aes(x = Gic, y = Contamination.Amount, col = Surface.Preparation)) + facet_wrap(~ Contaminate.Type)

# For Adhesive.Out.Time and Prep..to.Bond.Time, the values are widely apart and
# thus awkard to visualize straighforward
ggplot(mydata) + geom_point(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time))
# If we try to separate them into different facets and use free axis scales by
# using `facet_wrap(..., scales = "free")`, it is revealed that within each or 
# each combination of categories, these 2 times have a correlating pattern.
ggplot(mydata) + geom_point(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time)) + facet_wrap(~ Surface.Preparation, scales = "free")
ggplot(mydata) + geom_point(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time)) + facet_wrap(~ Contaminate.Type, scales = "free")
ggplot(mydata) + geom_point(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time)) + facet_wrap(Surface.Preparation ~ Contaminate.Type, scales = "free")

#
# 2.3.* 2-D Density Plot: Numerical vs Numerical (`geom_density_2d`)
# 

ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Contamination.Amount))
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Contamination.Amount)) + facet_wrap(~ Surface.Preparation)
ggplot(mydata) + geom_density_2d(aes(x = Gic, y = Contamination.Amount)) + facet_wrap(~ Contaminate.Type)
ggplot(mydata) + geom_density_2d(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time)) + facet_wrap(Surface.Preparation ~ Contaminate.Type, scales = "free")

################################################################################

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

################################################################################


################################################################################

# 
# Contigency Table: Factor1 vs Factor2 (Using `table()`)
# 

# Build a contingency table of the counts at each combination of factor levels
table(mydata$Contaminate.Type, mydata$Surface.Preparation)
table(Gic.Level, mydata$Surface.Preparation)
table(mydata$Contaminate.Type, Gic.Level)

################################################################################

# 
# Pairwise scatterplots
#  

# Use `plot()`
plot(mydata[c(doe.all, "Gic")])

# Use `GGally:ggpairs``
library(GGally)

# References:
# http://dkhramov.dp.ua/Comp.PlotMultidimensionalData#.WWepucbMyu4
# https://www.rdocumentation.org/packages/GGally/versions/1.3.1/topics/ggpairs#Details

# "400000" is a cutoff Adhesive.Out.Time value to subset the fake data
ggpairs(data = mydata[which(mydata$Adhesive.Out.Time < 400000), ], 
        columns = c(doe.all, "Gic"),
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

# Include only 1 categorical variable for clarity: Contaminate.Type
ggpairs(data = mydata[which(mydata$Adhesive.Out.Time < 400000), c(doe.all, "Gic")],
        columns = c(doe.num, "Contaminate.Type", "Gic"),
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

# There are other possible options include `pairs`, `lattice::splom`.

################################################################################

# Next start from here ...

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


#





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


