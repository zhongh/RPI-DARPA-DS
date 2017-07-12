# Lockheed: Predicting Gic using 5 DOE parameters
# Author: Hao Zhong

# Initialize environment and path (when restarting RStudio)
rm(list = ls())
setwd(dirname(getActiveDocumentContext()$path))
getwd()

# Packages
library(rstudioapi)
library(lattice)
library(ggplot2)
library(dummy)
library(caret)
library(arm)
library(wec)
library(class)
library(lattice)


# 
# Helper functions
# 
factors_to_integers <- function(x){
  levels(x) <- 1:length(levels(x))
  return(as.numeric(x))
}
scale_01 <-function(x){
  return(x/sum(x))
}

# 
# Data Import
# 
mydata <- read.csv("Fake Data DOE_IV 1_24_2017.csv", header = TRUE, stringsAsFactors = FALSE)
# PLEASE DO THIS FIRST TO MAKE SURE OUR ANALYSIS IS NOT AFFECTED BY NA'S IN OTHER
# UNNECESSARY COLUMNS: Retain neede column only
mydata <- mydata[, c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", "Surface.Preperation", "Contaminate.Type", "Gic")]
# Take a look
View(mydata)
head(mydata)
# Use str() and summary() to look at variable types and summary statistics; 
str(mydata)
summary(mydata)

#
# Data Preprocessing
# 

# Note we fixed the incorrect column name "Surface.Preperation" to:
#   "Surface.Preparation"
mydata$Adhesive.Out.Time <- abs(as.numeric(mydata$Adhesive.Out.Time))
mydata$Prep..to.Bond.Time <- abs(as.numeric(mydata$Prep..to.Bond.Time))
mydata$Surface.Preparation <- (mydata$Surface.Preperation)
mydata$Surface.Preperation <- NULL
mydata$Contaminate.Type <- (mydata$Contaminate.Type)
mydata$Contamination.Amount <- as.numeric(mydata$Contamination.Amount)

# Tabularized summary of categorical variables:
table(mydata$Contaminate.Type, mydata$Surface.Preparation)
# After Omit NA's:
mydata <- na.omit(mydata)
table(mydata$Contaminate.Type, mydata$Surface.Preparation)

#
cat.variables <- c("Surface.Preparation", "Contaminate.Type")
num.variables <- c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")
ind.variables <- c("Gic")

# 
# lm
#

# Scaling
mydata.backup <- mydata
mydata <- cbind(scale(mydata[, num.variables]), mydata[, c(cat.variables, ind.variables)])

# lm.model
formula <- paste(ind.variables, "~", paste(c(cat.variables, num.variables), collapse = " * "))
formula
lm.model <- lm(formula = formula, data = mydata)
summary(lm.model)

# lm.model.2
formula.2 <- "Gic ~ Adhesive.Out.Time * Contaminate.Type * Contamination.Amount"
lm.model <- lm(data = mydata, formula = formula.2)
summary(lm.model)

#
# Exploring
#

# Faceted by Surface.Preparation, colored by Contaminate.Type
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, scales = "free_y", ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, scales = "free_y", ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, scales = "free_y", ncol = 1)
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, scales = "free_y", ncol = 1)

# Faceted by Surface.Preparation * Contaminate.Type
ggplot(mydata) + geom_histogram(aes(x = Adhesive.Out.Time)) + facet_wrap(~ Surface.Preparation * Contaminate.Type, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Prep..to.Bond.Time)) + facet_wrap(~ Surface.Preparation * Contaminate.Type, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Contamination.Amount)) + facet_wrap(~ Surface.Preparation * Contaminate.Type, scales = "free")
ggplot(mydata) + geom_histogram(aes(x = Gic)) + facet_wrap(~ Surface.Preparation * Contaminate.Type, scales = "free")

# Subsetting data by AOT and PBT (LET'S NOT DO THIS FOR NOW)
# 
# View(mydata[which(mydata$Prep..to.Bond.Time < 1.6), ])
# mydata.lowAOT <- mydata[which(mydata$Adhesive.Out.Time < 400000 & mydata$Prep..to.Bond.Time >= 1.6), ]
# mydata.HighAOT <- mydata[which(mydata$Adhesive.Out.Time >= 400000 & mydata$Prep..to.Bond.Time >= 1.6), ]
# ggplot(mydata.lowAOT) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, scales = "free_y", ncol = 1)
# ggplot(mydata.lowAOT) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, scales = "free_y", ncol = 1)
# ggplot(mydata.HighAOT) + geom_histogram(aes(x = Adhesive.Out.Time, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, scales = "free_y", ncol = 1)
# ggplot(mydata.HighAOT) + geom_histogram(aes(x = Prep..to.Bond.Time, fill = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, scales = "free_y", ncol = 1)
# ggplot(mydata.lowAOT) + geom_jitter(aes(x = Prep..to.Bond.Time, y = Adhesive.Out.Time, col = Surface.Preparation)) + facet_wrap(~ Contaminate.Type, scales = "free")
# ggplot(mydata.lowAOT) + geom_jitter(aes(x = Prep..to.Bond.Time, y = Adhesive.Out.Time, col = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, scales = "free")
# ggplot(mydata.HighAOT) + geom_jitter(aes(x = Prep..to.Bond.Time, y = Adhesive.Out.Time, col = Surface.Preparation)) + facet_wrap(~ Contaminate.Type, scales = "free")
# ggplot(mydata.HighAOT) + geom_jitter(aes(x = Prep..to.Bond.Time, y = Adhesive.Out.Time, col = Contaminate.Type)) + facet_wrap(~ Surface.Preparation, scales = "free")
# ggplot(mydata.lowAOT) + geom_jitter(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time, color = Contamination.Amount, size = Gic)) + facet_wrap(~ Contaminate.Type, scales = "free")
# ggplot(mydata.lowAOT) + geom_jitter(aes(x = Adhesive.Out.Time, y = Prep..to.Bond.Time, color = Contamination.Amount, size = Gic)) + facet_wrap(~ Surface.Preparation, scales = "free")
# ggplot(mydata.lowAOT) + geom_jitter(aes(x = Adhesive.Out.Time, y = Contamination.Amount, color = Contaminate.Type, size = Gic)) + facet_wrap(~ Surface.Preparation, scales = "free")
# ggplot(mydata.lowAOT) + geom_jitter(aes(x = Adhesive.Out.Time, y = Contamination.Amount, color = Surface.Preparation, size = Gic)) + facet_wrap(~ Contaminate.Type, scales = "free")

# 
# knn
# 
set.seed(8341)
samp <- sample(nrow(mydata), 0.6 * nrow(mydata))
# 
# Classify Gic into 3 Intervals
km <- kmeans(mydata$Gic, centers = c(min(mydata$Gic), mean(mydata$Gic), max(mydata$Gic)), iter.max = 100000, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
y <- as.factor(km$cluster)
levels(y) <- c("Low Gic", "Mid Gic", "High Gic")
ggplot(mydata) + geom_histogram(aes(x = Gic, fill = y), bins = 100) + scale_fill_manual(name = "Gic Level", values = c("lightgreen", "green", "darkgreen"))
#
# Run knn based on the classification of Gic above
mydata.scaled <- mydata
# mydata.scaled <- cbind(mydata[, c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", "Gic")], dummy(mydata[, c("Surface.Preparation", "Contaminate.Type")], int = TRUE))
num.vars <- sapply(mydata.scaled, is.numeric)
mydata.scaled[num.vars] <- lapply(mydata.scaled[num.vars], scale)
train <- mydata.scaled[samp, c("Adhesive.Out.Time", "Prep..to.Bond.Time","Contamination.Amount")]
test <- mydata.scaled[-samp, c("Adhesive.Out.Time", "Prep..to.Bond.Time","Contamination.Amount")]
# train & test set when use dummy variables
train <- mydata.scaled[samp, !(names(mydata.scaled) %in% c("Gic"))]
test <- mydata.scaled[-samp, !(names(mydata.scaled) %in% c("Gic"))]

train.def <- y[samp]
test.def <- y[-samp]
knn.1 <-  knn(train, test, train.def, k=1)
knn.5 <-  knn(train, test, train.def, k=5)
knn.20 <- knn(train, test, train.def, k=20)
# Let's calculate the proportion of correct classification for k = 1, 5 & 20 
sum(test.def == knn.1)/nrow(test)  # For knn = 1
sum(test.def == knn.5)/nrow(test)  # For knn = 5
sum(test.def == knn.20)/nrow(test) # For knn = 20
# We should also look at the success rate against the value of increasing K.
table(knn.1 ,test.def)
table(knn.5 ,test.def)
table(knn.20 ,test.def)


#
# Creating dummy variables, PROPERLY
# 
mydata.cat <- mydata[, c("Surface.Preparation", "Contaminate.Type")]
mydata.num <- mydata[, c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")] 
mydata.ind <- mydata[, c("Gic")] 

mydata.dummy.fullrank <- as.data.frame(model.matrix(~., mydata)[, -1])

# Scale numerical variables
mydata.scaled <- mydata
mydata.scaled$Contamination.Amount <- scale_01(mydata$Contamination.Amount)
mydata.scaled$Adhesive.Out.Time <- scale_01(mydata$Adhesive.Out.Time)
mydata.scaled$Prep..to.Bond.Time <- scale_01(mydata$Prep..to.Bond.Time)

mydata.dummy <- cbind(mydata[, c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", "Gic")], dummy(mydata[, c("Surface.Preparation", "Contaminate.Type")], int = TRUE))
str(mydata.dummy)


x <- mydata

x$Contaminate.Type <- factors_to_integers(x$Contaminate.Type)
x$Surface.Preparation <- factors_to_integers(x$Surface.Preparation)
x$Gic <- scale_01(x$Gic)
x$Contamination.Amount <- scale_01(x$Contamination.Amount)
x$Adhesive.Out.Time <- scale_01(x$Adhesive.Out.Time)
x$Prep..to.Bond.Time <- scale_01(x$Prep..to.Bond.Time)
#corr
corrplot(cor(x), method="ellipse")
corrplot(cor(cbind(mydata.num, mydata.ind)), method="ellipse")

# FA
str(x)
fa <- factanal(cbind(mydata.num, mydata.ind), factors = 1)
summary(fa$factors)
screeplot(fa)
biplot(fa$scores, loadings(fa), xlabs=rep(".", nrow(x)))

# 
# PCA
# 
mydata.num.scaled <- as.data.frame(scale(mydata.num))
pca <- princomp(mydata.num.scaled)
summary(pca)
screeplot(pca)
biplot(pca$scores, loadings(pca))

#
# SVM
# 





#
# wec
# 
detach(mydata)
attach(mydata)
detach(mydata.dummy)
attach(mydata.dummy)

# Without Controls
model.dummy <- lm(Gic ~ ., data=mydata.dummy)
summary(model.dummy)
# With Controls
model.dummy.controls <- lm(Gic ~ ., data=mydata) 
summary(model.dummy.controls)

#
# arm
# 
hist(mydata$Gic)
mydata$Gic <- mydata$Gic / sum(mydata$Gic)
hist(mydata$Gic)
fit <- glm(Gic ~ ., data = mydata, family=binomial(link="logit"))
fit <- glm(Gic ~ ., data = mydata)
display(fit)
?fit

# bayesglm
# https://www.rdocumentation.org/packages/arm/versions/1.9-3/topics/bayesglm
fit <- bayesglm(Gic ~ ., data = mydata, family=binomial(link="logit"))
display(fit)

#
ggplot(mydata, aes(x = Contamination.Amount, y = Gic, col = Contaminate.Type)) + geom_point() + geom_smooth(method='lm')
#
xyplot(x = Gic ~ Contamination.Amount | Contaminate.Type, data = mydata, type = c("p","r"))
histogram(x = ~Contamination.Amount | Surface.Preparation, data = mydata)
histogram(x = ~Contamination.Amount | Surface.Preparation * Contaminate.Type, data = mydata)
cloud(x = Contamination.Amount ~ Adhesive.Out.Time  + Prep..to.Bond.Time | Surface.Preparation * Contaminate.Type, pretty = TRUE,
      data = mydata, groups = Gic)
