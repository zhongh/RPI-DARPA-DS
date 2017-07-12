#####  THIS IS CODE FR CHARLES (posted in ML channel on 3/27/17), 
#               WRITTEN BY COREY, USED TO DO REGRESSION
# 3 models are below:  Random Forest, C Forest, Lasso regression.  Only Lasso Regression
# yeilds coefficients for prediction.  All three yield a feature importance.

#
#LET ME TRY TO READ IN THE FOLLOWING FILE:  doe4regGic.csv  #  this didn't work for some reason
# Anne had to use the 'Import Dataset' button in the Environment Pane to correctly read the data in.
# ANNE; NOTE:  the models only use numerical inputs; they cannot take categorical inputs
#   so for the file from Charles, doe4regGic2.csv that i used here, the categorical variables
#      are input as Boolean.  

setwd("/Users/APHM/Documents/1_DATA ONTOLOGY/R_STUDIO/RSTUDIO_12-01-16/RStudio PROJECTS 2017/")
getwd()

# Another way to get working directory
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))


rm(list = ls())

install.packages("glmnet")
install.packages("xgboost")
install.packages("Metrics")
install.packages("plotly")
library(randomForest)
library(glmnet)
library(party)
library(xgboost)
library(Metrics)
library(plotly)


############## READ IN DATA ###########################
#mydata <- read.csv("doe4regGic2.csv", header = TRUE, stringsAsFactors = FALSE)
#mydata <- read.csv("doe4regGicONLY_DoE_Param.csv", header=TRUE, stringsAsFactors = FALSE)
#mydata <- read.csv("doe4logGic.csv", header=TRUE, stringsAsFactors = FALSE)
#### ANNE:  NOTE:  THE READ COMMANDS ARE NOT WORKING HERE, NO IDEA WHY, BUT JUST 
#####       INSERTING THE DATA INTO 'MYDATA' WORKS FINE IN NEXT LINE
#mydata <- doe4logGic   # this doesnt work
#View(doe4regDEParam)
#mydata <- doe4regDEParam
mydata <- read.csv("Fake Data DOE_IV 1_24_2017.csv", header = TRUE, stringsAsFactors = FALSE)
View(mydata)

View(doe4regGic)
mydata <- doe4regGic    # ALL DATA    LINEAR
View(mydata)
#mydata <- read.csv("doe_4_170304-1_orig from 01-17-17 Charles.csv", header = TRUE, stringsAsFactors = FALSE)
### Note: 563 rows of data.
# ANNE:  the following line assumes that the read cmmd is working which it isn't today 3/28/17
#mydata <- doe4regGic2
#mydata <- doe4regGicONLY_DoE_Param.csv
#View(mydata)
mydata <- na.omit(mydata)
mydata$Adhesive.Out.Time <- as.numeric(mydata$Adhesive.Out.Time)
mydata <- na.omit(mydata)
View(mydata)
m <- dim(mydata)[1]
# check the data type in each column, make sure there are not factors since cv.glmnet could not take factors
str(mydata)
# random sampling to pick the rows for test set
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 
train <- mydata[-val,]
test <- mydata[val,]
#ANNE: change wthe variable that you want to fit
target <- c('Gic')
predictors <- names(mydata)[!names(mydata) %in% target]
# calculate values for adjusted R-squared
k <- dim(mydata)[2] - 1
N <- dim(test)[1]
adj.coe <- (N - 1) / (N - k - 1)
means <- rep(mean(test[,target]), dim(test)[1])

###### ANNE'S QUESTION ON THESE FITS:  LOOKS LIKE EACH RUN CAN GENERATE A DIFFERENT RESULT
###########  IS THERE A WAY IN WHICH I CAN SET A SEED SO IT RUNS THE SAME EVERY TIME?

# random forest
# ANNE: note that here we're fitting Gic again in next line
set.seed(3605)

forest.model <- randomForest(Gic  ~ ., data = train, importance=TRUE)
#forest.model <- randomForest(formula=Gic  ~ ., data = train, importance=TRUE)
forest.predictions <- predict(forest.model, test[,predictors])
forest.r.squared <- sum((forest.predictions-means)^2)/sum((test[,target]-means)^2)
# ANNE: if you type next command in console you'll get the r squared (r2) and same variable is also in global environ
forest.adj.r.squared <- 1 - (1 - forest.r.squared) * adj.coe
# ANNE:  the next cmd display tre in numeric form
#getTree(randomForest(mydata[,-5], mydata[,5], ntree=10), 3, labelVar=TRUE)
# feature importance
# ANNE: 
importance(forest.model, type = 2)   # need to better define importance
#### ANNE see: https://www.r-bloggers.com/variable-importance-plot-and-variable-selection/
#mosaicplot(forest.model)   # ANNE this isn't running.  some kind of classification
# Simple Scatterplot
#plot_ly(data = iris, x = ~test[,target], y = ~forest.predictions)
#plot(test[,target], forest.predictions, xlab = "Actual UTS", ylab = "Forest Predictions", main = "Random Forest (R-sq = 0.92)")
# ANNE:  change into correct labels
#  Charles' labels:
#plot(test[,target], forest.predictions, xlab = "Actual UTS", ylab = "Forest Predictions", main = "Random Forest (R-sq = 0.92)")
#abline(lm(forest.predictions ~ test[,target]))
plot(test[,target], forest.predictions, xlab = "GIC", ylab = "Forest Predictions", main = "Random Forest")
abline(lm(forest.predictions ~ test[,target]))
### ANNE:  now plot residuals:
forest.lm = lm(Gic ~ ., data=mydata) 
forest.res = resid(forest.lm) 
View(forest.res)
#View(cforest.res) # these are the same, so something is wrong.
plot(mydata$Gic, forest.res, ylab="Residuals", xlab="GIC", main="Forest Residuals") 
abline(0, 0)                  # the horizon 


#Plot Tree
x <- ctree(Gic ~ ., data=mydata)
plot(x, type="simple")
#Full Model   #  in C FOrest method it can potentially allow you to select a given tree
x <- ctree(Gic ~ ., data=mydata, controls=cforest_control(mtry=2, mincriterion=0))
plot(x, type="simple")


# conditional inference random forest(Cforest)
set.seed(3605)
cforest.model <- cforest(Gic  ~ ., data = train, control = cforest_unbiased(ntree = 500))
cforest.predictions <- predict(cforest.model, newdata = test[,predictors])
cforest.r.squared <- sum((cforest.predictions-means)^2)/sum((test[,target]-means)^2)
cforest.adj.r.squared <- 1 - (1 - cforest.r.squared) * adj.coe
# feature importance
varimp(cforest.model)
# Simple Scatterplot
#plot_ly(data = iris, x = ~test[,target], y = ~cforest.predictions)
# ANNE plot not correct - check!!
#plot_ly(data = 'Gic', x = ~test[,target], y = ~cforest.predictions)
#Plot Tree
x <- ctree(Gic ~ ., data=mydata)
plot(x, type="simple")
#Full Model
x <- ctree(Gic ~ ., data=mydata, controls=cforest_control(mtry=2, mincriterion=0))
plot(x, type="simple")

plot(test[,target], cforest.predictions, xlab = "GIC", ylab = "C_Forest Predictions", main = "C-Forest")
abline(lm(cforest.predictions ~ test[,target]))
### ANNE:  now plot residuals:
cforest.lm = lm(Gic ~ ., data=mydata) 
cforest.res = resid(cforest.lm) 
plot(mydata$Gic, cforest.res, ylab="Residuals", xlab="GIC", main="C_forest Residuals") 
abline(0, 0)                  # the horizon 

# Lasso regression
set.seed(3605)
cvfit = cv.glmnet(as.matrix(train[,predictors]), train[,target], type.measure = "mse", alpha = 1, nfolds = 5)
plot(cvfit)
#extreme grad boosting gives lambda, not same lambda in data file
# lambda.min is the value of ?? that gives minimum mean cross-validated error. 
# The other ?? saved is lambda.1se, which gives the most regularized model such that error is within one standard error of the minimum. 
lasso.predictions <- predict(cvfit, newx = as.matrix(test[,predictors]), s = "lambda.min")
#lasso.predictions <- predict(cvfit, newx = as.matrix(test[,predictors]), s = "lambda.1se")
lasso.r.squared <- sum((lasso.predictions-means)^2)/sum((test[,target]-means)^2)
lasso.adj.r.squared <- 1 - (1 - lasso.r.squared) * adj.coe
# feature importance (for this model, coefficient of each feature). also try s = "lambda.1se"
coef <- coef(cvfit, s = "lambda.min")
lasso.result <- data.frame(matrix(nrow = k, ncol = 2), row.names = predictors)
colnames(lasso.result) <- c("coef", "importance")
var.y <- var(test[,target])
for (i in 1:k) {
  lasso.result[i, 1] = coef[i+1]
  lasso.result[i, 2] = (lasso.result[i, 1]^2) * var(test[,i]) / var.y
}
View(lasso.result)

plot(test[,target], lasso.predictions, xlab = "GIC", ylab = "Lasso Predictions", main = "Lasso")
abline(lm(lasso.predictions ~ test[,target]))

### ANNE:  now plot residuals:
lasso.lm = lm(Gic ~ ., data=mydata) 
lasso.res = resid(lasso.lm) 
plot(mydata$Gic, lasso.res, ylab="Residuals", xlab="GIC", main="Lasso Residuals") 
abline(0, 0)                  # the horizon 



# Simple Scatterplot
#plot_ly(data = iris, x = ~test[,target], y = ~lasso.predictions)

# eXtreme gradient boosting (XGBoost) using cross validation
# 10 folds cross validation for training set, can assign different values to cv
cv <- 10
cvDivider <- floor(nrow(train) / cv)
smallestError <- 100

for (depth in seq(1,10,1)) {
  for (rounds in seq(1,20,1)) {
    totalError <- c()
    for (cv in seq(1,cv,1)) {
      cvTestIndex <- c(((cv - 1) * cvDivider + 1):(cv * cvDivider))
      cvTest <- train[cvTestIndex,]
      cvTrain <- train[-cvTestIndex,]
      bst <- xgboost(data = as.matrix(cvTrain[,predictors]),
                     label = cvTrain[,target],
                     max.depth=depth, nround=rounds,
                     objective = "reg:linear", verbose=0)
      gc()
      predictions <- predict(bst, as.matrix(cvTest[,predictors]), outputmargin=TRUE)
      err <- rmse(as.numeric(cvTest[,target]), as.numeric(predictions))
      totalError <- c(totalError, err)
    }
    if (mean(totalError) < smallestError) {
      smallestError = mean(totalError)
      print(paste(depth,rounds,smallestError))
    } 
  }
} 

# use the best depth and nround values from the cross validation, fill in the two "?" below
bst <- xgboost(data = as.matrix(train[,predictors]),
               label = train[,target],
               max.depth=10, nround=10,
               objective = "reg:linear", verbose=0)
xgboost.predictions <- predict(bst, as.matrix(test[,predictors]), outputmargin=TRUE)
xgboost.r.squared <- sum((xgboost.predictions-means)^2)/sum((test[,target]-means)^2)
xgboost.adj.r.squared <- 1 - (1 - xgboost.r.squared) * adj.coe
# feature importance
xgbi <- xgb.importance(feature_names = predictors, model = bst)
#xgb.plot.importance(xgbi, numberOfClusters = c(1:10))