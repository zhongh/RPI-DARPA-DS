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

mydata <- na.omit(mydata)
m <- dim(mydata)[1]
# check the data type in each column, make sure there are not factors since cv.glmnet could not take factors
str(mydata)
# random sampling to pick the rows for test set
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 
train <- mydata[-val,]
test <- mydata[val,]
target <- c('Gic')
predictors <- names(mydata)[!names(mydata) %in% target]
# calculate values for adjusted R-squared
k <- dim(mydata)[2] - 1
N <- dim(test)[1]
adj.coe <- (N - 1) / (N - k - 1)
means <- rep(mean(test[,target]), dim(test)[1])

# random forest
forest.model <- randomForest(Gic  ~ ., data = train, importance=TRUE)
forest.predictions <- predict(forest.model, test[,predictors])
forest.r.squared <- sum((forest.predictions-means)^2)/sum((test[,target]-means)^2)
forest.adj.r.squared <- 1 - (1 - forest.r.squared) * adj.coe
getTree(randomForest(mydata[,-5], mydata[,5], ntree=10), 3, labelVar=TRUE)
# feature importance
importance(forest.model, type = 2)
mosaicplot(forest.model)
# Simple Scatterplot
#plot_ly(data = iris, x = ~test[,target], y = ~forest.predictions)
plot(test[,target], forest.predictions, xlab = "Actual UTS", ylab = "Forest Predictions", main = "Random Forest (R-sq = 0.92)")
abline(lm(forest.predictions ~ test[,target]))
#Plot Tree
x <- ctree(Gic ~ ., data=mydata)
plot(x, type="simple")
#Full Model
x <- ctree(Gic ~ ., data=mydata, controls=cforest_control(mtry=2, mincriterion=0))
plot(x, type="simple")


# conditional inference random forest(Cforest)
cforest.model <- cforest(Gic  ~ ., data = train, control = cforest_unbiased(ntree = 500))
cforest.predictions <- predict(cforest.model, newdata = test[,predictors])
cforest.r.squared <- sum((cforest.predictions-means)^2)/sum((test[,target]-means)^2)
cforest.adj.r.squared <- 1 - (1 - cforest.r.squared) * adj.coe
# feature importance
varimp(cforest.model)
# Simple Scatterplot
plot_ly(data = iris, x = ~test[,target], y = ~cforest.predictions)
#Plot Tree
x <- ctree(Gic ~ ., data=mydata)
plot(x, type="simple")
#Full Model
x <- ctree(Gic ~ ., data=mydata, controls=cforest_control(mtry=2, mincriterion=0))
plot(x, type="simple")



# Lasso regression
cvfit = cv.glmnet(as.matrix(train[,independent.variables.num]), train[,target], type.measure = "mse", alpha = 1, nfolds = 5)
plot(cvfit)
# lambda.min is the value of ?? that gives minimum mean cross-validated error. 
# The other ?? saved is lambda.1se, which gives the most regularized model such that error is within one standard error of the minimum. 
lasso.predictions <- predict(cvfit, newx = as.matrix(test[,independent.variables.num]), s = "lambda.min")
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
# Simple Scatterplot
plot_ly(data = iris, x = ~test[,target], y = ~lasso.predictions)

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