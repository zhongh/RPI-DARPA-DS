library(glmnet)

# References:
#   http://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html
#   https://gerardnico.com/wiki/lang/r/ridge_lasso
#   https://www.datarobot.com/blog/r-getting-started-with-data-science/
#   https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html (* GOOD *)

# 2 Helper functions to calculate R2 and adjusted R2
R2 <- function(obs, pred){
  SS.res <- sum((obs - pred)^2)
  SS.tot <- sum((obs - mean(obs))^2)
  return(1 - SS.res/SS.tot)
}
adjR2 <- function(obs, pred, p){
  r.squared <- R2(obs, pred)  
  adj.r.squared <- r.squared - ( (1 - r.squared) * ( p/(length(obs) - p - 1) ) )
}

# Data: Assume we have the data imported, preprocessed, and saved to "mydata"
# (***) NOTE: This code is using Lockheed. If you use another dataset, simply make sure:
#         x is a numerical matrix of all the independent variables
#         y is a numerical vector(matrix) of the reponse variable
#         This method changes categorical variable into dummies automatically:
#             model.matrix(~ ., {WHATEVER DATA FRAME HERE})[, -1]

x <- model.matrix(~ ., mydata[, c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount", "Surface.Preparation", "Contaminate.Type")])[, -1]
y <- as.matrix(mydata[c("Gic")])

# Fitting the model (Lasso: Alpha = 1)
set.seed(999)
# Explanations:
# When y is 2-level factor: amily = 'binomial'
# When y is multi-level factor: amily = 'multinomial'
# When y is quantitative: family = 'gaussian' or 'poisson' (HERE)
#   Only 'mse', 'deviance' or 'mae'  available for Gaussian models
#   Only 'deviance', 'mse' or 'mae'  available for Poisson models
# alpha=1 is the lasso penalty, and alpha=0 the ridge penalty
# Standardization is by-default, so no worry on that part.

lasso.cv <- cv.glmnet(x, y, family = 'gaussian', nfolds = 10, alpha=1, type.measure='mse') # OR:
lasso.cv <- cv.glmnet(x, y, family = 'poisson', nfolds = 10, alpha=1, type.measure='mse')

for (i in seq(0, 1, 0.1)){
  print(glmnet(x, y, family = 'gaussian', alpha = i)$dev.ratio)
}

# Results
plot(lasso.cv)
plot(lasso.cv$glmnet.fit, xvar="lambda", label=TRUE)
lasso.cv$lambda.min
lasso.cv$lambda.1se
coef(lasso.cv, s=lasso.cv$lambda.min)

lasso.pred <- predict(lasso.cv, s=lasso.cv$lambda.1se, newx=x)
lasso.resid <- lasso.pred - y

plot(y, lasso.pred, xlab = colnames(y)[1], main = "Lasso Predictions")
abline(0, 1)
segments(y, y, y, lasso.pred, col="red")

plot(y, lasso.resid, xlab = colnames(y)[1], main = "Lasso Residuals")
abline(0, 0)
segments(y, 0, y, lasso.resid, col="red")

lasso.mse <- sqrt(mean(lasso.resid^2))
lasso.mse

lasso.r.squared <- R2(y, lasso.pred)
lasso.r.squared

lasso.adj.r.squared <- adjR2(y, lasso.pred, ncol(x))
lasso.adj.r.squared

lasso.cv$glmnet.fit$dev.ratio[which(lasso.cv$glmnet.fit$lambda == lasso.cv$lambda.1se)] 
