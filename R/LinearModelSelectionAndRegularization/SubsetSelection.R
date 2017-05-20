## ###########################################
## Best Subset Selection
## ###########################################

library(ISLR)
dim(Hitters)
colnames(Hitters)
sum(is.na(Hitters$Salary)) # 59

## removes all of the rows that have missing values in any variable
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

## best subset
library(leaps)
regfit.full = regsubsets(Salary~., Hitters)
summary(regfit.full)

## An asterisk indicates that a given variable is included in the corresponding model.
regfit.full = regsubsets (Salary~., data = Hitters , nvmax =19)
reg.summary = summary(regfit.full)

names(reg.summary)
reg.summary$rsq
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
## label the largest adjusted r^2 statistic
which.max(reg.summary$adjr2) # 11
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp) # 10
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)

## bic
which.min(reg.summary$bic) # 6
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

## build-in
plot ( regfit.full , scale ="r2")
plot ( regfit.full , scale ="adjr2")
plot ( regfit.full , scale ="Cp")
plot ( regfit.full , scale ="bic")

## for bic, only 6 variables
coef(regfit.full, 6)


## ###########################################
## Forward and Backward Stepwise Selection
## ###########################################

## forward
regfit.fwd = regsubsets(Salary ~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary ~., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

## the best seven-variable models identified by forward stepwise selection, 
## backward stepwise selection, and best subset selection are different.
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

## ###########################################
## Cross-validation
## ###########################################

set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
test = (!train)

regfit.best = regsubsets(Salary~., data = Hitters[train, ], nvmax = 19)
test.mat = model.matrix(Salary~., data = Hitters[test, ])

## compute test MSE
val.errors = rep(NA, 19)
for (i in 1:19)
{
  coefi = coef(regfit.best, id = i)
  pred = test.mat[, names(coefi)] %*% coefi
  val.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best, 10)

## apture our steps above and write our own predict method.
predict.regsubsets = function(object, newdata, id, ...)
{
  form = as.formula(object$call[[2]]) ## extract the formula
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}

## cv
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k)
{
  best.fit = regsubsets(Salary~., data = Hitters[folds!=j, ], nvmax = 19)
  for (i in 1:19)
  {
    pred = predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

## a 10*19 matrix
## the (i, j)th element corresponds to the test MSE for the 
## i-th cross-validation fold for the best j-variable model.
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

## plot
plot(mean.cv.errors, type = 'b')
## choose 11
reg.best = regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best, 11)
