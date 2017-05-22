x = model.matrix(Salary ~., Hitters)[, -1] ## remove intercept
y = Hitters$Salary

## ###############################
## ridge regression
## ###############################
library(glmnet)
grid = 10^seq(10, -2, length = 100)
## alpha = 0 for ridge
## alpha = 1 for lasso
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod)) # 20 100(one for each value of lambda)

ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

## predict lambda
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]

## another sample
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train) ## except
y.test = y[test]

ridge.mod = glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred-y.test)^2)

## using cv to choose lambda
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

## pred
ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2)

## refit
## ridge does not select variables
out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

## ###############################
## lasso
## ###############################
lasso.mod = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((lasso.pred - y.test)^2)

out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s=bestlam)[1:20,]
lasso.coef

## variable selection
lasso.coef[lasso.coef!=0]