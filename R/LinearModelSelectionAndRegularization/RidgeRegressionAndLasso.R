x = model.matrix(Salary ~., Hitters)[, -1] ## remove intercept
y = Hitters$Salary

## ###############################
## ridge regression
## ###############################
library(glmnet)
grid = 10^seq(10, -2, length = 100)
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