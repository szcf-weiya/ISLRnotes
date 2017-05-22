library(ISLR)
dim(Hitters)
colnames(Hitters)
sum(is.na(Hitters$Salary)) # 59

## removes all of the rows that have missing values in any variable
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

## pcr
library(pls)
set.seed(2)
pcr.fit = pcr(Salary~., data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

## train
set.seed(1)
pcr.fit = pcr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
## pred
pcr.pred = predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test)^2)
## full
pcr.fit = pcr(y~x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

## ##################################
## pls
## ##################################
pls.fit = plsr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)

## pred
pls.pred = predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2)
pls.fit = plsr(Salary~., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)
