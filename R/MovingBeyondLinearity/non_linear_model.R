library(ISLR)
attach(Wage)
## overview the dataset
dim(Wage)
colnames(Wage)
head(Wage)
## Polynomial Regression and Step Functions
fit = lm ( wage ~ poly ( age ,4) , data = Wage )
## #########################################################
## The function returns a matrix whose columns are a basis of or-
## thogonal polynomials, which essentially means that each column is a linear
## combination of the variables age , age^2 , age^3 and age^4 .
## #########################################################
## fit = lm ( wage ~ poly ( age, 4, raw = T) , data = Wage )
coef ( summary ( fit ) )

## or
fit2a = lm ( wage ~ age + I ( age ^2) + I ( age ^3) + I ( age ^4) , data = Wage )
coef ( fit2a )
## or
fit2b = lm ( wage ~ cbind ( age , age ^2 , age ^3 , age ^4) , data = Wage )

## predict
agelims = range ( age )
age.grid = seq ( from = agelims [1] , to = agelims [2])
preds = predict (fit, newdata = list ( age = age.grid ) , se = TRUE )
se.bands = cbind ( preds$fit +2* preds$se.fit , preds$fit -2* preds$se.fit )

## plot
par ( mfrow = c (1 ,2) , mar = c (4.5 ,4.5 ,1 ,1) , oma = c (0 ,0 ,4 ,0) )
## ####################################
## oma for lines of text
## mar for sides of plot
## ####################################
plot ( age , wage , xlim = agelims , cex = .5 , col ="darkgrey")
title (" Degree -4 Polynomial " , outer = T )
lines ( age.grid, preds$fit, lwd =2 , col ="blue")
matlines ( age.grid, se.bands, lwd =1 , col ="blue" , lty =3)

## ########################################
## performs an analysis of variance
## test the null analysis of hypothesis that 
## 
## a model M1 is sufficient to explain the data against 
## the variance alternative hypothesis that a more complex model M2 is required.
##
## to use the anova() function, 
## M1 and M2 must be nested models: 
## the predictors in M1 must be a subset of the predictors in M2.
## ########################################

fit.1= lm ( wage~age , data = Wage )
fit.2= lm ( wage~poly ( age ,2) , data = Wage )
fit.3= lm ( wage~poly ( age ,3) , data = Wage )
fit.4= lm ( wage~poly ( age ,4) , data = Wage )
fit.5= lm ( wage~poly ( age ,5) , data = Wage )
anova ( fit.1 , fit.2 , fit.3 , fit.4 , fit.5)

## ###########################################
## The p-value comparing 
## the linear Model 1 to the quadratic Model 2 is essentially zero (<10 −15 ), 
## indicating that a linear fit is not sufficient. 
##
## Similarly the p-value comparing the quadratic Model 2 to the cubic Model 3
## is very low (0.0017), so the quadratic fit is also insufficient. 
##
## The p-value comparing the cubic and degree-4 polynomials, Model 3 and Model 4 , is ap-
## proximately 5 % while the degree-5 polynomial Model 5 seems unnecessary
## because its p-value is 0.37. 
##
## Hence, either a cubic or a quartic polynomial
## appear to provide a reasonable fit to the data, but lower- or higher-order
## models are not justified.
## ###########################################

## ###########################################
## In this case, instead of using the anova() function, we could have obtained
## these p-values more succinctly by exploiting the fact that poly() creates
## orthogonal polynomials.
## ###########################################

coef ( summary ( fit.5) )

## the square of the t-statistics are equal to the F-statistics from the anova() function

## ###########################################
## glm
## ###########################################

fit = glm ( I ( wage >250) ~ poly ( age ,4) , data = Wage , family = binomial )
preds = predict ( fit , newdata = list ( age = age.grid ) , se = T )

pfit = exp ( preds$fit ) /(1+ exp ( preds$fit ) )
se.bands.logit = cbind ( preds$fit +2* preds$se.fit , preds$fit -2*preds$se.fit )
se.bands = exp ( se.bands.logit ) /(1+ exp ( se.bands.logit ) )

## or
preds = predict ( fit , newdata = list ( age = age.grid ) , type ="response" , se = T )

## plot
plot ( age , I ( wage >250) , xlim = agelims , type ="n" , ylim = c (0 ,.2) )
points ( jitter ( age ) , I (( wage >250) /5) , cex =.5 , pch ="|" ,
           col ="darkgrey")
## jitter() function to jitter the age values a bit so that observations with the same age value do not cover each other up. This is often called a
## rug plot.
lines ( age.grid , pfit , lwd =2 , col ="blue")
matlines ( age.grid , se.bands , lwd =1 , col ="blue" , lty =3)

## #############################################
## fit a step function
## #############################################

table ( cut ( age ,4) )
fit = lm ( wage ~ cut ( age ,4) , data = Wage )
coef ( summary ( fit ) )

## #############################################
## splines
## #############################################

library ( splines )
fit = lm ( wage~bs ( age , knots = c (25 ,40 ,60) ) , data = Wage )
pred = predict ( fit , newdata = list ( age = age.grid ) , se = T )
plot ( age , wage , col ="gray")
lines ( age.grid , pred$fit , lwd =2)
lines ( age.grid , pred$fit +2* pred$se , lty ="dashed")
lines ( age.grid , pred$fit -2* pred$se , lty ="dashed")

## a cubic spline with three knots
## has seven degrees of freedom; these degrees of freedom are used up by an
## intercept, plus six basis functions.

## #############################################
## natural splines
## #############################################
fit2 = lm ( wage~ns ( age , df =4) , data = Wage )
pred2 = predict ( fit2 , newdata = list ( age = age.grid ) , se = T )
lines ( age.grid , pred2$fit , col ="red" , lwd =2)

## #############################################
## smooth splines
## #############################################

par(mfrow=c(1,1))
plot ( age , wage , xlim = agelims , cex =.5 , col ="darkgrey")
title (" Smoothing Spline ")
fit = smooth.spline ( age , wage , df = 16)
fit2 = smooth.spline ( age , wage , cv = TRUE )
lines ( fit , col ="red" , lwd =2)
lines ( fit2 , col ="blue" , lwd =2)
legend ("topright" , legend = c ("16 DF" ,"6.8 DF") ,
          col = c ("red", "blue") , lty =1 , lwd =2 , cex =.8)

## #############################################
## perform local regression (loess())
## #############################################

plot ( age , wage , xlim = agelims , cex =.5 , col ="darkgrey")
title (" Local Regression ")
fit = loess ( wage~age , span =.2 , data = Wage )
fit2 = loess ( wage~age , span =.5 , data = Wage )
lines ( age.grid , predict ( fit , data.frame ( age = age.grid ) ) ,
        col ="red" , lwd =2)
lines ( age.grid , predict ( fit2 , data.frame ( age = age.grid ) ) ,
          col ="blue" , lwd =2)
legend ("topright" , legend = c (" Span =0.2" ," Span =0.5") ,
          col = c ("red" ,"blue") , lty =1 , lwd =2 , cex =.8)

## Here we have performed local linear regression using spans of 0.2 and 0.5:
## that is, each neighborhood consists of 20 % or 50 % of the observations 

## #############################################
## GAMs
## #############################################
## using natural splines
gam1 = lm ( wage~ns ( year ,4) + ns ( age ,5) + education , data = Wage )
## using smoothing splines
library(gam)
gam.m3 = gam ( wage~s ( year ,4) + s ( age ,5) + education , data = Wage )
par ( mfrow = c (1 ,3) )
plot ( gam.m3 , se = TRUE , col = "blue")
gam.m1 = gam ( wage~s ( age ,5) + education , data = Wage )
gam.m2 = gam ( wage~year + s ( age ,5) + education , data = Wage )
anova ( gam.m1 , gam.m2 , gam.m3 , test ="F")

## based on the results of this ANOVA, M2 is preferred.

summary(gam.m3)
## The p-values for year and age correspond to a null hypothesis of a linear
## relationship versus the alternative of a non-linear relationship. The large
## p-value for year reinforces our conclusion from the ANOVA test that 
## a linear function is adequate for this term.

## predict
preds = predict ( gam.m2 , newdata = Wage )

## local regression
gam.lo = gam ( wage~s ( year , df =4) + lo ( age , span =0.7) + education ,
                 data = Wage )

## interaction
gam.lo.i = gam ( wage~lo ( year , age , span =0.5) + education ,
                     data = Wage )
library ( akima )
plot ( gam.lo.i)

## fit a logistic regression GAM
gam.lr = gam ( I ( wage >250)~year + s ( age , df =5) + education ,
                 family = binomial , data = Wage )

## It is easy to see that there are no high earners in the <HS category:
table ( education , I ( wage >250) )

## fit a logistic regression GAM using all but this category
gam.lr.s = gam ( I ( wage >250)~year + s ( age , df =5) + education , family =
                       binomial , data = Wage , subset =( education !="1. < HS Grad") )
plot ( gam.lr.s , se =T , col ="green")