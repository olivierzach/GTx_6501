# -------------------- Code for Question 8.2 -----------------------------
# Clear environment

rm(list = ls())


# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)


# ---------------------------- Data manipulation -------------------------------------

#First, Read in the data
#
dat <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#
# optional check to make sure the data is read correctly
#

head(dat)

## M So   Ed  Po1  Po2    LF   M.F Pop   NW    U1  U2 Wealth Ineq     Prob    Time Crime
## 1 15.1  1  9.1  5.8  5.6 0.510  95.0  33 30.1 0.108 4.1   3940 26.1 0.084602 26.2011   791
## 2 14.3  0 11.3 10.3  9.5 0.583 101.2  13 10.2 0.096 3.6   5570 19.4 0.029599 25.2999  1635
## 3 14.2  1  8.9  4.5  4.4 0.533  96.9  18 21.9 0.094 3.3   3180 25.0 0.083401 24.3006   578
## 4 13.6  0 12.1 14.9 14.1 0.577  99.4 157  8.0 0.102 3.9   6730 16.7 0.015801 29.9012  1969
## 5 14.1  0 12.1 10.9 10.1 0.591  98.5  18  3.0 0.091 2.0   5780 17.4 0.041399 21.2998  1234
## 6 12.1  0 11.0 11.8 11.5 0.547  96.4  25  4.4 0.084 2.9   6890 12.6 0.034201 20.9995   682
# NOTE: ALL ROWS OF THIS FILE STARTING WITH "##" DENOTE R OUTPUT
#
# Crime is response, other variables are predictors
#

# *****************************
# Solution using lm()
# *****************************


# We use the entire dataset to build a regression model which is then used for prediction
# We're not choosing between models (so validation isn't needed)
# and we're not bothering to estimate model quality (so test data isn't needed)

model <- lm( Crime ~ ., data = dat)

#Summary of the model

summary(model)

## Residuals:
##   Min      1Q  Median      3Q     Max 
## -395.74  -98.09   -6.69  112.99  512.67 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -5.984e+03  1.628e+03  -3.675 0.000893 ***
## M            8.783e+01  4.171e+01   2.106 0.043443 *  
## So          -3.803e+00  1.488e+02  -0.026 0.979765    
## Ed           1.883e+02  6.209e+01   3.033 0.004861 ** 
## Po1          1.928e+02  1.061e+02   1.817 0.078892 .  
## Po2         -1.094e+02  1.175e+02  -0.931 0.358830    
## LF          -6.638e+02  1.470e+03  -0.452 0.654654    
## M.F          1.741e+01  2.035e+01   0.855 0.398995    
## Pop         -7.330e-01  1.290e+00  -0.568 0.573845    
## NW           4.204e+00  6.481e+00   0.649 0.521279    
## U1          -5.827e+03  4.210e+03  -1.384 0.176238    
## U2           1.678e+02  8.234e+01   2.038 0.050161 .  
## Wealth       9.617e-02  1.037e-01   0.928 0.360754    
## Ineq         7.067e+01  2.272e+01   3.111 0.003983 ** 
## Prob        -4.855e+03  2.272e+03  -2.137 0.040627 *  
## Time        -3.479e+00  7.165e+00  -0.486 0.630708    
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 209.1 on 31 degrees of freedom
## Multiple R-squared:  0.8031,	Adjusted R-squared:  0.7078 
## F-statistic: 8.429 on 15 and 31 DF,  p-value: 3.539e-07


#Create the test datapoint manually using dataframe

test <-data.frame(M = 14.0,So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.040,Time = 39.0)


#Predict the crime rate for test data point

pred_model <- predict(model, test)
pred_model

## 155.4349 

# This is unexpected!
# The estimate is less than half of the crime rate of the next-lowest city.
# None of the factor values of the test data point
# are outside the range of the other data points, so that's not
# the explanation.
#
# What might be going on?
#
# I specifically chose this data point as a demonstration.
# The full model we used above includes a lot of insignificant factors.
# You might wonder, "Why not just use the whole model, even if some
# factors are insignificant?"
# This is why!
#
# Let's go back and just use the singificant factors to get an estimate.
# We'll try using all of the factors with p<=0.1.
# (In Module 11, we'll see better ways of going about this.)

model2 <- lm( Crime ~  M + Ed + Po1 + U2 + Ineq + Prob, data = dat)

#Summary of the model

summary(model2)

## Residuals:
##   Min      1Q  Median      3Q     Max 
## -470.68  -78.41  -19.68  133.12  556.23 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -5040.50     899.84  -5.602 1.72e-06 ***
##   Ed            196.47      44.75   4.390 8.07e-05 ***
##   Po1           115.02      13.75   8.363 2.56e-10 ***
##   Ineq           67.65      13.94   4.855 1.88e-05 ***
##   M             105.02      33.30   3.154  0.00305 ** 
##   Prob        -3801.84    1528.10  -2.488  0.01711 *  
##   U2             89.37      40.91   2.185  0.03483 *  
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 200.7 on 40 degrees of freedom
## Multiple R-squared:  0.7659,	Adjusted R-squared:  0.7307 
## F-statistic: 21.81 on 6 and 40 DF,  p-value: 3.418e-11

#Predict on our test observation

pred_model2 <- predict(model2, test)
pred_model2

## 1304.245 

#This seems like a more reasonable prediction, now that the insignificant factors are gone.

# Oops, I forgot that we actually *do* want to know the model quality.
# We can't just use what's reported above, because that's on the training data.
#

# Install the DAAG package, which has cross-validation functions

install.packages("DAAG")
library(DAAG)

# do 5-fold cross-validation

c <- cv.lm(dat,model2,m=5) # note that here, "m" is used for the number of folds, rather than the usual "k"
c

# The overall mean squared prediction error in cross-validation is shown as "ms".
# NOTE that there seems to be a typo in cv.lm -- 
# when it says "sum over all n folds", n is actually the number
# of data points in the last fold, not the number of folds.

# We can calculate the R-squared values directly.
# R-squared = 1 - SSEresiduals/SSEtotal
#
# total sum of squared differences between data and its mean

SStot <- sum((dat$Crime - mean(dat$Crime))^2)

# for model, model2, and cross-validation, calculated SEres

SSres_model <- sum(model$residuals^2)

SSres_model2 <- sum(model2$residuals^2)

SSres_c <- attr(c,"ms")*nrow(dat) # mean squared error, times number of data points, gives sum of squared errors

# Calculate R-squareds for model, model2, cross-validation

1 - SSres_model/SStot # initial model with insignificant factors

## 0.803

1 - SSres_model2/SStot # model2 without insignificant factors

## 0.766

1 - SSres_c/SStot # cross-validated

## 0.638

# So, this shows that including the insignificant factors overfits compared to removing them,
# and even the fitted model is probably overfitted.
# That's not so surprising, since we started with just 47 data points and we have 15 factors to predict from.
# The ratio of data points to factors is about 3:1, 
# and it's usually good to have 10:1 or more.
#
# We'll see in Module 11 ways we can try to get around this problem.

# We can also try cross-validation on the first, 15-factor model

cfirst <- cv.lm(dat,model,m=5)

SSres_cfirst <- attr(cfirst,"ms")*nrow(dat) # mean squared error, times number of data points, gives sum of squared errors

1 - SSres_cfirst/SStot # cross-validated

## 0.413

# That's a huge difference from the 0.803 reported by lm() on the training data, which demonstrates the need to validate!


# *****************************
# Solution using glm()
# *****************************

# We can do the same things using glm() instead of lm().
#
# glm() is a more-general function for regression.

g <- glm(Crime ~ . , data=dat, family="gaussian")
summary(g)

## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -5.98e+03   1.63e+03   -3.68  0.00089 ***
## M            8.78e+01   4.17e+01    2.11  0.04344 *  
## So          -3.80e+00   1.49e+02   -0.03  0.97977    
## Ed           1.88e+02   6.21e+01    3.03  0.00486 ** 
## Po1          1.93e+02   1.06e+02    1.82  0.07889 .  
## Po2         -1.09e+02   1.17e+02   -0.93  0.35883    
## LF          -6.64e+02   1.47e+03   -0.45  0.65465    
## M.F          1.74e+01   2.04e+01    0.86  0.39900    
## Pop         -7.33e-01   1.29e+00   -0.57  0.57385    
## NW           4.20e+00   6.48e+00    0.65  0.52128    
## U1          -5.83e+03   4.21e+03   -1.38  0.17624    
## U2           1.68e+02   8.23e+01    2.04  0.05016 .  
## Wealth       9.62e-02   1.04e-01    0.93  0.36075    
## Ineq         7.07e+01   2.27e+01    3.11  0.00398 ** 
## Prob        -4.86e+03   2.27e+03   -2.14  0.04063 *  
## Time        -3.48e+00   7.17e+00   -0.49  0.63071    
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

g2 <- glm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob , data=dat, family="gaussian")
summary(g2)

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -5040.5      899.8   -5.60  1.7e-06 ***
## M              105.0       33.3    3.15   0.0031 ** 
## Ed             196.5       44.8    4.39  8.1e-05 ***
## Po1            115.0       13.8    8.36  2.6e-10 ***
## U2              89.4       40.9    2.18   0.0348 *  
## Ineq            67.7       13.9    4.85  1.9e-05 ***
## Prob         -3801.8     1528.1   -2.49   0.0171 *  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# for cross-validation, we need the boot library

library(boot)

cg <- cv.glm(dat,g,K=5) # note that here, K is the number of folds
cg2 <- cv.glm(dat,g2,K=5)

# mean squared error is cg$delta[1]

1 - cg$delta[1]*nrow(dat)/SStot

## 0.281 
# depending on random seed, this could be different; 
# a second time I got 0.427
# That's a low R-squared value when cross-validating the 15-factor model

1 - cg2$delta[1]*nrow(dat)/SStot

## 0.671
# depending on random seed, this could be different; 
# a second time I got 0.673

