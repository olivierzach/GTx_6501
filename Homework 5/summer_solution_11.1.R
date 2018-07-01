# -------------------- Code for Question 11.1 -----------------------------
# Clear environment

rm(list = ls())

# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)


# ---------------------------- Data manipulation -------------------------------------

# First, read in the data
#

data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#
# Optional check to make sure the data is read correctly
#

head(data)

##      M So   Ed  Po1  Po2    LF   M.F Pop   NW    U1  U2 Wealth Ineq     Prob    Time Crime
## 1 15.1  1  9.1  5.8  5.6 0.510  95.0  33 30.1 0.108 4.1   3940 26.1 0.084602 26.2011   791
## 2 14.3  0 11.3 10.3  9.5 0.583 101.2  13 10.2 0.096 3.6   5570 19.4 0.029599 25.2999  1635
## 3 14.2  1  8.9  4.5  4.4 0.533  96.9  18 21.9 0.094 3.3   3180 25.0 0.083401 24.3006   578
## 4 13.6  0 12.1 14.9 14.1 0.577  99.4 157  8.0 0.102 3.9   6730 16.7 0.015801 29.9012  1969
## 5 14.1  0 12.1 10.9 10.1 0.591  98.5  18  3.0 0.091 2.0   5780 17.4 0.041399 21.2998  1234
## 6 12.1  0 11.0 11.8 11.5 0.547  96.4  25  4.4 0.084 2.9   6890 12.6 0.034201 20.9995   682
# NOTE: ALL ROWS OF THIS FILE STARTING WITH "##" DENOTE R OUTPUT
#
# Crime is response, other variables are predictors




# ---------------------------- Stepwise Regression -------------------------------------

# Stepwise Regression using original variables and Cross Validation
# In backward stepwise regression. Our lower model will have only the intercept
# and all variables in our full model.

#Scaling the data except the response variable and categorical

scaledData = as.data.frame(scale(data[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
scaledData <- cbind(data[,2],scaledData,data[,16]) # Add column 2 back in
colnames(scaledData)[1] <- "So"
colnames(scaledData)[16] <- "Crime"


library(caret)

# Now using the code below to perform 5 fold CV

ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

lmFit_Step <- train(Crime ~ ., data = scaledData, "lmStepAIC", scope = 
                 list(lower = Crime~1, upper = Crime~.), direction = "backward",trControl=ctrl)

##Step:  AIC=503.93
##.outcome ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob
##
##Df Sum of Sq     RSS    AIC
##<none>              1453068 503.93
##- M.F   1    103159 1556227 505.16
##- U1    1    127044 1580112 505.87
##- Prob  1    247978 1701046 509.34
##- U2    1    255443 1708511 509.55
##- M     1    296790 1749858 510.67
##- Ed    1    445788 1898855 514.51
##- Ineq  1    738244 2191312 521.24
##- Po1   1   1672038 3125105 537.93

#Fitting a new model with these 8 variables

mod_Step = lm(Crime ~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1, data = scaledData)
summary(mod_Step)

##Coefficients:
##Estimate Std. Error t value Pr(>|t|)    
##(Intercept)   905.09      28.52  31.731  < 2e-16 ***
##M.F            65.83      40.08   1.642  0.10874    
##U1           -109.73      60.20  -1.823  0.07622 .  
##Prob          -86.31      33.89  -2.547  0.01505 *  
##U2            158.22      61.22   2.585  0.01371 *  
##M             117.28      42.10   2.786  0.00828 ** 
##Ed            201.50      59.02   3.414  0.00153 ** 
##Ineq          244.70      55.69   4.394 8.63e-05 ***
##Po1           305.07      46.14   6.613 8.26e-08 ***
##---
##Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##Residual standard error: 195.5 on 38 degrees of freedom
##Multiple R-squared:  0.7888,	Adjusted R-squared:  0.7444 
##F-statistic: 17.74 on 8 and 38 DF,  p-value: 1.159e-10

#We obtain an Adjusted R-SQuared value = 0.7444 using the selected 8 variables using
# Backward StepWise regression and Cross Validation

# Now let's use cross-validation to see how good this model
# really is.  Because we only have 47 data points, let's use
# 47-fold cross-validation (equivalently, leave-one-out
# cross-validation).

SStot <- sum((data$Crime - mean(data$Crime))^2)
totsse <- 0
for(i in 1:nrow(scaledData)) {
  mod_Step_i = lm(Crime ~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1, data = scaledData[-i,])
  pred_i <- predict(mod_Step_i,newdata=scaledData[i,])
  totsse <- totsse + ((pred_i - data[i,16])^2)
}
R2_mod <- 1 - totsse/SStot
R2_mod

## 0.668

# Notice that in the model above, the p-value for M.F is above 0.1.
# We might keep it in the model, because it's close to 0.1 and
# might be important.  That's what we tested above.
# Or, we might remove it, and re-run the model without it.
# Let's see what happens if we do:

mod_Step = lm(Crime ~ U1+Prob+U2+M+Ed+Ineq+Po1, data = scaledData)
summary(mod_Step)

##Coefficients:
##            Estimate Std. Error t value Pr(>|t|)    
##(Intercept)    905.1       29.1   31.06  < 2e-16 ***
##U1             -63.9       54.5   -1.17   0.2482    
##Prob           -84.8       34.6   -2.45   0.0188 *  
##U2             134.1       60.7    2.21   0.0331 *  
##M              134.2       41.7    3.22   0.0026 ** 
##Ed             244.4       54.1    4.52  5.6e-05 ***
##Ineq           264.7       55.5    4.77  2.6e-05 ***
##Po1            314.9       46.7    6.74  4.9e-08 ***

# Now notice that U1 doesn't look significant... so we can take
# it out too, and re-run the model.

mod_Step = lm(Crime ~ Prob+U2+M+Ed+Ineq+Po1, data = scaledData)
summary(mod_Step)

##Coefficients:
##            Estimate Std. Error t value Pr(>|t|)    
##(Intercept)    905.1       29.3   30.92  < 2e-16 ***
##Prob           -86.4       34.7   -2.49   0.0171 *  
##U2              75.5       34.5    2.18   0.0348 *  
##M              132.0       41.8    3.15   0.0031 ** 
##Ed             219.8       50.1    4.39  8.1e-05 ***
##Ineq           269.9       55.6    4.85  1.9e-05 ***
##Po1            341.8       40.9    8.36  2.6e-10 ***
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
##Residual standard error: 201 on 40 degrees of freedom
##Multiple R-squared:  0.766,     Adjusted R-squared:  0.731 
##F-statistic: 21.8 on 6 and 40 DF,  p-value: 3.42e-11

# This model looks good, so now let's see how it cross-validates:

SStot <- sum((data$Crime - mean(data$Crime))^2)
totsse <- 0
for(i in 1:nrow(scaledData)) {
  mod_Step_i = lm(Crime ~ Prob+U2+M+Ed+Ineq+Po1, data = scaledData[-i,])
  pred_i <- predict(mod_Step_i,newdata=scaledData[i,])
  totsse <- totsse + ((pred_i - data[i,16])^2)
}
R2_mod <- 1 - totsse/SStot
R2_mod

## 0.666

# So, cross-validation shows that it's about the same whether
# we include M.F and U1 (0.668) or not (0.666).  That gives some
# support to the idea that M.F and U1 really aren't significant.
# Since the quality is about the same, we should probably use the 
# simpler model.  

# ---------------------------- Lasso Regression -------------------------------------

library(glmnet)

#building lasso

XP=data.matrix(scaledData[,-16])
YP=data.matrix(scaledData$Crime)
lasso=cv.glmnet(x=as.matrix(scaledData[,-16]),y=as.matrix(scaledData$Crime),alpha=1,
                nfolds = 5,type.measure="mse",family="gaussian")

#Output the coefficients of the variables selected by lasso

coef(lasso, s=lasso$lambda.min)

##(Intercept) 894.63
##So           30.73
##M            70.51
##Ed           79.21
##Po1         306.73
##Po2           .   
##LF            .   
##M.F          49.48
##Pop           .   
##NW            3.32
##U1            .   
##U2           21.04
##Wealth        .   
##Ineq        150.53
##Prob        -72.29
##Time          .   


#Fitting a new model with these 9 variables

mod_lasso = lm(Crime ~So+M+Ed+Po1+M.F+NW+U2+Ineq+Prob, data = scaledData)
summary(mod_lasso)

##Coefficients:
##Estimate Std. Error t value Pr(>|t|)    
##(Intercept)    871.4       50.5   17.24  < 2e-16 ***
##So              99.0      119.9    0.83  0.41440    
##M              109.9       48.9    2.25  0.03073 *  
##Ed             197.1       62.0    3.18  0.00300 ** 
##Po1            333.4       48.9    6.83  4.9e-08 ***
##M.F             40.5       39.0    1.04  0.30538    
##NW               4.1       57.4    0.07  0.94347    
##U2              63.3       36.9    1.71  0.09472 .  
##Ineq           237.7       66.1    3.60  0.00093 ***
##Prob          -101.9       39.5   -2.58  0.01389 *  
##---
##Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##Residual standard error: 204 on 37 degrees of freedom
##Multiple R-squared:  0.775,	Adjusted R-squared:  0.72 
##F-statistic: 14.2 on 9 and 37 DF,  p-value: 1.54e-09

#We obtain a slightly lower Adjusted R-SQuared value = 0.72 using the selected 9 variables using
# Lasso regression and Cross Validation

# Now let's see how it cross-validates:

SStot <- sum((data$Crime - mean(data$Crime))^2)
totsse <- 0
for(i in 1:nrow(scaledData)) {
  mod_lasso_i = lm(Crime ~ So+M+Ed+Po1+M.F+NW+U2+Ineq+Prob, data = scaledData[-i,])
  pred_i <- predict(mod_lasso_i,newdata=scaledData[i,])
  totsse <- totsse + ((pred_i - data[i,16])^2)
}
R2_mod <- 1 - totsse/SStot
R2_mod

## 0.62

# But notice that three of the variables above (So, M.F, and NW)
# don't appear to be significant.  Let's remove them and see
# what happens.
#
# In fact, it's exactly the same model we got above, after we
# removed the potentially-insignificant variables from the 
# stepwise regression model!
#
# So, I'm not going to re-run it here; just look up there.



# ---------------------------- Elastic Net -------------------------------------

#We vary alpha in steps of 0.1 from 0 to 1 and calculate the resultant R-Squared values

R2=c()
for (i in 0:10) {
  mod_elastic = cv.glmnet(x=as.matrix(scaledData[,-16]),y=as.matrix(scaledData$Crime),
                    alpha=i/10,nfolds = 5,type.measure="mse",family="gaussian")


#The deviance(dev.ratio ) shows the percentage of deviance explained, 
#(equivalent to r squared in case of regression)
  
  R2 = cbind(R2,mod_elastic$glmnet.fit$dev.ratio[which(mod_elastic$glmnet.fit$lambda == mod_elastic$lambda.min)])

}

R2

##      [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11]
##[1,] 0.736 0.744 0.767 0.758 0.765 0.777 0.774 0.782 0.791 0.788 0.792

#Best value of alpha

alpha_best = (which.max(R2)-1)/10
alpha_best

## 1

#Therefore we find that the best value of alpha may not lie somewhether between 0 and 1

#Lets build the model using this alpha value.

Elastic_net=cv.glmnet(x=as.matrix(scaledData[,-16]),y=as.matrix(scaledData$Crime),alpha=alpha_best,
                         nfolds = 5,type.measure="mse",family="gaussian")

#Output the coefficients of the variables selected by Elastic Net

coef(Elastic_net, s=Elastic_net$lambda.min)

##(Intercept) 890.51243
##So           42.80724
##M            96.24185
##Ed          156.03106
##Po1         266.57855
##Po2          18.97955
##LF            .      
##M.F          60.50018
##Pop          -7.39804
##NW           17.91207
##U1          -63.24503
##U2          102.37302
##Wealth       41.09751
##Ineq        216.57041
##Prob        -88.79877
##Time          . 

# The Elastic Net selects 13 variables compared to 10 in Lasso and 8 in Step Wise. Next we compare how this new model performs 
# compared to the Lasso and Step Wise models

mod_Elastic_net = lm(Crime ~So+M+Ed+Po1+Po2+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data = scaledData)
summary(mod_Elastic_net)

##Coefficients:
##Estimate Std. Error t value Pr(>|t|)    
##(Intercept)   893.38      52.51  17.012  < 2e-16 ***
##So             34.40     127.12   0.271  0.78840    
##M             109.87      49.82   2.205  0.03451 *  
##Ed            202.41      64.00   3.163  0.00335 ** 
##Po1           501.63     287.30   1.746  0.09012 .  
##Po2          -215.08     288.65  -0.745  0.46148    
##M.F            43.45      48.99   0.887  0.38162    
##Pop           -36.21      46.10  -0.785  0.43784    
##NW             24.91      58.61   0.425  0.67360    
##U1            -86.62      66.24  -1.308  0.20002    
##U2            136.97      67.41   2.032  0.05027 .  
##Wealth         82.03      96.17   0.853  0.39983    
##Ineq          275.77      86.79   3.177  0.00322 ** 
##Prob          -95.16      41.52  -2.292  0.02843 *  
##---
##Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##Residual standard error: 204 on 33 degrees of freedom
##Multiple R-squared:  0.8005,	Adjusted R-squared:  0.7219 
##F-statistic: 10.19 on 13 and 33 DF,  p-value: 4.088e-08

# The R-SQuared value is similar using Elastic Net and 13 variables. Therefore this method 
# may not be doing a good job as it selects 3 more variables for a similar RSquared value

# Now let's see how it cross-validates:

SStot <- sum((data$Crime - mean(data$Crime))^2)
totsse <- 0
for(i in 1:nrow(scaledData)) {
  mod_lasso_i = lm(Crime ~ So+M+Ed+Po1+Po2+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data = scaledData[-i,])
  pred_i <- predict(mod_lasso_i,newdata=scaledData[i,])
  totsse <- totsse + ((pred_i - data[i,16])^2)
}
R2_mod <- 1 - totsse/SStot
R2_mod

## 0.574

# That's a much worse cross-validated R-squared estimate. Why?
# As before, look at the p-values.  Most of those variables' 
# p-values seem to indicate that they're not significant.
# If we remove them all, we're left with M, Ed, Po1, U2, Ineq,
# and Prob.
# Does that look familiar?  It should -- it's the same set of 6
# variables we were left with after removing insignificant ones
# from the Stepwise and Lasso models above!

# Before we quit, let's go back and use PCA on the variables, 
# and then build Stepwise, Lasso, and Elastic Net models using
# the principal components.

# ================================================

# -------Implementing the above 3 models using Principal Component Analysis---------

# Run PCA on matrix of scaled predictors

pca <- prcomp(data[,1:15], scale. = TRUE)
summary(pca)

## Importance of components:
##                          PC1   PC2   PC3    PC4    PC5    PC6    PC7    PC8    PC9   PC10   PC11    PC12    PC13   PC14
## Standard deviation     2.453 1.674 1.416 1.0781 0.9789 0.7438 0.5673 0.5544 0.4849 0.4471 0.4191 0.35804 0.26333 0.2418
## Proportion of Variance 0.401 0.187 0.134 0.0775 0.0639 0.0369 0.0214 0.0205 0.0157 0.0133 0.0117 0.00855 0.00462 0.0039
## Cumulative Proportion  0.401 0.588 0.722 0.7992 0.8631 0.9000 0.9214 0.9419 0.9576 0.9709 0.9826 0.99117 0.99579 0.9997
##                          PC15
## Standard deviation     0.06793
## Proportion of Variance 0.00031
## Cumulative Proportion  1.00000


# The following are useful visualizations when deciding how many principal components to choose.

screeplot(pca, type="lines",col="blue")

var <- pca$sdev^2
propvar <- var/sum(var)

plot(propvar, xlab = "Principal Component", ylab = "Proportion of Variance Explained",ylim = c(0,1), type = "b")

plot(cumsum(propvar), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

# For the purpose of this question, let us use all the PCs instead of the original variables
# and evaluate the performance of the 3 above models


# Creating a dataframe of response variable and PCs
#------------------

PCcrime <- as.data.frame(cbind(pca$x, data[,16]))
colnames(PCcrime)[16] <- "Crime"


# ---------------------------- Stepwise Regression -------------------------------------

# Stepwise Regression using PCs and Cross Validation
# In backward stepwise regression. Our lower model will have only the intercept
# and all variables in our full model.


library(caret)

# Now use the code below to perform 5 fold CV

ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
set.seed(1)
lmFit_Step_PC <- train(Crime ~ ., data = PCcrime, "lmStepAIC", scope = 
                      list(lower = Crime~1, upper = Crime~.), direction = "backward",trControl=ctrl)

##Step:  AIC=507.37
##.outcome ~ PC1 + PC2 + PC4 + PC5 + PC6 + PC7 + PC12 + PC14 + 
##PC15
##
##Df Sum of Sq     RSS    AIC
##<none>              1498159 507.37
##- PC15  1     82173 1580332 507.88
##- PC6   1     92261 1590420 508.18
##- PC14  1    129212 1627371 509.26
##- PC7   1    203535 1701694 511.36
##- PC4   1    257832 1755991 512.83
##- PC12  1    494595 1992754 518.78
##- PC2   1    633037 2131196 521.94
##- PC1   1   1177568 2675727 532.63
##- PC5   1   2312556 3810715 549.25

#Fitting a new model with these 9 PCS

mod_Step_PC = lm(Crime ~ PC15+PC6+PC14+PC7+PC4+PC12+PC2+PC1+PC5, data = PCcrime)
summary(mod_Step_PC)

##Coefficients:
##Estimate Std. Error t value Pr(>|t|)    
##(Intercept)   905.09      29.35  30.836  < 2e-16 ***
##PC15         -622.21     436.77  -1.425 0.162660    
##PC6           -60.21      39.89  -1.509 0.139665    
##PC14          219.19     122.70   1.786 0.082235 .  
##PC7           117.26      52.30   2.242 0.031040 *  
##PC4            69.45      27.52   2.523 0.016048 *  
##PC12          289.61      82.87   3.495 0.001249 ** 
##PC2           -70.08      17.72  -3.954 0.000334 ***
##PC1            65.22      12.09   5.393 4.17e-06 ***
##PC5          -229.04      30.31  -7.557 5.20e-09 ***
##---
##Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##Residual standard error: 201.2 on 37 degrees of freedom
##Multiple R-squared:  0.7823,	Adjusted R-squared:  0.7293 
##F-statistic: 14.77 on 9 and 37 DF,  p-value: 8.755e-10

#We obtain an Adjusted R-SQuared value = 0.729 using the selected 9PCs using
# Backward StepWise regression and Cross Validation. This is slightly lower
# than using the same method on the original variables


# Now let's see how it cross-validates:

SStot <- sum((data$Crime - mean(data$Crime))^2)
totsse <- 0
for(i in 1:nrow(PCcrime)) {
  mod_lasso_i = lm(Crime ~ PC15+PC6+PC14+PC7+PC4+PC12+PC2+PC1+PC5, data = PCcrime[-i,])
  pred_i <- predict(mod_lasso_i,newdata=PCcrime[i,])
  totsse <- totsse + ((pred_i - PCcrime[i,16])^2)
}
R2_mod <- 1 - totsse/SStot
R2_mod

## 0.631


# Notice that PC15 and PC6 were not significant in the model
# above.  If we take them out, here's what we get:

mod_Step_PC = lm(Crime ~ PC14+PC7+PC4+PC12+PC2+PC1+PC5, data = PCcrime)
summary(mod_Step_PC)

##Coefficients:
##            Estimate Std. Error t value Pr(>|t|)    
##(Intercept)    905.1       30.2   29.96  < 2e-16 ***
##PC14           219.2      126.3    1.74  0.09050 .  
##PC7            117.3       53.8    2.18  0.03548 *  
##PC4             69.4       28.3    2.45  0.01879 *  
##PC12           289.6       85.3    3.40  0.00158 ** 
##PC2            -70.1       18.2   -3.84  0.00044 ***
##PC1             65.2       12.4    5.24  5.9e-06 ***
##PC5           -229.0       31.2   -7.34  7.3e-09 ***
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
##Residual standard error: 207 on 39 degrees of freedom
##Multiple R-squared:  0.757,     Adjusted R-squared:  0.713 
##F-statistic: 17.3 on 7 and 39 DF,  p-value: 3.41e-10

# Now let's see how it cross-validates:

SStot <- sum((data$Crime - mean(data$Crime))^2)
totsse <- 0
for(i in 1:nrow(PCcrime)) {
  mod_lasso_i = lm(Crime ~ PC14+PC7+PC4+PC12+PC2+PC1+PC5, data = PCcrime[-i,])
  pred_i <- predict(mod_lasso_i,newdata=PCcrime[i,])
  totsse <- totsse + ((pred_i - PCcrime[i,16])^2)
}
R2_mod <- 1 - totsse/SStot
R2_mod

## 0.627

# About the same as above, so the simpler model might be better to use.

# ---------------------------- Lasso Regression -------------------------------------

library(glmnet)

#building lasso

XP=data.matrix(PCcrime[,-16])
YP=data.matrix(PCcrime$Crime)
lasso_PC=cv.glmnet(x=as.matrix(PCcrime[,-16]),y=as.matrix(PCcrime$Crime),alpha=1,
                nfolds = 5,type.measure="mse",family="gaussian")

#Output the coefficients of the variables selected by lasso

coef(lasso_PC, s=lasso_PC$lambda.min)

##(Intercept)  905.085106
##PC1           57.080104
##PC2          -58.158625
##PC3           11.097588
##PC4           50.931178
##PC5         -208.653084
##PC6          -33.376936
##PC7           82.070982
##PC8            .       
##PC9            .       
##PC10          11.672337
##PC11           .       
##PC12         233.864614
##PC13           5.987125
##PC14         136.639295
##PC15        -328.368912


#Fitting a new model with these 12 PCs compared to 10 original variables

mod_lasso_PC = lm(Crime ~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC10+PC12+PC13+PC14+PC15, data = PCcrime)
summary(mod_lasso_PC)

##Coefficients:
##Estimate Std. Error t value Pr(>|t|)    
##(Intercept)   905.09      29.48  30.698  < 2e-16 ***
##PC1            65.22      12.15   5.369 5.70e-06 ***
##PC2           -70.08      17.80  -3.936 0.000389 ***
##PC3            25.19      21.05   1.197 0.239582    
##PC4            69.45      27.64   2.512 0.016913 *  
##PC5          -229.04      30.44  -7.523 9.82e-09 ***
##PC6           -60.21      40.07  -1.503 0.142142    
##PC7           117.26      52.53   2.232 0.032313 *  
##PC10           56.32      66.66   0.845 0.404101    
##PC12          289.61      83.24   3.479 0.001398 ** 
##PC13           81.79     113.18   0.723 0.474838    
##PC14          219.19     123.25   1.778 0.084288 .  
##PC15         -622.21     438.74  -1.418 0.165237    
##---
##Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##Residual standard error: 202.1 on 34 degrees of freedom
##Multiple R-squared:  0.7981,	Adjusted R-squared:  0.7269 
##F-statistic:  11.2 on 12 and 34 DF,  p-value: 1.408e-08

#We obtain a similar Adjusted R-SQuared value = 0.7269 using the selected 12 PCs instead
# of the 10 variables using Lasso regression

# Now let's see how it cross-validates:

SStot <- sum((data$Crime - mean(data$Crime))^2)
totsse <- 0
for(i in 1:nrow(PCcrime)) {
  mod_lasso_i = lm(Crime ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC10+PC12+PC13+PC14+PC15, data = PCcrime[-i,])
  pred_i <- predict(mod_lasso_i,newdata=PCcrime[i,])
  totsse <- totsse + ((pred_i - PCcrime[i,16])^2)
}
R2_mod <- 1 - totsse/SStot
R2_mod

## 0.586

# Looks worse.  But notice that PCs 3, 6, 10, 13, and 15 do not
# appear to be significant.  Let's take them out.
# When we do, we get the same model as when we use only significant
# variables from the stepwise PC model.


# ---------------------------- Elastic Net -------------------------------------

#We vary alpha in steps of 0.1 from 0 to 1 and calculate the resultant R-Squared values

R2_PC=c()
for (i in 0:10) {
  model = cv.glmnet(x=as.matrix(PCcrime[,-16]),y=as.matrix(PCcrime$Crime),
                    alpha=i/10,nfolds = 5,type.measure="mse",family="gaussian")
  
  #The deviance(dev.ratio ) shows the percentage of deviance explained, 
  #(equivalent to r squared in case of regression)
  
  R2_PC = cbind(R2_PC,model$glmnet.fit$dev.ratio[which(model$glmnet.fit$lambda == model$lambda.min)])
  
}

R2_PC

##        [,1]      [,2]      [,3]      [,4]     [,5]      [,6]      [,7]      [,8]      [,9]     [,10]     [,11]
##[1,] 0.7695465 0.7517182 0.7787271 0.8014505 0.749221 0.7857614 0.7590517 0.7981891 0.7635869 0.7937638 0.7940698

#Best value of alpha

alpha_best_PC = (which.max(R2_PC)-1)/10
alpha_best_PC

## 0.3

# An interesting observation after we use PCs instead of original variables. We observe that the best
# alpha value=0.3 which is slightly closer to a Lasso model. The R-Squared values are
# slightly higher here. Lets build the model using this alpha value.

Elastic_net_PC=cv.glmnet(x=as.matrix(PCcrime[,-16]),y=as.matrix(PCcrime$Crime),alpha=alpha_best,
                   nfolds = 5,type.measure="mse",family="gaussian")

#Output the coefficients of the variables selected by Elastic Net

coef(Elastic_net_PC, s=Elastic_net_PC$lambda.min)

##(Intercept)  905.085106
##PC1           49.405021
##PC2          -49.276924
##PC3            5.950417
##PC4           40.795690
##PC5         -183.328053
##PC6          -22.440753
##PC7           64.176098
##PC8            .       
##PC9            .       
##PC10           .       
##PC11           .       
##PC12         195.872359
##PC13           .       
##PC14          99.846177
##PC15        -212.066991

# The Elastic Net selects only 10 PCs compared to 12 in Lasso. Next we compare how this new model performs 
# compared to the Lasso model

mod_Elastic_net_PC = lm(Crime ~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC12+PC14+PC15, data = PCcrime)
summary(mod_Elastic_net_PC)

##Coefficients:
##Estimate Std. Error t value Pr(>|t|)    
##(Intercept)   905.09      29.17  31.029  < 2e-16 ***
##PC1            65.22      12.02   5.427 4.06e-06 ***
##PC2           -70.08      17.61  -3.979 0.000321 ***
##PC3            25.19      20.82   1.210 0.234195    
##PC4            69.45      27.35   2.539 0.015574 *  
##PC5          -229.04      30.12  -7.605 5.37e-09 ***
##PC6           -60.21      39.64  -1.519 0.137515    
##PC7           117.26      51.97   2.256 0.030239 *  
##PC12          289.61      82.35   3.517 0.001201 ** 
##PC14          219.19     121.94   1.798 0.080642 .  
##PC15         -622.21     434.06  -1.433 0.160350    
##---
##Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##Residual standard error: 200 on 36 degrees of freedom
##Multiple R-squared:  0.7908,	Adjusted R-squared:  0.7327 
##F-statistic: 13.61 on 10 and 36 DF,  p-value: 1.785e-09

# The R-SQuared value is slightly higher using Elastic Net and only 10 PCS compared to 12 PCs which
# was returned by Lasso. Elastic Net performs relatively better compared to Stepwise and Lasso

# Now let's see how it cross-validates:

SStot <- sum((data$Crime - mean(data$Crime))^2)
totsse <- 0
for(i in 1:nrow(PCcrime)) {
  mod_lasso_i = lm(Crime ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC12+PC14+PC15, data = PCcrime[-i,])
  pred_i <- predict(mod_lasso_i,newdata=PCcrime[i,])
  totsse <- totsse + ((pred_i - PCcrime[i,16])^2)
}
R2_mod <- 1 - totsse/SStot
R2_mod

## 0.627

# If we take out the seemingly-insignificant variables PC3, 
# PC6, and PC15, we're left with the same model we had before
# after taking insignificant variables out of a PC model.

