# -------------------- Code for Question 10.1-a -----------------------------
# Clear environment

rm(list = ls())

# Install the DAAG package, which has cross-validation functions

install.packages("DAAG")
library(DAAG)


# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)


# ---------------------------- Data manipulation -------------------------------------

# First, Read in the data
#

data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#
# optional check to make sure the data is read correctly
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





################### (a) Regression Tree ###################



########## Without splitting data into training and testing sets ##########

install.packages("tree")
library(tree)

set.seed(1)



# Fit a regression tree function to the crime data

tree.data <- tree(Crime~., data = data)
summary(tree.data)

## Regression tree:
##   tree(formula = Crime ~ ., data = data)
## Variables actually used in tree construction:
##   [1] "Po1" "Pop" "LF"  "NW" 
## Number of terminal nodes:  7 
## Residual mean deviance:  47400 = 1900000 / 40 
## Distribution of residuals:
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   -574     -98      -2       0     111     490 

# Notice that only 4 predictors were used in the construction of this tree



# More information about the way the tree was split

tree.data$frame

##       var  n     dev yval splits.cutleft splits.cutright
## 1     Po1 47 6880928  905          <7.65           >7.65
## 2     Pop 23  779243  670          <22.5           >22.5
## 4      LF 12  243811  550        <0.5675         >0.5675
## 8  <leaf>  7   48519  467                               
## 9  <leaf>  5   77757  668                               
## 5  <leaf> 11  179471  800                               
## 3      NW 24 3604163 1131          <7.65           >7.65
## 6     Pop 10  557575  887          <21.5           >21.5
## 12 <leaf>  5  146391 1049                               
## 13 <leaf>  5  147771  725                               
## 7     Po1 14 2027225 1305          <9.65           >9.65
## 14 <leaf>  6  170828 1041                               
## 15 <leaf>  8 1124985 1503                               



# Plot the regression tree

plot(tree.data)
text(tree.data)



# Determine if pruning the tree will improve performance through cross-validation
# by looking at the deviance of trees with different number of terminal nodes.
# Deviance is a quality-of-fit statistic.
# The x-axis represents the number of terminal nodes.

cv.data <- cv.tree(tree.data)
plot(cv.data$size, cv.data$dev, type = "b")

# This plot suggests that we get the best fit using all of the terminal nodes in the
# tree that we already plotted.



# We can prune the tree by limiting the number of terminal nodes of the tree if this
# is desired. This is just one example of how you can prune a regression tree.

termnodes <- 5
prune.data <- prune.tree(tree.data, best = termnodes)



# Plot the pruned tree

plot(prune.data)
text(prune.data)


# We decide to procede using the unpruned regression model.
# Calculate SSres of the unpruned regression model.

yhat <- predict(tree.data)
SSres <- sum((yhat-data$Crime)^2)



# Plot of actual vs. predicted crime values

plot(data$Crime, yhat)
abline(0,1)



# Plot the residuals

plot(data$Crime, scale(yhat - data$Crime))
abline(0,0)



# Calculate SStot and R-squared of this model on the training data

SStot <- sum((data$Crime - mean(data$Crime))^2)
R2 <- 1 - SSres/SStot
R2

## 0.724

# But that's just on training data.  What if we estimate the
# model quality using cross-validation?
# It turns out to not be good.
#
# Here's the sum of squared errors for trees of each size,
# from 7 nodes down to 1 node (1 leaf)

prune.tree(tree.data)$size
prune.tree(tree.data)$dev

## [1] 7 6 5 4 3 2 1
## [1] 1895722 2013257 2276670 2632631 3364043 4383406 6880928

# Now, compare with the sum of squared errors in cross-validation:

cv.data <- cv.tree(tree.data) # cross-validation
cv.data$size
cv.data$dev

## [1] 7 6 5 4 3 2 1
## [1] 7359435 7369732 7271228 7492308 8612002 7413281 8088892

# These errors are larger -- larger even than SStot --
# indicating that the model does not do well; it's just way overfit.


# Notice that for each leaf, the estimate is the average Crime 
# among the data points in the leaf.  
# Because the leaves have so few data points (5-11 in this case),
# we can't do much better than that; there won't be enough 
# data points to build a regression model.
#
# But, suppose we just have a 1-branch tree with 2 leaves:

prune.data <- prune.tree(tree.data,best=2)

# Now each data point is in one of two leaves:

prune.data$where

##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 
##  2  3  2  3  3  3  3  3  2  2  3  2  2  2  2  3  2  3  3  3  2  2  3  3  2  3  2  3  3  2 
## 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 
##  2  3  2  3  3  3  2  2  2  3  2  2  2  3  2  3  3 

# It branches on Po1 at value 7.65. The estimates are still the average in the branch.
# It's similar to having a regression model with no factors (just the constant)

p <- predict(prune.data)
1 - sum((p - data$Crime)^2)/SStot

## [1] 0.363
#
# This is the observed R-squared on the training data.
# Now let's try cross-validation

cv.data <- cv.tree(prune.data)
cv.data$size
cv.data$dev

# Like before (as we'd expect, since it's a subset of what we did before)
# the cross-validation shows the model doesn't do well.


# But suppose we actually build a regression model for each leaf,
# rather than just using the average of each leaf's points.
#
# With a small number of data points, that's hard.  So, let's
# just use the two-leaf tree with one branch.

# Separate rows of data in each leaf
d1 <- data[which(prune.data$where == 2),]
d2 <- data[which(prune.data$where == 3),]

# First leaf:

m1 <- lm(Crime~.,data=d1)
summary(m1)

# But only four factors are even marginally significant, so refine:

m1b <- lm(Crime~Ed+Pop+Prob+Time,data=d1)
summary(m1b)

# But now only two factors are even marginally significant, so refine:

m1c <- lm(Crime~Pop+Time,data=d1)
summary(m1c)

# And now just one factor is significant:

m1d <- lm(Crime~Pop,data=d1)
summary(m1d)

# R-squared on training data is 0.296, now estimate with leave-one-out cross-validation (since we have few data points):

c1d <- cv.lm(d1,m1d,m=nrow(d1))
1 - attr(c1d,"ms")*nrow(d1)/sum((d1$Crime - mean(d1$Crime))^2)

## [1] 0.181

# But what if we used PCA for this instead, especially because we have so few data points?
#

p1 <- prcomp(~.,data=d1[,1:15],scale.=TRUE)

# let's use the first two components

mp1 <- lm(V1~.,data=as.data.frame(cbind(d1[,16],p1$x[,1:2])))
summary(mp1)

# Only one of these two components is significant (the second), so let's just use it:

mp2 <- lm(V1~.,data=as.data.frame(cbind(d1[,16],p1$x[,2])))
summary(mp2)

## Multiple R-squared:  0.383,     Adjusted R-squared:  0.354 

# Now, cross-validation:

cm2 <- cv.lm(as.data.frame(cbind(d1[,16],p1$x[,2])),mp2,m=nrow(d1))
1 - attr(cm2,"ms")*nrow(d1)/sum((d1$Crime - mean(d1$Crime))^2)

## [1] 0.304

# So, this is a real improvement.
# * Using just the average of data points on this branch turned
# out to give no value, when tested with cross-validation.
# * Using a regression model for this branch explains about 18% 
# of the variation.
# * And using a regression model with PCA gets to about 30%. 

# Now, let's try the second leaf.

m2 <- lm(Crime~.,data=d2)
summary(m2)

# Only one factor is significant:

m2b <- lm(Crime~Ineq,data=d2)
summary(m2b)

# Uh oh... it looks like none of the factors are significant!
#
# Let's try PCA. 

p2 <- prcomp(~.,data=d2[,1:15],scale.=TRUE)

# let's use the first two components

mp2 <- lm(V1~.,data=as.data.frame(cbind(d2[,16],p2$x[,1:2])))
summary(mp2)

# None of these factors are significant either!
#
# So, what it means is that we can get a reasonable model 
# for the first leaf (Po1 < 7.65) using PCA + regression,
# but for the second leaf (Po1 > 7.65) we don't have a good
# model.
#
# So, it shows that for cities with Po1 > 7.65, we need to 
# work more on finding good predictive factors, and/or 
# finding a good model.



################### (a) Regression Tree - Alternate Method ###################



install.packages("rpart")
library(rpart)

set.seed(1)



# Fit a regression tree function to the training data.

tree.data.2 <- rpart(Crime~., data = data)
summary(tree.data.2)

# Notice that only 3 predictors were used in the construction of this tree



# More information about the way the tree was split

tree.data.2$frame



# Plot the regression tree

plot(tree.data.2)
text(tree.data.2)



# Calculate SSres of the unpruned regression model.

yhat.2 <- predict(tree.data.2)
SSres.2 <- sum((yhat.2-data$Crime)^2)



# Plot of actual vs. predicted crime values

plot(data$Crime, yhat.2)
abline(0,1)



# Plot the residuals

plot(data$Crime, scale(yhat.2 - data$Crime))
abline(0,0)



# Calculate SStot and R-squared of this model

SStot.2 <- sum((data$Crime - mean(data$Crime))^2)
R2.2 <- 1 - SSres.2/SStot.2
R2.2

## 0.563

# I'm not going to duplicate the same analysis as above;
# this is just here to show you how to use rpart()
