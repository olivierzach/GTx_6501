# ------------------------ Code for Question 5.1 -------------------------------------

# Clear environment

rm(list = ls())

# Installing and calling packages

install.packages("outliers")
library(outliers)

# Reading the data

data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

# optional check to make sure the data is read correctly

head(data)

## M So   Ed  Po1  Po2    LF   M.F Pop   NW    U1  U2 Wealth Ineq     Prob    Time Crime
## 1 15.1  1  9.1  5.8  5.6 0.510  95.0  33 30.1 0.108 4.1   3940 26.1 0.084602 26.2011   791
## 2 14.3  0 11.3 10.3  9.5 0.583 101.2  13 10.2 0.096 3.6   5570 19.4 0.029599 25.2999  1635
## 3 14.2  1  8.9  4.5  4.4 0.533  96.9  18 21.9 0.094 3.3   3180 25.0 0.083401 24.3006   578
## 4 13.6  0 12.1 14.9 14.1 0.577  99.4 157  8.0 0.102 3.9   6730 16.7 0.015801 29.9012  1969
## 5 14.1  0 12.1 10.9 10.1 0.591  98.5  18  3.0 0.091 2.0   5780 17.4 0.041399 21.2998  1234
## 6 12.1  0 11.0 11.8 11.5 0.547  96.4  25  4.4 0.084 2.9   6890 12.6 0.034201 20.9995   682
# NOTE: ALL ROWS OF THIS FILE STARTING WITH "##" DENOTE R OUTPUT

# Crime is the variable of interest

crime <- data[,"Crime"]

# Run the Shapiro-Wilk test to test the normality of the crime data

shapiro.test(crime)

## 	Shapiro-Wilk normality test
## 
## data:  crime
## W = 0.91273, p-value = 0.001882

# p-value rejects the null hypothesis that the data is normally distributed
# But, normality tests are prone to missing the forest for the trees.
# (I.e., they can give an answer that's probably wrong, because
# they focus too much on a few data points.
# That's especially true if there are outliers in the data,
# which is exactly what we're looking for.

# Look at the Q-Q plot of the crime data as another method to test the normality of the data

qqnorm(crime)
qqnorm(scale(crime))

# Q-Q plot suggests that the "middle" of the data is normally distributed, so we may assume
# that the data is approximately normally distributed and run the Grubbs' test

# Run the Grubbs' test for two outliers on opposite tails

test <- grubbs.test(crime, type = 11)

# Print results of grubbs test

test

## Grubbs test for two opposite outliers
## 
## data:  crime
## G = 4.26880, U = 0.78103, p-value = 1
## alternative hypothesis: 342 and 1993 are outliers

# With p-value = 1, at least one of the extremes (highest or lowest)
# is NOT an outlier.
#
# So, let's check each one individually.

test <- grubbs.test(crime, type = 10)
test

##        Grubbs test for one outlier
##
## data:  crime
## G = 2.8129, U = 0.8243, p-value = 0.07887
## alternative hypothesis: highest value 1993 is an outlier

# Depending on our threshold p-value, we might or might not 
# choose to call than an outlier.  For example, some people use 
# p=0.05 as a threshold, some use p=0.10.
#
# Let's go ahead and declare this an outlier.
#
# Now, even though you didn't need to for the homework, 
# let's check the second-highest point to see if it's an outlier too.

# Create a new data set without the largest value

crime2 <- crime[-which.max(crime)]

# Now test it

test <- grubbs.test(crime2, type = 10)
test

##         Grubbs test for one outlier
##
## data:  crime2
## G = 3.0634, U = 0.7868, p-value = 0.02848
## alternative hypothesis: highest value 1969 is an outlier

# That's a low p-value, suggesting that the second-highest-crime
# city in the original data set is also an outlier.
#
# So, let's remove it, and test the next one.

crime3 <- crime2[-which.max(crime2)]
test <- grubbs.test(crime3, type = 10)
test

##         Grubbs test for one outlier
##
## data:  crime3
## G = 2.5646, U = 0.8471, p-value = 0.1781
## alternative hypothesis: highest value 1674 is an outlier

# That's a high-enough p-value that it's not clear the point is an outlier.
# So let's stop here, having removed the two highest points as outliers.
#
# But let's also check the lowest point.
# grubbs.test picks the most-outlying point, and it always picked 
# the high ones.  To get the low one, we'll use the opposite=TRUE parameter.

test <- grubbs.test(crime3,type=10,opposite=TRUE)
test

##         Grubbs test for one outlier
##
## data:  crime3
## G = 1.6180, U = 0.9392, p-value = 1
## alternative hypothesis: lowest value 342 is an outlier

# The p-value rounds to 1, so the lowest-crime city does not 
# seem to be an outlier.
# That's why our first test that checked both extremes returned
# a p-value of 1.
#
# Note that the result would've been the same even if we hadn't
# removed the two outliers yet.

#
# Finally, let's do one more thing that's not necessary:
# Let's see what it looks like visually.
#
# Let's make a box-and-whisker plot

############# Outlier Visualization (Not Necessary) #############

# Installing and calling packages

# Here's the same plotting package as in question 2.

install.packages("ggplot2")
library(ggplot2)

# Define a function that finds 5%, 25%, 50%, 75%, and 95% quantiles of the data

quant <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# Create data frame with just the crime data

df <- data.frame(x = rep(1, nrow(data)), y = crime)

# Define a function that finds points below and aboce the 5% and 95% quantiles of the data
    
outliers <- function(x) {
  subset(x, x < quantile(x, 0.05) | quantile(x, 0.95) < x)
}

# Create the box-and-whisker plot

ggplot(df, aes(x, y)) + 
  stat_summary(fun.data = quant, geom="boxplot") + 
  stat_summary(fun.y = outliers, geom="point")

# This visualization shows us that the two cities with the
# highest amount of crime seem to be outliers.
# The two cities with the lowest amount of crime are close enough 
# to not necessarily be considered outliers.

