# -------------------- Code for Question 7.2 -----------------------------
# Clear environment

rm(list = ls())


# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)


# ---------------------------- Data manipulation -------------------------------------

data <- read.table("temps.txt", header = TRUE)

#
# optional check to make sure the data is read correctly
#

head(data)

# Console output for head(data)
##    DAY X1996 X1997 X1998 X1999 X2000 X2001 X2002 X2003 X2004 X2005
##1 1-Jul    98    86    91    84    89    84    90    73    82    91
##2 2-Jul    97    90    88    82    91    87    90    81    81    89
##3 3-Jul    97    93    91    87    93    87    87    87    86    86
##4 4-Jul    90    91    91    88    95    84    89    86    88    86
##5 5-Jul    89    84    91    90    96    86    93    80    90    89
##6 6-Jul    93    84    89    91    96    87    93    84    90    82
##        X2006 X2007 X2008 X2009 X2010 X2011 X2012 X2013 X2014 X2015
##1          93    95    85    95    87    92   105    82    90    85
##2          93    85    87    90    84    94    93    85    93    87
##3          93    82    91    89    83    95    99    76    87    79
##4          91    86    90    91    85    92    98    77    84    85
##5          90    88    88    80    88    90   100    83    86    84
##6          81    87    82    87    89    90    98    83    87    84
# NOTE: ALL ROWS OF THIS FILE STARTING WITH "##" DENOTE R OUTPUT
#

# create a vector of this data

data <- as.vector(unlist(data[,2:21]))

# turn the vector into a time series object

myts <- ts(data,start=1996,frequency=123)

# Single exponential smoothing

m1 <- HoltWinters(myts,beta=FALSE,gamma=FALSE)
m1

## Holt-Winters exponential smoothing without trend and without seasonal component.
## 
## Call:
## HoltWinters(x = myts, beta = FALSE, gamma = FALSE)
## 
## Smoothing parameters:
##  alpha: 0.8396301
##  beta : FALSE
##  gamma: FALSE
## 
## Coefficients:
##       [,1]
## a 63.30952
#
# So, the baseline estimate at the end is 63.30952, and the 
# best value of alpha found is 0.8396301.
# [Of course, both of those have more significant digits reported
# than are reasonable.]


# Double exponential smoothing

m2 <- HoltWinters(myts,gamma=FALSE)
m2

## Holt-Winters exponential smoothing with trend and without seasonal component.
## 
## Call:
## HoltWinters(x = myts, gamma = FALSE)
## 
## Smoothing parameters:
##  alpha: 0.8455303
##  beta : 0.003777803
##  gamma: FALSE
## 
## Coefficients:
##           [,1]
## a 81.729657393
## b -0.004838906
#
# Notice that the final trend estimate (b) is very close to zero
# (-0.004838906) and the value of beta is also very close to zero.
# This suggests that there isn't really a significant trend.


# Triple exponential smoothing (additive seasonality)

m3a <- HoltWinters(myts)
m3a

# Lots of output (123 seasonal factors) but the key is that
# b and beta are again both zero or very close to it.


# Triple exponential smoothing (multiplicative seasonality)

m3m <- HoltWinters(myts,seasonal="multiplicative")
m3m

# Lots of output (123 seasonal factors) but the key is that
# b and beta are again both zero or very close to it.

# m3m$fitted[4] shows the seasonal factors for each data point.
#
# Put the factors into a matrix

m <- matrix(m3m$fitted[,4],ncol=123)

From here, we can run the same CUSUM analysis as in the previous homework, but on the seasonal factors.
