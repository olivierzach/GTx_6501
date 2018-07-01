# ------------------------ Code for Question 3.1-A -------------------------------------

# Clear environment

rm(list = ls())

# Installing and calling packages

install.packages("kknn")
library(kknn)

# Reading the data

data <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

#
# optional check to make sure the data is read correctly
#

head(data)

##   V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11
## 1 1 30.83 0.000 1.25 1 0 1 1 202 0 1
## 2 0 58.67 4.460 3.04 1 0 6 1 43 560 1
## 3 0 24.50 0.500 1.50 1 1 0 1 280 824 1
## 4 1 27.83 1.540 3.75 1 0 5 0 100 3 1
## 5 1 20.17 5.625 1.71 1 1 0 1 120 0 1
## 6 1 32.08 4.000 2.50 1 1 0 0 360 0 1
# NOTE: ALL ROWS OF THIS FILE STARTING WITH "##" DENOTE R OUTPUT
#
# Fit the model.
# V11 is response, other variables are predictors
#

############# METHOD 1: Using train.kknn #############
#
# This method uses n-fold cross-validation, where n is the number
# of data points, because that's how train.kknn does
# cross validation.  It's also called "leave-one-out" cross
# validation.

# Setting the random number generator seed so that our results are reproducible

set.seed(1)

# set maximum value of k (number of neighbors) to test

kmax <- 30

# use train.kknn for leave-one-out cross-validation up to k=kmax

model <- train.kknn(V11~.,data,kmax=kmax,scale=TRUE)

# create array of prediction qualities

accuracy <- rep(0,kmax)

# calculate prediction qualities

for (k in 1:kmax) {
    predicted <- as.integer(fitted(model)[[k]][1:nrow(data)] + 0.5) # round off to 0 or 1
    accuracy[k] <- sum(predicted == data$V11)
}

# show accuracies

accuracy

##   [1] 533 533 533 533 557 553 554 555 554 557 557 558 557 557 558 558 558 557 556 556 555 554 552 553 553 552 550 548 549 550

############# METHOD 2: cv.kknn from kknn package #############

# Setting the random number generator seed so that our results are reproducible

set.seed(1)

# set maximum value of k (number of neighbors) to test

kmax <- 30

# create array of prediction qualities

accuracy_cv <- rep(0,kmax)

# calculate prediction qualities

for (k in 1:kmax) {

    # run cross-validation for each value of k (number of neighbors)
    model <- cv.kknn(V11~.,data,
                                        kcv=10, # 10-fold cross-validation
                                        k=k, # number of neighbors
                                        scale=TRUE) # scale data

    predicted <- as.integer(model[[1]][,2] + 0.5) # round off to 0 or 1
    accuracy_cv[k] <- sum(predicted == data$V11)
}

# show accuracies

accuracy_cv

## [1] 524 533 534 526 549 560 552 552 557 557 554 556 556 558 544 552 564 551 557 558 551 558 555
##[24] 550 553 545 549 547 554 553

############# METHOD 3: Using caret package #############

# Caret is a powerful package that uses a lot of other packages to give a comprehensive
# toolkit for model building and validation.

# Load the caret library to perform k-fold cross validation
# There could be issues installing this package and dependencies as our TAs faced

install.packages("caret",dependencies = TRUE)
install.packages("quantreg")
library(caret)

# Setting the random number generator seed so that our results are reproducible

set.seed(1)

# set number of values of k (number of neighbors) to test
# the default here is to try odd numbers, to avoid ties

kmax <- 15

# note that the double use of "k" (k-nearest neighbors and k-fold cross validation) can be confusing

knn_fit <- train(as.factor(V11)~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,
    data, 
    method = "knn", # choose knn model
    trControl=trainControl(
               method="repeatedcv", # k-fold cross validation
               number=10, # number of folds (k in cross validation)
               repeats=5), # number of times to repeat k-fold cross validation 
    preProcess = c("center", "scale"), # standardize the data
    tuneLength = kmax) # max number of neighbors (k in nearest neighbor)

# We now check the result to identify the best value of k and the associated accuracy

knn_fit

# The result from the model is summarized below

##k-Nearest Neighbors 
##
##654 samples
## 10 predictor
##  2 classes: '0', '1' 
##
##Pre-processing: centered, scaled 
##Resampling: Cross-Validated (10 fold, repeated 5 times) 
##
##Summary of sample sizes: 589, 589, 588, 588, 589, 588, ... 
##
##Resampling results across tuning parameters:
##
##  k   Accuracy   Kappa      Accuracy SD  Kappa SD  
##   5  0.8458633  0.6898403  0.04193016   0.08381092
##   7  0.8454707  0.6897756  0.04212679   0.08410594
##   9  0.8375406  0.6738288  0.03968248   0.07920391
##  11  0.8335688  0.6657697  0.04631226   0.09307300
##  13  0.8335166  0.6653878  0.04783943   0.09597735
##  15  0.8298571  0.6576198  0.05000524   0.10027540
##  17  0.8335734  0.6653173  0.04494915   0.09025457
##  19  0.8384454  0.6745661  0.04424897   0.08887068
##  21  0.8409496  0.6794267  0.04723457   0.09518618
##  23  0.8415554  0.6801108  0.04560664   0.09256435
##  25  0.8366410  0.6694879  0.04341832   0.08854984
##  27  0.8390746  0.6739308  0.04163978   0.08506440
##  29  0.8433781  0.6820578  0.04426343   0.09061453
##  31  0.8424452  0.6798053  0.04738523   0.09727813
##  33  0.8427531  0.6802807  0.04846920   0.09929343
##
##Accuracy was used to select the optimal model using  the largest value.
##The final value used for the model was k = 5. 

