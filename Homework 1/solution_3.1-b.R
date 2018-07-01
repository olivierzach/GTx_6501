# ------------------------ Code for Question 3.1-B -------------------------------------

# Clear environment

rm(list = ls())

# Load the kernlab library (which contains the ksvm function) and read in the data
#

library(kernlab)

# Installing and calling kknn packages

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

# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)

# --------- Split data into training, validation, and test sets ---------

# Creating a mask using the sample function for the split
# The "mask" is the set of row indices -- for example,
# if rows 1, 4, 5, and 8 are chosen, then mask will be
# (1,4,5,8).

# 60% for training -- "sample" selects a sample of data points

mask_train = sample(nrow(data), size = floor(nrow(data) * 0.6))
cred_train = data[mask_train,] # training data set

# Using the remaining data for test and validation split

remaining = data[-mask_train, ]  # all rows except training

# Half of what's left for validation, half for test

mask_val = sample(nrow(remaining), size = floor(nrow(remaining)/2))

cred_val = remaining[mask_val,]  # validation data set
cred_test = remaining[-mask_val, ] # test data set

#
# We'll pick the best of 9 SVM models and 20 KNN models

acc <- rep(0,29)  # 1-9 are SVM, 10-29 are KNN

#
# --------------- Train SVM models -------------------
#

# values of C to test

amounts <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000) 

for (i in 1:9) {

	# fit model using training set

     model_scaled <- ksvm(as.matrix(cred_train[,1:10]),
              as.factor(cred_train[,11]),
		   type = "C-svc", # Use C-classification method
              kernel = "vanilladot", # Use simple linear kernel
              C = amounts[i],
		   scaled=TRUE) # have ksvm scale the data for you

	#  compare models using validation set

               pred <- predict(model_scaled,cred_val[,1:10])
               acc[i] = sum(pred == cred_val$V11) / nrow(cred_val)
}

acc[1:9]

## [1] 0.5114504 0.5114504 0.7328244 0.8320611 0.8320611 0.8320611 0.8320611 0.8320611 0.8320611

# find best-performing SVM model on validation data

# Note: "\n" is a newline character

cat("Best SVM model is number ",which.max(acc[1:9]),"\n")
cat("Best C value is ",amounts[which.max(acc[1:9])],"\n")
cat("Best validation set correctness is ",max(acc[1:9]),"\n")

## Best SVM model is number  4
## Best C value is  0.01
## Best validation set correctness is  0.8320611
#
# Note that as you can see above when we printed acc[1:9],
# all C values we tested from 0.01 to 1000 look the same
#

# retrain the best model (since I've overwritten it above)

     model_scaled <- ksvm(as.matrix(cred_train[,1:10]),
              as.factor(cred_train[,11]),
		   type = "C-svc", # Use C-classification method
              kernel = "vanilladot", # Use simple linear kernel
              C = amounts[which.max(acc[1:9])],
		   scaled=TRUE) # have ksvm scale the data for you


cat("Performance on test data = ",sum(predict(model_scaled,cred_test[,1:10]) == cred_test$V11) / nrow(cred_test),"\n")

## Performance on test data =  0.8549618

#
# --------------- Train KNN models -------------------
#

for (k in 1:20) {

               # fit k-nearest-neighbor model using training set, validate on test set

     knn_model <- kknn(V11~.,cred_train,cred_val,k=k,scale=TRUE)

	#  compare models using validation set

     pred <- as.integer(fitted(knn_model)+0.5) # round off to 0 or 1

     acc[k+9] = sum(pred == cred_val$V11) / nrow(cred_val)
}

acc[10:29]

## [1] 0.7862595 0.7862595 0.7862595 0.7862595 0.7938931 0.7862595 0.7786260 0.7862595 0.7786260
##[10] 0.7938931 0.7938931 0.7938931 0.7862595 0.7938931 0.7938931 0.8091603 0.8091603 0.8091603
##[19] 0.8091603 0.8091603

# find best-performing KNN model on validation data

cat("Best KNN model is k=",which.max(acc[10:29]),"\n")
cat("Best validation set correctness is ",max(acc[10:29]),"\n")

## Best KNN model is k= 16 
## Best validation set correctness is  0.8091603

# run best model on test data

     knn_model <- kknn(V11~.,cred_train,cred_test,
               k=which.max(acc[10:29]),
               scale=TRUE)

     pred <- as.integer(fitted(knn_model)+0.5) # round off to 0 or 1

cat("Performance on test data = ",sum(pred == cred_test$V11) / nrow(cred_test),"\n")

## Performance on test data =  0.8778626 

#
# --------------- Evaluate overall best model on test data -------------------
#

if (which.max(acc) <= 9)  {        # if a ksvm method is best

         # evaluate the ksvm method on the test set to find estimated quality

         cat("Use ksvm with C = ",amounts[which.max(acc[1:9])],"\n")
         cat("Test performace = ",sum(predict(model_scaled,cred_test[,1:10]) == cred_test$V11) / nrow(cred_test),"\n")

} else {    # the best is a knn method

        # evaluate the knn method on the test set to find estimated quality

        cat("Use knn with k = ",which.max(acc[10:29]),"\n")
        cat("Test performance = ",sum(pred == cred_val$V11) / nrow(cred_val),"\n")

}

## Use ksvm with C =  0.01 
## Test performace =  0.8549618
