# -------------------- Code for Question 2.2 part 3 -----------------------------
# Clear environment

rm(list = ls())

#First, load the kknn library (which contains the kknn function) and read in the data
#

library(kknn)

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

# Create a function to calculate the accuracy of the model with k=X
#

check_accuracy = function(X){
  predicted <- rep(0,(nrow(data))) # predictions: start with a vector of all zeros

  # for each row, estimate its response based on the other rows

  for (i in 1:nrow(data)){

    # data[-i] means we remove row i of the data when finding nearest neighbors...
    #...otherwise, it'll be its own nearest neighbor!

    model=kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,data[-i,],data[i,],k=X, scale = TRUE) # use scaled data

    # record whether the prediction is at least 0.5 (round to one) or less than 0.5 (round to zero)

    predicted[i] <- as.integer(fitted(model)+0.5) # round off to 0 or 1
  }

  # calculate fraction of correct predictions

  accuracy = sum(predicted == data[,11]) / nrow(data)
  return(accuracy)
}

#
# Now call the function for values of k from 1 to 20 (you could try higher values of k too)
#

acc <- rep(0,20) # set up a vector of 20 zeros to start
for (X in 1:20){
  acc[X] = check_accuracy(X) # test knn with X neighbors
}

#
# report accuracies
#

acc

##          [,1]      [,2]      [,3]      [,4]     [,5]      [,6]      [,7]      [,8]
##[1,] 0.8149847 0.8149847 0.8149847 0.8149847 0.851682 0.8455657 0.8470948 0.8486239
##          [,9]     [,10]    [,11]    [,12]    [,13]    [,14]    [,15]    [,16]    [,17]
##[1,] 0.8470948 0.8501529 0.851682 0.853211 0.851682 0.851682 0.853211 0.851682 0.851682
##        [,18]     [,19]     [,20]    
##[1,] 0.851682 0.8501529 0.8501529
