# -------------------- Code for HW 10 Question 1 -----------------------------
# Clear environment

rm(list = ls())

# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)

# ---------------------------- Data manipulation -------------------------------------

# First, read in the data
#

data <- read.table("breast-cancer-wisconsin.data.txt", stringsAsFactors = FALSE, header = FALSE, sep = ",")

#
# Optional check to make sure the data is read correctly
#

head(data)

## V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11
## 1 1000025  5  1  1  1  2  1  3  1   1   2
## 2 1002945  5  4  4  5  7 10  3  2   1   2
## 3 1015425  3  1  1  1  2  2  3  1   1   2
## 4 1016277  6  8  8  1  3  4  3  7   1   2
## 5 1017023  4  1  1  3  2  1  3  1   1   2
## 6 1017122  8 10 10  8  7 10  9  7   1   4

# Find where the missing data is.

for (i in 2:11) {
  print(paste0("V",i))
  print(table(data[,i]))
}

# V7 is the only factor with missing data, where the missing data
# is represented with question marks.

# Show the observations with missing data.

data[which(data$V7 == "?"),]

# After examing the printed tables for each factor and the observations
# with missing data, there does not appear to be any extreme bias in the
# which data is missing. If there is any bias, it may be that the
# observations with missing V7 values tend to have V11 of 4 less than
# the overall data set. Only 12.5% of missing data has V11 values of 4,
# while 36% of the overal data set has V11 values of 4. Because this difference
# does not seem "too bad," we can continue with our analysis.

# Find the percentage of observations with missing data.

nrow(data[which(data$V7 == "?"),])/nrow(data)

## 0.0229

# Since this is less than 5%, it is okay to use data imputation.

# Find and store the indices of the missing V7 data.

missing <- which(data$V7 == "?", arr.ind = TRUE)
missing

## 24  41 140 146 159 165 236 250 276 293 295 298 316 322 412 618


#################### Mean/Mode Imputation ####################

# Use mode imputation since V7 is a categorical variable.

# R does not have a built in function to find the mode of a vector,
# so we create one ourselves.

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Find the mode of V7.

mode_V7 <- as.numeric(getmode(data[-missing,"V7"]))
mode_V7

## 1

# Impute V7 for observations with missing data for V7 to mode_V7.

data_mode_imp <- data
data_mode_imp[missing,]$V7 <- mode_V7
data_mode_imp$V7 <- as.integer(data_mode_imp$V7)



# The following code does mean imputation if it were desired. 

# Find the mean of V7.

#mean_V7 <- mean(as.numeric(data[-missing,"V7"]))
#mean_V7

## 3.54

# Impute V7 for observations with missing data for V7 to mode_V7.

#data_mean_imp <- data
#data_mean_imp[missing,]$V7 <- mean_V7
#data_mean_imp$V7 <- as.integer(data_mean_imp$V7)



#################### Regression Imputation ####################

# Do not include the response variable in regression imputation

data_modified <- data[-missing,2:10]
data_modified$V7 <- as.integer(data_modified$V7)

# Generate linear model using all other factors as predictors

model <- lm(V7~V2+V3+V4+V5+V6+V8+V9+V10, data = data_modified)
summary(model)

# Not all predictors are significant, so use backwards stepwise
# regression for variable selection.

step(model)

# Generate the linear model that stepwise regression recommends.

model2 <- lm(V7~V2+V4+V5+V8, data = data_modified)
summary(model2)

# Now all predictors are signifcant.

# Use cross-validation to test how good this model really is.

library(DAAG)
model_cv <- cv.lm(data_modified, model2, m=5)
SST <- sum((as.numeric(data[-missing,]$V7) - mean(as.numeric(data[-missing,]$V7)))^2)
R2_cv <- 1 - attr(model_cv,"ms")*nrow(data[-missing,])/SST
R2_cv

## 0.603

# Get predictions for missing V7 values.

V7_hat <- predict(model2, newdata = data[missing,])

# Impute V7 for observations with missing data for V7 to predicted
# values with this linear model.

data_reg_imp <- data
data_reg_imp[missing,]$V7 <- V7_hat
data_reg_imp$V7 <- as.numeric(data_reg_imp$V7)

# Round the V7_hat values since the originals are all integer

data_reg_imp[missing,]$V7 <- round(V7_hat)
data_reg_imp$V7 <- as.integer(data_reg_imp$V7)

# Make sure no V7 values are outside of the orignal range.

data_reg_imp$V7[data_reg_imp$V7 > 10] <- 10
data_reg_imp$V7[data_reg_imp$V7 < 1] <- 1

# NOTE: Using multinomial regression might be more proper here
# since V7 is categorical and not continuous, but we haven't
# covered that in this course.



#################### Regression with Perturbation Imputation ####################

set.seed(1)

# Perturb the predictions for missing V7 values with a random normal distriubtion
# in which the predicted values are the means and the standard deviation is the 
# standard deviation of the predicted values.

V7_hat_pert <- rnorm(nrow(data[missing,]), V7_hat, sd(V7_hat))
V7_hat_pert

## 4.078  8.386 -0.855  5.138  1.707  0.407  3.790  3.391  3.343  5.413  4.320  3.386  3.875 -3.118  3.467  0.564

# Notice that we get some negative values when we perturb the predicted values.

data_reg_pert_imp <- data
data_reg_pert_imp[missing,]$V7 <- V7_hat_pert
data_reg_pert_imp$V7 <- as.numeric(data_reg_pert_imp$V7)

# Round the V7_hat_pert values to integers.

data_reg_pert_imp[missing,]$V7 <- round(V7_hat_pert)
data_reg_pert_imp$V7 <- as.integer(data_reg_pert_imp$V7)

# Make sure no V7 values are outside of the orignal range.

data_reg_pert_imp$V7[data_reg_pert_imp$V7 > 10] <- 10
data_reg_pert_imp$V7[data_reg_pert_imp$V7 < 1] <- 1



#################### Comparing Results of Classifcation Models ####################

# --------- Split data into training and validation sets ---------

set.seed(1)

# 70% for training and 30% for validation.

training <- sample(nrow(data), size = floor(nrow(data) * 0.7))
validation <- setdiff(1:nrow(data), training)



# --------- KNN Models ---------

library(kknn)

# We will look at the accuracies for the knn model for 5 different k's
# on the 3 data sets we formed after data imputation, the data excluding
# missing values, and the data with a binary variable to indicate if an
# observation has a missing value.

acc_knn <- rep(0,25)

# Data with mode imputation

for (k in 1:5) {
  
  # Fit k-nearest-neighbor model using training set, validate on test set.
  
  knn_model <- kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, data_mode_imp[training,], data_mode_imp[validation,], k=k)
  
  # Compare models using validation set.
  
  pred <- as.integer(fitted(knn_model)+0.5) # round off to 2 or 4
  
  acc_knn[k] = sum(pred == data_mode_imp[validation,]$V11) / nrow(data_mode_imp[validation,])
}

# Data with regression imputation

for (k in 1:5) {
  
  # Fit k-nearest-neighbor model using training set, validate on test set.
  
  knn_model <- kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, data_reg_imp[training,], data_reg_imp[validation,], k=k)
  
  # Compare models using validation set.
  
  pred <- as.integer(fitted(knn_model)+0.5) # round off to 2 or 4
  
  acc_knn[k+5] = sum(pred == data_reg_imp[validation,]$V11) / nrow(data_reg_imp[validation,])
}

# Data with regression with perturbation imputation

for (k in 1:5) {
  
  # Fit k-nearest-neighbor model using training set, validate on test set.
  
  knn_model <- kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, data_reg_pert_imp[training,], data_reg_pert_imp[validation,], k=k)
  
  # Compare models using validation set.
  
  pred <- as.integer(fitted(knn_model)+0.5) # round off to 2 or 4
  
  acc_knn[k+10] = sum(pred == data_reg_pert_imp[validation,]$V11) / nrow(data_reg_pert_imp[validation,])
}

# Data without missing variables

# Check how many of the missing indices fall into the training set

length(intersect(missing, training))/length(missing)

## 0.875

# Since this is relatively close to 70% and since the number of observations
# with missing values is so small, we can simply take out the missing indices
# from the training and validation sets.

training_no_missing <- setdiff(training, intersect(missing, training))
validation_no_missing <- setdiff(validation, intersect(missing, validation))

# Replace missing data with 0's so that V7 can be read as type integer and
# skip over missing values in modeling.

data_no_missing <- data
data_no_missing$V7[data$V7 == "?"] <- 0
data_no_missing$V7 <- as.integer(data_no_missing$V7)

for (k in 1:5) {
  
  # Fit k-nearest-neighbor model using training set, validate on test set.
  
  knn_model <- kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, data_no_missing[training_no_missing,],
                    data_no_missing[validation_no_missing,], k=k)
  
  # Compare models using validation set.
  
  pred <- as.integer(fitted(knn_model)+0.5) # round off to 2 or 4
  
  acc_knn[k+15] = sum(pred == data_no_missing[validation_no_missing,]$V11) / 
    nrow(data_no_missing[validation_no_missing,])
}

# Add a binary variable to the original data to indicate if an
# observation has a missing V7 value.

data_binary <- data
data_binary$V12[data$V7 == "?"] <- 0
data_binary$V12[data$V7 != "?"] <- 1

# Create interaction factor for V7 and V12.

data_binary$V13[data$V7 == "?"] <- 0
data_binary$V13[data$V7 != "?"] <- as.integer(data[-missing,]$V7)

# Use the interaction factor in the modeling.

for (k in 1:5) {
  
  # Fit k-nearest-neighbor model using training set, validate on test set.
  
  knn_model <- kknn(V11~V2+V3+V4+V5+V6+V8+V9+V10+V13, data_binary[training,], data_binary[validation,], k=k)
  
  # Compare models using validation set.
  
  pred <- as.integer(fitted(knn_model)+0.5) # round off to 2 or 4
  
  acc_knn[k+20] = sum(pred == data_binary[validation,]$V11) / nrow(data_binary[validation,])
}

acc_knn

## [1] 0.952 0.952 0.924 0.924 0.919 0.948 0.948 0.919 0.919 0.914 0.948 0.948 0.919 0.919 0.914 0.952 0.952
## [18] 0.923 0.923 0.913 0.952 0.952 0.924 0.924 0.919

# It looks like there isn't much difference between the
# accuracy of the KNN models when the missing data is handled
# in different ways.
#     The important thing seems to be to use k=1 or k=2.



# --------- SVM Models ---------

library(kernlab)

# We will look at the accuracies for the SVM model for 6 different C's
# on the 3 data sets we formed after data imputation, the data excluding
# missing values, and the data with a binary variable to indicate if an
# observation has a missing value.

acc_svm<- rep(0,30)

amounts <- c(0.0001, 0.001, 0.01, 0.1, 1, 10) 

# Data with mode imputation

for (i in 1:6) {
  
  # Fit model using training set.
  
  model_svm <- ksvm(as.matrix(data_mode_imp[training,2:10]),
                       as.factor(data_mode_imp[training,11]),
                       type = "C-svc", # Use C-classification method
                       kernel = "vanilladot", # Use simple linear kernel
                       C = amounts[i])
  
  # Compare models using validation set.
  
  pred <- predict(model_svm, data_mode_imp[validation,2:10])
  acc_svm[i] = sum(pred == data_mode_imp[validation,11]) / nrow(data_mode_imp[validation,])
}

# Data with regression imputation

for (i in 1:6) {
  
  # Fit model using training set.
  
  model_svm <- ksvm(as.matrix(data_reg_imp[training,2:10]),
                    as.factor(data_reg_imp[training,11]),
                    type = "C-svc", # Use C-classification method
                    kernel = "vanilladot", # Use simple linear kernel
                    C = amounts[i])
  
  # Compare models using validation set.
  
  pred <- predict(model_svm, data_reg_imp[validation,2:10])
  acc_svm[i+6] = sum(pred == data_reg_imp[validation,11]) / nrow(data_reg_imp[validation,])
}

# Data with regression with perturbation imputation

for (i in 1:6) {
  
  # Fit model using training set.
  
  model_svm <- ksvm(as.matrix(data_reg_pert_imp[training,2:10]),
                    as.factor(data_reg_pert_imp[training,11]),
                    type = "C-svc", # Use C-classification method
                    kernel = "vanilladot", # Use simple linear kernel
                    C = amounts[i])
  
  # Compare models using validation set.
  
  pred <- predict(model_svm, data_reg_pert_imp[validation,2:10])
  acc_svm[i+12] = sum(pred == data_reg_pert_imp[validation,11]) / nrow(data_reg_pert_imp[validation,])
}

# Data without missing variables

for (i in 1:6) {
  
  # Fit model using training set.
  
  model_svm <- ksvm(as.matrix(data_no_missing[training_no_missing,2:10]),
                    as.factor(data_no_missing[training_no_missing,11]),
                    type = "C-svc", # Use C-classification method
                    kernel = "vanilladot", # Use simple linear kernel
                    C = amounts[i])
  
  # Compare models using validation set.
  
  pred <- predict(model_svm, data_no_missing[validation_no_missing,2:10])
  acc_svm[i+18] = sum(pred == data_no_missing[validation_no_missing,11]) / 
    nrow(data[validation_no_missing,])
}

# Data with binary variable to indicate if an observation
# has a missing V7 value. Use the interaction factor for modeling

for (i in 1:6) {
  
  # Fit model using training set.
  
  model_svm <- ksvm(as.matrix(data_binary[training,c(2:6,8:10,13)]),
                    as.factor(data_binary[training,11]),
                    type = "C-svc", # Use C-classification method
                    kernel = "vanilladot", # Use simple linear kernel
                    C = amounts[i])
  
  # Compare models using validation set.
  
  pred <- predict(model_svm, data_binary[validation,c(2:6,8:10,13)])
  acc_svm[i+24] = sum(pred == data_binary[validation,11]) / nrow(data_binary[validation,])
}

acc_svm

# Again, it looks like the way we deal with missing data is
# less important.  The important thing is to use a larger value
# of C, 0.1 or 1 or 10.
#
# There are two interesting things we can observe here:
# (1) SVM might be able to get slightly-better predictive 
# quality than KNN.
# (2) On the other hand, KNN is more robust; every value of k 
# we tried gave a good model, but in SVM using a value of C 
# that's too low led to a much worse model.

