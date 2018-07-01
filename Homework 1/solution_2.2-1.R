# -------------------- Code for Question 2.2 part 1 -----------------------------
# Clear environment

rm(list = ls())

# Load the kernlab library (which contains the ksvm function) and read in the data
#

library(kernlab)

# ---------------------------- Data manipulation -------------------------------------

data <- read.table("/Users/Chewy/Downloads/credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

#
# optional check to make sure the data is read correctly
#

head(data)

# Console output for head(data)
##   V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11
## 1 1 30.83 0.000 1.25 1 0 1 1 202 0 1
## 2 0 58.67 4.460 3.04 1 0 6 1 43 560 1
## 3 0 24.50 0.500 1.50 1 1 0 1 280 824 1
## 4 1 27.83 1.540 3.75 1 0 5 0 100 3 1
## 5 1 20.17 5.625 1.71 1 1 0 1 120 0 1
## 6 1 32.08 4.000 2.50 1 1 0 0 360 0 1
# NOTE: ALL ROWS OF THIS FILE STARTING WITH "##" DENOTE R OUTPUT
#

# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)

# -------------------------- Creating the models ------------------------------------

# -------------------------- Scaled=TRUE model ------------------------------------

# Fit the model using scaled=TRUE.
# V11 is response, other variables are predictors
#

model_scaled <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),
		   type = "C-svc", # Use C-classification method
              kernel = "vanilladot", # Use simple linear kernel
              C = 100,
		   scaled=TRUE) # have ksvm scale the data for you
              
# or you could use this call; it does the same thing

model_scaled <- ksvm(V11~.,data=data,
              type = "C-svc", # Use C-classification method
              kernel = "vanilladot", # Use simple linear kernel
              C = 100,
		   scaled=TRUE) # have ksvm scale the data for you

#Attributes model show what the data structure model has to reference
#For example, we use model@b to get the intercept and model@coef to get the coefficients
#Those references (b and coef) can be found listed in the console by using attributes(model)

attributes(model_scaled)

# Console output for attributes(model_scaled) is left out since it is a long output

#model lists some high level information about the model data structure

model_scaled

# Console output for model_scaled
##
## Support Vector Machine object of class "ksvm"
## SV type: C-svc (classification)
## parameter : cost C = 100
## Linear (vanilla) kernel function.
## Number of Support Vectors : 189 
## Objective Function Value : -17887.92 
## Training error : 0.136086

# -------------------------- Calculating the a coefficients ------------------------------------
#
#Classification is done using linear kernel, a*scaled(x) + a0. 
# Unfortunately, the model does not output a directly, but we can use the model output to find a.
# calculate a1 to am using the stored data point values in the model data structure and corresponding coefficients
# multiplying the xmatrix by the coef gives the linear combination of data points that define a1,...,am
# we use the xmatrix attribute since the model stores these data points as scaled

a_scaled <- colSums(model_scaled@xmatrix[[1]] * model_scaled@coef[[1]])

#
# a0 is just -model_scaled@b

a0_scaled<- -model_scaled@b

#

a_scaled
a0_scaled

#Console output for a_scaled 
## V1            V2            V3            V4            V5           
## -0.0010065348 -0.0011729048 -0.0016261967  0.0030064203  1.0049405641 
## V6            V7            V8            V9           V10 
## -0.0028259432 0.0002600295 -0.0005349551 -0.0012283758  0.1063633995 

#Console output for a0_scaled
## [1] 0.08158492

# -------------------------- Calculating the predicted values ------------------------------------
#
#The ksvm package provides a predict() function that implements this for us, but we also
#show how to get the predicted values using the a coefficients

# Calculate the predicted values using the a's we got above and our data set.
# The coefficients for this model are based on the SCALED data points, so we need to 
# scale our data points to get the correct predictions. We do this by using the scaled
# mean and standard deviation values for V1 to V10 stored in the model data structure as:
# model@scaling$x.scale$`scaled:center` (means for V1 to V10)
# model@scaling$x.scale$`scaled:scale` (standard deviation for V1 to V10)
# Then we transform the data points into their scaled equivalent by using the function:
# scaled data point[i,1:10] = (data point[i,1:10] - model@scaling$x.scale$`scaled:center`)/model@scaling$x.scale$`scaled:scale`
#
#Create predicted vector (to hold our calculated predicted values)

predicted_scaled<-rep(0,nrow(data))

#For each data point, perform the transformation, calculate a*scaled(data point)+a0, 
#and predict value of data point based on the resulting value

for (i in 1:nrow(data)){

  #If the data point is above the classifier, predicted value = 1

  if (sum(a_scaled*(data[i,1:10]-model_scaled@scaling$x.scale$`scaled:center`)/model_scaled@scaling$x.scale$`scaled:scale`) + a0_scaled >= 0){
    predicted_scaled[i] <- 1
  }

  #If the data point is below the classifier, predicted value = 0

  if (sum(a_scaled*(data[i,1:10]-model_scaled@scaling$x.scale$`scaled:center`)/model_scaled@scaling$x.scale$`scaled:scale`) + a0_scaled < 0){
    predicted_scaled[i] <- 0
  }
}
predicted_scaled

# Output from predicted_scaled
##  [1] 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [42] 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1
##  [83] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [124] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [165] 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [206] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0
##  [247] 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [288] 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [329] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [370] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [411] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [452] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [493] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [534] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 1
##  [575] 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [616] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0


# Get prediction from ksvm model we created, model_scaled
# Note that we could also get the predicted values of the model using model_scaled@fitted
#

pred_scaled <- predict(model_scaled,data[,1:10])
pred_scaled

#Output from pred_scaled
## [1] 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [42] 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1
## [83] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [124] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [165] 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [206] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0
## [247] 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [288] 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [329] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [370] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [411] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [452] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [493] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [534] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 1
## [575] 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [616] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

# typing "pred_scaled" will give the sequence of 1s and 0s showing the model's classification
# As you can see in the outputs, pred and predicted have the same predicted values
# so we know that our a coefficients are correct for the SCALED data version of the model

# -------------------------- Calculating the model's accuracy ------------------------------------
#
# I will use a simple accuracy measure that outputs the
# percent of testing observations that are correctly classified.

sum(pred_scaled == data$V11) / nrow(data)
sum(predicted_scaled == data$V11) / nrow(data)

#Output from sum(pred_scaled == data$V11) / nrow(data)
## [1] 0.8639144
#
#Output from sum(predicted_scaled == data$V11) / nrow(data)
## [1] 0.8639144
# Note that this result is found by a wide range of values of C.









# -------------------------- Scaled=FALSE model ------------------------------------

# Fit the model using scaled=FALSE.
# V11 is response, other variables are predictors
#

model_unscaled <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),
		   type = "C-svc", # Use C-classification method
              kernel = "vanilladot", # Use simple linear kernel
              C = 100,
		   scaled=FALSE) # ksvm will not scale the data for you
              
# or you could use this call; it does the same thing

model_unscaled <- ksvm(V11~.,data=data,
              type = "C-svc", # Use C-classification method
              kernel = "vanilladot", # Use simple linear kernel
              C = 100,
		   scaled=FALSE) # ksvm will not scale the data for you

#Attributes model show what the data structure model has to reference
#For example, we use model_unscaled@b to get the intercept and model_unscaled@coef to get the coefficients
#Those references (b and coef) can be found listed in the console by using attributes(model_unscaled)

attributes(model_unscaled)

# Console output for attributes(model_unscaled) is left out since it is a long output

#model lists some high level information about the model data structure

model_unscaled

# Console output for model_unscaled
##
## Support Vector Machine object of class "ksvm" 
## SV type: C-svc  (classification) 
## parameter : cost C = 100 
## Linear (vanilla) kernel function. 
## Number of Support Vectors : 186 
## Objective Function Value : -2213.731 
## Training error : 0.278287 

# -------------------------- Calculating the a coefficients ------------------------------------
#
#Classification is done using linear kernel, a*unscaled(x) + a0 = a*x + a0. 
# Unfortunately, the model does not output a directly, but we can use the model output to find a.
# calculate a1 to am using the stored data point values in the model data structure and corresponding coefficients
# multiplying the xmatrix by the coef gives the linear combination of data points that define a1,...,am
# we use the xmatrix attribute since the model stores these data points as unscaled

a_unscaled <- colSums(model_unscaled@xmatrix[[1]] * model_unscaled@coef[[1]])

#
# a0 is just -model_unscaled@b

a0_unscaled <- -model_unscaled@b

#

a_unscaled
a0_unscaled

#Console output for a_unscaled
## V1            V2            V3            V4            V5            
## -0.0483050561 -0.0083148473 -0.0836550114  0.1751121271  1.8254844547   
## V6           V7            V8            V9           V10 
## 0.2763673361 0.0654782414 -0.1108211169 -0.0047229653 -0.0007764962 

#Console output for a0_unscaled
## 0.5255393

# -------------------------- Calculating the predicted values ------------------------------------
#
#The ksvm package provides a predict() function that implements this for us, but we also
#show how to get the predicted values using the a coefficients

# Calculate the predicted values using the a's we got above and our data set
# The coefficients for this model are based on the UNSCALED data points, so we do not need to 
# scale our data points to get the correct predictions.

#Create predicted vector (to hold our calculated predicted values)

predicted_unscaled<-rep(0,nrow(data))

#For each data point, calculate a*(data point)+a0, 
#and predict value of data point based on the resulting value

for (i in 1:nrow(data)){

  #If the data point is above the classifier, predicted value = 1

  if (sum(a_unscaled*data[i,1:10]) + a0_unscaled >= 0){
    predicted_unscaled[i] <- 1
  }

  #If the data point is below the classifier, predicted value = 0

  if (sum(a_unscaled*data[i,1:10]) + a0_unscaled < 0){
    predicted_unscaled[i] <- 0
  }
}
predicted_unscaled

# Output from predicted_unscaled
## [1] 1 1 1 1 1 1 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 0 0 1 1 1
## [42] 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 1 1 0 1 1 0 1 1 1 0 1 0 0 0 0 0 1 0 1 0 1 1 1 0 1 1
## [83] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1
## [124] 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 1 1 0 1 1 0 1
## [165] 0 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1
## [206] 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 0 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 0 1 0
## [247] 0 0 0 0 0 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [288] 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1 1 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0
## [329] 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0
## [370] 1 0 1 0 0 0 1 1 1 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0
## [411] 0 0 0 0 0 0 1 0 1 0 0 1 0 0 0 1 0 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0
## [452] 1 1 0 1 0 0 0 1 0 1 0 0 0 0 1 0 0 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1
## [493] 0 1 1 1 1 1 1 0 1 0 1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 0
## [534] 0 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 0 0 1 1 0 1 1 1 0 0 0 1 0 1 1 0 1 0 1 0 0 1
## [575] 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1
## [616] 0 0 0 1 0 1 0 0 1 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1

# Get prediction from ksvm model we created
#

pred_unscaled <- predict(model_unscaled,data[,1:10])
pred_unscaled

#Output from pred_unscaled
## [1] 1 1 1 1 1 1 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 0 0 1 1 1
## [42] 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 1 1 0 1 1 0 1 1 1 0 1 0 0 0 0 0 1 0 1 0 1 1 1 0 1 1
## [83] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1
## [124] 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 1 1 0 1 1 0 1
## [165] 0 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1
## [206] 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 0 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 0 1 0
## [247] 0 0 0 0 0 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [288] 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1 1 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0
## [329] 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0
## [370] 1 0 1 0 0 0 1 1 1 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0
## [411] 0 0 0 0 0 0 1 0 1 0 0 1 0 0 0 1 0 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0
## [452] 1 1 0 1 0 0 0 1 0 1 0 0 0 0 1 0 0 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1
## [493] 0 1 1 1 1 1 1 0 1 0 1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 0
## [534] 0 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 0 0 1 1 0 1 1 1 0 0 0 1 0 1 1 0 1 0 1 0 0 1
## [575] 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1
## [616] 0 0 0 1 0 1 0 0 1 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1

# typing "pred_unscaled" will give the sequence of 1s and 0s showing the model's classification
# As you can see in the outputs, pred and predicted have the same predicted values
# so we know that our a coefficients are correct for the SCALED data version of the model

# -------------------------- Calculating the model's accuracy ------------------------------------
#
# I will use a simple accuracy measure that outputs the
# percent of testing observations that are correctly classified.

sum(pred_unscaled == data$V11) / nrow(data)
sum(predicted_unscaled == data$V11) / nrow(data)

#Output from sum(pred_unscaled == data$V11) / nrow(data)
## [1] 0.7217125
#
#Output from sum(predicted_unscaled == data$V11) / nrow(data)
## [1] 0.7217125
