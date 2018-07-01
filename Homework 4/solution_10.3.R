# -------------------- Code for Question 10.3 -----------------------------

# Clear environment

rm(list=ls())

# Set the working directory

setwd("C:\\Users\\akrishna39\\Desktop\\office desktop\\Courses\\Summer 2017\\TA course\\HWs\\HW4")


# Read the data file

data<-read.table("germancredit.txt",sep = " ")

#
# Optional check to make sure the data is read correctly
#

head(data)

##    V1 V2  V3  V4   V5  V6  V7 V8  V9  V10 V11  V12 V13  V14  V15 V16  V17 V18  V19  V20 V21
## 1 A11  6 A34 A43 1169 A65 A75  4 A93 A101   4 A121  67 A143 A152   2 A173   1 A192 A201   1
## 2 A12 48 A32 A43 5951 A61 A73  2 A92 A101   2 A121  22 A143 A152   1 A173   1 A191 A201   2
## 3 A14 12 A34 A46 2096 A61 A74  2 A93 A101   3 A121  49 A143 A152   1 A172   2 A191 A201   1
## 4 A11 42 A32 A42 7882 A61 A74  2 A93 A103   4 A122  45 A143 A153   1 A173   2 A191 A201   1
## 5 A11 24 A33 A40 4870 A61 A73  3 A93 A101   4 A124  53 A143 A153   2 A173   2 A191 A201   2
## 6 A14 36 A32 A46 9055 A65 A73  2 A93 A101   4 A124  35 A143 A153   1 A172   2 A192 A201   1


# Since binomial family of glm recognises 0 and 1 as the classfication values, 
# convert 1s and 2s to 0s and 1s for the response variable

data$V21[data$V21==1]<-0
data$V21[data$V21==2]<-1

# Set the seed to produce reproducible results as random sampling is done in 
#the next step

set.seed(1)

# Divide the data into 70% training and 30% test/validation data

m <- nrow(data)
trn <- sample(1:m, size = round(m*0.7), replace = FALSE)
d.learn <- data[trn,]
d.valid <- data[-trn,]

# Develop the logistic regression model

# 1st iteration: Use all the available variables

reg = glm(V21 ~.,family=binomial(link = "logit"),data=d.learn)
summary(reg)

# 2nd iteration: Use all the variables found significant in 
# the 1st iteration.
# For categorical variables, if any value is significant,
# we'll keep all values in the model.

reg = glm(V21 ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V12+V14+V16+V20,family=binomial(link = "logit"),data=d.learn)
summary(reg)

# V1, V2, V3, V4, V5, V6, V8, V9, V10, V14, V20 are significant.

# 3rd iteration: Use only the significant variables obtained in the 2nd iteration.

reg = glm(V21 ~ V1+V2+V3+V4+V5+V6+V8+V9+V10+V14+V20,family=binomial(link = "logit"),data=d.learn)
summary(reg)

# But for the categorical variables, not all values are significant.
# So, we can create a binary variable for each
# significant factor:

d.learn$V1A13[d.learn$V1 == "A13"] <- 1
d.learn$V1A13[d.learn$V1 != "A13"] <- 0

d.learn$V1A14[d.learn$V1 == "A14"] <- 1
d.learn$V1A14[d.learn$V1 != "A14"] <- 0

d.learn$V3A32[d.learn$V3 == "A32"] <- 1
d.learn$V3A32[d.learn$V3 != "A32"] <- 0

d.learn$V3A33[d.learn$V3 == "A33"] <- 1
d.learn$V3A33[d.learn$V3 != "A33"] <- 0

d.learn$V3A34[d.learn$V3 == "A34"] <- 1
d.learn$V3A34[d.learn$V3 != "A34"] <- 0

d.learn$V4A41[d.learn$V4 == "A41"] <- 1
d.learn$V4A41[d.learn$V4 != "A41"] <- 0

d.learn$V4A410[d.learn$V4 == "A410"] <- 1
d.learn$V4A410[d.learn$V4 != "A410"] <- 0

d.learn$V4A42[d.learn$V4 == "A42"] <- 1
d.learn$V4A42[d.learn$V4 != "A42"] <- 0

d.learn$V4A43[d.learn$V4 == "A43"] <- 1
d.learn$V4A43[d.learn$V4 != "A43"] <- 0

d.learn$V4A48[d.learn$V4 == "A48"] <- 1
d.learn$V4A48[d.learn$V4 != "A48"] <- 0

d.learn$V4A49[d.learn$V4 == "A49"] <- 1
d.learn$V4A49[d.learn$V4 != "A49"] <- 0

d.learn$V6A63[d.learn$V6 == "A63"] <- 1
d.learn$V6A63[d.learn$V6 != "A63"] <- 0

d.learn$V6A65[d.learn$V6 == "A65"] <- 1
d.learn$V6A65[d.learn$V6 != "A65"] <- 0

d.learn$V9A93[d.learn$V9 == "A93"] <- 1
d.learn$V9A93[d.learn$V9 != "A93"] <- 0

d.learn$V10A103[d.learn$V10 == "A103"] <- 1
d.learn$V10A103[d.learn$V10 != "A103"] <- 0

d.learn$V14A143[d.learn$V14 == "A143"] <- 1
d.learn$V14A143[d.learn$V14 != "A143"] <- 0

d.learn$V20A202[d.learn$V20 == "A202"] <- 1
d.learn$V20A202[d.learn$V20 != "A202"] <- 0

# Next round model:

reg = glm(V21 ~ V1A13 + V1A14 + V2 + V3A32 + V3A33 + V3A34 + V4A41 + V4A410 + V4A42 + V4A43 + V4A48 + V4A49 + V5 + V6A63 + V6A65 + V8 + V9A93 + V10A103 + V14A143 + V20A202,family=binomial(link = "logit"),data=d.learn)
summary(reg)

# Remove V4A48 and V6A63 (p-value above 0.05) and V20A202 (p-value above 0.1)

reg = glm(V21 ~ V1A13 + V1A14 + V2 + V3A32 + V3A33 + V3A34 + V4A41 + V4A410 + V4A42 + V4A43 + V4A49 + V5 + V6A65 + V8 + V9A93 + V10A103 + V14A143,family=binomial(link = "logit"),data=d.learn)
summary(reg)

## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  2.93e-01   5.04e-01    0.58  0.56052    
## V1A13       -1.47e+00   4.88e-01   -3.01  0.00261 ** 
## V1A14       -1.53e+00   2.38e-01   -6.43  1.3e-10 ***
## V2           3.34e-02   1.04e-02    3.22  0.00128 ** 
## V3A32       -6.58e-01   3.36e-01   -1.96  0.04989 *  
## V3A33       -8.60e-01   4.21e-01   -2.04  0.04123 *  
## V3A34       -1.44e+00   3.79e-01   -3.79  0.00015 ***
## V4A41       -1.99e+00   4.63e-01   -4.31  1.7e-05 ***
## V4A410      -2.82e+00   1.07e+00   -2.64  0.00834 ** 
## V4A42       -6.77e-01   2.76e-01   -2.45  0.01429 *  
## V4A43       -7.93e-01   2.61e-01   -3.04  0.00237 ** 
## V4A49       -7.88e-01   3.65e-01   -2.16  0.03100 *  
## V5           1.25e-04   5.05e-05    2.47  0.01342 *  
## V6A65       -1.14e+00   3.10e-01   -3.67  0.00025 ***
## V8           2.64e-01   1.02e-01    2.58  0.00987 ** 
## V9A93       -6.28e-01   2.08e-01   -3.02  0.00250 ** 
## V10A103     -1.15e+00   4.84e-01   -2.38  0.01735 *  
## V14A143     -7.70e-01   2.45e-01   -3.15  0.00164 ** 
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## AIC: 673.5

The coefficients of each significant factor are shown above in the "estimate" column.


# -------- Validation -------- #

# Now add the binary variables to the validation set

d.valid$V1A13[d.valid$V1 == "A13"] <- 1
d.valid$V1A13[d.valid$V1 != "A13"] <- 0

d.valid$V1A14[d.valid$V1 == "A14"] <- 1
d.valid$V1A14[d.valid$V1 != "A14"] <- 0

d.valid$V3A32[d.valid$V3 == "A32"] <- 1
d.valid$V3A32[d.valid$V3 != "A32"] <- 0

d.valid$V3A33[d.valid$V3 == "A33"] <- 1
d.valid$V3A33[d.valid$V3 != "A33"] <- 0

d.valid$V3A34[d.valid$V3 == "A34"] <- 1
d.valid$V3A34[d.valid$V3 != "A34"] <- 0

d.valid$V4A41[d.valid$V4 == "A41"] <- 1
d.valid$V4A41[d.valid$V4 != "A41"] <- 0

d.valid$V4A410[d.valid$V4 == "A410"] <- 1
d.valid$V4A410[d.valid$V4 != "A410"] <- 0

d.valid$V4A42[d.valid$V4 == "A42"] <- 1
d.valid$V4A42[d.valid$V4 != "A42"] <- 0

d.valid$V4A43[d.valid$V4 == "A43"] <- 1
d.valid$V4A43[d.valid$V4 != "A43"] <- 0

d.valid$V4A49[d.valid$V4 == "A49"] <- 1
d.valid$V4A49[d.valid$V4 != "A49"] <- 0

d.valid$V6A65[d.valid$V6 == "A65"] <- 1
d.valid$V6A65[d.valid$V6 != "A65"] <- 0

d.valid$V9A93[d.valid$V9 == "A93"] <- 1
d.valid$V9A93[d.valid$V9 != "A93"] <- 0

d.valid$V10A103[d.valid$V10 == "A103"] <- 1
d.valid$V10A103[d.valid$V10 != "A103"] <- 0

d.valid$V14A143[d.valid$V14 == "A143"] <- 1
d.valid$V14A143[d.valid$V14 != "A143"] <- 0

# test the model

y_hat<-predict(reg,d.valid,type = "response")
y_hat

# y_hat is a vector of fractions.
# Now we can use a threshold to make yes/no decisions,
# and view the confusion matrix.

y_hat_round <- as.integer(y_hat > 0.5)

t <- table(y_hat_round,d.valid$V21)
t

# Model's accuracy is (183 + 43) / (183 + 43 + 22 + 52) = 75%.

acc <- (t[1,1] + t[2,2]) / sum(t)
acc

## [1] 0.753

# Import the library for developing ROC curve

library(pROC)

# Develop ROC curve to determine the quality of fit

r<-roc(d.valid$V21,y_hat_round)

# Plot the ROC curve

plot(r,main="ROC curve")
r

# The area under the curve is 67%. This means that whenever a sample is chosen
# from the response group and another sample is chosen from the non-response 
# group, then the model will correctly classify both the samples 67% of the times.

# But how do we know that 0.5 is the right threshold?
#
# Let's try some others.

acc <- c()
auc <- c()

for (i in 1:9) {
  y_hat_round <- as.integer(y_hat > i/10)
  t <- table(y_hat_round,d.valid$V21)
  acc <- cbind(acc,(t[1,1] + t[2,2]) / sum(t))
  r<-roc(d.valid$V21,y_hat_round)
  auc <- cbind(auc,r$auc)
}

acc
auc


# So if we're just looking for the highest accuracy, 
# a threshold of 0.5 looks good.
# If we're judging by AUC, a smaller threshold (0.2 or 0.3) 
# is slightly better. but not by much.


# -------------------- Part 2 -----------------------------

# The loss of incorrectly classfying a "bad" customer is 5 times the loss of 
# incorrectly classifying a "good" customer. 
# Calulating loss for the value of 
# thresholds ranging from 0.01 to 1. 

loss <- c()
for(i in 1:100)
{
  y_hat_round <- as.integer(y_hat > (i/100)) # calculate threshold predictions

  tm <-as.matrix(table(y_hat_round,d.valid$V21))

  if(nrow(tm)>1) { c1 <- tm[2,1] } else { c1 <- 0 }
  if(ncol(tm)>1) { c2 <- tm[1,2] } else { c2 <- 0 }
  loss <- c(loss, c2*5 + c1)
}

plot(c(1:100)/100,loss,xlab = "Threshold",ylab = "Loss",main = "Loss vs Threshold")

which.min(loss)

## [1] 13

loss

##   [1] 201 198 190 183 185 178 169 169 168 174 173 166 165 171 176 178 176 187 187 182 188 189 195 190
##  [25] 197 204 212 210 220 219 217 225 228 230 238 244 248 251 254 254 261 265 263 270 279 278 276 281
##  [49] 277 282 287 302 307 311 309 314 312 332 336 341 339 353 358 367 372 377 386 390 403 408 407 416
##  [73] 421 430 440 439 439 443 442 447 446 451 456 461 461 461 461 460 460 465 470 470 470 470 475 475
##  [97] 475 475 475 475

# The threshold probability corresponding to minimum expected loss is 0.13.  
# The range from 0.07-0.14 is all pretty good.
# The expected loss at 0.13 is 165 over the 300 validation data points.
# That compares to 282 for a threshold of 0.5.
# So accounting for the situation is important.

#Here's the accuracy and area-under-curve for the 0.13 threshold:

y_hat_round <- as.integer(y_hat > (which.min(loss)/100)) # find 0/1 predictions
t <- table(y_hat_round,d.valid$V21) # put in table form 
acc <- (t[1,1] + t[2,2]) / sum(t) # calculate accuracy
r<-roc(d.valid$V21,y_hat_round) # calculate ROC curve
auc <- r$auc # get AUC

acc
auc

## [1] 0.57
## Area under the curve: 0.66
