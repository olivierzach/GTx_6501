# -------------------- Code for Question 9.1 -----------------------------
# Clear environment
rm(list = ls())


# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)
set.seed(1)

# ---------------------------- Data manipulation -------------------------------------

# First, read in the data

data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

# Optional check to make sure the data is read correctly

head(data)


##########################
## Examining Plots  ######
##########################

#Plot some 2D graphs of the data to see if there is correlation or not

for (i in 1:15){
  for (j in 1:15){
    if (i<j){
      plot(data[,i],data[,j], main="Scatterplot Example",xlab=colnames(data)[i],ylab=colnames(data)[j], pch=19)
    }
  }
}


ggpairs(data, columns = c("Po1", "Po2", "U1", "U2", "Ineq", "Crime"),
                 mapping=ggplot2::aes(color= "#3366FF"))

#Also can look at the correlation matrix of the data

corr <- cor(data)
round(corr, 2)






##########################
## Running PCA  ##########
##########################

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

#Another way to use the prcomp function

pca <- prcomp(~.,data = data[,1:15], scale. = TRUE)
summary(pca)







##########################
## PCA Visualizations  ###
##########################

# The following are useful visualizations when deciding how many principal components to choose.
# In this case, we are told to just use the first 4 principal components.

screeplot(pca, type="lines",col="blue")

# Calculate the variances and proportion of variances from the pca object

var <- pca$sdev^2
propvar <- var/sum(var)

# Plot the proportion of variances from PCA

plot(propvar, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = "b")

# Plot the cumsum proportion of variances from PCA

cumsum(propvar)
plot(cumsum(propvar), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",ylim = c(0,1), type = "b")








##########################
## Get first 4 PCs  ######
##########################

# Method 1: direct from prcomp output

PCs <- pca$x[,1:4]
attributes(pca$x)
pca$x
PCs

# Method 2: calculated from prcomp output

data.scale <- as.data.frame(scale(data[,1:15]))
data.mat = as.matrix(data.scale)
PCs2 <- data.mat %*% pca$rotation[,1:4]

pca$rotation[,1:4]

PCs[1,]
PCs2[1,]

# Method 3: calculated using the math, if you did not use the prcomp function

E <- eigen(t(data.mat) %*% data.mat)
PCs3 <- data.mat %*% E$vectors[,1:4]

# NOTE: Eigenvectors 3&4 are the negative of what we get using the other approaches; it doesn't matter




##########################
## Regress on first 4 PCs
##########################

# Build linear regression model with the first 4 principal components

PCcrime <- cbind(PCs, data[,16]) #Create new data matrix with first 4 PCs and crime rate

PCcrime

as.data.frame(PCcrime) #Shows why is it referencing V5

model <- lm(PCcrime[,5]~., data = PCcrime) #Not correct way

model <- lm(V5~., data = as.data.frame(PCcrime)) #Create regression model on new data matrix

summary(model)

## Call:
## lm(formula = V5 ~ ., data = as.data.frame(PCcrime))
## 
## Residuals:
##   Min     1Q Median     3Q    Max 
## -557.8 -210.9  -29.1  197.3  810.3 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    905.1       49.1   18.44   <2e-16 ***
## PC1             65.2       20.2    3.23   0.0024 ** 
## PC2            -70.1       29.6   -2.36   0.0227 *  
## PC3             25.2       35.0    0.72   0.4760    
## PC4             69.4       46.0    1.51   0.1387    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 336 on 42 degrees of freedom
## Multiple R-squared:  0.309,	Adjusted R-squared:  0.243 
## F-statistic:  4.7 on 4 and 42 DF,  p-value: 0.00318





################################################
## Get coefficients in terms of original data
## from PCA coefficients
################################################
# PCA Coefficients for this linear regression model

beta0 <- model$coefficients[1]
betas <- model$coefficients[2:5]
beta0

## (Intercept) 
## 905 

betas

##  PC1   PC2   PC3   PC4 
## 65.2 -70.1  25.2  69.4 

# Transform the PC coefficients into coefficients for the original variables

pca$rotation[,1:4]
alphas <- pca$rotation[,1:4] %*% betas
t(alphas)

##          M   So   Ed  Po1  Po2  LF   M.F  Pop   NW    U1   U2 Wealth  Ineq Prob  Time
## [1,] -21.3 10.2 14.4 63.5 64.6 -14 -24.4 39.8 15.4 -27.2 1.43   38.6 -27.5  3.3 -6.61

### BUT... these coefficients above are using scaled data.
# Now, we have to convert back to the original data.
#
# When scaling, this function subtracts the mean and divides by the standard deviation, for each variable.
#
# So, alpha * (x - mean)/sd = originalAlpha * x.
# That means:
# (1) originalAlpha = alpha/sd
# (2) we have to modify the constant term a0 by alpha*mean/sd

originalAlpha <- alphas/sapply(data[,1:15],sd)
originalBeta0 <- beta0 - sum(alphas*sapply(data[,1:15],mean)/sapply(data[,1:15],sd))

# Here are the coefficients for unscaled data:

t(originalAlpha)

            M   So   Ed  Po1  Po2   LF  M.F  Pop  NW    U1  U2    
## [1,] -16.9 21.3 12.8 21.4 23.1 -347 -8.3  1.0 1.5 -1510 1.7
   Wealth Ineq  Prob Time
## 0.0400 -6.9 144.9 -0.9

originalBeta0

## 1667

# Here are the estimates from this model:

estimates <- as.matrix(data[,1:15]) %*% originalAlpha + originalBeta0
estimates

# And now calculate R^2 and R^2_adj

SSE = sum((estimates - data[,16])^2)
SStot = sum((data[,16] - mean(data[,16]))^2)
1 - SSE/SStot

## 0.309

R2 <- 1 - SSE/SStot
R2 - (1 - R2)*4/(nrow(data)-4-1)

## 0.243

# AS EXPECTED, the R-squared and Adjusted R-squared are the same 
# using the PCA dimensions and converted back to the original
# variables.
#
# BUT note that we had to make sure to use "4" rather than "15" 
# in the Adjusted-R-squared calculation: even though it's back to
# the original 15 variables, we only fit coefficients for 4 
# principal components, so 4 is the appropriate value to use.

# Now let's compare with the regression model from the previous homework

model2 <- lm( Crime ~ ., data = data)
summary(model2)

# This model has R^2 = 0.803 and R^2_adj = 0.708.

# These results suggest that we are better off using a more straightforward regression model
# instead of PCA before using regression.
# If we had used all 15 principal components, we would have obtained
# an R-squared value of 0.803, which is the same R-squared value when using all 
# 15 regular predictors in a basic linear regression model.

# In fact, let's try all possibilities: for i=1..15, run a regression using the first i principal components
#

r2 <- numeric(15) # create a vector to store the R-squared values

for (i in 1:15) {
  pclist <- pca$x[,1:i]  # use the first i prinicipal components
  pcc <- cbind(data[,16],pclist)  # create data set
  model <- lm(V1~.,data = as.data.frame(pcc)) # fit model
  r2[i] <- 1 - sum(model$residuals^2)/sum((data$Crime - mean(data$Crime))^2) # calculate R-squared
}

r2

## [1] 0.1711351 0.2631339 0.2716416 0.3091121 0.6451941 0.6586023 0.6881819 0.6898765
## [9] 0.6920491 0.6962873 0.6973865 0.7692656 0.7723664 0.7911447 0.8030868

# Compare these two plots: 
# ... cumulative proportion of variance explained, and
# ... R-squared with this many principal components

plot(cumsum(propvar), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

plot(r2, xlab = "Principal Component", ylab = "R-squared with this many principal components",
     ylim = c(0,1), type = "b")

# Notice the difference between the curves.
# Even though PCA estimates "proportion of variance" between the factors, 
# and R-squared estimates "proportion of variance explained" in the response,
# it turns out that they sometimes don't track so well.

# Anyway, back to the question of the PCA model seeming way worse than the non-PCA model.
# Remember from HW3 the big overfitting problem.
# Cross-validation estimated a much lower R-squared than
# the model showed on its training set.
# So, let's see what cross-validation says for PCA models:

# Install the DAAG package, which has cross-validation functions

install.packages("DAAG")
library(DAAG)

# do 5-fold cross-validation

r2cross <- numeric(15) # create a vector to store the R-squared values

for (i in 1:15) {
  pclist <- pca$x[,1:i]  # use the first i prinicipal components
  pcc <- cbind(data[,16],pclist)  # create data set
  model <- lm(V1~.,data = as.data.frame(pcc)) # fit model
  c <- cv.lm(as.data.frame(pcc),model,m=5) # cross-validate 
  r2cross[i] <- 1 - attr(c,"ms")*nrow(data)/sum((data$Crime - mean(data$Crime))^2) # calculate R-squared
}

r2cross

##  [1] 0.0735 0.0910 0.0666 0.1057 0.4872 0.4628 0.4562 0.3664 0.3337 0.2954 0.1863 0.3897
## [13] 0.3902 0.4736 0.4134

plot(r2cross, xlab = "Principal Component", ylab = "Cross-validated R-squared with this many principal components",
     ylim = c(0,1), type = "b")

Notice that the 5th principal component seems to make a big difference (both on training data and in cross-validation).  So, let's see what happens if we use just that component in a model.

pcc <- cbind(data[,16],pca$x[,5])
model <- lm(V1~.,data = as.data.frame(pcc))
summary(model)

## Coefficients:
##            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    905.1       46.5   19.47   <2e-16 ***
## V2            -229.0       48.0   -4.77    2e-05 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Residual standard error: 319 on 45 degrees of freedom
## Multiple R-squared:  0.336,     Adjusted R-squared:  0.321 
## F-statistic: 22.8 on 1 and 45 DF,  p-value: 1.95e-05

c <- cv.lm(as.data.frame(pcc),model,m=5) # cross-validate 
1 - attr(c,"ms")*nrow(data)/sum((data$Crime - mean(data$Crime))^2) # calculate R-squared

## [1] 0.216

# NOTE: PCA generally does not work well with binary data.
# In this data set, the second column is binary.
# There are some advanced methods to work with binary data,
# but we're not going to cover them in this course.
# Instead, for this data set we could remove the binary factor,
# run PCA on the rest, and then add the binary factor back in.

# I won't go through all the steps above, but I'll just show how it's done below.

pca2 <- prcomp(cbind(data[,1],data[3:15]),scale.=TRUE) # PCA without column 2
PCs2 <- pca2$x[,1:4] # first 4 principal components
PCcrime2 <- cbind(data[,2],PCs2,data[,16]) # Add column 2 back in
model2 <- lm(V6~.,data=as.data.frame(PCcrime2)) # regression model
summary(model2)


# Alternatively, we could've used the pls package.  I'll just show the first couple of lines:

install.packages("pls")
library(pls)

# Run principal component regression function with only the first 4 principal components

numcomp <- 4
pcr.fit <- pcr(Crime ~ ., data = data, scale = TRUE, ncomp = numcomp)
summary(pcr.fit)

## Data: 	X dimension: 47 15 
## Y dimension: 47 1
## Fit method: svdpc
## Number of components considered: 4
## TRAINING: % variance explained
##        1 comps  2 comps  3 comps  4 comps
## X        40.13    58.81    72.17    79.92
## Crime    17.11    26.31    27.16    30.91

# These are the first 4 principal components

pcr.fit$scores
