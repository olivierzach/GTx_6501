rm(list=ls())
#CRTL+ L = clears the console
# help command: help(command) or ?command

### Logistic Regression Example with Newton's Method Application
X = matrix(c(1.3432504,-1.3311479,1.8205529,-0.6346681,0.98632067,-1.8885762,1.9443734,
             -1.635452,0.97673352,-1.3533151,1.9458584,-2.0443278,2.1075153,-2.1256684,
             2.070373,-2.4634101,0.86864964,-2.4119348,1.8006594,-2.7739689,3.1283787,
             -3.4452432,3.0947429,-3.6446145,2.9086652,-4.0065037,2.6770338,-3.0198592,
             2.7458671,-2.7100561,4.1714647,-3.4622482,3.931322,-2.1099044,4.378687,
             -2.3804743,4.8016565,-3.3803344,4.166105,-2.8138844,2.4670141,-1.6108444,
             3.4826743,-1.5533872,3.3652482,-1.8164936,2.8772788,-1.8511689,3.1090444,
             -1.6384946,2.2183701,0.074279558,1.9949873,0.16268659,2.9500308,0.016873016,
             2.0216009,0.17227387,2.0486921,-0.63581041,0.87548563,-0.54586168,0.57079941,
             -0.03327866,1.4266468,-0.75288337,0.72265633,-0.8669193,0.95346198,-1.4896956,
             4.8333333,0.070175439,4.3070175,1.4152047,6.0321637,0.4502924,5.4181287,
             -2.7076023,3.4590643,-2.8245614,2.7280702,-0.92397661,1.002924,0.77192982,
             3.6637427,-0.77777778,4.3070175,-1.0409357,3.6929825,-0.10526316,5.7397661,
             -1.625731,4.9795322,-1.5087719,6.5,-2.9122807,5.2426901,0.91812865,1.6754386,
             0.56725146,5.1708997,1.2103667,4.8795188,1.6081848,4.664987,1.0695532,4.4934321,
             1.2351592,4.1512967,0.8672126,3.717708,1.15172,3.6224477,1.3106769,3.0606943,
             1.4857163,7.0718465,-0.34961651,6.0391832,-0.24756832,6.674748,-0.12484766,
             6.8461291,0.25977167,6.4270724,-0.14713863,6.8456065,1.4754967,7.7054006,
             1.6045555,6.2870658,2.4156427,6.9810956,1.2599865,7.0990172,2.2155151,5.5275479,
             0.29968421,5.8303489,-0.21974408,6.3594527,0.23944217,6.1004524,-0.040957414,
             5.6237412,0.37135914,5.8836969,2.7768186,5.5781611,3.0682889,7.0050662,
             -0.25781727,4.4538114,0.83941831,5.6495924,1.3053929,4.6337489,1.9467546,
             3.6986847,2.2594084,4.1193005,2.547451,4.7665558,2.7531209,3.0812098,
             2.7985255,4.0730994,-3.0292398,3.4883041,-1.8888889,0.76900585,1.2105263,
             1.5,3.8128655,5.7982456,-2.0935673,6.8114529,-0.8345673,7.1106096,
             -1.0201158,7.494152,-1.7426901,3.1374269,0.42105263,1.6754386,0.50877193,
             2.494152,-0.86549708,4.7748538,0.099415205,5.8274854,-0.69005848,2.2894737,
             1.9707602,2.494152,1.4152047,2.0847953,1.3567251), nrow=99, 
              ncol=2,byrow = TRUE)
y= c(rep(0,50), rep(1,49))
plot(X[,1],y)
plot(X[,2],y)
 
# Set the initial guess and other parameters
beta_0 = 0
beta = matrix(0,nrow=2,ncol=1)
step = 0.001
precision = 0.00001
k=0 #to count number of iterations 

#Function to calculate h(xi;beta_0,beta)
h=function(X,beta_0,beta){
  1/(1+exp(-beta_0-X%*%beta))
}

### IMPORTANT: of course, there are built in function to get Gradient, Hessian and even
# the coefficients for Logistic Regression, however, I do wanna show you how to compute manually
# in the end, you're gonna see the function that does this automatically :) 

#Function to compute gradient
g=function (X,y,beta_0,beta){
  t(cbind(rep(1,dim(X)[1]),X))%*%(y-h(X,beta_0,beta))
}

#Function to compute Hessian
#install.packages("OpenMx")
library(OpenMx) #to use function vec2diag
hes=function(X,y,beta_0,beta){
  -t(cbind(rep(1,dim(X)[1]),X))%*%vec2diag(h(X,beta_0,beta)*(1-h(X,beta_0,beta)))%*%cbind(rep(1,dim(X)[1]),X)
}

grad = g(X,y,beta_0,beta)


while (t(grad)%*%grad > precision){ 
  hessian = matrix(0,nrow = 3, ncol=3)
  grad = matrix(0,nrow=3,ncol=1)
  grad = g(X,y,beta_0,beta)
  hessian = hes(X,y,beta_0,beta)
  beta_new = rbind(beta_0,beta)-step*solve(hessian)%*%grad
  beta_0 = beta_new[1]
  beta = t(t(beta_new[2:3])) 
  k=k+1
}

#values of beta after convergence 
beta_0
beta

# Let's try with R built in formula
model <- glm(y ~ X, family=binomial)
summary(model)
#for y=1, the log odds of having y=1 (versus y=0) decreases by -2.6205
#For a one unit increase in x1, the log odds of having y=1 (versus y=0) increases by 0.7604.
#For a one unit increase in x2, the log odds of having y=1 (versus y=0) increases by 1.1719.
# Null deviance is related to the model that only has intercept
# Residual deviance is related to the full model 
# They indicate how plausible it is to see the data in case the true model had the coeff.
# they can be evaluated with Chi2 stats
# Do we obtain statistically sig. reductions in deviance by adding the covariates x1 and x2? 
1-pchisq(137.233-65.171,98-96) #2.220446e-16 reject H0 that deviance of the null model and deviance
# of full model are the same
library(car)
Anova(model,type=3)

model$coefficients
exp(model$coefficients) #odds
exp(confint(model)) #CI for odds 
r = resid(model,type="deviance")
p = model$linear.predictors
plot(p,r,pch=19,xlab="linear predictor",ylab="deviance residuals")
plot(model)
plot(predict(model),residuals(model),col=c("blue","red")[1+y])
abline(h=0,lty=2,col="grey")



#Since we predict a probability for a variable taking values 0 or 1:
# if the true value is 0 (we always predict more) and residuals are negative (blue points)
# if the true value is 1 (we always predict less) and residuals are positive (the red points) 
## Crossvalidation
set.seed(1)
iterations = 9 # number of folds
n=dim(X)[1]#n is the number of rows of training set
s = matrix(0, nrow = (n/iterations), ncol=iterations)
sample_aux=sample.int(n,n, replace=FALSE) #sampling from 1:99 without replacement
pred_vector = matrix(0,ncol =iterations, nrow=1)
#building the sample matrix 
for (i in 1:iterations)
{ 
  s[,i] = sample_aux[((i-1)*(n/iterations)+1):(i*(n/iterations))]
}
# now performing CV

for (i in 1:iterations)
{ 
  test = X[s[,i],]
  train = X[-s[,i],]
  y_test = y[s[,i]]
  y_train = y[-s[,i]]
  dat_train=data.frame(x=train,y=as.factor(y_train))
  dat_test=data.frame(x=test,y=as.factor(y_test))
  model <- glm(y ~ ., data=dat_train,family=binomial)
  #prediction
  pred=round(predict(model, dat_test, type ="response")) #>0.5 = 1 and <=0.5 =0
  pred_vector[i]=mean(pred==dat_test[,3]) #dat_test[,3] is the column with original y
  table(pred, dat_test[,3])
  }
mean(pred_vector)

