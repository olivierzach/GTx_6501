rm(list=ls()) # clearing
#CRTL+ L = clears the console
## Ball Weights - Regression Example
y <- c(2,3,4) 
x1 <- c(1,0,1)
x2 <- c(0,1,1)
## Here we use "-1" to do regression without constant intercept
lm0 <- lm(y ~ x1 + x2 -1)
summary( lm0)

# confidence interval on ball A weight at different level
predict(lm0, data.frame(x1=1, x2=0), interval="confidence", level = 0.90)
predict(lm0, data.frame(x1=1, x2=0), interval="confidence", level = 0.80)
predict(lm0, data.frame(x1=1, x2=0), interval="confidence", level = 0.70)

## Prediction interval on the next weight of Ball A
predict(lm0, data.frame(x1=1, x2=0), interval="prediction", level = 0.90)

####Now manually, same example
x = cbind(x1,x2)
xtranspose = t(x)
xtx = xtranspose%*%x
inv_matrix = solve(xtx)  #invert matrix xtx
beta_vector = inv_matrix%*%xtranspose%*%y
tstat = qt(0.95,1) #getting t
###CI
upper = beta_vector[1]+ tstat*sqrt(inv_matrix[1,1])*sqrt(1/3)
lower = beta_vector[1]- tstat*sqrt(inv_matrix[1,1])*sqrt(1/3)




######Iris example of SVM
# download the dataset at https://archive.ics.uci.edu/ml/datasets/iris
# enter Download: source of data, then right click at iris.data (Save link as...)
install.packages("e1071")
rm(list=ls())
#CRTL+ L = clears the console
getwd()
setwd("~/Documents") ###Please choose the location of your Working Directory
# help command: help(command) or ?command
library(e1071)

backupdata<-read.table('iris.data',header = FALSE, sep=",")
# if the above command did not work, it's probably because
#the dataset was not saved in your Working Directory
#you can manually browse it by adding the function file.choose()

backupdata<-read.table(file.choose(),header = FALSE, sep=",") #don't need to do this if the first worked!
head(backupdata) #see first 6 rows
tail(backupdata) #see last 6 rows
readLines('iris.data', n=20) # see first n rows in a terrible display (I don't like it)
data<-backupdata
data[!complete.cases(data),] #list rows of data that have missing values
# no missing values in this case (that's good, folks!!!)
summary(data)  #get descriptive statistics (package (DoBy) has the function summaryBy() in case you want to be fancy)
#below is the description of each column, we want to rename accordingly
# 1. sepal length in cm 
# 2. sepal width in cm 
# 3. petal length in cm 
# 4. petal width in cm 
# 5. class: 
#-- Iris Setosa 
#-- Iris Versicolour 
#-- Iris Virginica
data =as.data.frame(data) #it's always nice to let R know that your data has data format
colnames(data) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Class")
#dev.new()  (use this function if you want to plot in a new window, if now, it's gonna appear in Plots right on the side)
par(mfrow=c(2,2)) # separate the window in 4 squares for plotting purposes
#attach(data)  if it works, you don't need to include data$, you can just call the variable
#detach(data) 
str(data)  # to check how the variables are stored (numeric, integer, factor, etc.)
#plots
plot(data$Sepal.Length, data$Sepal.Width, col=data$Class) 
plot(data$Petal.Length, data$Petal.Width, col=data$Class) 
plot(data$Sepal.Length, data$Petal.Length, col=data$Class) 
plot(data$Sepal.Width, data$Petal.Width, col=data$Class)
levels(data$Class)  #check the possibilities for the Class variable (3 species in this case)
#creating new subset (we just want to run svm in the 3 variables below)
Class=as.factor(data$Class)
Petal.Width = as.vector(data$Petal.Width)
Petal.Length = as.vector(data$Petal.Length)
newdata=data.frame(Class,Petal.Length,Petal.Width) #svm will only work with dataframe
str(newdata) #svm requires one variable as factor
par(mfrow=c(1,1))
plot(data$Petal.Length, data$Petal.Width, col=data$Class) 
#applying SVM to the full model
#the point after ~ indicates that all other variables are gonna be included
svm_full=svm(Class ~ ., data=newdata, kernel="linear", cost=1, scale=FALSE)
#scale= FALSE because we're using a linear svm
print(svm_full) 
par(mfrow=c(1,1))
plot(svm_full, newdata)
plot(Petal.Length, Petal.Width, col = Class, 
     pch = c("o","+")[1:150 %in% svm_full$index + 1], cex = 2, 
     xlab = "Petal Length", ylab = "Petal Width") # the + are support vectors

#prediction for full model
p_fullmodel=predict (svm_full, newdata, type="class") 
plot(p_fullmodel)
table(p_fullmodel, newdata[,1]) #plot the table of labels
mean(p_fullmodel==newdata[,1]) # prediction accuracy

#what's wrong with the above prediction?
#it's biased: we used the same data to construct the model and to predict!!!!!
#how to overcome this in the steps below (crossvalidation)
#subset model analysis
set.seed(1) # I am going to explain this to you soon, it's Random Number Generation
s = sample(150,135) #taking a sample of 135 numbers from 1:150 to use in crossvalidation
# each time the number are gonna be different = random!!!
#what this does is to randomly select 135 rows (from the 150 in dataset) to be training data
col=c("Petal.Length", "Petal.Width","Class") #we create the set with the names of the columns of the training set
training_set=newdata[s,col] #training set has 135 observations coming from random sample
str(training_set) #just to make sure nothing has changed!!!
reduced_svm=svm(Class ~., data=training_set, kernel="linear", cost=1, scale=FALSE)
#scale= FALSE because we're using a linear svm
print(reduced_svm) #the number of support vector varies according to the sample
plot(reduced_svm, training_set)
test_set=newdata[-s,col] #test set has 150-135 = 15 observations
#the - sign in front of s means, that we are taking the complement of rows of set s
str(test_set)
#prediction: use training set to build the model and test set to predict
p_reducedmodel=predict (reduced_svm, test_set, type="class") 
plot(p_reducedmodel)
table(p_reducedmodel, test_set[,3]) #plot the table of labels
mean(p_reducedmodel==test_set[,3])


#tune for Crossvalidation to identify the best cost parameter  
# this is usually done first (before prediction), we're only seeing afterwards for learning purposes
tune_linear=tune(svm,Class ~ ., data=training_set, kernel="linear", ranges=list(cost=c(0.001, 0.01, .1, 1, 10, 100)))
summary(tune_linear) 
bestmodel=tune_linear$best.model
summary(bestmodel)
#analyzing again with best cost parameter
new_reduced_svm=svm(Class ~., data=training_set, kernel="linear", cost=tune_linear$best.parameters, scale=FALSE)
tune_linear$best.performance #returns error
tune_linear$best.parameters  #returns cost
plot(tune_linear)



print(new_reduced_svm) #the number of support vector varies according to the sample
plot(new_reduced_svm, training_set)
test_set=newdata[-s,col]
str(test_set)
#prediction again with best cost parameter
new_p_reducedmodel=predict (new_reduced_svm, test_set, type="class") 
plot(new_p_reducedmodel)
table(new_p_reducedmodel, test_set[,3]) #plot the table of labels
mean(new_p_reducedmodel==test_set[,3])
#end



#### Another Regression Example

###roe01.txt
## ROE data
getwd()
setwd("~/Documents")
rm(list=ls())				# clearing
data=read.table("roe01.txt",header=T)	#read table

round(data[1:10,],4)			#first 10 rows, 4 digits after decimal point
a1=data      
Meana1  = sapply(a1,mean)		#mean per column
Mina1   = apply(a1,2, min)		# min per column
Mediana1= sapply(a1,median)		#median
Maxa1	=sapply(a1,max)			#max
SDa1	=sapply(a1,sd)			#standard deviation
cbind(Meana1,Mina1,Mediana1,Maxa1,SDa1)		#combine

round(cor(a),3)	#correlation, up to 3 digits after decimal points

plot(a1$ROEt,a1$ROE)		#plot ROE versus ROEt

lm1=lm(ROE~ROEt+ATO+PM+LEV+GROWTH+PB+ARR+INV+ASSET,data=a1)	
#fit the linear model, via least squares
summary(lm1)		#summary of results

### Linear Regression Diagnosis

###
round(a1[1:10,],3)		#first 10 rows again
par(mfrow=c(2,2))		#drawing in 2 by 2 format 
plot(lm1,which=c(1:4)) #plot four figures corresponding to lm1,Residual,QQ, Cook-distance

a1=a1[-47,]			#remove row 47
lm2=lm(ROE~ROEt+ATO+PM+LEV+GROWTH+PB+ARR+INV+ASSET,data=a1)	 #fit regression line again save in lm2	
plot(lm2,which=c(1:4)) 	# plot four figures for lm2


lm.aic=step(lm2,trace=F)	#AIC model selection
summary(lm.aic)		# The final model by AIC criterion

lm.bic=step(lm2,k=log(length(a1[,1])),trace=F)	#BIC model selection
summary(lm.bic)			# The final model by BIC criterion

