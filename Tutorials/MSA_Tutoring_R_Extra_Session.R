#Efron (1982) analyzes data on law school admission, with the object being to examine the
#correlation between LSAT score and the first year GPA. For each of 15 law schools, we
#have the pair of data points.
setwd("~/Documents") 
rm(list=ls())
lsat =c(576,580,653,635,555,575,558,661,545,578,651,572,666,605,594)
gpa = c(3.93,3.07,3.12,3.3,3,2.74,2.81,3.43,2.76,3.03,3.36,2.88,3.44,3.13,2.96)
plot(lsat, gpa)
cor = round(cor(lsat,gpa), 2)
help("cor")
mydata = cbind(lsat,gpa)
colnames(mydata) <- c("lsat","gpa")
mydata = as.data.frame(mydata)
#notice that some functions in R require matrix, not dataframe. Always check help("function")
class(mydata)
#in this case, both of our variables are numeric, so we don't really need to use dataframe format
#dataframe is particularly interesting when you have different types of variables (char/logic/etc.)
#however, if you only have numeric data, matrix format can be used (more memory efficient)
# matrix does not allow to call the variable, like in mydata$
#mydata = as.matrix(mydata) #revert to matrix
getwd()  #returns your working directory
#write.csv(mydata, "gpadata.csv") #export data to a .csv file in your working directory
subsetdata = subset(mydata,gpa>3, select = -lsat) 
#creates a subset of data by selecting only gpa>3 and removing column lsat)
summary(mydata) #get descriptive statistics
mydata = cbind(mydata,rep(0,15))  #add a new column to your dataset
colnames(mydata) <- c("lsat","gpa","situation")
mydata$situation[mydata$gpa<3]="Probation"
mydata$situation[mydata$gpa>=3 & mydata$gpa<3.5]="Good"
mydata$situation[mydata$gpa>=3.5]="Excellent"
str(mydata)
mydata$situation = as.factor(mydata$situation)
class(mydata$situation)

# let's do same thing as above, but using a function
situation= function(x)
{
  n = dim(x)[1]
  situation = character()
  for (i in 1:n)
  {
    if(x[i,2]<3)
      situation[i] = "Probation"
    else if(x[i,2]>=3 & x[i,2]<3.5)       
      situation[i] = "Good"
    else
      situation[i] = "Excellent"
  }
  return(situation)
}
vector_situation = situation(mydata)
vector_situation
mydata$situation=rep(0,15)
mydata$situation = vector_situation
#install.packages("doBy")
library(doBy)
summaryBy( mydata$lsat ~ mydata$situation, data = mydata,FUN = c(mean, var,sd, length, min, max))
#why is variance of Excellent = NA? Only 1 observation!!!

#another way to do SummaryBy, but manually
round(mean(as.numeric(mydata$lsat[which(mydata$situation=="Good")])),2)
#or more easily by using apply
x= as.matrix(mydata$lsat[which(mydata$situation=="Good")]) #to use apply, convert to matrix format
round(apply(X=x,FUN= mean, MARGIN = 2),2)
#apply a function (fun) to the array (x) over a vector (margin).
#Margin can be a row (1) or column(2),
#or build a function yourself!!!


boxplot(mydata$lsat ~ mydata$situation, data=mydata, 
        col=(c("blue","red","yellow" )),
        main="BoxPlot - lsat per situation", xlab="Situation", ylim=c(min(mydata$lsat),max(mydata$lsat)))

#install.packages("reshape2")
#install.packages("ggplot2")
library(reshape2)
library(ggplot2)
data <- melt(mydata,id.vars = "situation")
data2 <- melt(mydata, id.vars = c("situation","gpa" )) 
ggplot(data2,aes(x=factor(situation), y = value)) + 
  facet_wrap(~variable) +
  geom_bar(stat="identity", aes(fill = factor(situation)))

ggplot(data2,aes(x=factor(situation), y = value))+ ggtitle("Fancy Plot")+xlab("Situation")+ylab("Total Sum of lsat")+
  facet_grid(situation~.) +
  geom_bar(stat="identity", aes(fill = factor(situation)))+ scale_fill_manual(
    values = c("blue","red","yellow"),
    name = "Situation", labels = c("excellent", "good", "probation"))

#both plot don't make a lot of sense, but you can do SO many things with ggplot, give it a try!
# google ggplot cheatsheet
rm(data2) #remove data2 from Global Environment
rm(data)
rm(subsetdata)

#Let's continue the example with some bootstrapping (don't worry if you don't know what this is)
# bootstrapping to estimate the std dev and CI of the correlation coeff. Use B = 1000 resamples.
# 95% CI
###letter b
B=1000
n=dim(mydata)[1] #could also be length(lsat) or length(gpa) or nrow(mydata)
sample_cor=matrix(0,nrow=B,ncol=1) #creates a matrix of zeros with B rows and 1 column
# could also use vector(mode="double",length = 10) or just rep(0,15)
set.seed(1)  #reproducible results from function sample
for (i in 1:B) {
  resamples_index = sample(1:n, n, replace=TRUE) #takes a sample of size n from 1:n with replacement)
  sample_cor[i]=cor(lsat[resamples_index],gpa[resamples_index])
}
mu=mean(sample_cor)
stddev=sd(sample_cor)
mu
stddev
q=quantile(sample_cor, c(0.025,0.975))
q
hist(sample_cor)
mu*2-q[2] #CI lower bound
mu*2-q[1] #CI upper bound
#the above CI comes from bootstrap CI = [2*mean-q_(1-alpha/2),2*mean-q_(alpha/2)]

#use the parametric bootstrapping to estimate the sd of the correlation coeff.
# Assume that (LSAT;GPA) has bivariate normal distribution and estimate the 5 parameters
#Then generate 1000 samples of 15 pairs from this bivariate normal distribution.
#install.packages("MASS")
library(MASS)
mydata=as.data.frame(cbind(lsat,gpa))
mean_est = colMeans(mydata)
mean_est
var_est = cov(mydata)
var_est
sample_cor_2=matrix(0,nrow=B,ncol=1)
for (i in 1:B) {
  sample_matrix <- mvrnorm(n, mu = mean_est, Sigma = var_est ) 
  sample_cor_2[i]=cor(sample_matrix[,1],sample_matrix[,2])
}
hist(sample_cor_2)
mean(sample_cor_2)
sd(sample_cor_2)





#### Now let's get back to the SVM example of last class

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

