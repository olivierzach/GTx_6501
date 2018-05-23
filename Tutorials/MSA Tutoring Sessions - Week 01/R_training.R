#ISyE6501x GT Introduction to Analytics Modeling
#R training session
#Date: 05/24/2017

#How to install packages?
install.packages("kknn")

#How to load packages?
library(kknn)

#How to check working directory
getwd()

#How to set working directory?
#This is the directory where you may want to save your data. 
#Any files you export will also be saved in this directory by default
setwd("/Users/Krishna/Fractal stuff/PhD/Courses/Summer 2017/TA course")

#How to load data
data<-read.table("credit_card_data-headers.txt",header = T,sep='\t')
data<-read.csv("credit_card_data.csv")

#Finding number of rows and columns in data
nrow(data) #Rows
ncol(data) #Columns

#Removing unnecessary columns in data
data<-data[,-1]

#Removing variable by name
data<-subset(data, select = -c(A3,A8))

#Exporting data as csv
write.csv(data,"credit_card_data_v2.csv")

#Defining data structures

#A variable
var<-5

#A vector
vec<-c(1:5)
vec<-c("a","b","z")

#A matrix
mat<-matrix(1:6,nrow=2,ncol=3)
mat<-matrix(c("a","b","c","d","e","f"),2,3)
#Converting data frame into matrix
mat<-as.matrix(data)


#Example of model building

#Clear environment
rm(list=ls())

#Setting seed to produce reproducible results
set.seed(1)

#Taking (2/3)rd data as training and remaing for testing

m <- nrow(data) #Number of rows in data

#Randomly selecting (1/3)rd indexes among 654 indexes
val <- sample(1:m, size = round(m/3), replace = FALSE)
               
d.learn <- data[-val,] #Training data
d.valid <- data[val,]  #Test data

#Using leave one our cross-validation to find the optimal value of 'k'
fit<-train.kknn(R1 ~ ., data = d.learn, kmax = 100, kernel = "rectangular")

fit
#Value of 'k' with minimum missclassification error rate is 38.

#Plotting mis-classfication error rate for different values of 'k'
plot(fit)
title("Cross validation")

#Testing the model on test data
pred<-predict(fit, d.valid)
pred_bin<-round(pred)

#Finding the prediction accuracy
pred_accuracy<-table(pred_bin,d.valid$R1)

#Prediction accuracy = 88%

#Loops

#For loop

#Printing prime numbers less than 50
for(i in 2:50)
{
  if(min(i%%(2:(i-1)))!=0 || i==2)  #Checking if i is prime or not
  {
    print(i)   #Printing i if it is prime
  }
}

#While loop

#Printing first 50 prime numbers

i<-2
counter<-0
while(counter<=50)
{
  if(min(i%%(2:(i-1)))!=0 || i==2)  #Checking if i is prime or not
  {
    print(i)   #Printing i if it is prime
    counter<-counter+1
  }
  i<-i+1
}


#Plotting?
plot(data) #Plotting all possible combination of columns of data against each other
plot(x,y)  #Plotting x against y


