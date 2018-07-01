# -------------------- Code for Question 4.2 -----------------------------
# Clear environment

rm(list = ls())

# ---------------------------- Data manipulation -------------------------------------

data <- read.table("iris.txt", header = TRUE)
data <- data[,2:6] # shift data columns to remove column 1 (sequence number)

#
# optional check to make sure the data is read correctly
#

head(data)

# Console output for head(data)
##Num Sepal.Length Sepal.Width Petal.Length Petal.Width Species
##1   1          5.1         3.5          1.4         0.2  setosa
##2   2          4.9         3.0          1.4         0.2  setosa
##3   3          4.7         3.2          1.3         0.2  setosa
##4   4          4.6         3.1          1.5         0.2  setosa
##5   5          5.0         3.6          1.4         0.2  setosa
##6   6          5.4         3.9          1.7         0.4  setosa
# NOTE: ALL ROWS OF THIS FILE STARTING WITH "##" DENOTE R OUTPUT
#

#Let us see how many of each plant species we have for reference in k-means clustering results

table(data[,5], data$Species)

#Output from table(data[,5], data$Species)
##             setosa   versicolor virginica
##setosa         50          0         0
##versicolor      0         50         0
##virginica       0          0        50


# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)


# -------------------------- Visualizing Data -------------------------------------

#We can see that Petal.Length and Petal.Width are similar among the same species but 
#vary considerably between different species, as demonstrated below:
#library ggplot2 provides a powerful model of graphics that makes it easy to produce complex multi-layered graphics.
#This part is NOT NECESSARY for the homework submission and is just for learning purposes

library(ggplot2)
ggplot(data, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

#On the other hand we can see that Sepal.Length and Sepal.Width are not as easily divisible 
#between species (setosa is clearly separate from versicolor and virginica, but versicolor and virginica cross over quite a bit)

ggplot(data, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()


# -------------------------- Performing the kmeans clustering -----------------------

# -------------------------- Using all attributes (Septal Length, Septal Width,Petal Length, and Petal Width) -----------------------------------

#Since we know that there are 3 species involved, 
# we expect k = 3 to be a good cluster number, but we will look at k from 2 to 5 to see how more clusters affect clustering the points, 
#and since the starting assignments are random, we specify nstart = 20. This means that R will try 
#20 different random starting assignments and then select the one with the lowest within cluster variation.

# Also note that the default algorithm for kmeans() is a little different from what we saw in the lessons -- 
# it's the Hartigan-Wong algorithm.  In the lessons, we saw what's called Lloyd's algorithm.
# It turns out there's not much difference between the two algorithms' results on this data,
# so we report just the default.

irisClusterALL2 <- kmeans(data[,1:4], 2, nstart = 20)
irisClusterALL3 <- kmeans(data[,1:4], 3, nstart = 20)
irisClusterALL4 <- kmeans(data[,1:4], 4, nstart = 20)
irisClusterALL5 <- kmeans(data[,1:4], 5, nstart = 20)

# This is how we'd get Lloyd's algorithm:
# irisClusterALL2lloyd <- kmeans(iris[,1:4], 2, algorithm="Lloyd", nstart = 20)

# This is with unscaled data.  It's usually good practice to
# scale the data:

scdata <- data # initialize value/size of sdata
for (i in 1:4) { scdata[,i] <- (data[,i]-min(data[,i]))/(max(data[,i])-min(data[,i])) }

# Now we can run the clustering algorithm on the scaled data:

irisClusterALLsc2 <- kmeans(scdata[,1:4], 2, nstart = 20)
irisClusterALLsc3 <- kmeans(scdata[,1:4], 3, nstart = 20)
irisClusterALLsc4 <- kmeans(scdata[,1:4], 4, nstart = 20)
irisClusterALLsc5 <- kmeans(scdata[,1:4], 5, nstart = 20)

# Let's see what the total distance is between data points and cluster centers:

# initialize distance to zero

csum = 0

# for each data point...

for (i in 1:nrow(data)) {

# ...add the distance between its point and its cluster center

    csum = csum + dist(rbind(data[i,1:4],irisClusterALL2$centers[irisClusterALL2$cluster[i],]))
}

# report the total

csum[1]

#Now, let's compare the clusters with the species.
#We can only do this because we happen to know the species of each data point.
# Normally, this information wouldn't be available.

table(irisClusterALL2$cluster, data$Species)
table(irisClusterALLsc2$cluster, data$Species)
table(irisClusterALL3$cluster, data$Species)
table(irisClusterALLsc3$cluster, data$Species)
table(irisClusterALL4$cluster, data$Species)
table(irisClusterALLsc4$cluster, data$Species)
table(irisClusterALL5$cluster, data$Species)
table(irisClusterALLsc5$cluster, data$Species)


# -------------------------- Using Petal Length and Petal Width ---------------------

#Because of the graphical analysis above, let's try just using petal length and petal width

irisClusterPET2 <- kmeans(data[,3:4], 2, nstart = 20)
irisClusterPET3 <- kmeans(data[,3:4], 3, nstart = 20)
irisClusterPET4 <- kmeans(data[,3:4], 4, nstart = 20)
irisClusterPET5 <- kmeans(data[,3:4], 5, nstart = 20)

irisClusterPETsc2 <- kmeans(scdata[,3:4], 2, nstart = 20)
irisClusterPETsc3 <- kmeans(scdata[,3:4], 3, nstart = 20)
irisClusterPETsc4 <- kmeans(scdata[,3:4], 4, nstart = 20)
irisClusterPETsc5 <- kmeans(scdata[,3:4], 5, nstart = 20)

#Let us compare the clusters with the species.
#We can only do this because we happen to know the species of each data point.
# Normally, this information wouldn't be available.

table(irisClusterPET2$cluster, data$Species)
table(irisClusterPETsc2$cluster, data$Species)
table(irisClusterPET3$cluster, data$Species)
table(irisClusterPETsc3$cluster, data$Species)
table(irisClusterPET4$cluster, data$Species)
table(irisClusterPETsc4$cluster, data$Species)
table(irisClusterPET5$cluster, data$Species)
table(irisClusterPETsc5$cluster, data$Species)

#We can see the cluster centroids, the clusters that each data point was assigned to, and the within cluster variation.
# Here's an example using a 3-cluster solution with just the petal factors and scaled data:

irisClusterPETsc3

## K-means clustering with 3 clusters of sizes 50, 52, 48
## 
## Cluster means:
##   Petal.Length Petal.Width
## 1   0.07830508  0.06083333
## 2   0.55867014  0.51041667
## 3   0.77401130  0.81510417
## 
## Clustering vector:
##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
##  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 
##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
##  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
##   1   1   1   1   1   1   1   1   1   1   2   2   2   2   2   2   2   2   2   2 
##  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80 
##   2   2   2   2   2   2   2   2   2   2   3   2   2   2   2   2   2   3   2   2 
##  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 
##   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2 
## 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 
##   3   3   3   3   3   3   2   3   3   3   3   3   3   3   3   3   3   3   3   2 
## 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 
##   3   3   3   3   3   3   3   3   3   3   3   3   3   2   2   3   3   3   3   3 
## 141 142 143 144 145 146 147 148 149 150 
##   3   3   3   3   3   3   3   3   3   3 
## 
## Within cluster sum of squares by cluster:
## [1] 0.1369325 0.6791299 0.8858123
##  (between_SS / total_SS =  94.0 %)
## 
## Available components:
## 
## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
## [6] "betweenss"    "size"    

# plot solution

ggplot(iris, aes(Petal.Length, Petal.Width, color = irisClusterPETsc3$cluster)) + geom_point()



