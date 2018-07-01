# -------------------- Code for Question 12.2 -----------------------------
# Clear environment

rm(list = ls())

# Call the library 'FrF2'

library(FrF2)

# Set seed = 1 to produce reproducible result

set.seed(1)

#Generate a 16 run, 10 factor, 2 level fractional factorial design

design<-FrF2(nruns = 16,nfactors = 10)

design

##     A  B  C  D  E  F  G  H  J  K
## 1  -1 -1  1 -1  1 -1 -1  1  1 -1
## 2   1 -1  1 -1 -1  1 -1 -1  1  1
## 3  -1 -1 -1  1  1  1  1 -1  1 -1
## 4   1  1 -1  1  1 -1 -1  1 -1 -1
## 5  -1  1 -1 -1 -1  1 -1  1  1 -1
## 6   1 -1 -1  1 -1 -1  1  1  1  1
## 7  -1  1 -1  1 -1  1 -1 -1 -1  1
## 8  -1  1  1  1 -1 -1  1 -1  1 -1
## 9   1 -1  1  1 -1  1 -1  1 -1 -1
## 10 -1 -1 -1 -1  1  1  1  1 -1  1
## 11  1 -1 -1 -1 -1 -1  1 -1 -1 -1
## 12 -1  1  1 -1 -1 -1  1  1 -1  1
## 13 -1 -1  1  1  1 -1 -1 -1 -1  1
## 14  1  1  1 -1  1  1  1 -1 -1 -1
## 15  1  1 -1 -1  1 -1 -1 -1  1  1
## 16  1  1  1  1  1  1  1  1  1  1
## class=design, type= FrF2 
