# ------------------------ Code for Question 6.2.1 -------------------------------------

# Clear environment
rm(list = ls())

# Installing and calling packages
install.packages(qcc)
library(qcc)

# Reading the data
data <- read.table("temps.txt", stringsAsFactors = FALSE, header = TRUE)

# optional check to make sure the data is read correctly
head(data)

# DAY X1996 X1997 X1998 X1999 X2000 X2001 X2002 X2003 X2004 X2005 X2006 X2007 X2008 X2009
# 1 1-Jul    98    86    91    84    89    84    90    73    82    91    93    95    85    95
# 2 2-Jul    97    90    88    82    91    87    90    81    81    89    93    85    87    90
# 3 3-Jul    97    93    91    87    93    87    87    87    86    86    93    82    91    89
# 4 4-Jul    90    91    91    88    95    84    89    86    88    86    91    86    90    91
# 5 5-Jul    89    84    91    90    96    86    93    80    90    89    90    88    88    80
# 6 6-Jul    93    84    89    91    96    87    93    84    90    82    81    87    82    87

# Create matrix without day column (for easier processing and referencing)
temps = matrix(data[,2:ncol(data)])

# Create a vector of this data
temps_vec <- as.vector(unlist(temps))

# Turn the vector into a time series object and view plot
myts <- ts(temps_vec,start=1996,frequency=1)
plot(myts)

# Determine the summer mean and standard deviation value we want to use
# We just used the month of July as a base average summer temperature with no change
avg_summer <- rep(0,nrow(temps))
sd_summer <- rep(0,nrow(temps))
for (i in 1:nrow(temps)){
  avg_summer[i] <- mean(temps[[i]][1:30])
  sd_summer[i] <- sd(temps[[i]][1:30])
}

# Use the cusum function
# We can create a cusum chart for each year and note when the temperautes changes from summer to fall
# We can use the se.shift parameter to indicate the threshold in terms of the std.dev parameter
# We can use the decision.interval parameter to indicate the C value in terms of std.dev parameter 

#Creates a data object/frame to store all our cusum models together for easy reference
CUSUMmodels <- vector(mode="list", length=nrow(temps)) 
CUSUMviolations <- vector(mode="list", length=nrow(temps)) 

# Loop through each year and run the CUSUM function
# center is the "target" summer temperature each year
# std.dev is the standard deviation of the summer temperature each year
# We set decision.interval as the upper and lower bound in standard deviations/errors from 0 change
# We set se.shift as the amount of shift to detect (which corresponds to 2*C in terms of standard deviations/errors)
DI = 5 #out of control when past 5 standard deviations/errors
SS = 6 #equals 6/2 = 3 standard deviations/errors shift
for (i in 1:nrow(temps)){
  CUSUMmodels[[i]] <- cusum(temps[[i]], center=avg_summer[i], std.dev = sd_summer[i], decision.interval=DI, se.shift=SS, plot = TRUE)
  CUSUMviolations[[i]] <- CUSUMmodels[[i]]$violations$lower
}

#We are concerned with the lower violations since we want to catch significant drops in temperature that indicate transfer from summer to fall
#For simplicity, I'll take the minimum of each list of lower violations
CUSUMlowest <- rep(0, nrow(temps))
CUSUMlowesttemp <- rep(0, nrow(temps))
for (i in 1:nrow(temps)){
  CUSUMlowest[i] <- min(CUSUMviolations[[i]])
  CUSUMlowesttemp[i] <- as.integer(mean(temps[[i]][1:CUSUMlowest[i]]))
}

#CUSUMlowest and CUSUMlowesttemp store the index of the lower violation in the CUSUM analysis for each year
#and the average temperatures up to that day associated with that index.
#These two vectors contain the unofficial end of summer day number and summer climate (average summer temperature up to the day of change)

#From the plot, we can see an expected trend of a decrease in temperature as the day number get larger
#I.e. as we get farther into fall, the average temperature tends to drop
plot(CUSUMlowest,CUSUMlowesttemp)


# ------------------------ Code for Question 6.2.2 -------------------------------------
#Now we take the unofficial end of summer day and temperature data and use CUSUM analysis again to
#determine if the unofficial end of summer days and summer climate are changing

# Determine the mean and standard deviation value we want to use
# We just used the averages of the CUSUMlowest and CUSUMlowesttemp
avg_day <- mean(CUSUMlowest)
sd_day <- sd(CUSUMlowest)
avg_temp <- mean(CUSUMlowesttemp)
sd_temp <- sd(CUSUMlowesttemp)

# Run the CUSUM function on unofficial end of summer day
DI = 1 #out of control when past 1 standard deviations/errors
SS = 1 #equals 1/2 = 1/2 standard deviations/errors shift
#We are being more sensitive to changes in days since the standard deviation is almost 2 weeks
CUSUMmodel_day <- cusum(CUSUMlowest, center=avg_day, std.dev = sd_day, decision.interval=DI, se.shift=SS, plot = TRUE)
CUSUMviolations_day <- CUSUMmodel_day$violations

#There are a few years where the unofficial end of summer changed significantly from the mean day

# Run the CUSUM function on unofficial end of summer day average summer temperature (summer climate)
DI = 3 #out of control when past 3 standard deviations/errors = about 6 degrees
SS = 1 #equals 1/2 = 1/2 standard deviations/errors shift = about 1 degree
#We are being less sensitive to changes in temperature since the standard deviation is 2 degrees
CUSUMmodel_temp <- cusum(CUSUMlowesttemp, center=avg_temp, std.dev = sd_temp, decision.interval=DI, se.shift=SS, plot = TRUE)
CUSUMviolations_temp <- CUSUMmodel_temp$violations

#There are a few years where the summer climate drops significantly from the mean summer climate
#These years align with the years where the unofficial end of summer day also change significantly (years 16 and 17)
