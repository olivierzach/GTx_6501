
# isye6501 homework 2 -------------------------------------------------------------------------

# reference code for homework 2
# solutions presented in the pdf document


# workflow ------------------------------------------------------------------------------------

# set up directory and packages
setwd('~/Desktop/GTX/Homework 1/')

# load packages
pacman::p_load(tidyverse, kernlab, caret, kknn, modelr, ggthemes, corrplot)


# question 4.2 --------------------------------------------------------------------------------


data("iris") 

# load the iris dataset
iris_df <- iris %>% as_tibble()

# investigate dataset
str(iris_df)

# correlation plot - maybe we should eliminate correlated value?
corrplot(cor(iris_df[,-5]), type = 'upper', diag = F)

# plot results - EDA
ggplot(data = iris_df %>% gather(key, value, -Species), aes(x = key, y = value, color = Species)) +
        geom_boxplot() +
        geom_jitter(alpha = .3, pch = 21) +
        theme_few() +
        xlab('Measurement') +
        ylab('Inches') +
        labs(title = 'Iris Dataset Visualization',
             subtitle = 'Species shown in color by measurements')

# quick look at correlation #1
ggplot(data = iris_df) +
        geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        theme_few() +
        xlab('Measurement') +
        labs(title = 'Sepal Width vs. Sepal Length',
             subtitle = 'Attributes grouped by Species') +
        geom_smooth(data = iris_df, aes(x = Sepal.Length, y = Sepal.Width),
                    method = 'lm', se = F, color = 'dark grey')

# quick look at correlation #2
ggplot(data = iris_df) +
        geom_point(aes(x = Petal.Length, y = Petal.Width, color = Species)) +
        theme_few() +
        xlab('Measurement') +
        labs(title = 'Petal Width vs. Petal Length',
             subtitle = 'Attributes grouped by Species') +
        geom_smooth(data = iris_df, aes(x = Petal.Length, y = Petal.Width),
                    method = 'lm', se = F, color = 'dark grey')



# modeling data frame - scale our inputs!
iris_mod <- iris_df %>% 
        dplyr::select(Sepal.Length, Petal.Width, Petal.Length) %>% 
        scale(.) %>% 
        as_tibble()




# set up kmeans - will iterate through optimal K by using within cluster distance
possible_K <- as.list(seq(from = 1, to = 15, by = 1))

# blank list to store for loop values
within_dist <- list()

for (i in seq_along(possible_K)) {
        
        k = possible_K[[i]]
        
        kmean_fit <- kmeans(
                x = iris_mod,
                centers = k,
                nstart = 20
        )
        
        within_dist[[i]] = data.frame(between = kmean_fit$betweenss,
                                      total = kmean_fit$totss,
                                      total.within = kmean_fit$tot.withinss)
}

(distance_df <- data.frame(
        ss = reduce(within_dist, rbind),
        possible_K = reduce(possible_K, rbind)
) %>% 
                mutate(ratio = (ss.between / ss.total)) %>% 
                ggplot(aes(x = possible_K, y = ss.total.within)) +
                geom_line(color = 'dark grey') +
                geom_point(color = 'black') +
                theme_few() +
                ylab('Total Sum of Squared Distances') +
                xlab('Possible K Values') +
                labs(title = 'Iris K-Means Clustering: K Optimization',
                     subtitle = 'Total Within Sum of Squared Distance by possible K values') +
                geom_vline(xintercept = 3, linetype = 'dotted')
)


# fit model with our best k
(kmeans_bestfit <- kmeans(
        iris_mod,
        centers = 3, 
        nstart = 100
))

# quick look at the model's clusters
kmeans_bestfit$cluster

# quick look at the model's final centroids - we can draw inference from here but need to unscale
kmeans_bestfit$centers %>% as_tibble()

# check accuracy against the original data
table(kmeans_bestfit$cluster, iris_df$Species)



# question 5.1 --------------------------------------------------------------------------------

# https://www.r-bloggers.com/measures-of-skewness-and-kurtosis
# https://brownmath.com/stat/shape.htm#KurtosisVisualize
# https://cran.r-project.org/web/packages/outliers/outliers.pdf

# load outliers library
library(outliers); library(scales); library(moments)

# read in the crime data
crime = read_delim('5.1uscrimeSummer2018.txt', delim = '\t') %>% 
        as_tibble()

# df for outliers in the last column
crime_df = crime %>% 
        dplyr::select(Crime)

# stats summary of the last column
summary(crime_df)

# skewness - measure of the asymmetry of the probability distribution around the mean
skew_crime <- skewness(crime_df)

# kurtosis - measures the 'tailed-ness' of a random variable
kurtosis_crime <- kurtosis(crime_df)

# outliers detection with outliers package - gives the most extreme value from the mean
outlier_crime <- outlier(crime_df)


# density plot to visualize outliers
ggplot(data = crime_df, aes(x = crime_df)) +
        geom_density(stat="density", fill = 'light grey') +
        theme_few() +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = percent) +
        xlab('Crime Counts per 100K people') +
        ylab('Density') +
        labs(title = 'Density Plot of Crime Data',
             subtitle = 'Searching for outliers on tail extremes') +
        geom_vline(
                xintercept = quantile(crime_df$Crime, .25),
                linetype = 'solid', color = 'dark orange'
        ) + # 1st quartile
        geom_vline(
                xintercept = median(crime_df$Crime),
                linetype = 'dotted', color = 'dark orange'
        ) + # median
        geom_vline(
                xintercept = quantile(crime_df$Crime, .75),
                linetype = 'solid', color = 'dark orange'
        ) # 3rd quartile



# show initial results
print(paste0('Skewness: ', skew_crime))
print(paste0('Kurtosis: ', kurtosis_crime))
print(paste0('Most Extreme Value: ', outlier_crime))


(grubbs_crime <- grubbs.test(
        crime_df$Crime, # data to test
        type = 10, # type will detect is the data contains one outlier
        two.sided = F # test will only detect outlier on one tail
)
)




# question 6.2 --------------------------------------------------------------------------------

# load libraries
library(lubridate); library(qcc); library(changepoint); library(bda)

#------------------------------------------------------------------------------------------------
## EDA

# read in the temp data
temps_df = read_delim('6.2tempsSummer2018.txt', delim = '\t') %>% 
        as_tibble() %>% 
        gather(year, temp, -DAY) %>% 
        mutate(year = as.factor(year),
               date = paste(DAY,year, sep = '-')) %>% 
        mutate(date_val = dmy(date),
               color = ifelse(temp > mean(.$temp), 'Above', 'Below'),
               month = month(date_val),
               day = day(date_val)) %>% 
        dplyr::select(date_val, DAY, year, temp,color, month, day)

# visualize trends - warmer over time?
ggplot(data = temps_df, aes(x = year, y = temp)) +
        geom_jitter(pch = 21, alpha = .2, color = 'dark orange') +
        geom_boxplot(color = 'dark blue') +
        theme_few() +
        theme(legend.position = 'none') +
        geom_hline(yintercept = mean(temps_df$temp), linetype = 'dotted') +
        xlab('') +
        ylab('Temperature') +
        labs(title = 'Daily Temperature by Year',
             subtitle = 'Summer Temperatures 1996-2015')


# visualize trends - when does whether start to cool off?
ggplot(data = temps_df, aes(x = as.factor(month), y = temp)) +
        geom_jitter(pch = 21, alpha = .2, color = 'dark orange') +
        geom_boxplot(color = 'dark blue') +
        theme_few() +
        theme(legend.position = 'none') +
        geom_hline(yintercept = mean(temps_df$temp), linetype = 'dotted') +
        xlab('Month') +
        ylab('Temperature') +
        labs(title = 'Daily Temperature by Month',
             subtitle = 'Sample Data from 1996-2015')

#------------------------------------------------------------------------------------------------
## Cusum Model

# setup cusum algorithm for changes over time
temps_model_df <- read_delim('6.2tempsSummer2018.txt', delim = '\t') %>% 
        as_data_frame() %>% 
        gather(year, temp, -DAY) %>% 
        mutate(year = as.factor(year),
               date = paste(DAY,year, sep = '-')) %>% 
        mutate(date_val = dmy(date),
               color = ifelse(temp > mean(.$temp), 'Above', 'Below'),
               month = month(date_val),
               day = day(date_val)) %>% 
        dplyr::select(date_val, DAY, year, temp,color, month, day)

# grab only the summer dates for the cusum model
summer_df = temps_model_df %>% 
        filter(month %in% c(as.Date(7), as.Date(8)))

# determine baseline mean and sd metrics in the summer months only
summer_mean = mean(summer_df$temp)
summer_sd = sd(summer_df$temp)

# list of years to loop through
years = as.list(unique(as.character(temps_model_df$year)))

# empty list to store values of the for loop into
store_days <- list()

# cusum for loop
for (i in seq_along(years)) {
        
        # take a year subset
        year_index <- years[[i]]
        
        df <- temps_model_df %>% 
                filter(as.character(year) == year_index) %>% 
                dplyr::select(temp)
        
        # fit a cusum model to that year
        qsum <- qcc::cusum(
                data = df$temp,
                centervalue = summer_mean, 
                std.dev = summer_sd,
                se.shift = 1.96,
                plot = F
        )
        
        # extract the first day that starts at least 4 consecutive days of temperature flagged by cusum model
        qsum_results <- qsum$neg %>%
                as_tibble() %>%
                rownames_to_column() %>% 
                cbind(date = temps_model_df$DAY) %>% 
                mutate( # current cusum value times the next and the fourth value cannot be 0!
                        consecutive = value * lead(value,1) * lead(value, 4) == 0
                ) %>% 
                filter(consecutive == F) %>% 
                .[1,] %>% 
                cbind(year_index) %>% 
                dplyr::select(., -consecutive)
        
        # store the first day of a string of flagged temperatures into a list
        store_days[[i]] = qsum_results
        
}

# reduce the list of stored temperatures and format into a readable format
end_summer <- reduce(store_days, rbind) %>% 
        mutate(date = paste(date,year_index, sep = '-')) %>% 
        mutate(date_val = dmy(date)) %>% 
        dplyr::select(date_val, 'cusum_val' = value) %>% 
        mutate(year_date = year(date_val)) %>% 
        dplyr::select(year_date, date_val, cusum_val) %>% 
        mutate(month_val = month(date_val),
               day_val = day(date_val))

# find the earliest end of summer
earliest_end <- end_summer %>% 
        filter(month_val == min(month_val))

# find the latest end of summer
latest_end <- end_summer %>% 
        filter(month_val == max(month_val))

# print outputs
print(paste('Earliest Summer End: ', earliest_end$date_val))
print(paste('Latest Summer End: ', latest_end$date_val))
end_summer



# setup cusum algorithm for changes over time
temps_df2 <- read_delim('6.2tempsSummer2018.txt', delim = '\t') %>% 
        as_data_frame() %>% 
        gather(year, temp, -DAY) %>% 
        mutate(year = as.factor(year),
               date = paste(DAY,year, sep = '-')) %>% 
        mutate(date_val = dmy(date),
               month = month(date_val),
               day = day(date_val)) %>% 
        filter(month %in% c(7, 8)) %>% 
        dplyr::select(date_val, DAY, year, temp)  

# center value and std.dev
summer_center2 <- mean(temps_df2$temp)
summer_sd2 <- sd(temps_df2$temp)

# print outputs
print(paste('Overall Summer  Mean: ', summer_center2))
print(paste('Overall Summer Standard Deviation: ', summer_sd2))


# plot the cusum chart for all summer days
qsum_chart <- qcc::cusum(
        data = temps_df2$temp,
        centervalue = summer_center2, 
        std.dev = summer_sd2,
        se.shift = 1.96,
        plot = T
)



# extract the cusum results to see exactly when summers get hotter than normal
qsum_results <- qcc::cusum(
        data = temps_df2$temp,
        centervalue = summer_center2, 
        std.dev = summer_sd2 ,
        se.shift = 1.96,
        plot = F
)

qsum_positive <- qsum_results$pos %>%
        as_tibble() %>% 
        rownames_to_column() %>% 
        cbind(date = temps_df2$date_val) %>% 
        left_join(., temps_df2, by = c('date' = 'date_val')) %>% 
        dplyr::select(date, value, temp) %>% 
        filter(value != 0) %>% 
        arrange(date)

highest_cusum <- qsum_positive %>% filter(value == max(value)) %>% 
        dplyr::select(date)

print(paste('Date with Highest Cusum: ', as.character(highest_cusum$date)))

# look at the actual temps in August 2007 - 18 days above 95 degrees!
(summer_2018 <- qsum_positive %>% 
                filter(month(date) %in% as.Date(8),
                       year(date) %in% as.Date(2007))
)




