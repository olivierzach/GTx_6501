
# Homework #1 ISYE6501 ------------------------------------------------------------------------


# code from the first homework is below
# document is meant to supplement pdf report
# please refer to the pdf report for offical answers, descriptions, and commentary
# this document is provided to make replicating code easier for homework grader
# http://html2pdf.com


# workflow ------------------------------------------------------------------------------------

# set up directory and packages
setwd('~/Desktop/GTX/Homework 1/')

# load packages
pacman::p_load(tidyverse, kernlab, caret, kknn, modelr)



# get data ------------------------------------------------------------------------------------

# get data and format
credit_df <- read.table('2.2credit_card_data-headersSummer2018.txt',header = T) %>% 
        as_tibble() %>% 
        dplyr::rename(., 'response_y' = R1)

# data summary
str(credit_df)

# get data and format
credit_df <- read.table('2.2credit_card_data-headersSummer2018.txt',header = T) %>% 
        as_tibble() %>% 
        dplyr::rename(., 'response_y' = R1)

# data summary
str(credit_df)




# linear svm ----------------------------------------------------------------------------------

# set seed 
set.seed(2)

# data frame of model parameters
model_param <- data.frame(
        type = 'C-svc',
        kernal = 'vanilladot',
        cross = 5,
        scaled = T, 
        stringsAsFactors = F
)


# set up a list of possible C values: choose C from .5 to 100 by .05
# - will build a model for each C and determine the 'best' C based on accuracy 
cost_list <- as.list(seq(from = 0.5, to = 100, by = .05))

# set up a list to put the error metrics of each model into
error_list <- list()


# cost loop - for each value of cost - fit a model - extract accuracy - put into error list
for (i in seq_along(cost_list)) {
        
        c <- cost_list[[i]]
        
        set.seed(2)
        
        # fit ksvm model based on model parameters
        ksvm_fit <- ksvm(
                response_y ~ ., 
                data = credit_df, 
                scaled = model_param$scaled ,
                type = model_param$type,
                kernal = model_param$kernal,
                C = c,
                cross = model_param$cross
        )
        
        error_list[[i]] = ksvm_fit@error
}


# reduce list of errors and cost parameters into a column in a data frame
error_df <- reduce(error_list, rbind.data.frame) 
cost_df <- reduce(cost_list, rbind.data.frame) 

# performance measures - put list of errors and costs next to each other to find highest accuracy
performance_train <- cbind(error_df, cost_df) %>% 
        rename(
                'svm_error' = !!names(.[1]),
                'svm_cost' =  !!names(.[2])
        ) %>% 
        mutate(svm_accuracy = 1-svm_error) %>% 
        filter(svm_error == min(svm_error)) %>% 
        arrange(., svm_cost) %>% 
        .[1,]


# fit model with our best C determined by training error
svm_bestfit <- ksvm_fit <- ksvm(
        response_y ~ ., 
        data = credit_df, 
        scaled = model_param$scaled ,
        type = model_param$type,
        kernal = model_param$kernal,
        C = performance_train$svm_cost,
        cross = model_param$cross
)

## extracting the model formula

# calculate a1...am the coefficients for each predictor!
svm_formula <- colSums(svm_bestfit@xmatrix[[1]] * svm_bestfit@coef[[1]]) %>%
        as.data.frame() %>% 
        rownames_to_column('predictor') %>% 
        rbind(., data.frame(predictor ='z.Intercept', . = svm_bestfit@b)) %>% 
        spread(., key = predictor, value = .) %>% 
        as_tibble()

# print results
performance_train
svm_bestfit
svm_formula



# flexible svm --------------------------------------------------------------------------------

# set up list of possible cost values
cost_list <- as.list(seq(from = 0.5, to = 200, by = .05))

# set up empty list to put error results for each cost into
error_list <- list()

# fit a radial svm for every value of cost...extract error to compare
for (i in seq_along(cost_list)) {
        
        c <- cost_list[[i]]
        
        set.seed(2)
        
        # fit ksvm model
        ksvm_fit <- ksvm(
                response_y ~ ., 
                data = credit_df, 
                scaled = T, 
                type = 'C-svc',
                kernal = 'rbfdot',
                C = c,
                cross = 10, 
                kpar = 'automatic')
        
        error_list[[i]] = ksvm_fit@error
}

# collapse lists to data frames
error_df <- reduce(error_list, rbind.data.frame) 
cost_df <- reduce(cost_list, rbind.data.frame) 

# find the optimal value of Cost - cost value with lowest error
performance_train <- cbind(error_df, cost_df) %>% 
        rename(
                'svm_error' = !!names(.[1]),
                'svm_cost' =  !!names(.[2])
        ) %>% 
        filter(svm_error == min(svm_error)) %>% 
        arrange(., svm_cost) %>% 
        .[1,]


# fit model with our best C determined by training error
svm_bestfit <- ksvm_fit <- ksvm(
        response_y ~ ., 
        data = credit_df, 
        scaled = T,
        type = 'C-svc',
        kernal = 'rbfdot',
        C = performance_train$svm_cost,
        cross = 10,
        kpar = 'automatic'
)


# look at the results of our model
svm_bestfit@error
svm_bestfit@cross



# knn first attempt ---------------------------------------------------------------------------

set.seed(2)

# set up train.kknn to find the optimal k using leave one out classification
(knn_fit_LOOCV <- train.kknn(
        response_y ~ ., 
        data = credit_df,
        # test = credit_df,
        kmax = 100,
        # kcv = 10,
        scale = T)
)

# apply optimal K to the original training dataset
knn_fit <- kknn(
        response_y ~ ., 
        train = credit_df,
        test = credit_df,
        k = 58,
        scale = T)

# accuracy measures
fitted <- fitted(knn_fit) %>%
        as_tibble() %>%
        mutate(value = ifelse(value > .5, 1, 0)) %>% # round to 1 if prob > .5, 0 otherwise
        cbind(., credit_df$response_y) %>% 
        mutate(acc = value == credit_df$response_y) # if prediction == actual then True, else False

# percetage accuracy of the model
(test_accuracy <- mean(fitted$acc))



# knn cross validation ------------------------------------------------------------------------

set.seed(4)

# from documentation - k fold cross validation needs kcv arguement
# kcv = Number of partitions for k-fold cross validation.


# find best value of K by cross validation - what is the estimated test accuracy?
knn_fit <- train.kknn(response_y ~ ., 
                      data = credit_df,
                      kmax = 60, # search K values up to 600
                      kcv = 10, # base accuarcy on 10-fold cross validation
                      scale = T)

summary(knn_fit)



# fit a model with less that the optimal K and compare accuracy
# K lower than our best fit should give inferior results
knn_fit2 <- train.kknn(response_y ~ ., 
                       data = credit_df,
                       kmax = 30,
                       kcv = 10,
                       scale = T)

# mean squared error        
summary(knn_fit2)


# fit a model with less that the optimal K and compare accuracy
# K lower than our best fit should give inferior results
knn_fit3 <- train.kknn(response_y ~ ., 
                       data = credit_df,
                       kmax = 20,
                       kcv = 10,
                       scale = T)

# mean squared error        
summary(knn_fit3)


# fit a model with less that the optimal K and compare accuracy
# K lower than our best fit should give inferior results
knn_fit4 <- train.kknn(response_y ~ ., 
                       data = credit_df,
                       kmax = 10,
                       kcv = 10,
                       scale = T)

# mean squared error        
summary(knn_fit4)



# knn train, test, validation -----------------------------------------------------------------

set.seed(2)

# develop the training and holdout partition
partition <- createDataPartition(credit_df$response_y, p = .6,
                                 list = F)
# develop the training set
train <- credit_df[partition,]

# develop the holdout set
holdout <- credit_df[-partition,]

# develop the split of the holdout data into test and validation
partition_valid <- createDataPartition(holdout$response_y, p = .5,
                                       list = F)
# split holdout into test
test <- holdout[partition_valid, ]

# split holdout in validation
validation <- holdout[-partition_valid, ]

# check splits
dim(train);dim(test);dim(validation)






# set up list of possible K values from 1 to 100 by 3
possible_k <- as.list(seq(from = 1, to = 100, by = 3))

# set up a blank list to put accuracy values into
test_accuracy <- list()

# fit a model for each possible value of K and extract the accuracy from each model
for (i in seq_along(possible_k)) {
        
        k = possible_k[[i]]
        
        knn_fit <- kknn(
                response_y ~ ., 
                train = train,
                test = test,
                # valid = validation$response_y,
                k = k,
                scale = T)
        
        fitted <- fitted(knn_fit) %>%
                as_tibble() %>%
                mutate(value = ifelse(value > .5, 1, 0)) %>% 
                cbind(., test$response_y) %>% 
                mutate(acc = value == test$response_y)
        
        test_accuracy[[i]] <- mean(fitted$acc)
        
}

# put the K and test accuracy lists into dataframes
k_df <- reduce(possible_k, rbind.data.frame) 
test_acc_df <- reduce(test_accuracy, rbind.data.frame)

# find the best K associated with the highest accuracy
(performance_test <- cbind(k_df, test_acc_df) %>% 
                rename(
                        'knn_error' = !!names(.[2]),
                        'knn_k' =  !!names(.[1])
                ) %>% 
                filter(knn_error == max(knn_error)) %>% 
                arrange(., knn_k) %>% 
                .[1,])

# fit the best model on the training + testing data - find accuracy on the validation set
knn_best_fit <-  kknn(
        response_y ~ ., 
        train = rbind(train, test), # combine train to be train + test datasets
        test = validation,
        k = performance_test$knn_k, # best value of K found from test set accuracy
        scale = T)

# view the validation set accuracy
best_fitted <- fitted(knn_best_fit) %>%
        as_tibble() %>%
        mutate(value = ifelse(value > .5, 1, 0)) %>% # kknn outputs a score for each value
        cbind(., validation$response_y) %>% 
        mutate(acc = value == validation$response_y)

# accuracy of our best fit knn model on the validation dataset
(valid_accuracy <- mean(best_fitted$acc))

