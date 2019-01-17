# --------------------------------------------------------------------------------
#
# Comprehension Check - Linear regression for prediction - Question 2
#
# --------------------------------------------------------------------------------

# Setup 
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)
library(caret)
library(e1071)

# Now we will repeat the above but using larger datasets. Repeat the previous
# exercise but for datasets with n <- c(100, 500, 1000, 5000, 10000). Save the
# average and standard deviation of RMSE from the 100 repetitions using a seed
# of 1. Hint: use the sapply or map functions.

# Establish the data set
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>% 
     data.frame() %>% setNames(c("x", "y"))

# Replication
set.seed(1) 
results <- replicate(100, expr = {
     
     # Partition the dataset into test and training sets of equal size
     train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
     train_set <- dat[-train_index,]
     test_set <- dat[train_index,] 
     
     # Train the model
     model <- lm(y ~ x, data = train_set)
     model_prediction <- predict(model, test_set) 
     
     # Calculate the RMSE
     model_result <- test_set$y - model_prediction
     model_rmse <- sqrt(mean(model_result^2))
})
mean(results) # two decimal places are sufficient
# [1] 2.488661
sd(results)   # for a correct answer three decimal places are necessary
# [1] 0.1243952



