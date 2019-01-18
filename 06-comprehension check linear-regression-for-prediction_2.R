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

n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
sapply(n, rmse)

# rmse function
rmse <- function(n) {
     Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
     dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>% 
          data.frame() %>% setNames(c("x", "y"))
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
     print(mean(results))
     print(sd(results))
}
# [1] 2.497754
# [1] 0.1180821
# [1] 2.720951
# [1] 0.08002108
# [1] 2.555545
# [1] 0.04560258
# [1] 2.624828
# [1] 0.02309673
# [1] 2.618442
# [1] 0.01689205     



