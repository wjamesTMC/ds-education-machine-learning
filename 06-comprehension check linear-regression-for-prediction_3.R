# --------------------------------------------------------------------------------
#
# Comprehension Check - Linear regression for prediction - Question 3
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

# What happens to the RMSE as the size of the dataset becomes larger?

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
     sd(results)
}
# print(rmse)

n <- c(100, 500, 1000, 5000, 10000, 20000, 4000, 60000, 80000, 100000, 120000, 160000)
set.seed(1)
sapply(n, rmse)

# [1] 2.497754
# [1] 2.720951
# [1] 2.555545
# [1] 2.624828
# [1] 2.618442
# [1] 2.576645
# [1] 2.621275
# [1] 2.600002
# [1] 2.591228
# [1] 2.592776
# [1] 2.590052
# [1] 2.608187    

# Answer: On average, the RMSE does not change much as n gets larger, but the
# variability of the RMSE decreases

