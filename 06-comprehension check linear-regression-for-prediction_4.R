# --------------------------------------------------------------------------------
#
# Comprehension Check - Linear regression for prediction - Question 4
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

# Now repeat the exercise from Q1, this time making the correlation between x
# and y larger, as in the following code

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
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
# [1] 2.488661 (for c(1.0, 0.5, 0.5, 1.0))
# [1] 0.9099808 (for c(1.0, 0.95, 0.95, 1.0))
sd(results)   # for a correct answer three decimal places are necessary
# [1] 0.1243952 (for c(1.0, 0.5, 0.5, 1.0))
# [1] 0.06244347 (for c(1.0, 0.95, 0.95, 1.0))



