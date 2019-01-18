# --------------------------------------------------------------------------------
#
# Comprehension Check - Linear regression for prediction - Question 5
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

# Which of the following best explains why the RMSE in question 4 is so much
# lower than the RMSE in question 1?

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
# [1] 1.888025 (for c(1.0, 0.75, 0.75, 1.0))
# [1] 0.9099808 (for c(1.0, 0.95, 0.95, 1.0))
# [1] 0.4097947 (for c(1.0, 0.99, 0.99, 1.0))
sd(results)   # for a correct answer three decimal places are necessary
# [1] 0.1243952 (for c(1.0, 0.5, 0.5, 1.0))
# [1] 0.1041058 (for c(1.0, 0.5, 0.5, 1.0))
# [1] 0.06244347 (for c(1.0, 0.95, 0.95, 1.0))
# [1] 0.02782272 (for c(1.0, 0.99, 0.99, 1.0))

# Answer: When we increase the correlation between x and y, x has more
# predictive power and thus provides a better estimate of y

