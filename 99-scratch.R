# --------------------------------------------------------------------------------
#
# Comprehension Check - Linear regression for prediction - Question 6
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

# Create a data set using the following code

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
     data.frame() %>% setNames(c("y", "x_1", "x_2"))

# x <- cor(dat)
# Note that y is correlated with both x_1 and x_2 but the two predictors are
# independent of each other, as seen by cor(dat).

# Use the caret package to partition into a test and training set of equal size.
# Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train a
# linear model for each.

# Which of the three models performs the best (has the lowest RMSE)?

# I did get right answer for Q7 after fixing an apparent typo in the instruction
# code. set.seed(1) n <- 1000 Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0,
# 0.25, 0.75, 0.25, 1.0), 3, 3) dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma)
# %>% <--- correction made here ... n = 1000

# -----------------------------------------------------------------
# x_1
# -----------------------------------------------------------------
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 1000, c(0, 0, 0), Sigma) %>%
     data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
results <- {
     
     # Partition the dataset into test and training sets of equal size
     train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
     train_set <- dat[-train_index,]
     test_set <- dat[train_index,] 
     
     # Train the model
     model <- lm(y ~ x_1, data = train_set)
     model_prediction <- predict(model, test_set) 
     
     # Calculate the RMSE
     model_result <- test_set$y - model_prediction
     model_rmse <- sqrt(mean(model_result^2))
}
mean(results) # two decimal places are sufficient
# [1] 0.7165624

# -----------------------------------------------------------------
# x_2
# -----------------------------------------------------------------
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 1000, c(0, 0, 0), Sigma) %>%
     data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
results <- {
     
     # Partition the dataset into test and training sets of equal size
     train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
     train_set <- dat[-train_index,]
     test_set <- dat[train_index,] 
     
     # Train the model
     model <- lm(y ~ x_2, data = train_set)
     model_prediction <- predict(model, test_set) 
     
     # Calculate the RMSE
     model_result <- test_set$y - model_prediction
     model_rmse <- sqrt(mean(model_result^2))
}
mean(results) # two decimal places are sufficient
# [1] 0.6633693

# x_1 and x_2
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 1000, c(0, 0, 0), Sigma) %>%
     data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
results <- replicate(n, expr = {
     
     # Partition the dataset into test and training sets of equal size
     train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
     train_set <- dat[-train_index,]
     test_set <- dat[train_index,] 
     
     # Train the model
     model <- lm(y ~ x_1 + x_2, data = train_set)
     model_prediction <- predict(model, test_set) 
     
     # Calculate the RMSE
     model_result <- test_set$y - model_prediction
     model_rmse <- sqrt(mean(model_result^2))
})
mean(results) # two decimal places are sufficient
# [1] 0.3278193

# Comparisons - putting 1000 in the dat clause, no replication
# x_1 0.7165624
# x_2 0.6633693
# Both 0.3278193

# Comparisons - leaving 100 in the dat clause, 1000 replications
# x_1 0.7165624
# x_2 0.6633693
# Both 0.3278193

# Comparisons - leaving 100 in the dat clause, no replications
# x_1 0.600666
# x_2 0.630699
# Both 0.3070962

replicate(n, expr = {
     
     })

# When I finally got it right, it was with the original n=100 inside
# mcrnorm(n=100,......), no replications of the data, and set.seed(1) twice.
# Once was just after defining n<-1000, and the other was just before defining
# the test_index.

# -----------------------------------------------------------------
# x_1
# -----------------------------------------------------------------
n <- 1000
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
     data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
results <- {
     
     # Partition the dataset into test and training sets of equal size
     train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
     train_set <- dat[-train_index,]
     test_set <- dat[train_index,] 
     
     # Train the model
     model <- lm(y ~ x_1, data = train_set)
     model_prediction <- predict(model, test_set) 
     
     # Calculate the RMSE
     model_result <- test_set$y - model_prediction
     model_rmse <- sqrt(mean(model_result^2))
}
mean(results) # two decimal places are sufficient
# [1] 0.600666

# -----------------------------------------------------------------
# x_2
# -----------------------------------------------------------------
n <- 1000
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
     data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
results <- {
     
     # Partition the dataset into test and training sets of equal size
     train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
     train_set <- dat[-train_index,]
     test_set <- dat[train_index,] 
     
     # Train the model
     model <- lm(y ~ x_2, data = train_set)
     model_prediction <- predict(model, test_set) 
     
     # Calculate the RMSE
     model_result <- test_set$y - model_prediction
     model_rmse <- sqrt(mean(model_result^2))
}
mean(results) # two decimal places are sufficient
# [1] 0.630699

# x_1 and x_2
n <- 1000
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
     data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
results <- {
     
     # Partition the dataset into test and training sets of equal size
     train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
     train_set <- dat[-train_index,]
     test_set <- dat[train_index,] 
     
     # Train the model
     model <- lm(y ~ x_1 + x_2, data = train_set)
     model_prediction <- predict(model, test_set) 
     
     # Calculate the RMSE
     model_result <- test_set$y - model_prediction
     model_rmse <- sqrt(mean(model_result^2))
}
mean(results) # two decimal places are sufficient
# [1] 0.3070962

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 1000, c(0, 0, 0), Sigma) %>%
     data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
results <- replicate(100, expr = {
     
     # Partition the dataset into test and training sets of equal size
     train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
     train_set <- dat[-train_index,]
     test_set <- dat[train_index,] 
     
     # Train the model
     model <- lm(y ~ x_1 + x_2, data = train_set)
     model_prediction <- predict(model, test_set) 
     
     # Calculate the RMSE
     model_result <- test_set$y - model_prediction
     model_rmse <- sqrt(mean(model_result^2))
})
mean(results) # two decimal places are sufficient
# [1] 0.3308286
