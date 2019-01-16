# --------------------------------------------------------------------------------
#
# Comprehension Check - Linear regression for prediction
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

# Question 1

# ******************
# Sample from LJames
# ******************
#Create data
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>% 
     data.frame() %>% setNames(c("x", "y"))

#Begin replication experiment

     
     #Partition
     train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
     train <- dat[-train_index,]
     test <- dat[train_index,] 
     
     #Model
     model <- lm(y ~ x, data = train)
     pred <- predict(model, test) 
     
     #Performance
     res <- test$y-pred
     model_rmse <- sqrt(mean(res^2))
     model_rmse
# Create a data set using the following code
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
     data.frame() %>% setNames(c("x", "y"))

# Use the caret package to partition the dataset into test and training sets of
# equal size. 

# *** DETERMINE WHICH TO USE test_index or train_index ***
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

# Train a linear model 
model <- lm(y ~ x, data = train_set)
model_prediction <- predict(model, test_set) 

# Calculate the RMSE
model_result <- test_set$y-model_prediction
model_rmse <- sqrt(mean(model_result^2))
model_rmse

# Repeat this exercise 100 times and report the mean and standard deviation of
# the RMSEs. (Hint: You can use the code shown in a previous course inside a
# call to replicate using a seed of 1). Calculate the RMSE

mean(RMSE) : two decimal places are sufficient
sd(RMSE): for a correct answer three decimal places are necessary.




