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

# Question 1: Create a data set using the following code:
     
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
     data.frame() %>% setNames(c("x", "y"))

# Use the caret package to partition the dataset into test and training sets of
# equal size. Train a linear model and calculate the RMSE. Repeat this exercise
# 100 times and report the mean and standard deviation of the RMSEs. (Hint: You
# can use the code shown in a previous course inside a call to replicate using a
# seed of 1).

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

mean(RMSE) : two decimal places are sufficient
sd(RMSE): for a correct answer three decimal places are necessary.




