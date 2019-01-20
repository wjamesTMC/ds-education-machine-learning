#---------------------------------------------------------------------------
#
# Comprehension Check for Logistic Regression
#
#---------------------------------------------------------------------------

library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(HistData)
library(caret)
library(e1071)

# Define a dataset using the following code:

set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
     
     y <- rbinom(n, 1, p)
     f_0 <- rnorm(n, mu_0, sigma_0)
     f_1 <- rnorm(n, mu_1, sigma_1)
     x <- ifelse(y == 1, f_1, f_0)
     
     test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
     
     list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
          test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

# Note that we have defined a variable x that is predictive of a binary outcome

y: dat$train %>% ggplot(aes(x, color = y)) + geom_density()

# Generate 25 different datasets changing the difference between the two classes
# using delta <- seq(0, 3, len=25) and plot accuracy vs mu_1.

