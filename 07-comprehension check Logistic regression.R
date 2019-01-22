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
     
     # First line creates a list of 1000 0's and 1's
     y <- rbinom(n, 1, p)
     
     # f_0 Creates a list of 1000 normally distributed values between 
     # -2.686312 and 3,302766 where the mean is 0 and sd is 1
     f_0 <- rnorm(n, mu_0, sigma_0)
     
     # f_1 creates a list of 1000 normally distributed values between
     # -1.435958 and 4.842957 where the mean is 2 and sd is 1
     f_1 <- rnorm(n, mu_1, sigma_1)
     
     # x creates a list of 1000 values ranging between -2.538682 and 4.842957 -
     # whenever y == 1, then the value is taken from f_1, else the value is f_0
     x <- ifelse(y == 1, f_1, f_0)
     
     # Creates a list of 526 index entries to create the test set
     test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
     
     # Creates train and test sets using the test_index
     list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
          test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
# Runs the function above to create the data set. The result is 2 data sets, one
# with 1000 0s and 1s and the other with the values that went into the train and
# test sets. The train and test sets each have 500 values of x predicting y.
#
# The mean of x in the train set is 0.9616811
# The mean of x in the test set is 0.8432318
#
# They  look like those shown below. One notices that if the value of x is larger
# than about 1.86, the prediction is 1, else 0.

dat <- make_data()

# $test
#                x y
# 1   -0.459789425 0
# 2    2.070429275 1
# 3    3.581194908 1
# 4   -0.583511943 0
# 5    1.876629485 1
# ...
# 495  2.280088527 1
# 496  0.167933611 0
# 497 -0.992523774 0
# 498  0.168884033 0
# 499  1.860617168 1
# 500  0.355300038 0

# We have now defined a variable x that is predictive of a binary outcome y. It
# results in 2 curves: a 0 curve that peaks at around -0.8 and a 1 curve that
# peaks around 2 or so. The curves range from -3.0 to +4.25

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

# dat$train has 500 values that look like this (and they are the same every run):
# > dat$train
#               x y
# 1    0.74262110 1
# 2    2.47615428 1
# 3    2.36687477 1
# 4    1.34689666 1
# 5    1.57745245 1
# ...
# 495  0.59752541 1
# 496  0.01976771 0
# 497  1.17305376 0
# 498  0.90821380 0
# 499  0.18994327 0
# 500  3.08910755 1

dat$test %>% ggplot(aes(x, color = y)) + geom_density()
# This also results in two curves, a 0 curve that peaks between -1.2 and +1.0
# and a 1 curve that peaks around 2.3. The range is -2.5 to 5.0.

# --------------------------------------------------------------------------------
# The assignment: Generate 25 different datasets changing the difference between
# the two classes using delta <- seq(0, 3, len=25) and plot accuracy vs mu_1.
# --------------------------------------------------------------------------------

# Establish the 25 different values (differences)
delta <- seq(0, 3, len=25)
# [1] 0.000 0.125 0.250 0.375 0.500 0.625 0.750 0.875 1.000 1.125 1.250 1.375 1.500 1.625
# [15] 1.750 1.875 2.000 2.125 2.250 2.375 2.500 2.625 2.750 2.875 3.000

set.seed(1)
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
     dat <- make_data(mu_1 = d)
     fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
     y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
     mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)

# Which is the correct plot?
# Answer - a linear line of points moving up from lower left to upper right
# x axis is delta (0 to 3) and y axis is accuracy (going from 0.5 to almost 1.0)



