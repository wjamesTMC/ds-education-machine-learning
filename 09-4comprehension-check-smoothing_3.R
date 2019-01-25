# --------------------------------------------------------------------------------
#
# COmprehension check - Smoothing - Question 3
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
library(purrr)
library(pdftools)
library(lubridate)
library(stringr)

# Suppose we want to predict 2s and 7s in the mnist_27 dataset with just the
# second covariate. Can we do this? On first inspection it appears the data does
# not have much predictive power.

# In fact, if we fit a regular logistic regression the coefficient for x_2 is
# not significant! This can be seen using this code:
     
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

# Plotting a scatterplot here is not useful since y is binary:
     
qplot(x_2, y, data = mnist_27$train)

# ************************************************************************* 
# Assignment: Fit a loess line to the data above and plot the results.
# What do you observe?
# *************************************************************************

# span is not specified in the problem - from the discussions: use 0.04
library(broom)
span <- 0.04
fit <- loess(as.numeric(y)~ x_2, degree = 1, span = span, data = mnist_27$train)
qplot(x_2, fit$fitted, data = mnist_27$train)

mnist_27$train %>% glm(y ~ fit$fitted, family = "binomial", data = .) %>% tidy()

# Answer options:
# There is no predictive power and the conditional probability is linear.
# There is no predictive power and the conditional probability is non-linear.
# There is predictive power and the conditional probability is linear.
# There is predictive power and the conditional probability is non-linear.
