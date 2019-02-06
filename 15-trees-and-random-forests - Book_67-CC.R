# --------------------------------------------------------------------------------
#
# Trees and Random Forests - Comprehension Check
#
# --------------------------------------------------------------------------------

# Setup
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(Lahman)
library(HistData)
library(caret)
library(e1071)
library(matrixStats)

#
# Q1
#

# Create a simple dataset where the outcome grows 0.75 units on average for
# every increase in a predictor, using this code:
     
library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Which code correctly uses rpart to fit a regression tree and saves the result to fit?
# fit <- rpart(y ~ .)
# fit <- rpart(y, ., data = dat)
# fit <- rpart(x ~ ., data = dat)
# fit <- rpart(y ~ ., data = dat)


#
# Q2
#

# Which of the following plots correctly shows the final tree obtained in Q1?
     
#
# Q3
#

# Below is most of the code to make a scatter plot of y versus x along with the
# predicted values based on the fit.

dat %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(x, y)) +
     # BLANK
    
# Which line of code should be used to replace #BLANK in the code above?
# geom_step(aes(x, y_hat), col=2)
# geom_smooth(aes(y_hat, x), col=2)
# geom_quantile(aes(x, y_hat), col=2)
# geom_step(aes(y_hat, x), col=2)


#
# Q4
# 

# Now run Random Forests instead of a regression tree using randomForest from
# the __randomForest__ package, and remake the scatterplot with the prediction
# line. Part of the code is provided for you below.

library(randomForest)
fit <- #BLANK 
     dat %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(x, y)) +
     geom_step(aes(x, y_hat), col = 2)

# What code should replace #BLANK in the provided code?
# randomForest(y ~ x, data = dat)
# randomForest(x ~ y, data = dat)
# randomForest(y ~ x, data = data)
# randomForest(x ~ y)

# Save Save Your Answer

#
# Q5
# 

# Use the plot function to see if the Random Forest from Q4 has converged or if
# we need more trees. Which is the correct plot to assess whether the Random
# Forest has converged?
     
#
# Q6
#
# It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth). Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.

# Part of the code is provided for you below.

library(randomForest)
fit <- #BLANK
     dat %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(x, y)) +
     geom_step(aes(x, y_hat), col = 2)

# What code should replace #BLANK in the provided code?
# randomForest(y ~ x, data = dat, nodesize = 25, maxnodes = 25)
# randomForest(y ~ x, data = dat, nodes = 50, max = 25)
# randomForest(x ~ y, data = dat, nodes = 50, max = 25)
# randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
# randomForest(x ~ y, data = dat, nodesize = 50, maxnodes = 25)

     
     
