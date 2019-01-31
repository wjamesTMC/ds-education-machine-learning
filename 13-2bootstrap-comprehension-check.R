# --------------------------------------------------------------------------------
#
# Bootstrap Comprehension Check
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
library(matrixStats)

data(mnist_27)

# 
# Q1
#

# The createResample function can be used to create bootstrap samples. For
# example, we can create 10 bootstrap samples for the mnist_27 dataset like
# this:

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

# How many times do 3, 4, and 7 appear in the first resampled index?
# 3 - 1
# 4 - 4
# 7 - 0

#
# Q2
#

# We see that some numbers appear more than once and others appear no times.
# This has to be this way for each dataset to be independent. Repeat the
# exercise for all the resampled indexes.

# What is the total number of times that 3 appears in all of the resampled
# indexes?
# 11

#
# Q3
#

# Generate a random dataset using the following code:
     
set.seed(1)
y <- rnorm(100, 0, 1)

# Estimate the 75th quantile, which we know is qnorm(0.75), with the sample
# quantile: quantile(y, 0.75).

sq <- quantile(y, 0.75)
# 75% 
# 0.6915454 

# Run a Monte Carlo simulation with 10,000 repetitions to learn the expected
# value and standard error of this random variable. Set the seed to 1.

set.seed(1)

B <- 10000
res <- replicate(B, {
     y_hat <- rnorm(100, 0, 1)
     t75q <- quantile(y_hat, 0.75)
})

mean(res)
# [1] 0.6656107
sd(res)
# [1] 0.1353809
     

#
# Q4
#

# In practice, we can't run a Monte Carlo simulation. Use 10 bootstrap samples
# to estimate the standard error using just the initial sample y. Set the seed
# to 1.

set.seed(1)
y <- rnorm(100, 0, 1)

# Create the indexes 
set.seed(1)
ind10 <- createResample(y, 10, list = FALSE)

# Create the dataframe 
df10 <- as.data.frame(as.table(ind10))

# Add a new column y in the dataframe that contains the actual values
df10 <- df10 %>% mutate(y = y[ind10])
head(df10)
# Calculate the quartile 
Q_stars <- df10 %>% group_by(df10$Var2) %>% summarize(Q_star = quantile(y, 0.75))
                                                                        
# Calculate the mean and sd of Q_stars$Q_star
mean(Q_stars$Q_star)
# [1] 0.7312648
sd(Q_stars$Q_star)
# [1] 0.07419278


#
# Q5
#

# Repeat the exercise from Q4 but with 10,000 bootstrap samples instead of 10.
# Set the seed to 1.

set.seed(1)
y <- rnorm(100, 0, 1)

# Create the indexes 
set.seed(1)
ind10 <- createResample(y, 10000, list = FALSE)

# Create the dataframe 
df10 <- as.data.frame(as.table(ind10))

# Add a new column y in the dataframe that contains the actual values
df10 <- df10 %>% mutate(y = y[ind10])
head(df10)
# Calculate the quartile 
Q_stars <- df10 %>% group_by(df10$Var2) %>% summarize(Q_star = quantile(y, 0.75))

# Calculate the mean and sd of Q_stars$Q_star
mean(Q_stars$Q_star)
# [1] 0.6737512
sd(Q_stars$Q_star)
# [1] 0.0930575

#
# Q6
#

# Compare the SD values obtained using 10 vs 10,000 bootstrap samples.
# What do you observe?

# Answer: they are roughly about the same
