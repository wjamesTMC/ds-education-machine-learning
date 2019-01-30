# --------------------------------------------------------------------------------
#
# Comprehension check - kfold Cross Validation
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

#
# Q1
#

# Generate a set of random predictors and outcomes using the following code:

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

# Because x and y are completely independent, you should not be able to predict
# y using x with accuracy greater than 0.5. Confirm this by running
# cross-validation using logistic regression to fit the model. Because we have
# so many predictors, we selected a random sample x_subset. Use the subset when
# training the model.

# Which code correctly performs this cross-validation?

fit <- train(x_subset, y)
fit$results
#   mtry  Accuracy        Kappa AccuracySD    KappaSD
# 1    2 0.4981610 -0.008162189 0.02534202 0.04992821
# 2   51 0.4974338 -0.009608012 0.01591205 0.02938649
# 3  100 0.4984860 -0.007617203 0.01799449 0.03727960
# > fit$results
# mtry  Accuracy        Kappa AccuracySD    KappaSD
# 1    2 0.4981610 -0.008162189 0.02534202 0.04992821
# 2   51 0.4974338 -0.009608012 0.01591205 0.02938649
# 3  100 0.4984860 -0.007617203 0.01799449 0.03727960

# The correct answer is this one
fit <- train(x_subset, y, method = "glm")
fit$results
#   parameter  Accuracy      Kappa AccuracySD    KappaSD
# 1      none 0.5123772 0.02541254 0.02403992 0.04670411

fit <- train(y, x_subset, method = "glm")
# Error: Please use column names for `x`
fit$results

fit <- test(x_subset, y, method = "glm")
# Error in test(x_subset, y, method = "glm") : 
# could not find function "test"
fit$results

#
# Q2
#

# Now, instead of using a random selection of predictors, we are going to search
# for those that are most predictive of the outcome. We can do this by comparing
# the values for the  group to those in the  group, for each predictor, using a
# t-test. You can do perform this step like this:
     
library(devtools)
devtools::install_bioc("genefilter")

library(genefilter)

tt <- colttests(x, y)

# Which of the following lines of code correctly creates a vector of the
# p-values called pvals?

pvals <- tt$dm
pvals <- tt$statistic
pvals <- tt
pvals <- tt$p.value

