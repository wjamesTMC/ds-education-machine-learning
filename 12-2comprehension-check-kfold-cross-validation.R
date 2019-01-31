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
# y using x with accuracy greater than 0.5. Confirm this by running cross
# validation using logistic regression to fit the model. Because we have so many
# predictors, we selected a random sample x_subset. Use the subset when training
# the model.

# Which code correctly performs this cross-validation?

# Incorrect - no model given (e.g., glm)
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

# Incorrect - generates an error
fit <- train(y, x_subset, method = "glm")
# Error: Please use column names for `x`
fit$results

# Incorrect - generates an error
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

tt <- colttests(x, y)   # Need to study and understand t value function
head(tt)

# Which of the following lines of code correctly creates a vector of the
# p-values called pvals?

pvals <- tt$dm
pvals <- tt$statistic
pvals <- tt
pvals <- tt$p.value     # This is the correct answer - output below
# [1] 0.37502121 0.42956387 0.45774730 0.77109863 0.85255029 0.09225341 etc.

#
# Q3
#

# Create an index ind with the column numbers of the predictors that were
# "statistically significantly" associated with y. Use a p-value cutoff of 0.01
# to define "statistically significantly."

ind <- which(pvals < 0.01)
length(ind)
# [1] 108

#
# Q4
#

# Now re-run the cross-validation after redefinining x_subset to be the subset
# of x defined by the columns showing "statistically significant" association
# with y. What is the accuracy now?

# In Q4 we have to extract from x the columns that correspond to a p-value <=
# 0.01; to build up a new x_subset with these columns; to apply again the
# train() function just as we did in Q1.

ind <- which(pvals <= 0.01)
newx_subset <- x[,ind]
head(newx_subset)

fit <- train(newx_subset, y, method = "glm")
fit$results
#   parameter  Accuracy     Kappa AccuracySD    KappaSD
# 1      none 0.7571395 0.5134142 0.01922097 0.03805696


#
# Q5
#

# Re-run the cross-validation again, but this time using kNN. Try out the
# following grid k = seq(101, 301, 25) of tuning parameters. Make a plot of the
# resulting accuracies. Which code is correct?

# Start by rerunning the code prior to the knn fit statement
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

tt <- colttests(x, y)
pvals <- tt$p.value

ind <- which(pvals <= 0.01)
newx_subset <- x[,ind]

# Now we can try each fit statement in turn to see which is right, but note
# that only one has the code stated correctly (k values) as in the problem.

# Correct code
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# Incorrect - no knn parameters
fit <- train(x_subset, y, method = "knn")
ggplot(fit)

# Incorrect - wrong knn parameters
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(103, 301, 25)))
ggplot(fit)

# Incorrect - wrong knn parameters
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 5)))
ggplot(fit)


#
# Q6
#

# In the previous exercises, we see that despite the fact that x and y are
# completely independent, we were able to predict y with accuracy higher than
# 70%. We must be doing something wrong then. What is it?

# Choices

# The function train estimates accuracy on the same data it uses to train the
# algorithm. 

# We are overfitting the model by including 100 predictors. 

# We used the entire dataset to select the columns used in the model.
#
# This is the correct answer. Explanation: Because we used the entire dataset to
# select the columns in the model, the accuracy is too high. The selection step
# needs to be included as part of the cross-validation algorithm, and then the
# cross-validation itself is performed after the column selection step.

# The high accuracy is just due to random variability.


#
# Q7
#

# Use the train function to predict tissue from gene expression in the
# tissue_gene_expression dataset. Use kNN. What value of k works best?

# Answer: 1 See the example in the videos and text book that although a
# knn of 1 will work best, it is way overfitted because it essentially
# is an equality, not a predictor.
