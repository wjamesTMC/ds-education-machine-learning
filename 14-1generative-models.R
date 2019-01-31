# --------------------------------------------------------------------------------
#
# Generative Models
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

# We have described how when using square loss
# the conditional expectation or probabilities
# provide the best approach to developing a decision rule.
# In a binary case, the best we can do is called Bayes' rule
# which is a decision rule based on the true conditional probability,
# probably y equals one given the predictors x.
# We have described several approaches to estimating
# this conditional probability.
# Note that in all these approaches, we estimate the conditional probability
# directly and do not consider the distribution of the predictors.
# In machine learning, these are referred to as discriminative approaches.
# However, Bayes' theorem tells us that knowing the distribution
# of the predictors x may be useful.
# Methods that model the joint distribution of y and the predictors x
# are referred to as generative models.
# We start by describing the most general generative model naive Bayes
# and then proceed to describe some more specific cases, quadratic discriminant
# analysis QDA and linear discriminant analysis LDA.
# Recall that Bayes' theorem tells us that we
# can rewrite the conditional probability like this
# with the f's representing the distribution
# functions of the predictors x for the two classes y equals 1
# and when y equals 0.
# The formula implies that if we can estimate
# these conditional distributions, the predictors,
# we can develop a powerful decision realm.
# However, this is a big if.
# As we go froward, we will encounter examples
# in which the predictors x have many dimensions
# and we do not have much information about their distribution.
# So it will be very hard to estimate those conditional distributions.
# In these cases, naive Bayes would be practically impossible to implement.
# However, there are instances in which we have a small number of predictors,
# not much more than two, and many categories in which
# generated models can be quite powerful.
# We describe two specific examples and use our previously described case study
# to illustrate them.

