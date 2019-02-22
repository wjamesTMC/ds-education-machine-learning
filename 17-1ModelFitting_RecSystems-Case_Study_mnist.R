# --------------------------------------------------------------------------------
#
# Model Fitting and Recommendation Systems - Case Study mnist
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
library(Rborist)
library(randomForest)
library(gam)

# The graphical theme used for plots throughout the book can be recreated using
# the ds_theme_set() function from dslabs package.

# We have learned several machine learning algorithms and demonstrated how to
# use them with illustrative examples. But we are now going to try them out on a
# real example. This is a popular data set used in machine learning competitions
# called the MNIST digits.

# We can load the data using the following dslabs package, like this. 

mnist <- read_mnist()

# The data set includes two components, a training set and a test set. You can
# see that by typing this.

names(mnist)
# [1] "train" "test" 

# Each of these components includes a matrix with features in the columns. You
# can access them using code like this.

dim(mnist$train$images)
# [1] "train" "test" 
# > dim(mnist$train$images)
# [1] 60000   784

# It also includes a vector with the classes as integers. You can see that by
# using this code.

class(mnist$train$labels)
# [1] "integer"
table(mnist$train$labels)
#    0    1    2    3    4    5    6    7    8    9 
# 5923 6742 5958 6131 5842 5421 5918 6265 5851 5949 

# Because we want this example to run on a small laptop and in less than an
# hour, we'll consider a subset of the data set.

# We will sample 10,000 random rows from the training set and 1,000 random rows
# from the test set.

set.seed(123)
index  <- sample(nrow(mnist$train$images), 10000)
x      <- mnist$train$images[index,]
y      <- factor(mnist$train$labels[index])

index  <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index,]
y_test <- factor(mnist$train$labels[index])

# In machine learning, we often transform predictors before running the machine
# learning algorithm. This is actually an important step. We also remove
# predictors that are clearly not useful. Also an important step. We call all
# this pre-processing.

# Examples of pre-processing include standardizing the predictors, taking the
# log transform of some predictors, or some other transformation, removing
# predictors that are highly correlated with others, and removing predictors
# with very few non-unique values or close to 0 variation. We're going to show
# an example of one of these. The example we're going to look at relates to the
# variability of the features.

# We can see that there are a large number of features with 0 variability or
# almost 0 variability. We can use this code to compute the standard deviation
# of each column and then plot them in a histogram. 

library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

# Here's what it looks like. [See plot]

# This is expected because there are parts of the image that rarely contain
# writing, very few dark pixels. So there's very little variation. And almost
# all the values are 0.

# The caret package includes a function that recommends features to be removed
# due to near 0 variance. You can run it like this.

nzv <- nearZeroVar(x)

# We can see that columns that are removed, they're the yellow ones in this
# plot, by simply making an image of the matrix. Once we remove these columns,
# we end up keeping this many columns. Now we're ready to fit some models.

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)
# [1] 252
