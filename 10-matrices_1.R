# --------------------------------------------------------------------------------
#
# Matrices and Matrix Notation
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

# --------------------------------------------------------------------------------
#
# Introduction to Matrices
#
# --------------------------------------------------------------------------------

# In machine learning, situations in which all predictors are numeric, or can be
# converted to numeric in a meaningful way, are common. The digits data set is
# an example. Every pixel records a number between 0 and 255. We can actually
# load the 60,000 digits using this code.

mnist <- read_mnist()

# In these cases, it is often convenient to save the predictors in a matrix and
# the outcomes in a vector rather than using a data frame. In fact, we can see
# that the data set that we just downloaded does this. You can see that the
# training data image is a matrix by typing this code. 

class(mnist$train$images)

# This matrix represents 60,000 digits. It's a pretty big matrix. So for the
# example, in this video, we'll take a more manageable subset. We will take the
# first 1,000 predictors and the first 1,000 labels, which we can do using this
# code.

x <- mnist$train$images[1:1000, ]
y <- mnist$train$labels[1:1000]

# In machine learning, the main reason for using matrices is that certain
# mathematical operations needed to develop efficient code can be performed
# using techniques from a branch of mathematics called linear algebra. In fact,
# linear algebra and matrix notation are key elements of the language used in
# academic papers describing machine learning techniques. We will not cover
# linear algebra in detail here, but we'll demonstrate how to use matrices in R,
# so that you can apply the linear algebra techniques already implemented in R
# Base and other packages.

# To motivate the use of matrices, we will pose five challenges. 

# First, we're going to study the distribution of the total pixel darkness and
# how it varies by digits.
#
# Second, we're going to study the variation of each pixel and remove
# predictors, columns, associated with pixels that don't change much and thus
# can't provide much information for classification.
#
# Third, we're going to zero out low values that are likely smudges. First,
# we're going to look at the distribution of all pixel values, use this to pick
# a cutoff to define unwritten space, then make anything below that cutoff a
# zero.
#
# Fourth, we're going to binarize the data. We're going to first look at the
# distribution of all pixel values, use this to pick a cutoff, and distinguish
# between writing and no writing. Then convert all entries into either zero or
# one.
#
# Finally, we're going to scale each of the predictors in each entry to have the
# same average and standard deviation. To complete these, we'll have to perform
# mathematical operations involving several variables.

# The tidyverse is not developed to perform this type of mathematical operation.
# For this task, it is convenient to use matrices. Before we attack the
# challenges, we will introduce matrix notation and basic R code to define and
# operate on matrices.

# --------------------------------------------------------------------------------
#
# Matrix Notation
#
# --------------------------------------------------------------------------------

# In matrix algebra we have three main types of objects, scalars,
# vectors, and matrices.
# A scalar is just one number.
# For example, a equals one, a is a scalar.
# To denote scalars in matrix notation, we usually use a lowercase letter
# and we don't bold it.

# Vectors are like the numeric vectors we define in r.
# They include several scalar entries.
# For example, the column containing the first pixel is a vector.
# It has length 1000.
# Here is the code that shows it.

length(x[,1])
# [1] 1000

# In matrix algebra we use the following notation to define vectors, like this.
# Similarly, we can use math notation to represent different features
# mathematically by adding an index.
# So here's x1, the first feature and x2, the second feature.
# Both are vectors.
# If we're writing out a column such as x1, in a sentence
# we often use the notation x1 through xn and then
# we have the transpose symbol t.
# This transpose operation converts columns into rows and rows
# into columns.
# A matrix can be defined as a series of vectors of the same size
# joined together, each forming a column.
# So in our code, we can write it like this.
# Mathematically, we represent them with bold uppercase letters like this.
# The dimension of a matrix is often an important characteristic
# needed to assure certain operations can be performed.
# The dimension is a two number summary defined as the number of rows
# and the number of columns.
# In r we can extract the dimensions of the matrix
# with the function dim like this.
# Note that vectors can be thought of as n by 1 matrices.
# However, in r, a vector does not have dimensions.
# You can see it by typing this.
# However, we can explicitly convert a vector
# into a matrix using the as.matrix function.
# So if we do that, then we see that indeed this is a matrix that is 5 by 1.
# We can use this notation to denote an arbitrary number of predictors
# with the following n by p matrix.
# For example, if we have 784 columns we could do this.
# P is 784, here's the arbitrary matrix representing our data.
# We store this into x.
# So when you do dim x, you can see it's 1000 by 784.



