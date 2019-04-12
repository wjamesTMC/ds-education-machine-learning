# --------------------------------------------------------------------------------
#
# Distance, Knn, Cross-validation, and Generative Models Overview
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

# The concept of distance is quite intuitive. For example, when we cluster
# animals into subgroups, reptiles, amphibians, mammals, we're implicitly
# defining a distance that permits us to say what animals are close to each
# other. Many machine learning techniques rely on being able to define distance
# between observations using features or predictors.

# As a review, let's define the distance between two points, A and B, on the
# Cartesian plane, like these two. The Euclidean distance between AB is simply
# given by this formula.

# dist(A,B) = √(Ax−Bx)2+(Ay−By)2

# Note that this definition applies to the case of one dimension. In which case,
# the distance between two numbers is simply the absolute value of their
# difference. So if our two one dimensional numbers are A and B, the distance is
# simply this, which turns into the absolute value.

# |A - B|

# In an earlier video, we introduced a training data set with feature matrix
# measurements of 784 features. For illustrative purposes, we look at random
# samples of 2s and 7s. We can generate this data set using this piece of code.

set.seed(0)
if(!exists("mnist")) mnist <- read_mnist()

ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

# The predictors are on x and the labels are on y. For the purposes of, for
# example, smoothing, we're interested in describing distances between
# observations. In this case, digits. Later, for the purposes of selecting
# features, we might also be interested in finding pixels that behave similar
# across samples. Now, to define distance, we need to know what points are,
# since mathematical distance is computed between two points. With high
# dimensional data, points are no longer on the Cartesian plane. Instead, points
# are higher dimensional. We can no longer visualize them and need to think
# abstractly.

# For example, in our digits example, a predictor, xi, is defined as a point in
# 784 dimensional space. We can write it out like this.

# Xi=(xi,1,…,xi,784)⊤

# Once we define points this way, the Euclidean distance is defined very similar
# as it was for the two dimensional case. For instance, the distance between
# observations 1 and 2 is given by this formula.

# dist(1, 2) = √⎷784∑j=1(x1,j−x2,j)2

# Note that this is a non-negative number, just as it is for the two dimensions.
# So any two observations, there's a distance and it's just one number.

# Now let's look at an example. Let's look at the first three observations.
# Let's look at their labels. This is a 7, a 7, and a 2.

y[1:3]
# [1] 7 7 2

# The vector of predictors for each of these observations are going to be saved
# in these three objects.

x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

# Now let's look at the distances. And remember, the first two numbers are a 7
# and the third one is a 2. We expect the distances between the same number,
# like this,

sqrt(sum((x_1-x_2)^2))
# [1] 2079.753

# to be smaller than between different numbers.

sqrt(sum((x_1-x_3)^2))
# [1] 2252
sqrt(sum((x_2-x_3)^2))
# [1] 2643

# And that's what happens. We can see it here. As expected, the 7s are closer to
# each other. Now, if you know matrix algebra, note that a faster way to compute
# this is using the cross-product. So we can actually type this.

sqrt(crossprod(x_1-x_2))
#      [,1]
# [1,] 2080
sqrt(crossprod(x_1-x_3))
#      [,1]
# [1,] 2252
sqrt(crossprod(x_2-x_3))
#      [,1]
# [1,] 2643

# We can also compute all the distances between all the observations at once
# relatively quickly using the function, dist. If you feed it a matrix, the dist
# function computes the distance between each row and produces an object of
# class dist. Here's the code demonstrating this.

d <- dist(x)
class(d)
#> [1] "dist"

# Now there are several machine learning-related functions in R that take
# objects of class dist as input. But to access the entries using row and column
# indices, we need to coerce this object into a matrix. We can do this like
# this.

as.matrix(d)[1:3,1:3]
#>      1    2    3
#> 1    0 2080 2252
#> 2 2080    0 2643
#> 3 2252 2643    0

# If we look at the first three entries, we can see that the distances that we
# calculated match what the function, dist, calculates. We can also quickly see
# an image of these distances using the image function. So we type this.

image(as.matrix(d))

# We see a visual representation of the distance between every pair of
# observations. If we order that distances by labels, we can see that, in
# general, the 2s are closer to each other and the 7s are closer to each other.
# We can achieve this using this code.

image(as.matrix(d)[order(y), order(y)])

# Those red squares demonstrate that digits that are the same
# are closer to each other. But another thing that comes out of this plot is
# that there appears to be more uniformity in how the 7s are drawn since they
# appear to be closer. It's more red up there. 

# Now, we can also compute distance between predictors. If N is the number of
# observations, the distance between two predictors, say the first and the
# second, can be computed like this.

dist(1,2)= √N∑i=1(xi,1−xi,2)2

# To compute the distance between all pairs of the 784 predictors, we can
# transpose the matrix first and then use the dist function. We can write this
# code.

d <- dist(t(x))
dim(as.matrix(d))
# [1] 784 784

# Note that the dimension of the resulting distance matrix is a 784 by 784
# matrix.

# An interesting thing to note here is that, if we pick a predictor, a pixel, we
# can see which pixels are close, meaning that they are either inked together or
# they don't have ink together. So as an example, let's just look at the 492nd
# pixel and let's look at the distances between each pixel and the 492nd pixel.

d_492 <- as.matrix(d)[492,]

# Here is what it looks like. 

image(1:28, 1:28, matrix(d_492, 28, 28))

# We can see the spatial pattern. Not surprisingly, pixels that are physically
# close on the image are actually also close mathematically. So in summary, the
# concept of distance is important in machine learning. And we will see this as
# we learn more about specific algorithms.

