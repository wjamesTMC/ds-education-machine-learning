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

# In matrix algebra we have three main types of objects, scalars, vectors, and
# matrices. A scalar is just one number. For example, a equals one, a is a
# scalar. To denote scalars in matrix notation, we usually use a lowercase
# letter and we don't bold it.

# Vectors are like the numeric vectors we define in r. They include several
# scalar entries. For example, the column containing the first pixel is a
# vector. It has length 1000. Here is the code that shows it.

length(x[,1])
# [1] 1000

# In matrix algebra we use the following notation to define vectors, like this.

#    X = (x1, x2, ... xn)

# Similarly, we can use math notation to represent different features
# mathematically by adding an index. So here's x1, the first feature and x2, the
# second feature. Both are vectors. If we're writing out a column such as x1, in
# a sentence we often use the notation x1 through xn and then we have the
# transpose symbol t.

#    X1=(x1,1,…xN,1)⊤

# This transpose operation converts columns into rows and rows
# into columns.
# A matrix can be defined as a series of vectors of the same size
# joined together, each forming a column.
# So in R code, we can write it like this.

x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
#      x_1 x_2
# [1,]   1   6
# [2,]   2   7
# [3,]   3   8
# [4,]   4   9
# [5,]   5  10

# Mathematically, we represent them with bold uppercase letters like this. The
# dimension of a matrix is often an important characteristic needed to assure
# certain operations can be performed. The dimension is a two number summary
# defined as the number of rows and the number of columns. In r we can extract
# the dimensions of the matrix with the function dim like this.

dim(x)
# [1] 1000  784

# Note that vectors can be thought of as n by 1 matrices. However, in r, a
# vector does not have dimensions. You can see it by typing this.

dim(x_1)
# NULL

# However, we can explicitly convert a vector into a matrix using the as.matrix
# function. So if we do that, then we see that indeed this is a matrix that is 5
# by 1.

dim(as.matrix(x_1))
# [1] 5 1

# We can use this notation to denote an arbitrary number of predictors with the
# following n by p matrix. For example, if we have 784 columns we could do this.
# P is 784, here's the arbitrary matrix representing our data. We store this
# into x. So when you do dim x, you can see it's 1000 by 784.

# --------------------------------------------------------------------------------
#
# Converting a Vector to a Matrix
#
# --------------------------------------------------------------------------------

# We will learn several useful operations related to matrix algebra. We'll use
# some of the motivating examples we described in an earlier video to
# demonstrate this. It is often useful to convert a vector to a matrix. For
# example, because the variables are pixels on a grid, we can convert the rows
# of pixel intensities into a matrix representing this grid. We can convert a
# vector into a matrix with the matrix function and specifying the number of
# rows and columns the resulting matrix should have. The matrix is filled by
# column. The first column is filled first, and the second is filled second, and
# so on. So here's an example to illustrate what we mean.

my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat
#      [,1] [,2] [,3]
# [1,]    1    6   11
# [2,]    2    7   12
# [3,]    3    8   13
# [4,]    4    9   14
# [5,]    5   10   15

# If we define a vector, that's the numbers 1 through 15. And then we use the
# matrix function on this factor, and say it has five rows and three columns, we
# end up with the following matrix. We can fill in by row instead of by column
# by using the byrow argument. So, for example, to transpose the matrix we just
# showed, we would use the matrix function like this. Now we have three rows,
# five columns, and we fill it in by row. Here's the code.

mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    2    3    4    5
# [2,]    6    7    8    9   10
# [3,]   11   12   13   14   15

# This is essentially transposing the matrix.
# In R, we can use the function t to directly transpose a matrix.
# Now notice that these two are the same.

identical(t(mat), mat_t)
# [1] TRUE

# An important warning. The matrix function in R recycles values in the vector
# without warnings. If the product of columns and rows does not match the length
# of the vector, this happens.

matrix(my_vector, 5, 5)
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    6   11    1    6
# [2,]    2    7   12    2    7
# [3,]    3    8   13    3    8
# [4,]    4    9   14    4    9
# [5,]    5   10   15    5   10

# So look at what happens when I try to turn my vector, which has 15 entries,
# into a 5 by 5 matrix.

# So how can we use this in practice? Let's look at an example. To put the pixel
# intensities of, say, the third entry, which we know is a digit that represents
# a 4, into a grid, we can use this.

grid <- matrix(x[3,], 28, 28)
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    6   11    1    6
# [2,]    2    7   12    2    7
# [3,]    3    8   13    3    8
# [4,]    4    9   14    4    9
# [5,]    5   10   15    5   10
# > grid <- matrix(x[3,], 28, 28)
# > grid
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17]
# [1,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0     0     0
# [2,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0     0     0
# [3,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0     0     0
# [4,]    0    0    0    0    0    0    0    0    0     0    46   120   159   159   159   150     0
# [5,]    0    0    0    0    0    0   62  126  220   222   245   254   254   254   254   253   119
# [6,]    0    0    0    0    0    0   81  163  163   163   163   163   120    67    85   237   177
# [7,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0   207   177
# [8,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0   207   177
# [9,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0   207   177
# [10,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0    47   253   177
# [11,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0    49   254    98
# [12,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0   116   250    56
# [13,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0   144   240     0
# [14,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0   150   198     0
# [15,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0   241   143     0
# [16,]    0    0    0    0    0    0    0    0    0     0     0     0     0    14   243    91     0
# [17,]    0    0    0    0    0    0    0    0    0     0     0     0     0    86   234    28     0
# [18,]    0    0    0    0    0    0    0    0    0     0     0     0     0   178   179     5   102
# [19,]    0    0    0    0    0    0    0    0    0     0     0    23   163   248   241   233   254
# [20,]    0    0    0    0    0    0    0    2   27   183   198   231   254   254   252   250   220
# [21,]    0    0    0    0    0   67  120  153  254   254   254   254   216    91    40     0     0
# [22,]    0    0    0    0    0  232  180  210  162   125    56    29    16     0     0     0     0
# [23,]    0    0    0    0    0   39   39   40    0     0     0     0     0     0     0     0     0
# [24,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0     0     0
# [25,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0     0     0
# [26,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0     0     0
# [27,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0     0     0
# [28,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0     0     0
# [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26] [,27] [,28]
# [1,]     0     0     0     0     0     0     0     0     0     0     0
# [2,]     0     0     0     0     0     0     0     0     0     0     0
# [3,]     0     0     0     0     0     0     0     0     0     0     0
# [4,]     0     0     0     0     0     0     0     0     0     0     0
# [5,]     0     0     0     0     0     0     0     0     0     0     0
# [6,]     0     0     0     0     0     0     0     0     0     0     0
# [7,]     0     0     0     0     0     0     0     0     0     0     0
# [8,]     0     0     0     0     0     0     0     0     0     0     0
# [9,]     0     0     0     0     0     0     0     0     0     0     0
# [10,]     0     0     0     0     0     0     0     0     0     0     0
# [11,]     0     0     0     0     0     0     0     0     0     0     0
# [12,]     0     0     0     0     0     0     0     0     0     0     0
# [13,]     0     0     0     0     0     0     0     0     0     0     0
# [14,]     0     0     0     0     0     0     0     0     0     0     0
# [15,]     0     0     0     0     0     0     0     0     0     0     0
# [16,]     0     0     0     0     0     0     0     0     0     0     0
# [17,]     0     0     0     0     0     0     0     0     0     0     0
# [18,]   169   169   169   169   169   169   169    96     0     0     0
# [19,]   254   254   254   255   254   254   255   254     0     0     0
# [20,]   137    57    57    94    96   153   153   153     0     0     0
# [21,]     0     0     0     0     0     0     0     0     0     0     0
# [22,]     0     0     0     0     0     0     0     0     0     0     0
# [23,]     0     0     0     0     0     0     0     0     0     0     0
# [24,]     0     0     0     0     0     0     0     0     0     0     0
# [25,]     0     0     0     0     0     0     0     0     0     0     0
# [26,]     0     0     0     0     0     0     0     0     0     0     0
# [27,]     0     0     0     0     0     0     0     0     0     0     0
# [28,]     0     0     0     0     0     0     0     0     0     0     0

# To confirm that, in fact, we have done this correctly,
# we can use a function image, which shows an image of the third argument.
# Here's how we use it.

image(1:28, 1:28, grid)

# We can see that this looks like an upside down 4.
# Now it looks upside down because the top of this image, pixel one,
# is shown at the bottom.
# This is how R plots images.
# So it's flipped.
# If we want to flip it back, we can use this code.

image(1:28, 1:28, grid[,28:1])

# And now we get an image that looks like a 4.


