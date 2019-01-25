# --------------------------------------------------------------------------------
#
# Indexing with Matrices and Binarizing the Data
# Vectorization for Matrices and Matrix Algebra Operations
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

# Code carried over from earlier exercises
mnist <- read_mnist()
class(mnist$train$images)
x <- mnist$train$images[1:1000, ]
y <- mnist$train$labels[1:1000]
length(x[,1])
dim(x)
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)
image(1:28, 1:28, grid[,28:1])

# --------------------------------------------------------------------------------
#
# Indexing with Matrices and Binarizing the Data
#
# --------------------------------------------------------------------------------

# For our next challenge, we want to be able to look at a histogram of all our
# pixels. We already saw how we can turn vectors into matrices, but we can also
# undo this and turn matrices into vectors. Here's how it works. It's the
# function as vector. Here's an example.

mat <- matrix(1:15, 5, 3)
mat
#>      [,1] [,2] [,3]
#> [1,]    1    6   11
#> [2,]    2    7   12
#> [3,]    3    8   13
#> [4,]    4    9   14
#> [5,]    5   10   15
as.vector(mat)
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15

# So to see a histogram of all our predictors, we can simply type this code.

qplot(as.vector(x), bins = 30, color = I("black"))

# When we look at this plot we see a clear dichotomy which is explained as parts
# with ink and parts without ink. If we think that values below say, 25, are
# smudges, we can quickly make them zero using this very simple code.

new_x <- x
new_x[new_x < 50] <- 0

# To see what this does, let's look at a smaller matrix at a smaller example.
# Type this code and notice what happens.

mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat
#>      [,1] [,2] [,3]
#> [1,]    0    6   11
#> [2,]    0    7   12
#> [3,]    3    8   13
#> [4,]    4    9   14
#> [5,]    5   10   15

# It changes all the values that are less than three to zero. We can also use
# more complicated logical operations with matrices like this. Here's an example
# where we zero out all the values that are between 6 and 12.

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat <12] <- 0
mat
#>      [,1] [,2] [,3]
#> [1,]    1    6    0
#> [2,]    2    0   12
#> [3,]    3    0   13
#> [4,]    4    0   14
#> [5,]    5    0   15

# Now for the next challenge, we want a binarize the data.
# The histogram we just saw suggests that this data is mostly binary
# pixels, are either ink or no ink.
# Using what we've learned, we can binarize
# the data using just matrix operations.
# For example, using this code we turn all the values
# below 255 divided by 2 to 0 and above it to 1.

bin_x <- x
bin_x[bin_x < 255/2] <- 0 
bin_x[bin_x > 255/2] <- 1

# But we can also convert it to a matrix using logicals
# and coerce it into numbers like this.

bin_X <- (x > 255/2)*1

# Here's an example showing that by converting things into 0 and 1
# we don't lose that much information.
# The figure on the left includes all the pixel values.
# The picture on the right is binarized.
# You can see it's three.

# --------------------------------------------------------------------------------
#
# Vectorization for Matrices and Matrix Algebra Operations
#
# --------------------------------------------------------------------------------

# For our final challenge in which
# we're standardizing the rows or the columns,
# we're going to use vectorization.
# In R, we subtract a vector from a matrix,
# the first element of each vector is subtracted
# from the first row of the matrix.
# The second element from the vector is subtracted from the second row
# of the matrix and so on.
# So using mathematical notation, we would write it like this.
# This is what R does when you subtract a vector from a matrix.
# The same holds true for other arithmetic operations.
# This implies that we can scale each row of a matrix using this simple code.
# Now, if you want to scale each column, be
# careful because it does not work for columns.
# For columns, we would have to transpose a matrix.
# So we would have to do it like this.
# We transpose the matrix, subtract the columns, and then transpose it back.
# For this task, we can also use a function call sweep,
# which works in a similar way to apply.
# It takes each entry of a vector and subtracts it
# from the corresponding row or column.
# So for example, in this code, we take each column.
# There's a two there.
# That tells you it's a column.
# And it subtracts the column mean from each column and returns the new matrix.
# 
# Now, the function sweep actually has an other argument
# that lets you define the arithmetic operation.
# By default, it's subtraction.
# But we can change it.
# So to divide by the standard deviation, we can do the following.
# So we have seen powerful ways in which we can use matrix algebra in R
# to perform certain tasks.
# Finally, although we do not cover matrix algebra operations such as matrix
# multiplication, we share here the relevant commands
# for those that know the mathematics and want to learn the code.
# Matrix multiplication is done with the following operation--
# percent star percent.
# So the cross-product, for example, can be written like this.
# We can compute the cross-product directly
# with the function with that name. cross product
# x gives us as the cross-product.
# To compute the inverse of a function, we use solve.
# Here it is applied to the cross-product.
# Finally, the qr decomposition is readily available by using the qr
# function like this.






