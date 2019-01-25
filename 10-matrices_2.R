# --------------------------------------------------------------------------------
#
# Row and Column Summaries and Apply / Filtering Columns Based on Summaries
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
# Row and Column Summaries and Apply
#
# --------------------------------------------------------------------------------

# So now let's start to attack the challenges that we posed earlier. For the
# first one which related to the total pixel darkness, we want to sum the values
# of each row and then visualize how these values vary by digit. The function
# rowSums takes a matrix as input and computes the desired values. It takes the
# sum of each row. So this little simple code does that very quickly.

sums <- rowSums(x)
head(sums)
# [1] 27525 31095 19443 17135 23214 29601

# We can also compute the averages with the function rowMeans like this.

avg <- rowMeans(x)
head(avg)
# [1] 35.10842 39.66199 24.79974 21.85587 29.60969 37.75638

# Once we have this, we can simply generate a box plot to see how the average
# pixel intensity changes from digit to digit. Here it is.

data.frame(labels = as.factor(y), row_averages = avg) %>%
     ggplot(aes(labels, row_averages)) +
     geom_boxplot()

# From this plot, we see that, not surprisingly, ones use less ink than other
# digits.
# 
# Note that we can also compute the column sums
# and averages using the functions colSums and colMeans respectively.
# The package matrixStats adds functions that
# perform operations on each row or column very efficiently, including
# the functions rowSds and colSds.

library(matrixStats)
sds <- colSds(x)
sds
#  [1]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
#  [7]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [13]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [19]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [25]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [31]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [37]   0.0000000   0.0000000   0.6324555   3.9212243   0.0000000   2.8776727
# [43]   8.0325703   9.2193620  10.2570328   8.3119388   4.3955659   0.0000000
# Etc. (rows not shown)

# Note that the functions just describe are performing an operation similar to
# two functions that we've already learned, sapply and the per function map.
# They apply the same function to a part of our object. In this case, either
# each row or each column. The apply function lets you apply any function, not
# just sum or mean, to a matrix. The first argument of the apply function is the
# matrix. The second is the dimension that you want to apply the function to,
# one for rows, two for columns. And the third argument is the function.
#
#    apply(matrix, dimension, function)
#
# So for example, rowMeans can be written like this.

avgs <- apply(x, 1, mean)
head(avgs)
# [1] 35.10842 39.66199 24.79974 21.85587 29.60969 37.75638

# But note that just like sapply and map, we can perform any function.
# So if we wanted the standard deviation for each column, we could write this.

sds <- apply(x, 2, sd)
sds
#  [1]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
#  [7]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [13]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [19]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [25]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [31]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [37]   0.0000000   0.0000000   0.6324555   3.9212243   0.0000000   2.8776727
# [43]   8.0325703   9.2193620  10.2570328   8.3119388   4.3955659   0.0000000
# Etc. (rows not shown)

# Now what you pay for in this flexibility is that these are not as fast as the
# dedicated functions such as rowMeans, colMeans, et cetera.

# --------------------------------------------------------------------------------
#
# Filtering Columns Based on Summaries
#
# --------------------------------------------------------------------------------

# Now let's turn to our second challenge. Let's study the variation of each
# pixel and remove columns associated with pixels that don't change much, thus
# not informing the classification. Although a simplistic approach, we will
# quantify the variation of each pixel with its standard deviation across all
# entries. Since each column represents a pixel, we use the colSds function from
# the matrix stats package like this.

library(matrixStats)
sds <- colSds(x)
sds
#  [1]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
#  [7]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [13]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [19]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [25]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [31]   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
# [37]   0.0000000   0.0000000   0.6324555   3.9212243   0.0000000   2.8776727
# [43]   8.0325703   9.2193620  10.2570328   8.3119388   4.3955659   0.0000000
# Etc. (rows not shown)

# A quick look at the distribution of these values shows that some pixels have
# very low entry to entry variability.

qplot(sds, bins = "30", color = I("black"))

# This makes sense, since we don't write in some parts of the box. Here is the
# variance plotted by location.

image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

# We see that there is little variation in the corners.
# This makes sense.
# We'd write the digits in the center.
# So we could remove features that have no variation since these
# can't help us predict much.
# In the R basics course, we describe the operations used to extract columns.
# Here's an example showing the 351st and 352nd columns and the rows.

x[ ,c(351,352)]
#         [,1] [,2]
#   [1,]   70    0
#   [2,]    0    0
#   [3,]    0    0
#   [4,]  205  253
#   [5,]    8   78
#   [6,]    0    0
#   [7,]  253  253
#   [8,]   91  212
#   [9,]  254  143
#  [10,]    0    0
# Etc.

# Here are the second and third rows.

x[c(2,3),]
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15]
# [1,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0
# [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26] [,27] [,28]
# [1,]     0     0     0     0     0     0     0     0     0     0     0     0     0
# [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38] [,39] [,40] [,41]
# [1,]     0     0     0     0     0     0     0     0     0     0     0     0     0
# Etc.

# We can also use logical indices to determine which columns or rows to keep. So
# if we wanted to remove uninformative predictors from our Matrix, we could
# write this one line of code, like this.

new_x <- x[ ,colSds(x) > 60]
dim(new_x)
#> [1] 1000  314

# Only the columns for which the standard deviation is above 60 are kept. Here
# we add an important warning related to subsetting matrices. If you select one
# column or one row, the result is no longer a matrix, but a vector. Here's an
# example.

class(x[,1])
#> [1] "integer"
dim(x[1,])
#> NULL

# This could be a problem if you're assuming that operations on matrices will
# result in matrices. However, we can preserve the matrix class by using the
# argument drop, like this.

class(x[ , 1, drop=FALSE])
#> [1] "matrix"
dim(x[, 1, drop=FALSE])
#> [1] 1000    1






