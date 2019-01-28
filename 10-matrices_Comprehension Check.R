# --------------------------------------------------------------------------------
#
# Comprehension Check
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

# Q1 - Which line of code correctly creates a 100 by 10 matrix of randomly
# generated normal numbers and assigns it to x?

x <- matrix(rnorm(1000), 100, 100)
x <- matrix(rnorm(100*10), 100, 10) # This is the correct answer
x <- matrix(rnorm(100*10), 10, 10)
x <- matrix(rnorm(100*10), 10, 100)

# Q2 - Write the line of code that would give you the specified information
# about the matrix x that you generated in q1. Do not include any spaces in your
# line of code.

dim(x)
nrow(x)
ncol(x)

# Q3 - Which of the following lines of code would add the scalar 1 to row 1, the
# scalar 2 to row 2, and so on, for the matrix x? Select ALL that apply.

x <- matrix(rnorm(100*10), 100, 10)
head(x)
#             [,1]       [,2]       [,3]       [,4]       [,5]       [,6]       [,7]       [,8]        [,9]        [,10]
# [1,]  0.38765755  0.8492641 -1.8776842  1.5180886 -1.0410382 -0.6180672  0.7890221  0.4791804 -0.41957280  0.284496969
# [2,] -0.90831038  0.6365345  0.1547728  1.6459448 -0.3529558 -0.2923756 -1.4265152 -0.4623612 -0.39254865 -2.239440001
# [3,]  0.09529003  1.2187804  0.6074752  1.3236616 -0.9825473  0.7943291 -0.8894373  0.9595351  1.77981784 -0.266768748
# [4,]  0.13939206  0.4937718 -1.6024122  0.3312874 -0.2711830 -0.3713675  0.5825960 -0.3987537 -0.07117565 -0.764738661
# [5,] -1.09208412 -0.2551028 -0.1023960 -1.1532229 -0.6480308  0.7287769 -0.6745791 -0.6008566 -0.65967174  0.005057637
# [6,]  0.23482621 -0.3350758  0.4681056 -0.9853412  0.1823904  1.1555130 -1.2142003  0.6731665  0.79944695 -0.989549845

A1 <- x + seq(nrow(x)) # Correct
head(A1)
#          [,1]     [,2]       [,3]     [,4]        [,5]      [,6]      [,7]     [,8]      [,9]     [,10]
# [1,] 1.387658 1.849264 -0.8776842 2.518089 -0.04103821 0.3819328 1.7890221 1.479180 0.5804272  1.284497
# [2,] 1.091690 2.636535  2.1547728 3.645945  1.64704418 1.7076244 0.5734848 1.537639 1.6074513 -0.239440
# [3,] 3.095290 4.218780  3.6074752 4.323662  2.01745271 3.7943291 2.1105627 3.959535 4.7798178  2.733231
# [4,] 4.139392 4.493772  2.3975878 4.331287  3.72881700 3.6286325 4.5825960 3.601246 3.9288243  3.235261
# [5,] 3.907916 4.744897  4.8976040 3.846777  4.35196924 5.7287769 4.3254209 4.399143 4.3403283  5.005058
# [6,] 6.234826 5.664924  6.4681056 5.014659  6.18239038 7.1555130 4.7857997 6.673166 6.7994469  5.010450

A2 <- 1:nrow(x)  # Wrong
head(A2)
# [1] 1 2 3 4 5 6

A3 <- sweep(x, 2, 1:nrow(x),"+")  # Wrong
head(A3)
#           [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]     [,8]      [,9]    [,10]
# [1,]  1.387658  2.849264  1.122316  5.518089  3.958962  5.381933  7.789022  8.47918  8.580427 10.28450
# [2,] 10.091690 12.636535 13.154773 15.645945 14.647044 15.707624 15.573485 17.53764 18.607451 17.76056
# [3,] 21.095290 23.218780 23.607475 25.323662 24.017453 26.794329 26.110563 28.95954 30.779818 29.73323
# [4,] 31.139392 32.493772 31.397588 34.331287 34.728817 35.628633 37.582596 37.60125 38.928824 39.23526
# [5,] 39.907916 41.744897 42.897604 42.846777 44.351969 46.728777 46.325421 47.39914 48.340328 50.00506
# [6,] 51.234826 51.664924 53.468106 53.014659 55.182390 57.155513 55.785800 58.67317 59.799447 59.01045

A4 <- sweep(x, 1, 1:nrow(x),"+")  # Correct
head(A4)
#          [,1]     [,2]       [,3]     [,4]        [,5]      [,6]      [,7]     [,8]      [,9]     [,10]
# [1,] 1.387658 1.849264 -0.8776842 2.518089 -0.04103821 0.3819328 1.7890221 1.479180 0.5804272  1.284497
# [2,] 1.091690 2.636535  2.1547728 3.645945  1.64704418 1.7076244 0.5734848 1.537639 1.6074513 -0.239440
# [3,] 3.095290 4.218780  3.6074752 4.323662  2.01745271 3.7943291 2.1105627 3.959535 4.7798178  2.733231
# [4,] 4.139392 4.493772  2.3975878 4.331287  3.72881700 3.6286325 4.5825960 3.601246 3.9288243  3.235261
# [5,] 3.907916 4.744897  4.8976040 3.846777  4.35196924 5.7287769 4.3254209 4.399143 4.3403283  5.005058
# [6,] 6.234826 5.664924  6.4681056 5.014659  6.18239038 7.1555130 4.7857997 6.673166 6.7994469  5.010450

# Q4 - Which of the following lines of code would add the scalar 1 to column 1,
# the scalar 2 to column 2, and so on, for the matrix x? Select ALL that apply.

A1 <- 1:ncol(x)
head(A1)
A2 <- 1:col(x)
head(A2)
A3 <- sweep(x, 2, 1:ncol(x), FUN = "+")  # Correct answer
head(A3)
A4 <- -x
head(A4)

# Q5 - Answer is rowMeans()

# Q6 - For each digit in the mnist training data, compute the proportion of
# pixels that are in the grey area, defined as values between 50 and 205. (To
# visualize this, you can make a boxplot by digit class.)

mnist <- read_mnist()
class(mnist$train$images)
x <- mnist$train$images
y <- mnist$train$labels

avg <- rowMeans(x)

data.frame(labels = as.factor(y), row_averages = avg) %>%
     ggplot(aes(labels, row_averages)) +
     geom_boxplot()

# What proportion of pixels are in the grey area overall, defined as values
# between 50 and 205?

# Do directly the mean of the matrix once it is binarised

mnist <- read_mnist()
class(mnist$train$images)

# x will be a matrix of 60,000 rows by 784 columns (one row per
# character (number 0-9) - each value is the RGB value of a pixel).
# Value are between 0 (white) and 255 (some color). 
x <- mnist$train$images 

# y will be a value of 0-9, or what number is defined by the pixels
# for a given row in x
y <- mnist$train$labels

# Binarize the data points - establish the copies
new_x <- x
new_x[x<50 | x>205] <- 0 
new_x[x>=50 & x<=205] <- 1

mean(new_x)
# [1] 0.06275886
