# --------------------------------------------------------------------------------
#
# Comprehension Check - Overtraining and over-smoothing
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

# Q1 - Previously, we used logistic regression to predict sex based on height.
# Now we are going to use knn to do the same. Use the code described in these
# videos to select the F_1 measure and plot it against k. Compare to the F_1 of
# about 0.6 we obtained with regression. Set the seed to 1.

# Part A: What is the max value of F_1?
# Part B: At what value of k does the max occur?

library(tidyverse)
library(caret)
library(dslabs)

data(heights)

ks <- seq(1, 101, 3)
y <-heights$sex
x <- heights$height
set.seed(1)

accuracy <- map_df(ks, function(k) {
     
     test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
     
     train_set<- heights[-test_index,]
     test_set<- heights[test_index,]
     
     fit<- knn3(sex~height, data=train_set, k=k)
     y_hat<-predict(fit, test_set, type="class")
     F_val <- F_meas(data = y_hat, reference = factor(test_set$sex))
     list(k = k, F_val = F_val)
})

# Print out the whole tibble
print(tbl_df(accuracy), n=100)
# A tibble: 34 x 2
# k F_val
#   <dbl> <dbl>
#  1     1 0.567
# 2     4 0.516
# 3     7 0.529
# 4    10 0.569
# 5    13 0.576
# 6    16 0.554
# 7    19 0.548
# 8    22 0.609
# 9    25 0.525
# 10    28 0.587
# 11    31 0.540
# 12    34 0.630 THIS IS THE CORRECT ANSWSER FOR PART B - 34
# 13    37 0.545
# 14    40 0.571
# 15    43 0.599
# 16    46 0.618
# 17    49 0.618
# 18    52 0.569
# 19    55 0.609
# 20    58 0.582
# 21    61 0.603
# 22    64 0.584
# 23    67 0.582
# 24    70 0.538
# 25    73 0.492
# 26    76 0.537
# 27    79 0.497
# 28    82 0.573
# 29    85 0.497
# 30    88 0.489
# 31    91 0.484
# 32    94 0.589
# 33    97 0.558
# 34   100 0.556

max(accuracy$F_val)
# [1] 0.6019417  

# The above is the correct answer for Part A.
# You run the example with the partition and the indexes OUTSIDE
# the function to get the correct F_val and INSIDE the function to
# get the correct k value. Crazy.

accuracy %>% ggplot(aes(k, F_val)) + geom_line()

# --------------------------------------------------------------------
# Q2 - Next we will use the same gene expression example used in the
# Comprehension Check: Distance exercises. You can load it like this:
# --------------------------------------------------------------------
     
library(dslabs)
data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

# Split the data into training and test sets, and report the accuracy you
# obtain. Try it for k = 1, 3, 5, 7, 9, 11. Set the seed to 1.

train_index <- createDataPartition(y, times = 1, list = FALSE)

train_set_x = x[train_index,] 
test_set_x  = x[-train_index,] 
train_set_y = y[train_index] 
test_set_y  = y[-train_index]

ks <- seq(1, 11, 2)
accuracy <- map_df(ks, function(k) {
     set.seed(1)
     knn_fit <- knn3(train_set_x,train_set_y, k = k) 
     y_hat <- predict(knn_fit, test_set_x, type = "class") 
     test_error <- confusionMatrix(data = y_hat, reference = test_set_y, mode = "everything")$overall["Accuracy"] 
     list(k = k, test = test_error) 
}) 

accuracy
print(as.matrix(accuracy))
sum(accuracy$test)

accuracy %>% ggplot(aes(k, test)) + geom_line()

# Correct answers
#       k      test
# [1,]  1 
# [2,]  3 
# [3,]  5 0.9892473
# [4,]  7 
# [5,]  9 
# [6,] 11 
