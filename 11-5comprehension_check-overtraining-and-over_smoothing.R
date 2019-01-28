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

# What is the max value of F_1?
# At what value of k does the max occur?

#
# *** Help from discussion forums ***
#

# So, used my last attempt and got the 'answer' - apparently you do have to use
# seq(1, 101, 3), put your set.seed outside the function and the
# createDataPartition() block inside the function.

# Allow me to add one more hint for Q1: the grader does not expect you to try k=
# all numbers from 1 to 101 (1, 2, 3...); they're counting by 3s (1, 4, 7).
# Thus, thy grader does not find the truly correct answer because its test set
# does not include the associated k value. To get the answer the grader expects,
# use ks <- seq(1, 101, 3)`

# The correct answer for Q1 in terms of the K value is 38.... seq(1,101,3) does
# not contain the correct value of 38

# Just why in videos we do not use sapply, but we are expected to use it in the
# exercise?

# I solved both Q1 and Q2 with createDataPartition being before map_dbl

#
# Some sample code
#
library(tidyverse)
library(caret)
library(dslabs)

data(heights)

ks <- seq(1, 101, 1)
y<-heights$sex
x<- heights$height
set.seed(1)

test_index<- createDataPartition(y, times=1, p=0.5, list=FALSE)
train_set<- heights %>% slice(-test_index,)
test_set<- heights %>% slice(test_index,)

set.seed(1)

accuracy <- map_df(ks, function(k) {
})

accuracy %>% print(n=1000)
accuracy %>% ggplot(aes(k,F_val)) + geom_line()

#
# More Sample code
#
     
library(tidyverse)
library(caret)
library(dslabs)

data(heights)
set.seed(1)
ks <- seq(1, 101, 3)
y <-heights$sex
x <- heights$height

f1 <- sapply(ks, function(k) {
     
     test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
     train_set <- heights[test_index]
     test_set <- heights[-test_index]
     fit <- knn3(sex ~ height, .....)
     yhat <- predict(fit, test, type = ...) %>% factor(level = level(.....))
     fmea(data = yhat, .....)
     
})

plot(ks, f1)

# Q2 - Next we will use the same gene expression example used in the
# Comprehension Check: Distance exercises. You can load it like this:

library(dslabs)
data("tissue_gene_expression")

# Split the data into training and test sets, and report the accuracy you
# obtain. Try it for k = 1, 3, 5, 7, 9, 11. Set the seed to 1.

#
# *** Help from discussion forums ***
#

# In Q2 you create train_index with createDataPartition and createDataPartition
# is used before sapply. So here you have just one split of data (which is
# correct approach), then for this split you check which of various k values
# works the best.

# Use index rather than -index when creating test data - ie
#              tissue_gene_expression$x[test_index,] 
# Use 3 decimal places for the answers

# Apparently you do have to use seq(1, 101, 3) Put your set.seed # outside the
# function and the createDataPartition() block inside the function. 

# Allow me to add one more hint for Q1: the grader does not expect you to try k=
# all numbers from 1 to 101 (1, 2, 3...); they're counting by 3s (1, 4, 7).
# Thus, thy grader does not find the truly correct answer because its test set
# does not include the associated k value. To get the answer the grader expects,
# use ks <- seq(1, 101, 3)`

# I solved both Q1 and Q2 with createDataPartition being before map_dbl

# 
# Some sample code 
#

