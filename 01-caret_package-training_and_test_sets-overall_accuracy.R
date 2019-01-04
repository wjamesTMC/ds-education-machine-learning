# --------------------------------------------------------------------------------
#
# Caret package, training and test sets, and overall accuracy
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

# We start by briefly introducing the caret package, which has several useful
# functions for building and assessing machine learning methods.
#
# Later in this chapter we provide more details on this package.
#
# In this section, we focus on describing ways in which machine learning
# algorithms are evaluated. For our first introduction to machine learning
# concepts, we will start with a boring and simple example: how to predict sex
# using height. As we explain machine learning step by step, this example will
# let us set down the first building block. Soon enough we will be attacking
# more interesting challenges.

# For this first example, we use the height data in dslabs:
     
data(heights)

# We start by defining the outcome and predictors. In this case, we have only
# one predictor:
     
y <- heights$sex
x <- heights$height

# This is clearly a categorical outcome since Y can be Male or Female and we
# only have one predictor: height. We know that we will not be able to predict Y
# very accurately based on X because male and female average heights are not
# that different relative to within group variability. But can we do better than
# guessing? To answer this question, we need a quantitative definition of
# better.

# Training and test sets 
#
# Ultimately, a machine learning algorithm is evaluated on how it performs in
# the real world with others running the code. However, when developing an
# algorithm, we usually have a dataset for which we know the outcomes, as we do
# with the heights: we know the sex of every student. Therefore, to mimic the
# ultimate evaluation process, we typically split the data into two and act as
# if we don’t know the outcome for one of these two sets. We stop pretending we
# don’t know the outcome to evaluate the algorithm, but only after we are done
# constructing it. We refer to the group for which we know the outcome and use
# it to develop the algorithm as the training set, and the group for which we
# pretend we don’t know the outcome as the test set.
#
# A standard way of generating the training and test sets is by randomly
# splitting the data. The caret package includes the function
# createDataPartition that helps us generates indexes for randomly splitting the
# data into training and test sets:

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

# The argument times is used to define how many random samples of indexes to
# return, the argument p is used to define what proportion of the data is
# represented by the index, and the argument list is used to decide if we want
# the indexes returned as a list or not. We can use the result of the function
# call to define the training and test sets like this:
      
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

# We will now develop an algorithm using only the training set. Once we are done
# developing the algorithm, we will freeze it and evaluate it using the test
# set. The simplest way to evaluate the algorithm when the outcomes are
# categorical is simply by reporting the proportion of cases that were correctly
# predicted in the test set. This metric is usually referred to as overall
# accuracy.

# Overall accuracy
#
# To demonstrate the use of overall accuracy, we will build two competing
# algorithms and compare them.
#
# Let’s start by developing the simplest possible machine algorithm: guessing
# the outcome.
 
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)

# Note that we are completely ignoring the predictor and simply guessing the
# sex.
 
# In machine learning applications, it is useful to use factors to represent the
# categorical outcomes. R functions developed for machine learning, such
# as those in the caret package, require or recommend that categorical outcomes
# be coded as factors. So we can do that like this:
      
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
     factor(levels = levels(test_set$sex))
y_hat
# [Output is a list of n entries (depnding on p) and the two factors, Male and
# Female]

# The overall accuracy is simply defined as the overall proportion that is
# predicted correctly:
      
mean(y_hat == test_set$sex)
# [1] 0.5238095

# Not surprisingly, our accuracy is about 50%. We are guessing!
#      
# Can we do better? Exploratory data analysis suggests we can because, on
# average, males are slightly taller than females:

heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
#   A tibble: 2 x 3
#   sex    `mean(height)` `sd(height)`
#   <fct>           <dbl>        <dbl>
# 1 Female           64.9         3.76
# 2 Male             69.3         3.61

# But how do we make use of this insight? Let’s try another simple approach:
# predict Male if height is within two standard deviations from the average
# male:
     
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))

# The accuracy goes up from 0.50 to about 0.80:
     
mean(y == y_hat)
# [1] 0.7933333

# But can we do even better? In the example above, we used a cutoff of 62 inches, but
# we can examine the accuracy obtained for other cutoffs and then pick the value
# that provides the best results. But remember, it is important that we optimize
# the cutoff using only the training set: the test set is only for evaluation.
# Although for this simplistic example it is not much of a problem, later we
# will learn that evaluating an algorithm on the training set can lead to
# overfitting, which often results in dangerously over-optimistic assessments.

# Here we examine the accuracy of 10 different cutoffs and pick the one yielding
# the best result:
     
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
     y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
          factor(levels = levels(test_set$sex))
     mean(y_hat == train_set$sex)
})

# We can make a plot showing the accuracy on the training set for males and females
# NEED TO DETERMINE CORRECT GGPLOT SYNTAX
max(accuracy) %>% ggplot(cutoff, accuracy, data =., xlab = "Cutoff", ylab = "Accuracy") + geom_line()
     
# We see that the maximum value is:
     
max(accuracy)
#> [1] 0.8361905

# which is much higher than 0.5. The cutoff resulting in this accuracy is:
     
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
#> [1] 64

# Now we can now test this cutoff on our test set to make sure our accuracy is
# not overly optimistic:
     
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
     factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)

mean(y_hat == test_set$sex)
#> [1] 0.8171429

# NOTE: The function factor is used to encode a vector as a factor (the terms
# ‘category’ and ‘enumerated type’ are also used for factors). If argument
# ordered is TRUE, the factor levels are assumed to be ordered. For
# compatibility with S there is also a function ordered.

# We see that it is a bit lower than the accuracy observed for the training set,
# but it is still better than guessing. And by testing on a dataset that we did
# not train on, we know it is not due to cherry-picking a good result.

