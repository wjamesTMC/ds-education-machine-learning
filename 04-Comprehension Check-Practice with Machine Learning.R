# --------------------------------------------------------------------------------
#
# Comprehension Check - Practice with machine learning
#
# --------------------------------------------------------------------------------

# Setup Libraries
library(dslabs)
library(caret)
library(dplyr)
library(ggplot2)
library(tidyverse)
data(iris)

iris <- iris[-which(iris$Species == 'setosa'),]
y <- iris$Species

# Question 1 - what line of code is missing (answer - line 3)
set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Question 2: figure out the signular feature in the dataset that yields the
# greatest overall accuracy.

# FYI - plots of the petal width vs. petal length for the two species
iris %>% ggplot(aes(Sepal.Length, Sepal.Width, col=Species)) + geom_point()
iris %>% ggplot(aes(Sepal.Width, Sepal.Length, col=Species)) + geom_point()
iris %>% ggplot(aes(Petal.Length, Petal.Width, col=Species)) + geom_point()
iris %>% ggplot(aes(Petal.Width, Petal.Length, col=Species)) + geom_point()

# Establish the minimums and maximums for each of the predictors
min_sl <- min(train$Sepal.Length) # Value is 5.0
max_sl <- max(train$Sepal.Length) # value is 7.9

min_sw <- min(train$Sepal.Width)  # Value is 2.0
max_sw <- max(train$Sepal.Width)  # Value is 3.8

min_pl <- min(train$Petal.Length) # Value is 3.0
max_pl <- max(train$Petal.Length) # Value is 6.9

min_pw <- min(train$Petal.Width)  # Value is 1.0
max_pw <- max(train$Petal.Width)  # Value is 2.5

# Establish the cutoff ranges
cutoff_sl <- seq(min_sl, max_sl)
cutoff_sw <- seq(min_sw, max_sw)
cutoff_pl <- seq(min_pl, max_pl)
cutoff_pw <- seq(min_pw, max_pw)

# Begin examining the accuracy of the cutoffs - Sepal.Length
sl_accuracy <- map_dbl(cutoff_sl, function(x){
     y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == train$Species)
})
max(sl_accuracy)
# [1] 0.66

# Next is Sepal Width
sw_accuracy <- map_dbl(cutoff_sw, function(x){
     y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == train$Species)
})
max(sw_accuracy)
# [1] 0.54

# Petal Length
pl_accuracy <- map_dbl(cutoff_pl, function(x){
     y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == train$Species)
})
max(pl_accuracy)
# [1] 0.9

# Petal Width
pw_accuracy <- map_dbl(cutoff_pw, function(x){
     y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == train$Species)
})
max(pw_accuracy)
# [1] 0.72

# Calculating the best cutoff for Petal Length, the more accurate determinator
best_cutoff <- cutoff_pl[which.max(pl_accuracy)]
best_cutoff
# [1] 5
best_cutoff <- cutoff_pw[which.max(pw_accuracy)]
best_cutoff
# [1] 2 (but on the graph it is 1.75)

# Calculating the overall accuracy in the test data
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>%
     factor(levels = levels(test$Species))

mean(y_hat == test$Species)

# Now let's see if the TEST SET data suggests a better cutoff and which 

# Begin examining the accuracy of the cutoffs - Sepal.Length
sl_accuracy <- map_dbl(cutoff_sl, function(x){
     y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == test$Species)
})
max(sl_accuracy)
# [1] 0.76  [improved from 0.66]

# Next is Sepal Width
sw_accuracy <- map_dbl(cutoff_sw, function(x){
     y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == test$Species)
})
max(sw_accuracy)
# [1] 0.64  [Improved from 0.54]

# Petal Length
pl_accuracy <- map_dbl(cutoff_pl, function(x){
     y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == test$Species)
})
max(pl_accuracy)
# [1] 0.9  [Unchanged]

# Petal Width
pw_accuracy <- map_dbl(cutoff_pw, function(x){
     y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == test$Species)
})
max(pw_accuracy)
# [1] 0.74  [Improved from 0.72]

# Calculating the best cutoff for Petal Length, the most accurate determinator
best_cutoff <- cutoff_pl[which.max(pl_accuracy)]
best_cutoff
# [1] 5  [Unchanged]

# Calculating the overall accuracy in the test data
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>%
     factor(levels = levels(test$Species))

mean(y_hat == test$Species)
# [1] 0.9  [Unchanged]

# Note - my calculations indicate that Sepal.Length improved to 0.76 while
# Petal.Width improved to only 0.74. However, the grader says that Petal.Width
# improves the accuracy more.

# Question 5: Now we will perform some exploratory data analysis on the data.
# Notice that Petal.Length and Petal.Width in combination could potentially be
# more information than either feature alone.
#
# Optimize the combination of the cutoffs for Petal.Length and Petal.Width in
# the train data and report the overall accuracy when applied to the test
# dataset. For simplicity, create a rule that if either the length OR the width
# is greater than the length cutoff or the width cutoff then virginica or
# versicolor is called. (Note, the F1 will be similarly high in this example.)

# Petal Length
pl_accuracy <- map_dbl(cutoff_pl, function(x){
     y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == train$Species)
})
max(pl_accuracy)
# [1] 0.9

# Petal Width
pw_accuracy <- map_dbl(cutoff_pw, function(x){
     y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == train$Species)
})
max(pw_accuracy)
# [1] 0.72

# Calculating the best cutoff for Petal Length, the more accurate determinator
best_cutoff_pl <- cutoff_pl[which.max(pl_accuracy)]
best_cutoff_pl
# [1] 5
best_cutoff_pw <- cutoff_pw[which.max(pw_accuracy)]
best_cutoff_pw

# Create a rule that if either the length OR the width
# is greater than the length cutoff or the width cutoff then virginica or
# versicolor is called. (Note, the F1 will be similarly high in this example.)
#
# What is the overall accuracy for the test data now?
# First, develop the algoritm using the training set

y_hat <- ifelse(train$Petal.Length > 4.7 & train$Petal.Width > 1.75, "virginica","versicolor") %>% factor(levels = levels(train$Species))
mean(y_hat==train$Species)
# [1] 0.94

# Then apply this to the test set
y_hat <- ifelse(test$Petal.Length > 4.7 & test$Petal.Width > 1.75, "virginica","versicolor") %>% factor(levels = levels(test$Species))
mean(y_hat==test$Species)

# [1] 0.92

# Note - need to figure out WHY I showed 5 and 2 for the cutoffs and others got 4.7 and 1.5. The graphics at the top of this file indicated that 4.7 would be a good choice. Maybe looking at the plots is the best way?
