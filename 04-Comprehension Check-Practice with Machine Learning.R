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

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# FYI - plots of the petal width vs. petal length for the two species
iris %>% ggplot(aes(Sepal.Length, Sepal.Width, col=Species)) + geom_point()
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

# Next section - Question 5: Now we will perform some exploratory data analysis
# on the data. Notice that Petal.Length and Petal.Width in combination could
# potentially be more information than either feature alone.
#
# Optimize the combination of the cutoffs for Petal.Length and Petal.Width in
# the train data and report the overall accuracy when applied to the test
# dataset. For simplicity, create a rule that if either the length OR the width
# is greater than the length cutoff or the width cutoff then virginica or
# versicolor is called. (Note, the F1 will be similarly high in this example.)
#
# What is the overall accuracy for the test data now?

# [START HERE]

# Petal Length
pl_accuracy <- map_dbl(cutoff_pl, function(x){
     y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == test$Species)
})
max(pl_accuracy)
# [1] 0.9  [Unchanged]

# Calculating the best cutoff for Petal Length, the more accurate determinator
pl_best_cutoff <- cutoff_pl[which.max(pl_accuracy)]
pl_best_cutoff
# [1] 5  [Unchanged]

# Petal Width
pw_accuracy <- map_dbl(cutoff_pw, function(x){
     y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>%
          factor(levels = levels(test$Species))
     mean(y_hat == test$Species)
})
max(pw_accuracy)
# [1] 0.74  [Improved from 0.72]

# Calculating the best cutoff for Petal Width, the more accurate determinator
pw_best_cutoff <- cutoff_pw[which.max(pw_accuracy)]
pw_best_cutoff
# [1] 2  [Unchanged]

# Calculating the overall accuracy in the test data
y_hat <- ifelse(test$Petal.Length > pl_best_cutoff | test$Petal.Width > pw_best_cutoff, "virginica", "versicolor") %>%
     factor(levels = levels(test$Species))

mean(y_hat == test$Species)
confusionMatrix(data = y_hat, reference = test$Species)
