# --------------------------------------------------------------------------------
#
# Comprehension Check - Practice with machine learning
#
# --------------------------------------------------------------------------------

# Setup Libraries
library(dslabs)
library(caret)
data(iris)

iris <- iris[-which(iris$Species == 'setosa'),]
y <- iris$Species

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

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

# Begin examining the accuracy of the cutoffs [START FROM HERE...]
accuracy <- map_dbl(cutoff_sl, function(x){
     y_hat <- ifelse(train > x, "versicolor", "virginica") %>%
          factor(levels = levels(test))
     mean(y_hat == train$Species)
})

