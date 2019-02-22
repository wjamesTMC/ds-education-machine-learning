# --------------------------------------------------------------------------------
#
# Model Fitting and Recommendation Systems - model fitting mnist
#
# --------------------------------------------------------------------------------

# Setup
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(Lahman)
library(HistData)
library(caret)
library(e1071)
library(matrixStats)
library(Rborist)
library(randomForest)
library(gam)

# Bring over code from prior video
mnist <- read_mnist()
names(mnist)
dim(mnist$train$images)
set.seed(123)
index  <- sample(nrow(mnist$train$images), 10000)
x      <- mnist$train$images[index,]
y      <- factor(mnist$train$labels[index])

index  <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index,]
y_test <- factor(mnist$train$labels[index])

sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))
nzv <- nearZeroVar(x)
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

# In this video, we're going to actually implement k-nearest neighbors and
# random forest on the mnist data.

# However, before we start, we need to add column names to the feature matrices,
# as this is a requirement of the caret package. We can do this using this code.

colnames(x)      <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

# # We're going to add as a name the number of the column. OK, so let's start
# with knn. The first step is to optimize for the number of neighbors. Now, keep
# in mind that when we run the algorithm, we will have to compute a distance
# between each observation in the test set and each observation in the training
# set. These are a lot of calculations.

# We will therefore use k-fold cross-validation to improve speed. So we can
# use the caret package to optimize our k-nearest neighbor algorithm.

control <- trainControl(method = "cv", number = 10, p = 0.9)
train_knn <- train(x[,col_index], y,
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)

# This will find the model that maximizes the accuracy.

# Note that running this code takes quite a bit of time on a standard laptop.
# It could take several minutes. In general, it is a good idea to test out a
# piece of code with a small subset of the data first to get an idea of timing,
# before we start running code that might take hours to run or even days. So
# always performs checks or make calculations by hand to make sure that your
# code isn't going to take too much time. You want to know-- have an idea-- of
# how long your code will take. So one thing you can do is test it out on
# smaller datasets.

# # Here we define n and b as the number of rows that we're going to use and b,
# the number of cross-validation folds that we're going to use.

n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = 0.9)
train_knn <- train(x[index, col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)


# # Then we can start increasing these numbers slowly to get an idea of how long
# the final code will take.

# # Now, once we're done optimizing our algorithm, we can fit the entire
# dataset. So we would code like this.

fit_knn <- knn3(x[ ,col_index], y, k = 3)

# We see that our accuracy is almost 0.95.

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type =  "class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]
# Accuracy 
# 0.954 

# # From the specificity and sensitivity output coming from the confusion matrix
# function, we see that the eights are the hardest to detect, and the most
# commonly incorrect predicted digit seven.

cm$byClass[,1:2]
# Sensitivity Specificity
# Class: 0   0.9800000   0.9966667
# Class: 1   0.9906542   0.9899216
# Class: 2   0.9320388   0.9977703
# Class: 3   0.9431818   0.9967105
# Class: 4   0.9189189   0.9966254
# Class: 5   0.9591837   0.9911308
# Class: 6   0.9895833   0.9966814
# Class: 7   0.9791667   0.9933628
# Class: 8   0.9072165   0.9988926
# Class: 9   0.9423077   0.9910714

# # Now, let's see if we can do even better with random forest. With random
# forest, computation time is even a bigger challenge than with k-nearest
# neighbors. For each forest, we need to build hundreds of trees. We also have
# several parameters that we can tune. Here we use the random forest
# implementation in the Rborist package, which is faster than the one in the
# random forest package.

# # It has less features, but it is faster. Because with random forest, the
# fitting is the slowest part of the procedure rather than the predicting, as
# with knn, we will only use five-fold cross-validation. We'll also reduce the
# number of trees that are fit, since we are not yet building our final model.
# Finally, we'll take a random subset of observations when constructing each
# tree.

# # We can change this number with the nSamp argument in the Rborist function.
# So here's a code to optimize around a random forest.

control <- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5), predFixed = c(10, 15, 25, 35, 50))

train_rf <- train(x[ , col_index], y,
                  method = "Rborist",
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)

# # It takes a few minutes to run. We can see the final results using ggplot.

ggplot(train_rf)

# # And we can choose the parameters using the best tune component of the
# training object.

train_rf$bestTune
# predFixed minNode
# 3        25       1

# And now we're ready to optimize our final tree. Now we're going to set the
# number of trees to a larger number. We can write this piece of code.

fit_rf <- Rborist(x[, col_index], y,
nTree = 1000,
minNode = train_rf$bestTune$minNode,
predFixed = train_rf$bestTune$predFixed)

# Once the code is done running, and it takes a few minutes, we can see, using
# the confusion matrix function, that our accuracy is above 0.95.

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat, y_test)
cm$overall["Accuracy"]
# Accuracy 
# 0.954 

# We have indeed improved over k-nearest neighbors.

# FOR BELOW, NEED TO FIND THE RIGHT CODE FOR THE DISPLAY OF THE INTEGERS

image(matrix(1:784 %in% y_hat_rf, 28, 28))

# Now, let's look at some examples of the original image in the test set in our
# calls. You can see that that first one we called an eight. It's an eight. The
# second is also called an eight. Looks like an eight. And all of them look like
# we made the right call. Not surprising-- we have an accuracy above 0.95.

# Now, note that we have done minimal tuning here. And with some further tuning,
# examining more parameters, growing out more trees, we can get even higher
# accuracy.

