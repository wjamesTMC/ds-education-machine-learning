# --------------------------------------------------------------------------------
#
# Caret Package - Introduction
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

# We have already learned about regression, logistic regression, and k-nearest
# neighbors as machine-learning algorithms. In later sections, we learn several
# others. And this is just a small subset of all the algorithms out there. Many
# of these algorithms are implemented in R. However, they are distributed via
# different packages, developed by different authors, and often use different
# syntax. The caret package tries to consolidate these differences and provide
# consistency.

# It currently includes 237 different methods, which are summarized in the
# following site. 

#    https://topepo.github.io/caret/available-models.html

# We'll include the link in the courseware. Note that caret does not
# automatically install the packages needed to run these methods.

# So to implement a package through caret, you still need to install the
# library. The required package for each method is included in this page. 

#    https://topepo.github.io/caret/train-models-by-tag.html

# The caret package also provides a function that performs cross-validation for
# us. Here we provide some examples showing how we use this incredibly helpful
# package. We will use the two-or-seven example to illustrate this. You can load
# it like this.

data("mnist_27")

# The train function lets us train different algorithms using similar
# syntax.

# So for example, we can train a logistic regression model or a k-NN model using
# very similar syntax, like this.

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

# Now, to make predictions, we can use the output of this function directly
# without needing to look at the specifics of predict.glm or predict.knn.

# Instead, we can learn how to obtain predictions from the predict.train
# function. Once we read this help page, we know how to use a predict function
# for these objects. Here is the code to get the predictions for the logistic
# regression and the k-NN.

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

# Notice that the syntax is very similar. We can also very quickly study the
# confusion matrix. For example, we can compare the accuracy of both these
# methods like this.

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]
