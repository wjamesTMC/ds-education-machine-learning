# --------------------------------------------------------------------------------
#
# Model Fitting and Recommendation Systems - variable importance and ensembles
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

#----------------------------------------------------------------------------
#
# Bring over code from prior videos
#
#----------------------------------------------------------------------------

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
colnames(x)      <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)
control <- trainControl(method = "cv", number = 10, p = 0.9)
train_knn <- train(x[,col_index], y,
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = 0.9)
train_knn <- train(x[index, col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
fit_knn <- knn3(x[ ,col_index], y, k = 3)
y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type =  "class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

control <- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5), predFixed = c(10, 15, 25, 35, 50))

train_rf <- train(x[ , col_index], y,
                  method = "Rborist",
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)

ggplot(train_rf)

train_rf$bestTune
fit_rf <- Rborist(x[, col_index], y,
nTree = 1000,
minNode = train_rf$bestTune$minNode,
predFixed = train_rf$bestTune$predFixed)
y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat, y_test)
cm$overall["Accuracy"]

#----------------------------------------------------------------------------
#
# Variable Importance
#
#----------------------------------------------------------------------------

# Earlier we described that one of the limitations of random forest is that
# they're not very interpretable. However, the concept of variable importance
# helps a little bit in this regard. Unfortunately, the current implementation
# of the Rborist package does not yet support variable importance calculations.

# So to demonstrate the concept the variable importance, we're going to use the
# random forest function in the random forest package. Furthermore, we're not
# going to filter any columns out of the feature matrix out. We're going to use
# them all. So the code will look like this.

library(randomForest)
rf <- randomForest(x, y,  ntree = 50)

# Once we run this, we can compute the importance of each feature using the
# important function. So we would type something like this.

imp <- importance(rf)
imp
# MeanDecreaseGini
# 1         0.00000000
# 2         0.00000000
# 3         0.00000000
# 4         0.00000000 Etc. -> 784 rows

# If you look at the importance, we immediately see that the first few features
# have zero importance. They're never used in the prediction algorithm. This
# makes sense because these are the features on the edges, the features that
# have no writing in them, no dark pixels in them.

# In this particular example, it makes sense to explore the importance of this
# feature using an image. We'll make an image where each feature is plotted in
# the location of the image where it came from. So we can use this code.

image(matrix(imp, 28, 28))

# And we see where the important features are. It makes a lot of sense. They're
# in the middle, where the writing is. And you can kind of see the different
# numbers there-- six, eight, seven. These are the features that distinguish one
# digit from another. An important part of data science is visualizing results
# to discern why we're failing. How we do this depends on the application. For
# the examples with the digits, we'll find digits for which we were quite
# certain of a call, but it was incorrect. We can compare what we got with
# k-nearest neighbors to what we got with random forest. So we can write code
# like this and then make images of the cases where we made a mistake. Here they
# are.

# [SEE EXAMPLE GRID IN BOOK]

# That first one was called a zero. It's actually a two. You can kind of see
# why. The second one was called a four, but it's a six. That one you definitely
# see why we made a mistake, et cetera. But by looking at these images, you
# might get ideas of how you could improve your algorithm. We can do the same
# for random forest. Here are the top 12 cases where we were very sure it was
# one digit, when it was, in fact, another.

#----------------------------------------------------------------------------
#
# Ensembles
#
#----------------------------------------------------------------------------

# A very powerful approach in machine learning is the idea of ensembling
# different machine learning algorithms into one. Let's explain what we mean by
# that.

# The idea of an ensemble is similar to the idea of combining data from
# different pollsters to obtain a better estimate of the true support for
# different candidates.

# In machine learning, one can usually greatly improve the final result of our
# predictions by combining the results of different algorithms.

# Here we present a very simple example, where we compute new class
# probabilities by taking the average of the class probabilities provided by
# random forest and k-nearest neighbors.

p_rf <- predict(fit_rf, x_test[,col_index])$census  
p_rf<- p_rf / rowSums(p_rf)
p_knn  <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)$overall["Accuracy"]
# Accuracy 
#    0.966

confusionMatrix(y_pred, y_test)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1   2   3   4   5   6   7   8   9
# 0 100   0   1   1   1   0   0   0   0   0
# 1   0 106   2   0   3   0   0   1   1   0
# 2   0   0  98   2   0   0   0   0   1   0
# 3   0   0   0  84   0   1   0   0   2   0
# 4   0   0   0   0 102   0   1   0   0   1
# 5   0   0   0   1   0  95   0   0   1   2
# 6   0   0   0   0   0   1  95   0   0   0
# 7   0   0   2   0   1   0   0  95   0   2
# 8   0   0   0   0   0   0   0   0  92   0
# 9   0   1   0   0   4   1   0   0   0  99
# 
# Overall Statistics
# 
# Accuracy : 0.966           
# 95% CI : (0.9528, 0.9763)
# No Information Rate : 0.111           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9622          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            1.0000   0.9907   0.9515   0.9545   0.9189   0.9694   0.9896   0.9896   0.9485   0.9519
# Specificity            0.9967   0.9922   0.9967   0.9967   0.9978   0.9956   0.9989   0.9945   1.0000   0.9933
# Pos Pred Value         0.9709   0.9381   0.9703   0.9655   0.9808   0.9596   0.9896   0.9500   1.0000   0.9429
# Neg Pred Value         1.0000   0.9989   0.9944   0.9956   0.9900   0.9967   0.9989   0.9989   0.9945   0.9944
# Prevalence             0.1000   0.1070   0.1030   0.0880   0.1110   0.0980   0.0960   0.0960   0.0970   0.1040
# Detection Rate         0.1000   0.1060   0.0980   0.0840   0.1020   0.0950   0.0950   0.0950   0.0920   0.0990
# Detection Prevalence   0.1030   0.1130   0.1010   0.0870   0.1040   0.0990   0.0960   0.1000   0.0920   0.1050
# Balanced Accuracy      0.9983   0.9914   0.9741   0.9756   0.9583   0.9825   0.9942   0.9920   0.9742   0.9726

# We can use this code to simply average these probabilities. And we can see
# that once we do this, when we form the prediction, we actually improve the
# accuracy over both k-nearest neighbors and random forest.

# Now, notice, in this very simple example, we ensemble just two methods. In
# practice, we might ensemble dozens or even hundreds of different methods. And
# this really provides substantial improvements.

