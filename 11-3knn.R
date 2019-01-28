# --------------------------------------------------------------------------------
#
# knn
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

# In this video, we will learn our first machine learning algorithm, the
# k-nearest neighbors algorithm. To demonstrate it, we're going to use the
# digits data with two predictors that we created in a previous video.

data("mnist_27")
mnist_27$test%>% ggplot(aes(x_1, x_2, color = y)) +
     geom_point()

# K-nearest neighbors is related to smoothing. To see this, we will think about
# the conditional probably, the probably of being a seven, y equals 1, given the
# two predictors.

# p(x1,x2)=Pr(Y=1∣X1=x1,X2=x2)

# This is because the zeros and ones we observe are noisy because some of the
# regions of the conditional probability are not close to zero or one, which
# means that you can go either way sometimes. So we have to estimate the
# conditional probability. How do we do this?

# We're going to try smoothing. K-nearest neighbors is similar to bin smoothing.
# But it is easier to adapt to multiple dimensions. We first defined the
# distance between observations based on the features. Basically, for any point
# for which you want to estimate the conditional probability, we look at the
# k-nearest points and then take an average of these points. We refer to the set
# of points used to compute the average as a neighborhood. Due to the connection
# we described earlier between conditional expectations and conditional
# probabilities, this gives us the estimated conditional probability, just like
# bin smoothers gave us an estimated trend.

# We can control, flexibility of our estimate through k.
# Larger Ks result in smaller estimates, while smaller Ks
# result in more flexible and more wiggly estimates.

# So let's implement k-nearest neighbors. We're going to compare it to logistic
# regression, which will be the standard we need to beat. We can write this code
# to compute the glm predictions.

library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]
# Accuracy 
#     0.76

# And notice that we have an accuracy of 0.76. Now let's compare this to knn. We
# will use the function knn3 which comes with the caret package. If we look at
# the help file of this package, we see that we can call it in one of two ways.
# In the first, we specify a formula and a data frame. The data frame contains
# all the data to be used. The formula has the form outcome tilde predictor 1
# plus predictor 1 plus predictor 3 and so on.

# So in this m where we only have two predictors, we would type y-- those are
# the outcomes-- tilde x1 plus x2. But if we're going to use all the predictors,
# we can use a shortcut, and it's the dot. We would type y tilde dot. And that
# says use all the predictors. So the call to knn3 looks simply like this.

knn_fit <- knn3(y ~ ., data = mnist_27$train)
#   2   7 
# 379 421 

# The second way to call this function is that the first argument
# being the matrix predictors and the second, a vector of outcomes.
# So the code would look like this instead.

x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)
#   2   7 
# 379 421 

# We would define our matrix with the predictors. Then when we would define a
# vector with the outcomes. And then we would call it simply like this.

# The reason we have two ways of doing this is because the formula is a quicker,
# simpler way to write it when we're in a hurry. But once we face large data
# sets, we will want to use the matrix approach, the second approach.
 
# All right, now, for this function, we also need to pick a parameter, the
# number of neighbors to include. Let's start with the default, which is k
# equals 5. We can write it explicitly like this.

knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)
#   2   7 
# 379 421 

# Because this data set is balanced - there's many twos as there are sevens -
# and we care just as much about sensitivity as we do about specificity - both
# mistakes are equally bad - we will use accuracy to quantify performance.

# The predict function for this knn function produces either a probability for
# each class, or it could actually produce the outcome that maximizes the
# probability, the outcome with the highest probability.

# So we're going to use the code predict, the fitted object, the new data that
# we're predicting for. That's the test data set. And then we're going to type,
# type equals class. This will give us the actual outcomes that are predicted.
# So once we have this, we can compute our accuracy using the confusion matrix
# formula like this.

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
# Accuracy 
#    0.815

# And we see that we already have an improvement over the logistic regression.
# Our accuracy is 0.815.


