# --------------------------------------------------------------------------------
#
# Overtraining and over-smoothing
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

# See bottom of file for prior units code

# Now to see why we improved over logistic regression in this case that we only
# have two predictors, we can actually make some visualizations. Here is the
# true conditional probability on the left. And on the right, you can see the
# estimate that we obtained with knn with five neighbors. So you see that the
# estimate has the essence of the shape of the true conditional probability.
# Therefore we do better than logistic regression. However, we can probably do
# better. Because if you look closely, this estimate, we see some isles of blue
# in the red areas. Intuitively this does not make much sense. Why are they on
# their own like that?

# This is due to what we call overtraining. To understand what overtraining is,
# notice that we have higher accuracy when we predict on a training set than we
# compare on a test set. We can do it using this code.

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, 
                reference = mnist_27$train$y)$overall["Accuracy"]
# Accuracy 
#    0.8825

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
# Accuracy 
#    0.815

# You can see that the accuracy computed on the training side is quite higher.
# It's 0.882 compared to what we get on the test set, which is only 0.815. This
# is because we overtrained. Overtraining is at its worst when we set k equals
# to 1. With k equals to 1, the estimate for each point in the training set is
# obtained with just the y corresponding to that point because you are your
# closest neighbor. So in this case, we obtain practically perfect accuracy in
# the training set because each point is used to predict itself.
 
# Perfect accuracy will occur when we have unique predictors, which we almost do
# have here. So you can see that when we use a k equals to 1,

knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, 
                reference=mnist_27$train$y)$overall["Accuracy"]
# Accuracy 
#    0.995

# Our accuracy on a training set is 0.995, almost perfect accuracy.
# However, when we check on the test set, the accuracy is actually worse than
# with logistic regression. It's only 0.735. We can see that using this code.
 
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall["Accuracy"]
# Accuracy 
#    0.735

# To see the over-fitting in a figure, we can plot the data and then use
# contours to show what divides the twos from the sevens. And this is what you
# get when you use k equals 1. Notice all the little islands that in the
# training set fit the data perfectly. You'll have this little red point on its
# own, and a little island will be formed around it so that you get the perfect
# prediction. But once you look at the test set, that point is gone. There's no
# red there anymore. Now there's perhaps a blue, and you make a mistake.
 
# The estimated conditional probability followed the training data too closely.
# Although it's not as bad, we see this overtraining with k equals 5, or the
# default. So we should consider a larger k. Let's try an example. Let's try a
# much larger example. Let's try 401. We can fit the model just by simply
# changing the k to 401 like this.

knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]
# Accuracy 
#     0.79

# We see that the accuracy on a test set is only 0.79, not very good, a similar
# accuracy to logistic regression.

fit <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
library(caret)

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
     geom_raster() +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(breaks=c(0.5),color="black")

p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
     ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
     geom_raster() +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(breaks=c(0.5),color="black") 

# Need to adapt the code below to produce the plot in the book
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
     ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
     geom_raster() +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(breaks=c(0.5),color="black") 

# In fact, the estimates actually look quite similar. On the left is logistic
# regression. On the right is k-nearest neighbors with k equals 401.

# The size of k is so large that it does not permit enough flexibility. We're
# almost including half the data to compute each single estimated conditional
# probability. We call this oversmoothing.

# So how do we pick k? Five seems to be too small. 401 seems to be too big.
# Something in the middle might be better. So what we can do is we can repeat
# what we just did for different values of k So we can try all the odd numbers
# between 3 and 251.

ks <- seq(3, 251, 2)

# And we'll do this using the map df function to repeat what we just did for
# each k. For comparative purposes, we will compute the accuracy by using both
# training set-- that's incorrect. We shouldn't do that, but just for comparison
# we're going to do it-- and the test set, which is the correct way to do it.
# The code looks simply like this.

library(purrr)
accuracy <- map_df(ks, function(k){
     fit <- knn3(y ~ ., data = mnist_27$train, k = k)
     
     y_hat <- predict(fit, mnist_27$train, type = "class")
     cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
     train_error <- cm_train$overall["Accuracy"]
     
     y_hat <- predict(fit, mnist_27$test, type = "class")
     cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
     test_error <- cm_test$overall["Accuracy"]
     
     list(train = train_error, test = test_error)
})
# Once we run that code, we can now plot the accuracy against the value of k,
# and that looks like this.

# NEED CODE FOR PLOT

# First, note that the accuracy versus k plot is quite jagged. We don't not
# expect this because small changes in k should not affect the algorithm's
# performance too much. The jaggedness is explained by the fact that the
# accuracy is computed on this sample and therefore is a random variable.

# This demonstrates why we prefer to minimize the expectation loss, rather than
# the loss we observe with one dataset. We will soon learn a better way of
# estimating this expected loss.

# Now, despite the noise present in the plot, we still see a general pattern.
# Low values of k give low test set accuracy but high train set accuracy, which
# is evidence of overtraining. Large values of k result in low accuracy, which
# is evidence of oversmoothing. The maximum is achieved somewhere between 25 and
# 41. And the maximum accuracy is 0.85, substantially higher than logistic
# regression. In fact, the resulting estimate with k equals to 41 looks quite
# similar to the true conditional probability, as we see in this plot.

plot_cond_prob <- function (p_hat) {
     GS <- 150
     new_x <- expand.grid(x_1 = seq(0, 0.5, len=GS), x_2 = seq(0, 0.5, len=GS)) new_x %>% mutate(p = p_hat) %>%
          ggplot(aes(x_1, x_2, z = p, fill = p)) + geom_raster() +
          scale_fill_gradientn(colors = c("#F8766D", "white", "#00BFC4")) +
                    stat_contour(breaks = c(0.5), color = "black")
}
p1 <- plot_cond_prob() + ggtitle("True conditional probability")

knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 41)
p2 <- plot_cond_prob(predict(knn_fit, newdata = mnist_27$true_p)[,2]) +
     ggtitle("kNN-41 estimate")
grid.arrange(p1, p2, nrow=1)

# Now, is an accuracy of 0.85 what we should expect if we apply this algorithm
# in the real world? The answer is actually no because we broke a golden rule of
# machine learning. We selected the k using the test set. So how do we select
# the k? In the next videos, we introduce the important concept of cross
# validation, which provides a way to estimate the expected loss for a given
# method using only the training set.

max(accuracy$test)
#> [1] 0.86

# ----------------------------------------------------------------------------
#
# Background code
#
# ----------------------------------------------------------------------------

data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) +
     geom_point()

library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]
#> Accuracy 
#>     0.76

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
     geom_raster() 

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
     geom_raster() +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(breaks=c(0.5),color="black")

p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
     ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
     geom_raster() +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(breaks=c(0.5),color="black") 
