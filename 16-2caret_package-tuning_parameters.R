# --------------------------------------------------------------------------------
#
# Caret Package - tuning parameters
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

# When an algorithm includes a tuning parameter, train automatically uses
# cross-validation to decide among a few default values.

# To find out what parameter or parameters are optimized, you can read this page
# that explains it. We'll include the link in the courseware.

#    http://topepo.github.io/caret/available-models.html

# Or study the output of the following code.

getModelInfo("knn")
# $knn$tags
# [1] "Prototype Models"

# $knn$prob
function (modelFit, newdata, submodels = NULL) 
     predict(modelFit, newdata, type = "prob")

# $knn$levels
function (x) 
     levels(x$learn$y)

# $knn$sort
function (x) 
     x[order(-x[, 1]), ]

# The get model info function can be used to get information of the method
# that you're interested in.

# You can do a quick lookup using the model lookup function like this.

modelLookup("knn")
# model parameter      label forReg forClass probModel
# 1   knn         k #Neighbors   TRUE     TRUE      TRUE

# When we run this code, we see that for knn, the parameter that's optimized
# is k.

# So if we run the function train with default values,

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

# you can quickly see the results of the cross-validation using
# the ggplot function.

# You can use the argument highlight to highlight the parameter that
# optimizes the algorithm. # So you can type this.

ggplot(train_knn, highlight = TRUE)

# By default, the cross-validation is performed by testing on 25 bootstrap
# samples comprised of 25% of the observations. Also, for the knn method, the
# default is to try out k=5, 7, and 9. We already saw that 9 maximizes this. But
# maybe there's another k that's even better.

# So to change this, we need to use the tunegrid parameter in the train
# function.

# The grid of values that are going to be compared must be supplied by a data
# frame with the column names as specified by the parameters that you get in the
# model lookup output. Here we present an example trying out 30 values between 9
# and 67. We need to use a column in k, so the data frame we use is going to be
# this one.

data.frame(k = seq(9, 67, 2))

# Now, note that when running this code, we are fitting 30 versions of knn to 25
# bootstrap samples, so we're fitting 750 knn models. And thus, running this
# code will take several seconds. Here's the code.

train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))

ggplot(train_knn, highlight = TRUE)

# In the plot, we can see the k that maximizes accuracy, but we can also access
# it using this code.

train_knn$bestTune
# k
# 15 37

# The bestTune component gives us the parameter that maximizes the accuracy.
# We can also access the best-performing model using this code.

train_knn$finalModel
# 2   7 
# 379 421 

# Now, if you apply the function predict to the output of the train function, it
# will use this best-performing model to make predictions. Note that the best
# model was obtained using the training set. We did not use the test set at all.
# The cross-validation was performed on the training set. So now, if we want to
# see the accuracy we obtain on the test set, which hasn't been used, we can use
# the following code.

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                        mnist_27$test$y)$overall["Accuracy"]
# Accuracy 
# 0.855 

# Sometimes we like to change the way we perform cross-validation. We might
# change the method, we might change how we do the partitions, et cetera.

# If we want to do this, we need to use a trainControl function. So for example,
# we can make the code that we just showed go a bit faster by using 10-fold
# cross-validation. This means we're going to have 10 validation samples that
# use 10% of the observations each.

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)

ggplot(train_knn_cv, highlight = TRUE)

# Notice that if we plot the estimated accuracy versus k plot, we notice that
# the accuracy estimates are more variable than in the previous example. Now
# this is expected since we changed a number of samples we use to estimate
# accuracy. In the first example, we used 25 bootstrap samples, and in this
# example, we use 10-fold cross-validation.

# One more thing to point out. Note that the train function also provides
# standard deviation values for each parameter that was tested. This is obtained
# from the different validation sets. So we can make a plot like this that shows
# the point estimates of the accuracy along with standard deviations.

train_knn$results %>% 
     ggplot(aes(x = k, y = Accuracy)) +
     geom_line() +
     geom_point() +
     geom_errorbar(aes(x = k, 
                       ymin = Accuracy - AccuracySD, 
                       ymax = Accuracy + AccuracySD))

# To finish this example up, let's notice that the best-fitting knn model
# approximates the true condition of probability pretty well. However, we do see
# that the boundary is somewhat wiggly. This is because knn, like the basic bin
# smoother, does not use a smooth kernel. To improve this, we could try loess.

# By reading through the available models of the caret package, which you can
# get to through this link, which we include in the course material, we see that
# we can use the gamLoess method.

# Also from the caret documentation link you can see that we need to install the
# gam package if we have not done so already.

# Then we will see that we have two parameters to optimize if we use this
# particular method. You can see this with the model lookup function, like this.

modelLookup("gamLoess")
# model parameter  label forReg forClass probModel
# 1 gamLoess      span   Span   TRUE     TRUE      TRUE
# 2 gamLoess    degree Degree   TRUE     TRUE      TRUE

# For this example, we'll keep the degree fixed at one. We won't try out degree
# two. But to try out different values for the span, we still have to include a
# column in the table with the named degree. This is a requirement of the caret
# package. So we would define a grid using the expand.grid function, like this.

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

# Now, we use the default cross-validation control parameters, so we type code
# like this to train our model.  

train_loess <- train(y ~ ., 
                     method = "gamLoess", 
                     tuneGrid=grid,
                     data = mnist_27$train)

# Then, select the best-performing model, and now we can see the final result.

ggplot(train_loess, highlight = TRUE)

# It performs similarly to knn.

confusionMatrix(data =predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]
# Accuracy 
# 0.84

# However, we can see that the conditional probability estimate is indeed
# smoother than what we get with knn.

plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])

# For reference - earlier plots in the text book
p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
     ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
     geom_raster() +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(breaks=c(0.5),color="black")

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
     geom_raster() +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(breaks=c(0.5),color="black")

# Note that not all parameters in machine-learning algorithms are tuned. For
# example, in regression models or in LDA, we fit the best model using the
# squares estimates or maximum likelihood estimates. Those are not tuning
# parameters. We obtained those using least squares, or MLE, or some other
# optimization technique.

# Parameters that are tuned are parameters that we can change and then get
# an estimate of the model for each one.

# So in k-nearest neighbors, the number of neighbors is a tuning parameter. In
# regression, the number of predictors that we include could be considered a
# parameter that's optimized. So in the caret package, in the train function, we
# only optimize parameters that are tunable.

# So it won't be the case that, for example, in regression models, the caret
# package will optimize the regression coefficients that are estimated.

# Instead, it will just estimate using least squares.

# This is an important distinction to make when using the caret package--
# knowing which parameters are optimized, and which ones are not.
