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

# When an algorithm includes a tuning parameter,
# train automatically uses cross-validation to decide
# among a few default values.
# To find out what parameter or parameters are optimized,
# you can read this page that explains it.
# We'll include the link in the courseware.
# Or study the output of the following code.
# The get model info function can be used to get information of the method
# that you're interested in.
# You can do a quick lookup using the model lookup function like this.
# When we run this code, we see that for knn, the parameter that's optimized
# is k.
# So if we run the function train with default values,
# you can quickly see the results of the cross-validation using
# the ggplot function.
# You can use the argument highlight to highlight the parameter that
# optimizes the algorithm.
# So you can type this.
# By default, the cross-validation is performed
# by testing on 25 bootstrap samples comprised of 25% of the observations.
# Also, for the knn method, the default is to try out k=5, 7, and 9.
# We already saw that 9 maximizes this.
# But maybe there's another k that's even better.
# So to change this, we need to use the tunegrid parameter
# in the train function.
# The grid of values that are going to be compared
# must be supplied by a data frame with the column names
# as specified by the parameters that you get in the model lookup output.
# Here we present an example trying out 30 values between 9 and 67.
# We need to use a column in k, so the data frame we use
# is going to be this one.
# Now, note that when running this code, we are fitting 30 versions of knn
# to 25 bootstrap samples, so we're fitting 750 knn models.
# And thus, running this code will take several seconds.
# Here's the code.
# In the plot, we can see the k that maximizes accuracy,
# but we can also access it using this code.
# The bestTune component gives us the parameter that maximizes the accuracy.
# We can also access the best-performing model using this code.
# Now, if you apply the function predict to the output of the train function,
# it will use this best-performing model to make predictions.
# Note that the best model was obtained using the training set.
# We did not use the test set at all.
# The cross-validation was performed on the training set.
# So now, if we want to see the accuracy we
# obtain on the test set, which hasn't been used,
# we can use the following code.
# Sometimes we like to change the way we perform cross-validation.
# We might change the method, we might change
# how we do the partitions, et cetera.
# If we want to do this, we need to use a trainControl function.
# So for example, we can make the code that we just
# showed go a bit faster by using 10-fold cross-validation.
# This means we're going to have 10 validation samples that
# use 10% of the observations each.
# Notice that if we plot the estimated accuracy versus k plot,
# we notice that the accuracy estimates are more
# variable than in the previous example.
# Now this is expected since we changed a number
# of samples we use to estimate accuracy.
# In the first example, we used 25 bootstrap samples, and in this example,
# we use 10-fold cross-validation.
# One more thing to point out.
# Note that the train function also provides standard deviation values
# for each parameter that was tested.
# This is obtained from the different validation sets.
# So we can make a plot like this that shows
# the point estimates of the accuracy along with standard deviations.
# To finish this example up, let's notice that the best-fitting knn
# model approximates the true condition of probability pretty well.
# However, we do see that the boundary is somewhat wiggly.
# This is because knn, like the basic bin smoother, does not use a smooth kernel.
# To improve this, we could try loess.
# By reading through the available models of the caret package, which
# you can get to through this link, which we include in the course material,
# we see that we can use the gamLoess method.
# Also from the caret documentation link--
#      you can go to it here--
#      you can see that we need to install the gam
# package if we have not done so already.
# So we will type something like this.
# Then we will see that we have two parameters to optimize
# if we use this particular method.
# You can see this with the model lookup function, like this.
# For this example, we'll keep the degree fixed at one.
# We won't try out degree two.
# But to try out different values for the span,
# we still have to include a column in the table with the named degree.
# This is a requirement of the caret package.
# So we would define a grid using the expand.grid function, like this.
# Now, we use the default cross-validation control parameters,
# so we type code like this to train our model.
# Then, select the best-performing model, and now we can see the final result.
# It performs similarly to knn.
# 
# However, we can see that the conditional probability estimate is indeed
# smoother than what we get with knn.
# Note that not all parameters in machine-learning algorithms are tuned.
# For example, in regression models or in LDA,
# we fit the best model using the squares estimates or maximum likelihood
# estimates.
# Those are not tuning parameters.
# We obtained those using least squares, or MLE, or some other optimization
# technique.
# Parameters that are tuned are parameters that we can change and then get
# an estimate of the model for each one.
# So in k-nearest neighbors, the number of neighbors is a tuning parameter.
# In regression, the number of predictors that we include
# could be considered a parameter that's optimized.
# So in the caret package, in the train function,
# we only optimize parameters that are tunable.
# So it won't be the case that, for example, in regression models,
# the caret package will optimize the regression coefficients that
# are estimated.
# Instead, it will just estimate using least squares.
# This is an important distinction to make when using the caret package--
#      knowing which parameters are optimized, and which ones are not.
