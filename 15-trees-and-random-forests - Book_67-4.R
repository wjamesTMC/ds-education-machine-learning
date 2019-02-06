# --------------------------------------------------------------------------------
#
# Trees and Random Forests - Random Forests (Book 67.4)
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

# Random forests are a very popular approach
# that address the shortcomings of decision trees using a clever idea.
# The goal is to improve prediction performance
# and reduce instability by averaging multiple decision trees, a forest
# of trees constructed with randomness.
# It has two features that help accomplish this.
# The first feature is referred to as bootstrap aggregation, or bagging.
# The general scheme for bagging is as follows.
# First, we build many decision trees, T1 through TB, using the training set.
# We later explain how we're sure they're different.
# Second, for every observation j in the test set,
# we form a prediction y hat j using tree Tj.
# Now, to obtain a final prediction, we combine the predictions
# for each tree in two different ways, one for continuous outcomes
# and one for categorical outcomes.
# For continuous outcomes, we simply take the average of the y hat j's.
# For categorical data, we predict y hat with a majority vote.
# The class that appears most across all the trees is the one we predict.
# OK, now, but how do we get many decision trees from a single training set?
# For this, we use the bootstrap.
# So to create, let's say, B bootstrap trees, we do the following.
# To create tree Tj from a training set of size N,
# we create a bootstrap training set by sampling
# N observations from this training set with replacement.
# Now we build a decision tree for each one of these bootstrap training sets.
# And then we apply the algorithm that we just
# described to get a final prediction.
# Here's the code for applying random forest to the 2008 polls data.
# It's quite simple.
# We do it like this.
# We can see the algorithm improves as we add more trees.
# If you plot the object that comes out of this function like this,
# we get a plot of the error versus the number of trees that have been created.
# In this case, we see that by the time we get to about 200 trees,
# the algorithm is not changing much.
# But note that for more complex problems will require more trees
# for the algorithm to converge.
# Here is the final result for the polls 2008 data.
# Note that the final result is somewhat smooth.
# It's not a step function like the individual trees.
# The averaging is what permits estimates that are not step functions.
# To see this, we've generated an animation
# to help illustrate the procedure.
# In the animated figure, you see each of 50 trees, B equals 1 up to 50.
# Each one is a bootstrap sample which appears in order.
# For each one of the bootstrap samples, we
# see the tree that is fitted to that bootstrap sample.
# And then in blue, we see the result of bagging the trees up to that point.
# So you can see the blue line changing with time.
# Now let's look at another example.
# Let's fit a random forest to our two or seven digit example.
# The code would look like this.
# And here's what the conditional probabilities look like.
# Note that we now have much more flexibility than just a single tree.
# This particular random forest is a little bit too wiggly.
# We want something smoother.
# However, note that we have not optimized the parameters in any way.
# So let's use the caret package to do this.
# We can do it using this code.
# Here we're going to use a different random forest algorithm, Rborist,
# that is a little bit faster.
# And here is the final result. We also see that our accuracy is much improved.
# 
# So we can control the smoothness of the random forest estimate in several ways.
# One is to limit the size of each node.
# We can require the number of points per node to be larger.
# A second feature of random forest that we have not yet described
# is that we can use a random selection of features to use for the splits.
# Specifically, when building each tree at each recursive partition,
# we only consider a randomly selected subset of predictors
# to check for the best split.
# And every tree has a different random selection of features.
# This reduces correlation between trees in the forests, which
# in turn improves prediction accuracy.
# The argument for this tuning parameter in the random forest function is mtry.
# But each random forest implementation has a different name.
# You can look at the help file to figure out which one.
# A disadvantage of random forest is that we lose interpretability.
# We're averaging hundreds or thousands of trees.
# However, there's a measure called variable importance
# that helps us interpret the results.
# Variable [? importance ?] basically tells us
# how much each predictor influences the final predictions.
# We will see an example later.


