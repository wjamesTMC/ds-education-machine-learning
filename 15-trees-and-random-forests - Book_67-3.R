# --------------------------------------------------------------------------------
#
# Trees and Random Forests - Classification (Decision) Trees (Book 67.3)
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

# When the outcome is categorical,
# we refer to these methods as classification trees or decision trees.
# We use the same partitioning principles, that we use for the continuous case,
# but with some slight differences to account for the fact
# that we are now working with categorical data.
# The first difference is that rather than taking the average
# at the end of each node, now in the partitions,
# we predict with the class that has the majority vote in each node.
# So the class that appears the most in a node, that will be what we predict.
# 
# The second difference is that we can no longer use residual sum of squares
# to decide on the partition because the outcomes are categorical.
# Well, we could use a naive approach, for example,
# looking four partitions that minimize training error.
# Better performing approaches use more sophisticated metrics.
# Two of the more popular ones are the Gini index and entropy.
# Let's define those two concepts.
# If we define p hat m, k as a proportion of observations in partition m
# that are of class k, then the Gini index is defined as follows.
# 
# And entropy is defined in the following way.
# 
# Both of these metrics seek to partition observations
# into subsets that have the same class.
# They want what is called purity.
# Note that of a partition-- let's call it m--
#      has only one class--
#      let's say it's the first one--
#      then p hat of 1 for that partition is equal to 1,
# while all the other p hats are equal to 0.
# When this happens, both the Gini index and entropy are 0, the smallest value.
# So let's look at an example.
# Let's see how classification trees perform on the two or seven
# example we have examined in previous videos.
# This is the code that we would write to fit a tree.
# We then look at the accuracy versus complexity parameter function,
# and we can pick the best complexity parameter from this plot.
# And now we use that tree and see how well we do.
# We see that we achieve an accuracy of 0.82.
# We can use this code.
# Note that this is better than logistic regression but not as good
# as the kernel methods.
# If we plot the estimate of the conditional probability obtained
# with this tree, it shows us the limitations of classification trees.
# Note that with decision trees, the boundary can't be smoothed.
# Despite these limitations, classification trees
# have certain advantages that make them very useful.
# First, they're highly interoperable, even more so
# than linear regression models or logistic regression models.
# They're also easy to visualize if they're small enough.
# Finally, they sometimes model human decision processes.
# On the other hand, the greedy approach via recursive partitioning
# is a bit harder to train than, for example, linear regression
# or k-nearest neighbors.
# Also, it may not be the best performing method since it's not very flexible,
# and it's actually quite susceptible to changes in the training data.
# Random forests, explained in the next video,
# improve on several of these shortcomings.

