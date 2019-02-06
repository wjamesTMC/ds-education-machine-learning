# --------------------------------------------------------------------------------
#
# Trees and Random Forests - The curse of dimensionality (Book 67.1)
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

# We described how methods lda and qda are not meant to be used with
# datasets that have many predictors. This is because the number of parameters
# that we need to estimate becomes too large. For example, with the digits
# example where we have 784 predictors, lda would have to estimate over 600,000
# parameters. With qda, you would have to multiply that by the number of
# classes, which is 10 here. Kernel methods such k-nearest neighbors or local
# regression do not have model parameters to estimate. But they also face a
# challenge when multiple predictors are used due to what is referred to as the
# curse of dimensionality.

# The dimension here refers to the fact that when we have p predictors, the
# distance between two observations is computed in p dimensional space. A useful
# way to understand the curse of dimensionality is by considering how large we
# have to make a neighborhood, the neighborhood we used to make the estimates,
# to include a given percentage of the data. Remember that with large
# neighborhoods, our methods lose flexibility. For example, suppose we have one
# continuous predictor with equally spaced points in the [? 01 ?] interval, and
# you wanted to create windows that include 1/10 of the data. Then it's easy to
# see that our windows have to be of size 0.1. You can see it in this figure.

# Now, for two predictors, if we decide to keep the neighborhood just a small,
# 10% of each dimensions we only include one point. If we want to include 10% of
# the data, then we need to increase the size of each side of the square to the
# square root of 10 so that the area is 10 out of 100. This is now 0.316.

# In general, to include 10% of the data in a case with p dimensions, we need an
# interval with each side having a size of 0.10 to the 1/p. This proportion gets
# close to 1, which means we're including practically all the data, and it's no
# longer smoothing very quickly. You can see it in this graph, plotting p versus
# 0.1 to the 1/p.

p <- 1:100
qplot(p, .1^(1/p), ylim = c(0,1))

# So by the time we reach 100 predictors, the neighborhood is no longer very
# local, as each side covers almost the entire dataset. In this video, we
# introduce a set of elegant and versatile methods that adapt to higher
# dimensions and also allow these regions to take more complex shapes, while
# still producing models that are interpretable. These are very popular
# well-known and studied methods. We will focus on regression and decision trees
# and their extension, random forests.
