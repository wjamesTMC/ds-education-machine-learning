# --------------------------------------------------------------------------------
#
# Regularization
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
library(lubridate)

# In this video, we're going to introduce the concept of regularization
# and show how it can improve our results even more.
# This is one of the techniques that was used by the winners of the Netflix
# challenge.
# All right.
# So how does it work?
# Note that despite the large movie to movie variation,
# our improvement in residual mean square error when we just included
# the movie effect was only about 5%.
# So let's see why this happened.
# Let's see why it wasn't bigger.
# Let's explore where we made mistakes in our first model
# when we only used movies.
# Here are 10 of the largest mistakes that we made when only using the movie
# effects in our models.
# Here they are.
# Note that these all seem to be obscure movies and in our model many of them
# obtained large predictions.
# So why did this happen?
# To see what's going on, let's look at the top 10 best
# movies in the top 10 worst movies based on the estimates of the movie effect
# b hat i.
# So we can see the movie titles, we're going
# to create a database that includes movie ID and titles using
# this very simple code.
# So here are the best 10 movies according to our estimates.
# America is number one, Love and Human Remains also
# number one, Infer L number one.
# Look at the rest of the movies in this table.
# And here are the top 10 worst movies.
# The first one started with Santa with Muscles.
# Now they all have something in common.
# They're all quite obscure.
# So let's look at how often they were rated.
# Here's the same table, but now we include the number of ratings
# they received in our training set.
# 
# We can see the same for the bad movies.
# So the supposed best and worst movies were rated by very few users,
# in most cases just one.
# These movies were mostly obscure ones.
# This is because with just a few users, we have more uncertainty,
# therefore larger estimates of bi, negative or positive,
# are more likely when fewer users rate the movies.
# These are basically noisy estimates that we should not trust, especially
# when it comes to prediction.
# Large errors can increase our residual mean squared error,
# so we would rather be conservative when we're not sure.
# Previously we've learned to compute standard errors
# and construct confidence intervals to account
# for different levels of uncertainty.
# However, when making predictions we need one number,
# one prediction, not an interval.
# For this, we introduce the concept of regularization.
# Regularization permits us to penalize large estimates that
# come from small sample sizes.
# It has commonalities with the Bayesian approaches that shrunk predictions.
# The general idea is to add a penalty for large values of b to the sum of squares
# equations that we minimize.
# So having many large b's makes it harder to minimize the equation that we're
# trying to minimize.
# One way to think about this is that if we
# were to fit an effect to every rating, we
# could of course make the sum of squares equation
# by simply making each b match its respective rating y.
# This would yield an unstable estimate that changes drastically
# with new instances of y.
# Remember y is a random variable.
# But by penalizing the equation, we optimize to b bigger
# when the estimate b are far from zero.
# We then shrink the estimates towards zero.
# Again, this is similar to the Bayesian approach we've seen before.
# So this is what we do.
# To estimate the b's instead of minimizing the residual sum of squares
# as is done by least squares, we now minimize this equation.
# Note the penalty term.
# The first term is just the residual sum of squares
# and the second is a penalty that gets larger when many b's are large.
# Using calculus, we can actually show that the values
# of b that minimized equation are given by this formula, where ni
# is a number of ratings b for movie i.
# Note that this approach will have our desired effect.
# When ni is very large which will give us a stable estimate,
# then lambda is effectively ignored because ni plus lambda
# is about equal to ni.
# However, when ni is small, then the estimate of bi
# is shrunken towards zero.
# The larger lambda, the more we shrink.
# So let's compute these regularized estimates
# of vi using lambda equals to 3.0.
# Later we see why we picked this number.
# So here is the code.
# To see how the estimates shrink, let's make
# a plot of the regularized estimate versus the least square estimates
# with the size of the circle telling us how large ni was.
# 
# You can see that when n is small, the values are shrinking more towards zero.
# All right, so now let's look at our top 10 best movies based on the estimates
# we got when using regularization.
# Note that the top five movies are now All
# About Eve, Shawshank Redemption, The Godfather, The Godfather
# II, and the Maltese Falcons.
# This makes much more sense.
# We can also look at the worst movies and the worst
# five are Battlefield Earth, Joe's Apartment, Speed 2, Cross
# Control, Super Mario Bros, and Police Academy 6: City Under Siege.
# Again, this makes sense.
# So do we improve our results?
#      We certainly do.
# We get the residual mean squared error all the way down to 0.885 from 0.986.
# So this provides a very large improvement.
# Now note that lambda is a tuning parameter.
# We can use cross-fertilization to choose it.
# We can use this code to do this.
# And we see why we picked 3.0 as lambda.
# One important point.
# Note that we show this as an illustration
# and in practice, we should be using full cross-validation just
# on a training set without using the test it until the final assessment.
# 
# We can also use regularization to estimate the user effect.
# The equation we would minimize would be this one now.
# It includes the parameters for the user effects as well.
# The estimates that minimizes can be found similarly
# to what we do previously.
# Here we again use cross-validation to pick lambda.
# The code looks like this, and we see what lambda minimizes our equation.
# For the full model including movie and user effects,
# the optimal lambda is 3.75.
# And we can see that we indeed improved our residual mean squared error.
# Now it's 0.881.

