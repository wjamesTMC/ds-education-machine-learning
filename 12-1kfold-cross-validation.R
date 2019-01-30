# --------------------------------------------------------------------------------
#
# kfold Cross Validation
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

# In previous videos, we've described how the goal of machine learning is often
# to find an algorithm that produces predictors, y hat for an outcome y, that
# minimizes mean squared error. When all we have to our disposal is one data
# set, we can estimate the mean squared error with the observed mean squared
# error like this.

#    ^MSE = 1/N N∑i=1(^yi−yi)2

# These two quantities are often referred to as a true error and the apparent
# error respectively. There are two important characteristics of the apparent
# error we should always keep in mind. First, it is a random variable since our
# data is random. For example, the data set we have may be a random sample from
# a larger population. So an algorithm having lower apparent error than another
# may be due to luck. Second, if we train an algorithm on the same data set that
# we used to compute the apparent error, we might be overtraining. In general
# when we do this, the apparent error will be an underestimate of the true
# error. We saw an extreme example of this with the k-nearest neighbors when we
# said k equals to 1.

# Cross-validation is a technique that permits us to alleviate both these
# problems. There are several approaches. I will go over some of them here. To
# understand cross-validation, it helps to think of the true error, a
# theoretical quantity, as the average of many, many apparent errors obtained by
# applying the algorithm to, let's call it, B, new random samples of the data,
# none of them used to train the algorithm. When we think this way, we can think
# of the true error as the average of the apparent errors obtained in each of
# the random samples. The formula would be like this.

#    1/B B∑b=1 1/N N∑i=1(^ybi−ybi)2  

# Here B a large number that can be thought of as practically infinite. Now,
# this is a theoretical quantity because we only get to see one set of outcomes.
# We don't get to see them over and over again. The idea of cross-validation is
# to imitate this theoretical setup as best we can with the data that we have.
# To do this, we have to generate a series of different random samples. There
# are several approaches to doing this. But the general idea for all of them is
# to randomly generate smaller data sets that are not used for training and
# instead are used to estimate the true error.

# The first one we describe, and the one we focus on in this video, is k-fold
# cross-validation. Let's describe it.

# Remember, that generally speaking, a machine learning challenge starts with a
# data set. And we need to build an algorithm using this data set that will
# eventually be used in a completely independent data set. So here we have the
# data set we have in blue and the independent data set that we'll never see in
# yellow.

# So we don't get to see the yellow, so all we see is the blue.

# So as we have already described, to imitate the situation, we carve out a
# piece of our data set and pretend it is an independent data set. We divide the
# data set into training set, blue, and a test set, red. We'll train our
# algorithm exclusively on the training set and use the test set only for
# evaluation purposes. We usually try to select a small piece of the data set so
# we have as much data as possible to train. However, we also want a test set to
# be large so that we can obtain stable estimates of the loss.

# Typical choices to use for the size of the test that are 10% to 20%
# of the original data set.

# Let's reiterate that it is indispensable that we do not use the test set at
# all when training our algorithm, not for filtering out rows, not for selecting
# features, nothing.

# Now, this presents a new problem. Because for most machine learning
# algorithms, we need to select parameters, for example, the number of neighbors
# k in the k-nearest neighbors algorithm. Here we'll refer to the set of
# parameters as lambda (λ).

# So we need to optimize the algorithm parameters lambda without using our test
# set. And we know that if we optimize and evaluated on the same data set, we
# will overtrain. So here is where we use cross-validation. This is where
# cross-pollination is most useful.

# So let's describe k-fold cross-validation. For each set of algorithm
# parameters being considered, we want to estimate the MSE. And then we will
# choose the parameters with the smallest MSE. Cross-validation will provide
# this estimate. First, it is important that before we start the
# cross-validation procedure we fix all the algorithm parameters. We're
# computing the MSE for a given parameter. So as we describe later, we will
# trained the algorithm on a set of training sets. The parameter lambda will be
# the same across all these training sets.

# We'll use the notation y hat i lambda - y^i(λ) - to denote the prediction
# obtained when we use a parameter lambda for observation i. So if we're going
# to imitate the definition of the expected loss, we could write it like this.

#    MSE(λ) = 1/B B∑b=1 1/N N∑i=1(^ybi(λ)−ybi)2

# It is the average over B samples that we've taken of the MSE that we obtain on
# the data separated out as a test set. For this formula, we want to consider
# data sets that can be thought of as independent random samples. And you'll
# want to do this several times. With k-fold cross-validation, we do it k times.
# In the cartoons we're showing, we use as an example k equals 5. We will
# eventually end up with k samples. But let's start by describing how to
# construct the first one. We simply pick N divided K rounded to the nearest
# integer. Let's call that M. M = N /K. So we have M observations that we pick
# at random and think of these as a random sample. We could denote them using
# this equation.

#    yb1,…,ybM, with b=1

# And here b equals 1. # It's the first, what we call, fold. So here's what it
# looks like graphically. We have our training set. We separate out the test
# set. And then we take our training set, and we take a small sample of it,
# which we're going to call the validation set, the first one. And that is where
# we're going to test. [PICTURE: vertical bar 85% blue named Train and 15% red
# named Test. Second verifical bar 70% blue named Train 1, 15% purple named
# Validate 1 and 15% red named Test]

# Now we can fit the model in the training set, with the validation set
# separated out, and compute the apparent error on the independent set like
# this.

#    ^MSEb(λ) = 1/M M∑i=1(^ybi(λ)−ybi)2

# Note that this is just one sample and will therefore return a noisy estimate
# of the true error. This is why we take k sample not just one. So graphically
# it would look like this. In k-fold cross-validation, we randomly split the
# observations into k non-overlapping sets. [PICTURE: 5 additional vertical bars
# with the 70% purple and in each case a 15% Validate set carved out but never
# overlapping with any other Validate set]
 
# So now we repeat this calculation for each of these sets, b going from 1 all
# the way up to k (b = 1, ..., K). So we obtain k estimates of the MSE
# (^MSE1(λ),…,^MSEk(λ)).

# In our final estimate, we compute the average like this.

#    ^MSE(λ) = 1/B K∑b=1^MSEb(λ)

# And this gives us an estimate of our loss. A final step would be to select the
# lambda, the parameters, that minimize the MSE. So this is how we use
# cross-validation to optimize parameters.

# However, now we have to take into account the fact that the optimization
# occurred on the training data. So we need to compute an estimate of our final
# algorithm based on data that was not used to optimize this choice. And this is
# why we separated out the test set. That is where we'll compute our final
# estimate of the MSE.

# So note that we can do cross-validation again. Note that this is not for
# optimization purpose. This is simply to know what the MSE of our final
# algorithm is. [PICTURE: 5 vertical bars with 5 15% test portions being taken
# from a different, non-overlapping portion of the vertical bar] So doing this
# would give us a better estimate.

# However, note that to do this, we have to go through the entire optimization
# process k times. You will soon learn that performing machine learning tasks
# can take time because we're performing many complex computations. And
# therefore we're always looking for ways to reduce this. So for the final
# evaluation, we often just use one test set. We use cross-validation to
# optimize our algorithm. But once we've optimized it, we're done, and we want
# to have an idea of what our MSE is, we just use this one last test set.

# Once we're satisfied with this model, and we want to make it available to
# others, we could refit the model on the entire data set, but without changing
# the parameters.

# Now, how do we pick the cross-validation k? We used five in these examples. We
# could use other numbers. Large values of k are preferable because the training
# data better imitate the original data. However, larger values of k will have
# much lower computation time. For example, hundredfold cross-validation will be
# 10 times slower than tenfold cross-validation. For this reason, the choices of
# k equals to 5 and 10 are quite popular.

# Now, one way we can improve the variance of our final estimate is to take more
# samples. To do this, we would no longer require that training set be
# partitioned into non-overlapping sets. Instead we would just pick k sets of
# some size at random. One popular version of this technique, at each fold,
# picks observations at random with replacement, which means that the same
# observation can appear twice. This approach has some advantages not discussed
# here is generally referred to as the bootstrap approach. In fact, this is the
# default approach in the caret package. In another video, we'll describe the
# concept of the bootstrap.
