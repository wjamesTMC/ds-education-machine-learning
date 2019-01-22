# --------------------------------------------------------------------------------
#
# Introduction to smoothing
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

# Before continuing with machine learning algorithms,
# we introduce the important concept of smoothing.
# Smoothing is a very powerful technique used all across data analysis.
# Other names given to this technique are curve fitting and low band
# pass filtering.
# It's designed to detect trends in the presence of noisy data
# in cases in which the shape of the trend is unknown.
# The smoothing name comes from the fact that
# to accomplish this feat we assume that the trend is smooth,
# as in a smooth surface, and the noise is unpredictably wobbly.
# Something like this.
# Part of what we explain here are the assumptions
# that permit us to extract a trend from the noise.
# To understand why we cover this topic, note
# that the concepts behind smoothing techniques
# are extremely useful in machine learning because conditional expectations
# and probabilities can be thought of as trends of unknown shapes
# that we need to estimate in the presence of uncertainty.
# To explain these concepts, we will focus first
# on a problem with just one predictor.
# Specifically we try to estimate the time trend in the popular vote
# from the 2008 election, the difference between Obama and McCain.
# You can load the data like this and we can see a plot here.
# For the purposes of this example, do not think of it as a forecasting problem.
# We're simply interested in learning the shape of the trend
# after collecting all the data.
# We assume that for any given day x, there's
# a true preference among the electorate, f
# of x, but due to the uncertainty introduced by polling,
# each data point comes with an error, epsilon.
# A mathematical model for the observed poll margin, y,
# is y equals f of x plus epsilon.
# To think of this as a machine learning problem,
# consider that we want to predict y given the day x.
# And that if we knew it, we would use the conditional expectation, f of x
# equals expectation of y given x.
# But we don't know it, so we have to estimate it.
# We're going to start by using regression, the only method we know,
# to see how it does.
# 
# The line we see does not appear to describe the trend very well.
# Note, for example, that on September 4, this is day negative 62,
# 62 days until Election Day, the Republican Convention was held.
# This appeared to give McCain a boost in the polls, which
# can be clearly seen in the data.
# The regression line does not capture this.
# To further see the lack of fit, we note that points
# above the fitted line, blue, and those below, red, are not evenly distributed.
# We therefore need an alternative, a more flexible approach.



