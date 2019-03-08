# --------------------------------------------------------------------------------
#
# SVD and PCA
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

# The matrix vectorization decomposition
# that we showed in the previous video that looks something
# like this is very much related to singular value composition and PCA.
# Singular value composition and principal component analysis
# are complicated concepts, but one way to understand them
# is to think of, for example, singular value
# decomposition as an algorithm that finds the vectors p and q that
# permit us to write the matrix of residuals r with m rows and n columns
# in the following way.
# But with the added bonus that the variability of these terms
# is decreasing and also that the p's are uncorrelated to each other.
# The algorithm also computes these variabilities
# so that we can know how much of the matrix's total variability
# is explained as we add new terms.
# This may permit us to see that with just a few terms,
# we can explain most of the variability.
# Let's see an example with our movie data.
# To compute the decomposition, will make all DNA zero.
# So we will write this code.
# The vectors q are called the principal components
# and they are stored in this matrix.
# While the p vectors which are the user's effects are stored in this matrix.
# The PCA function returns a component with the variability
# of each of the principal components and we can access it like this and plot it.
# We can also see that just with a few of these principal components
# we already explain a large percent of the data.
# So for example, with just 50 principal components
# we're already explaining about half the variability out of a total
# of over 300 principal components.
# To see that the principal components are actually capturing something important
# about the data, we can make a plot of for example, the first two
# principal components, but now label the points with the movie
# that each one of those points is related to.
# Just by looking at the top three in each direction, we see meaningful patterns.
# The first principle component shows the difference
# between critically acclaimed movies on one side.
# Here are the one extreme of the principal component.
# You can see Pulp Fiction, Seven, Fargo, Taxi Driver, and Hollywood blockbusters
# on the other.
# So this principle component has critically acclaimed movies
# on one side and blockbusters on the other.
# It's separating out movies that have structure
# and they're determined by users that like these more than these and others
# that like these more than that.
# We can also see that the second principle component also
# seems to capture structure in the data.
# If we look at one extreme of this principle component,
# we see arts and independent films such as Little Miss Sunshine, the Truman
# Show, and Slumdog Millionaire.
# When we look at the other extreme, we see
# what I would call nerd favorites, The Lord of the Rings,
# Star Wars, The Matrix.
# So using principal component analysis, we
# have shown that a matrix factorisation approach can
# find important structure in our data.
# Now to actually fit the matrix factorization model
# that we presented earlier that takes into account that there
# is missing data, that there's missing cells in the matrix,
# is a bit more complicated.
# For those interested we recommend trying the recommended lab
# package which fits these models.

