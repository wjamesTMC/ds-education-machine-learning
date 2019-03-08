# --------------------------------------------------------------------------------
#
# Matrix Factorization
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

# Matrix factorisation is a widely used concept in machine learning.
# It is very much related to factor analysis, single value composition,
# and principal component analysis, or PCA.
# Here we describe the concept in the context of movie recommendation
# systems.
# We have previously described the following model which
# accounts for movie and movie differences through the parameters bi,
# and user reviews or differences through parameters bu.
# But this model leaves out an important source of variation related to the fact
# that groups of movies have similar rating patterns and groups of users
# have similar rating patterns as well.
# We will discover these patterns by studying the residuals obtained
# after fitting our model.
# These residuals.
# To study these residuals, we will convert the data into a matrix
# so that each user gets a row and each movie gets a column.
# So yui is the entry in row u and column i.
# User u, movie i.
# For illustration purposes, we will only consider
# a small subset of movies with many ratings and users
# that have rated many movies.
# We will use this code to generate our training data.
# To facilitate exploration we add row names and column names.
# The column names will be the movie names.
# And we convert these residuals by removing the column and row averages.
# Here's the code.
# OK, now.
# If the model we've been using describes all the signal
# and the extra ones are just noise, then the residuals for different movies
# should be independent of each other.
# But they are not.
# Here's an example.
# Here's a plot of the residuals for The Godfather and The Godfather II.
# They're very correlated.
# This plot says that users that liked the godfather more than what
# the model expects them to based on the movie and user effects
# also like The Godfather II more than expected.
# The same is true for The Godfather and Goodfellas.
# You can see it in this plot.
# Although not as strong, there still is a correlation.
# We see a correlation between other movies as well.
# For example, here's a correlation between You've
# Got Mail and Sleepless in Seattle.
# We can see a pattern.
# If we look at the pairwise correlation for these five movies,
# we can see that there's a positive correlation between the gangster movies
# Godfathers and Goodfellas, and then there's
# a positive correlation between the romantic comedies You've
# Got Mail and Sleepless in Seattle.
# We also see a negative correlation between the gangster
# movies and the romantic comedies.
# This means that users that like gangster movies
# a lot tend to not like romantic comedies and vise versa.
# This result tells us that there is structure in the data
# that the model does not account for.
# So how do we model this?
#      Here is where we use matrix factorization.
# We're going to define factors.
# Here's an illustration of how we could use some structure
# to predict the residuals.
# Suppose the residuals look like this.
# This is a simulation.
# There seems to be a pattern here.
# It's based on what we saw with the real data.
# There's a gangster movie effect and there's a romantic comedy effect.
# In fact, we see a very strong correlation pattern,
# which we can see here.
# This structure could be explained using the following coefficients.
# We assign a 1 to the gangster movies and a minus one to the romantic comedies.
# In this case, we can narrow down movies to two groups, gangster
# and romantic comedy.
# Note that we can also reduce the users to three groups, those
# that like gangster movies but hate romantic comedies,
# the reverse, and those that don't care.
# The main point here is that we can reconstruct
# this data that has 60 values with a couple of vectors totaling 17 values.
# Those two vectors we just showed can be used to form the matrix with 60 values.
# We can model the 60 residuals with the 17 parameter model like this.
# And this is where the factorization name comes in.
# We have a matrix r and we factorised I used it into two things, the vector p,
# and the vector q.
# 
# Now we should be able to explain much more of the variance
# if we use a model like this one.
# 
# Now the structure in our movie data seems
# to be much more complicated than gangster
# movie versus romantic comedies.
# We have other factors.
# For example, and this is a simulation, let's suppose
# we had the movie Scent of a Woman, and now the data looks like this.
# Now we see another factor, a factor that divides users into those that love,
# those that hate, and those that don't care for Al Pacino.
# The correlation is a bit more complicated now.
# We can see it here.
# Now to explain the structure, we need two factors.
# Here they are.
# The first one divides gangster movies from romantic comedies.
# The second factor divide Al Pacino movies and non Al Pacino movies.
# And we also have two sets of coefficients to describe the users.
# You can see it here.
# The model now has more parameters, but still less than the original data.
# So we should be able to fit this model using,
# for example, the least squares method.
# However, for the Netflix challenge, they used regularization,
# and they penalize not just the user and movie effects,
# but also large values of the factors p or q.
# Now does this simulation match the actual data?
#      Here are the correlation we get for the movies we just showed,
# but using the actual data.
# Notice that the structure is similar.
# However, if we want to find the structure
# using the data as opposed to constructing
# it ourselves as we did in the example, we need to fit models to data.
# So now we have to figure out how to estimate factors from the data as
# opposed to defining them ourselves.
# One way to do this is to fit models, but we can also
# use principle component analysis or equivalently, the singular reality
# composition to estimate factors from data.
# And we're going to show that in the next video.

