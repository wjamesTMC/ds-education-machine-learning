# --------------------------------------------------------------------------------
#
# Recommendation Systems
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

# Recommendation systems use ratings that users have given items to make
# specific recommendations to users. Companies like Amazon that sell many
# products to many customers and permit these customers to rate their products
# are able to collect massive data sets that can be used to predict what rating
# a given user will give a specific item. Items for which a high rating is
# predicted for specific users are then recommended to that user. Netflix uses
# recommendation systems to predict how many stars a user will give a specific
# movie. Here we provide the basics of how these recommendations are predicted,
# motivated by some of the approaches taken by the winners of the Netflix
# challenge.

# What's the Netflix challenge? On October 2006, Netflix offered a challenge to
# the data science community. Improve our recommendation algorithm by 10% and
# win a $1 million. In September 2009, the winners were announced. You can
# follow this link to see the news article. You can read a good summary of how
# the winning algorithm was put together following this link. We'll include it
# in the class material. And a more detailed explanation following this link,
# also included in the class material.

# Here we show you some of the data analysis strategies used by the winning
# team. Unfortunately, the Netflix data is not publicly available. But the
# GroupLens research lab generated their own database with over 20 million
# ratings for over 27,000 movies by more than 138,000 users. We make a small
# subset of this data available via the DS labs package. You can upload it like
# this.

data("movielens")
head(movielens)

# We can see that the movie lens table is tidy formant and contains thousands of
# rows. Each row represents a rating given by one user to one movie. We can see
# the number of unique users that provide ratings and for how many unique movies
# they provided them using this code.

movielens %>% 
     summarize(n_users = n_distinct(userId),
               n_movies = n_distinct(movieId))

# If we multiply those two numbers, we get a number much larger than 5 million.
# Yet our data table has about 100,000 rows. This implies that not every user
# rated every movie. So we can think of this data as a very large matrix with
# users on the rows and movies on the columns with many empty cells. The gather
# function permits us to convert to this format, but if we try to do it for the
# entire matrix it will crash R. So lets look at a smaller subset. This table
# shows a very small subset of seven users and five movies.

# You can see the ratings that each user gave each movie and you also see NA's
# for movies that they didn't watch or they didn't rate. You can think of the
# task in recommendation systems as filling in the NA's in the table we just
# showed. To see how sparse the entire matrix is, here the matrix for a random
# sample of 100 movies and 100 users is shown with yellow indicating a user
# movie combination for which we have a rating.

# All right so let's move on to try to make predictions. The machine learning
# challenge here is more complicated than we have studied up to now because each
# outcome y has a different set of predictors. To see this, note that if we are
# predicting the rating for movie i by user u, in principle, all other ratings
# related to movie i and by user u may be used as predictors. But different
# users rate a different number of movies and different movies. Furthermore, we
# may be able to use information from other movies that we have determined are
# similar to movie i or from users determined to be similar to user u. So in
# essence, the entire matrix can be used as predictors for each cell.

# So let's get started. Let's look at some of the general properties of the data
# to better understand the challenge. The first thing we notice is that some
# movies get rated more than others. Here's the distribution.
 
# This should not surprise us given that there are blockbusters watched by
# millions and artsy independent movies watched by just a few.

# A second observation is that some users are more active than others at rating
# movies. Notice that some users have read it over 1,000 movies while others
# have only rated a handful. To see how this is a machine learning challenge,
# note that we need to build an algorithm with data we have collected. And this
# algorithm will later be used by others as users look for movie
# recommendations. So let's create a test set to assess the accuracy of the
# models we implement, just like in other machine learning algorithms. We use
# the caret package using this code.

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

# To make sure we don't include users and movies in the test set that do not
# appear in the training set, we removed these using the semi_join function,
# using this simple code.

test_set <- test_set %>% 
     semi_join(train_set, by = "movieId") %>%
     semi_join(train_set, by = "userId")

# To compare different models or to see how well we're doing compared to some
# baseline, we need to quantify what it means to do well. We need a loss
# function. The Netflix challenge used the typical error and thus decided on a
# winner based on the residual mean squared error on a test set.

# So if we define yui as the rating for movie i by user u and y hat ui as our
# prediction, then the residual mean squared error is defined as follows. Here n
# is a number of user movie combinations and the sum is occurring over all these
# combinations. 

# SEE LECTURE FOR FORMULA: RMSE = sqrt(1/N E (^yu,i - yu,i)^2)

# Remember that we can interpret the residual mean squared error
# similar to standard deviation. It is the typical error we make when predicting
# a movie rating. If this number is much larger than one, we're typically
# missing by one or more stars rating which is not very good. So let's quickly
# write a function that computes this residual means squared error for a vector
# of ratings and their corresponding predictors. It's a simple function that
# looks like this.

RMSE <- function(true_ratings, predicted_ratings){
     sqrt(mean((true_ratings - predicted_ratings)^2))
}

# And now we're ready to build models and compare them to each other.

