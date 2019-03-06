# --------------------------------------------------------------------------------
#
# Building Recommendation Systems
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

# Code from prior lesson
data("movielens")
movielens %>% 
     summarize(n_users = n_distinct(userId),
               n_movies = n_distinct(movieId))
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
test_set <- test_set %>% 
     semi_join(train_set, by = "movieId") %>%
     semi_join(train_set, by = "userId")
RMSE <- function(true_ratings, predicted_ratings){
     sqrt(mean((true_ratings - predicted_ratings)^2))
}

# The Netflix challenge winners implemented two general classes of models. One
# was similar to k-nearest neighbors, where you found movies that were similar
# to each other and users that were similar to each other. The other one was
# based on an approach called matrix factorization. That's the one we we're
# going to focus on here. So let's start building these models.

# Let's start by building the simplest possible recommendation system. We're
# going to predict the same rating for all movies, regardless of the user and
# movie. So what number should we predict? We can use a model-based approach. A
# model that assumes the same rating for all movies and all users, with all the
# differences explained by random variation would look something like this.

#    Yu,i=μ+εu,i

# Here epsilon represents independent errors sampled from the same distribution
# centered at zero, and mu represents the true rating for all movies and users.
# We know that the estimate that minimizes the residual mean squared error is
# the least squared estimate of mu. And in this case, that's just the average of
# all the ratings. We can compute it like this.

mu_hat <- mean(train_set$rating)
mu_hat
# [1] 3.542793

# That average is 3.54. So that is the average rating of all movies across all
# users. So let's see how well this movie does. We compute this average on the
# training data. And then we compute the residual mean squared error on the test
# set data. So we're predicting all unknown ratings with this average. We get a
# residual mean squared error of about 1.05. That's pretty big.

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse
# [1] 1.04822

# Now note, if you plug in any other number, you get a higher RMSE. That's
# what's supposed to happen, because we know that the average minimizes the
# residual mean squared error when using this model. And you can see it with
# this code:

predictions <- rep(3, nrow(test_set))
RMSE(test_set$rating, predictions)
# [1] 1.187517

# So we get a residual mean squared error of about 1. To win the grand prize of
# $1 million, a participating team had to get to a residual mean squared error
# of about 0.857. So we can definitely do better.

# Now because as we go along we will be comparing different approaches, we're
# going to create a table that's going to store the results that we obtain as we
# go along. We're going to call it RMSE results. It's going to be created using
# this code.

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# So let's see how we can do better. We know from experience that some movies
# are just generally rated higher than others. We can see this by simply making
# a plot of the average rating that each movie got. So our intuition that
# different movies are rated differently is confirmed by data. So we can augment
# our previous model by adding a term, b i, to represent the average rating for
# movie i. In statistics, we usually call these b's, effects. But in the Netflix
# challenge papers, they refer to them as "bias," thus the b in the notation.

# We can again use these squares to estimate the b's in the following way,

fit <- lm(rating ~ as.factor(movieId), data = movielens)

# However, note that because there are thousands of b's, each movie gets one
# parameter, one estimate. So the lm function will be very slow here. So we
# don't recommend running the code we just showed. However, in this particular
# situation, we know that the least squared estimate, b hat i, is just the
# average of yui minus the overall mean for each movie, i. So we can compute
# them using this code.

mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
     group_by(movieId) %>% 
     summarize(b_i = mean(rating - mu))

# Note that we're going to drop the hat notation in the code to represent the
# estimates going forward, just to make the code cleaner. So this code completes
# the estimates for the b's. We can see that these estimates vary substantially,
# not surprisingly. Some movies are good. Other movies are bad. Remember, the
# overall average is about 3.5. So a b i of 1.5 implies a perfect five-star
# rating.

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

# Now let's see how much our prediction improves once we predict using the model
# that we just fit. We can use this code and see that our residual mean squared
# error did drop a little bit.

predicted_ratings <- mu + test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse))
rmse_results 

# We already see an improvement. Now can we make it better? All right. How about
# users? Are different users different in terms of how they rate movies? To
# explore the data, let's compute the average rating for user, u, for those that
# have rated over 100 movies. We can make a histogram of those values. And it
# looks like this.

train_set %>% 
     group_by(userId) %>% 
     summarize(b_u = mean(rating)) %>% 
     filter(n()>=100) %>%
     ggplot(aes(b_u)) + 
     geom_histogram(bins = 30, color = "black")

# Note that there is substantial variability across users, as well. Some users
# are very cranky. And others love every movie they watch, while others are
# somewhere in the middle. This implies that a further improver to our model may
# be something like this. We include a term, bu, which is the user-specific
# effect. So now if a cranky user-- this is a negative bu-- rates a great movie,
# which will have a positive b i, the effects counter each other, and we may be
# able to correctly predict that this user gave a great movie a three rather
# than a five, which will happen. And that should improve our predictions. So
# how do we fit this model? Again, we could use lm. The code would look like
# this.

lm(rating ~ as.factor(movieId) + as.factor(userId))

# But again, we won't do it, because this is a big model. It will probably crash
# our computer. Instead, we will compute our approximation by computing the
# overall mean, u-hat, the movie effects, b-hat i, and then estimating the user
# effects, b u-hat, by taking the average of the residuals obtained after
# removing the overall mean and the movie effect from the ratings yui. The code
# looks like this.

user_avgs <- train_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     group_by(userId) %>%
     summarize(b_u = mean(rating - mu - b_i))

# And now we can see how well we do with this new model by predicting values and
# computing the residual mean squared error. We see that now we obtain a further
# improvement. Our residual mean squared error dropped down to about 0.89.

predicted_ratings <- test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     mutate(pred = mu + b_i + b_u) %>%
     pull(pred)


model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse))
rmse_results
# A tibble: 3 x 2
# method                      RMSE
# <chr>                      <dbl>
# 1 Just the average           1.05 
# 2 Movie Effect Model         0.986
# 3 Movie + User Effects Model 0.908
