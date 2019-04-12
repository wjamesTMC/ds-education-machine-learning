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
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
predictions <- rep(3, nrow(test_set))
RMSE(test_set$rating, predictions)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse) 
# fit <- lm(rating ~ as.factor(movieId), data = movielens)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
     group_by(movieId) %>% 
     summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse))
rmse_results 

user_avgs <- train_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     group_by(userId) %>%
     summarize(b_u = mean(rating - mu - b_i))

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

# ----------------------------------------------------------------------------
# end of code from prior lesson
# ----------------------------------------------------------------------------

# In this video, we're going to introduce the concept of regularization and show
# how it can improve our results even more. This is one of the techniques that
# was used by the winners of the Netflix challenge. All right. So how does it
# work? Note that despite the large movie to movie variation, our improvement in
# residual mean square error when we just included the movie effect was only
# about 5%. So let's see why this happened. Let's see why it wasn't bigger.
# Let's explore where we made mistakes in our first model when we only used
# movies. Here are 10 of the largest mistakes that we made when only using the
# movie effects in our models. Here they are.

test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     mutate(residual = rating - (mu + b_i)) %>%
     arrange(desc(abs(residual))) %>% 
     select(title,  residual) %>% slice(1:10) 
#                                               title  residual
# 1      Day of the Beast, The (Día de la Bestia, El)  4.500000
# 2                                    Horror Express -4.000000
# 3                                   No Holds Barred  4.000000
# 4  Dear Zachary: A Letter to a Son About His Father -4.000000
# 5                                             Faust -4.000000
# 6                                      Hear My Song -4.000000
# 7                       Confessions of a Shopaholic -4.000000
# 8        Twilight Saga: Breaking Dawn - Part 1, The -4.000000
# 9                                       Taxi Driver -3.806931
# 10                                      Taxi Driver -3.806931

# Note that these all seem to be obscure movies and in our model many of them
# obtained large predictions. So why did this happen? To see what's going on,
# let's look at the top 10 best movies in the top 10 worst movies based on the
# estimates of the movie effect b hat i. So we can see the movie titles, we're
# going to create a database that includes movie ID and titles using this very
# simple code. So here are the best 10 movies according to our estimates.

movie_titles <- movielens %>% 
     select(movieId, title) %>%
     distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
     arrange(desc(b_i)) %>% 
     select(title, b_i) %>% 
     slice(1:10) 

#    title                                                     b_i
#    <chr>                                                   <dbl>
#  1 Lamerica                                                 1.46
#  2 Love & Human Remains                                     1.46
#  3 Enfer, L'                                                1.46
#  4 Picture Bride (Bijo photo)                               1.46
#  5 Red Firecracker, Green Firecracker (Pao Da Shuang Deng)  1.46
#  6 Faces                                                    1.46
#  7 Maya Lin: A Strong Clear Vision                          1.46
#  8 Heavy                                                    1.46
#  9 Gate of Heavenly Peace, The                              1.46
# 10 Death in the Garden (Mort en ce jardin, La)              1.46

# America is number one, Love and Human Remains also number one, Infer L number
# one. Look at the rest of the movies in this table. And here are the top 10
# worst movies.

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
     arrange(b_i) %>% 
     select(title, b_i) %>% 
     slice(1:10) 

#   title                                          b_i
#   <chr>                                        <dbl>
# 1 Santa with Muscles                           -3.04
# 2 B*A*P*S                                      -3.04
# 3 3 Ninjas: High Noon On Mega Mountain         -3.04
# 4 Barney's Great Adventure                     -3.04
# 5 Merry War, A                                 -3.04
# 6 Day of the Beast, The (Día de la Bestia, El) -3.04
# 7 Children of the Corn III                     -3.04
# 8 Whiteboyz                                    -3.04
# 9 Catfish in Black Bean Sauce                  -3.04
# 10 Watcher, The                                 -3.04

# The first one started with Santa with Muscles. Now they all have something in
# common. They're all quite obscure. So let's look at how often they were rated.
# Here's the same table, but now we include the number of ratings they received
# in our training set.

train_set %>% count(movieId) %>% 
     left_join(movie_avgs) %>%
     left_join(movie_titles, by="movieId") %>%
     arrange(desc(b_i)) %>% 
     select(title, b_i, n) %>% 
     slice(1:10) 

#> Joining, by = "movieId"
#> # A tibble: 10 x 3
#>   title                                                     b_i     n
#>   <chr>                                                   <dbl> <int>
#> 1 Lamerica                                                 1.46     1
#> 2 Love & Human Remains                                     1.46     3
#> 3 Enfer, L'                                                1.46     1
#> 4 Picture Bride (Bijo photo)                               1.46     1
#> 5 Red Firecracker, Green Firecracker (Pao Da Shuang Deng)  1.46     3
#> 6 Faces                                                    1.46     1
#> # ... with 4 more rows

train_set %>% count(movieId) %>% 
     left_join(movie_avgs) %>%
     left_join(movie_titles, by="movieId") %>%
     arrange(b_i) %>% 
     select(title, b_i, n) %>% 
     slice(1:10)

#> Joining, by = "movieId"
#> # A tibble: 10 x 3
#>   title                                          b_i     n
#>   <chr>                                        <dbl> <int>
#> 1 Santa with Muscles                           -3.04     1
#> 2 B*A*P*S                                      -3.04     1
#> 3 3 Ninjas: High Noon On Mega Mountain         -3.04     1
#> 4 Barney's Great Adventure                     -3.04     1
#> 5 Merry War, A                                 -3.04     1
#> 6 Day of the Beast, The (Día de la Bestia, El) -3.04     1
#> # ... with 4 more rows 

# We can see the same for the bad movies. So the supposed best and worst movies
# were rated by very few users, in most cases just one. These movies were mostly
# obscure ones. This is because with just a few users, we have more uncertainty,
# therefore larger estimates of bi, negative or positive, are more likely when
# fewer users rate the movies. These are basically noisy estimates that we
# should not trust, especially when it comes to prediction. Large errors can
# increase our residual mean squared error, so we would rather be conservative
# when we're not sure. Previously we've learned to compute standard errors and
# construct confidence intervals to account for different levels of uncertainty.
# However, when making predictions we need one number, one prediction, not an
# interval. For this, we introduce the concept of regularization.

# Regularization permits us to penalize large estimates that come from small
# sample sizes. It has commonalities with the Bayesian approaches that shrunk
# predictions. The general idea is to add a penalty for large values of b to the
# sum of squares equations that we minimize. So having many large b's makes it
# harder to minimize the equation that we're trying to minimize. One way to
# think about this is that if we were to fit an effect to every rating, we could
# of course make the sum of squares equation by simply making each b match its
# respective rating y. This would yield an unstable estimate that changes
# drastically with new instances of y. Remember y is a random variable. But by
# penalizing the equation, we optimize to b bigger when the estimate b are far
# from zero. We then shrink the estimates towards zero. Again, this is similar
# to the Bayesian approach we've seen before. So this is what we do.

# To estimate the b's instead of minimizing the residual sum of squares
# as is done by least squares, we now minimize this equation.

#    1N∑u,i(yu,i−μ−bi)2+λ∑ib2

# Note the penalty term. The first term is just the residual sum of squares and
# the second is a penalty that gets larger when many b's are large. Using
# calculus, we can actually show that the values of b that minimized equation
# are given by this formula, where ni is a number of ratings b for movie i.

#    ^bi(λ)=1λ+nini∑u=1(Yu,i−^μ)

# Note that this approach will have our desired effect. When ni is very large
# which will give us a stable estimate, then lambda is effectively ignored
# because ni plus lambda is about equal to ni. However, when ni is small, then
# the estimate of bi is shrunken towards zero. The larger lambda, the more we
# shrink. So let's compute these regularized estimates of vi using lambda equals
# to 3.0. Later we see why we picked this number. So here is the code.

lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
     group_by(movieId) %>% 
     summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

# To see how the estimates shrink, let's make a plot of the regularized estimate
# versus the least square estimates with the size of the circle telling us how
# large ni was.

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
     ggplot(aes(original, regularlized, size=sqrt(n))) + 
     geom_point(shape=1, alpha=0.5)

#  # A tibble: 10 x 3
#    title                          b_i     n
#    <chr>                         <dbl> <int>
#  1 All About Eve                0.927    26
#  2 Shawshank Redemption, The    0.921   240
#  3 Godfather, The               0.897   153
#  4 Godfather: Part II, The      0.871   100
#  5 Maltese Falcon, The          0.860    47
#  6 Best Years of Our Lives, The 0.859    11
#  # ... with 4 more rows

# You can see that when n is small, the values are shrinking more towards zero.
# All right, so now let's look at our top 10 best movies based on the estimates
# we got when using regularization.

train_set %>%
     count(movieId) %>% 
     left_join(movie_reg_avgs, by = "movieId") %>%
     left_join(movie_titles, by = "movieId") %>%
     arrange(desc(b_i)) %>% 
     select(title, b_i, n) %>% 
     slice(1:10)

# Note that the top five movies are now All
# About Eve, Shawshank Redemption, The Godfather, The Godfather
# II, and the Maltese Falcons.
# This makes much more sense.


# We can also look at the worst movies and the worst five are Battlefield Earth,
# Joe's Apartment, Speed 2, Cross Control, Super Mario Bros, and Police Academy
# 6: City Under Siege. Again, this makes sense.

train_set %>%
     count(movieId) %>% 
     left_join(movie_reg_avgs, by = "movieId") %>%
     left_join(movie_titles, by="movieId") %>%
     arrange(b_i) %>% 
     select(title, b_i, n) %>% 
     slice(1:10) 

#> # A tibble: 10 x 3
#>   title                                b_i     n
#>   <chr>                              <dbl> <int>
#> 1 Battlefield Earth                  -2.06    14
#> 2 Joe's Apartment                    -1.78     7
#> 3 Speed 2: Cruise Control            -1.69    20
#> 4 Super Mario Bros.                  -1.60    13
#> 5 Police Academy 6: City Under Siege -1.57    10
#> 6 After Earth                        -1.52     4
#> # ... with 4 more rows

# So do we improve our results? We certainly do.

predicted_ratings <- test_set %>% 
     left_join(movie_reg_avgs, by = "movieId") %>%
     mutate(pred = mu + b_i) %>%
     pull(pred)

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse))
rmse_results

#   method                          RMSE
#   <chr>                          <dbl>
# 1 Just the average               1.05 
# 2 Movie Effect Model             0.986
# 3 Movie + User Effects Model     0.908
# 4 Regularized Movie Effect Model 0.908

# We get the residual mean squared error all the way down to 0.885 from 0.986.
# So this provides a very large improvement. Now note that lambda is a tuning
# parameter. We can use cross-fertilization to choose it. We can use this code
# to do this.

lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
     group_by(movieId) %>% 
     summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
     predicted_ratings <- test_set %>% 
          left_join(just_the_sum, by='movieId') %>% 
          mutate(b_i = s/(n_i+l)) %>%
          mutate(pred = mu + b_i) %>%
          pull(pred)
     return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

# And we see why we picked 3.0 as lambda. One important point. Note that we show
# this as an illustration and in practice, we should be using full
# cross-validation just on a training set without using the test it until the
# final assessment.
 
# We can also use regularization to estimate the user effect. The equation we
# would minimize would be this one now.

#    1N∑u,i(yu,i−μ−bi−bu)2+λ(∑ib2i+∑ub2u)

# It includes the parameters for the user effects as well. The estimates that
# minimizes can be found similarly to what we do previously. Here we again use
# cross-validation to pick lambda.

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
     
     mu <- mean(train_set$rating)
     
     b_i <- train_set %>% 
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/(n()+l))
     
     b_u <- train_set %>% 
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu)/(n()+l))
     
     predicted_ratings <- 
          test_set %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          mutate(pred = mu + b_i + b_u) %>%
          pull(pred)
     
     return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses) 

# We see what lambda minimizes our equation. For the full model including movie
# and user effects, the optimal lambda is 3.75. And we can see that we indeed
# improved our residual mean squared error. Now it's 0.881.

lambda <- lambdas[which.min(rmses)]
lambda
#> [1] 3.75
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()
# 
#      |method                                |      RMSE|
#      |:-------------------------------------|---------:|
#      |Just the average                      | 1.0482202|
#      |Movie Effect Model                    | 0.9862839|
#      |Movie + User Effects Model            | 0.9077043|
#      |Regularized Movie Effect Model        | 0.9077043|
#      |Regularized Movie + User Effect Model | 0.8806419|
