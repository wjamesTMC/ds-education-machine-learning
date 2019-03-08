# --------------------------------------------------------------------------------
#
# Recommendation Systems - Comprehension Check
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

data("movielens")
movielens %>% 
     summarize(n_users = n_distinct(userId),
               n_movies = n_distinct(movieId))

head(movielens)

#
# Q1
#

# Compute the number of ratings for each movie and then plot it against the year
# the movie came out. Use the square root transformation on the counts. What
# year has the highest median number of ratings?
     
     
movies <- movielens %>% 
     select(year, movieId, title, genres, rating)

x <- movies %>% group_by(year)
x <- data.frame(x[order(x$year, decreasing=FALSE),])

z <- movies %>% group_by(title)
z <- data.frame(x[order(x$title, decreasing=FALSE),])
                         
year_ratings <- data_frame(x_year = 1:115, x_ratings = 1:115)

counter <- 1
for(i in 1902:2016) {
     p <- x %>% filter(year == i)
     year_ratings[counter,1] <- i
     year_ratings[counter,2] <- sqrt(nrow(p))
     
     counter <- counter + 1
}

tail(year_ratings,25)
x <- year_ratings$x_year
y <- year_ratings$x_ratings

year_ratings %>% ggplot(aes(x, y, fill = x, group_by(x))) +
     geom_bar(colour = "black", fill="#DD8888", width=.8, stat = "identity") +
     geom_text(aes(label=y),position=position_stack(1.1)) +
     xlab("Year") + ylab("Number of Ratings") 

# Instructor solution
movielens %>% group_by(movieId) %>%
     summarize(n = n(), year = as.character(first(year))) %>%
     qplot(year, n, data = ., geom = "boxplot") +
     coord_trans(y = "sqrt") +
     theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Answer: 1995

#
# Q2
#

# We see that, on average, movies that came out after 1993 get more ratings. We
# also see that with newer movies, starting in 1993, the number of ratings
# decreases with year: the more recent a movie is, the less time users have had
# to rate it.

# Among movies that came out in 1993 or later, what are the 25 movies with the
# most ratings per year, and what is the average rating of each of the top 25
# movies?

# What is the average rating for the movie The Shawshank Redemption?

movies <- movielens %>% 
     select(movieId, year, title, genres, rating)

z <- movies %>% group_by(title)
z <- data.frame(x[order(x$title, decreasing=FALSE),])
head(z)

p <- z %>% filter(title == "Shawshank Redemption, The")
mean(p$rating)
# [1] 4.487138

# What is the average number of ratings per year for the movie Forrest Gump?

z <- movies %>% group_by(title)
ny <- 0
nr <- z %>% filter(title == "Forrest Gump") # 341


fg <- data.frame(year = 1:22, num_ratings = 1:22)
x <- z %>% filter(title == "Forrest Gump" & year == 1994)
nrow(x) # 341 ratings between 1994 and 2016. Ave # ratings 14.826
for(i in 1:1000) {
     print(x$year)
}

yc <- 1
nr <- 0
for(i in 1994:2016) {
     x <- z %>% filter(title == "Forrest Gump" & year == i)
     nr <- nr + nrow(x)
     yc <- yc + 1
}
nr # 341
yc # 24
nr / yc # 14.20833


#
# Q3
#

# From the table constructed in Q2, we can see that the most frequently rated
# movies tend to have above average ratings. This is not surprising: more people
# watch popular movies. To confirm this, stratify the post-1993 movies by
# ratings per year and compute their average ratings. Make a plot of average
# rating versus ratings per year and show an estimate of the trend.

post_1993 <- movies %>% filter(year > 1993)
nrow(post_1993) # 60253

df <- data.frame(year = 1:23, rpy = 1:23, avg_rpy = 1:23)

counter <- 1
tot_ratings <- 0
for(i in 1994:2016) {
     x <- post_1993 %>% filter(year == i)
     df[counter,1] <- i
     df[counter,2] <- nrow(x)
     df[counter,3] <- mean(x$rating)
     counter <- counter + 1
}
df %>% ggplot(aes(x = avg_rpy, y = rpy)) + scale_y_log10() + geom_point()
df %>% ggplot(aes(x = rpy, y = avg_rpy)) + scale_y_log10() + geom_point() +
     geom_smooth(method="lm")

# Answer: The more often a movie is rated, the higher its average rating.

#
# Q4
#

# Suppose you are doing a predictive analysis in which you need to fill in the
# missing ratings with some value.

# Given your observations in the exercise in Q3, which of the following
# strategies would be most appropriate?
     
# Answer: Fill in the missing values with a lower value than the average rating
# across all movies. Because a lack of ratings is associated with lower ratings,
# it would be most appropriate to fill in the missing value with a lower value
# than the average. You should try out different values to fill in the missing
# value and evaluate prediction in a test set.

#
# Q5
#

# The movielens dataset also includes a time stamp. This variable represents the
# time and data in which the rating was provided. The units are seconds since
# January 1, 1970. Create a new column date with the date.

library(lubridate)
data("movielens")
movielens %>% 
     summarize(n_users = n_distinct(userId),
               n_movies = n_distinct(movieId))

# Which code correctly creates this new column?
movielens <- mutate(movielens, date = as.date(timestamp))     # Not correct
movielens <- mutate(movielens, date = as_datetime(timestamp)) # Correct but mis-typed
movielens <- mutate(movielens, date = as.data(timestamp))     # Does not exist
movielens <- mutate(movielens, date = timestamp)              # Just dups the final field

head(movielens)

#
# Q6
#

# Compute the average rating for each week and plot this average against day.
# Hint: use the round_date function before you group_by. What type of trend do
# you observe?

movielens %>% mutate(date = round_date(date, unit = "week")) %>%
     group_by(date) %>%
     summarize(rating = mean(rating)) %>%
     ggplot(aes(date, rating)) +
     geom_point() +
     geom_smooth()

# We can see that there is some evidence of a time effect in the plot, but there
# is not a strong effect of time.

#
# Q7
#

# Consider again the plot you generated in Q6. If we define du,i as the day for
# user's u rating of movie i, which of the following models is most appropriate?

# Answer: Yu,i = u + bi + bu + f(du,i) + Eu,i where f(du,i) is a smooth function

#
# Q8
#

# The movielens data also has a genres column. This column includes every genre
# that applies to the movie. Some movies fall under several genres. Define a
# category as whatever combination appears in this column. Keep only categories
# with more than 1,000 ratings. Then compute the average and standard error for
# each category. Plot these as error bar plots.

# Which genre has the lowest average rating? Enter the name of the genre exactly
# as reported in the plot, including capitalization and punctuation.

new_category <- unique(movielens$genres) # 901
len_category <- length(new_category)

df <- data.frame(movielens[order(movielens$genres, decreasing=FALSE),])
df <- df %>% select(-movieId, -title, -year, genres, -userId, rating, -timestamp)
#str(df)
df <- mutate(df, avg_rating = 0, sd_rating = 0, nrows = 0)
#str(df)
df <- df[1:901,]
#str(df)

counter <- 1
for(i in 1:len_category) {
     x <- movielens %>% filter(genres == new_category[i])
     df[counter,1] <- new_category[i]
     df[counter,3] <- mean(x$rating, na.rm=TRUE)
     df[counter,4] <- sd(x$rating, na.rm=TRUE)
     df[counter,5] <- nrow(x)
     counter <- counter + 1
}

nrow(df) # 901
head(df,50)

final_df <- df %>% filter(nrows > 1000)
nrow(final_df)
final_df[order(final_df$avg_rating, decreasing=FALSE),]

# Instructor answer
movielens %>% group_by(genres) %>%
     summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
     filter(n >= 1000) %>% 
     mutate(genres = reorder(genres, avg)) %>%
     ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
     geom_point() +
     geom_errorbar() + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Answer: Comedy

#                              genres   rating avg_rating sd_rating nrows
# 11                           Comedy 3.273266   3.273266 1.1113090  6748
# 8                    Comedy|Romance 3.369872   3.369872 1.0509699  3973
# 6         Action|Adventure|Thriller 3.378273   3.378273 0.9709167  1413
# 17                     Comedy|Crime 3.406478   3.406478 1.1624506  1096
# 12                   Drama|Thriller 3.457917   3.457917 0.9726881  1402
# 4             Action|Crime|Thriller 3.481610   3.481610 0.9786834  1441
# 2  Action|Adventure|Sci-Fi|Thriller 3.486579   3.486579 1.0089094  1453
# 5           Action|Adventure|Sci-Fi 3.582945   3.582945 1.1103950  2146
# 16           Action|Sci-Fi|Thriller 3.612676   3.612676 1.0718891  1065
# 9              Comedy|Drama|Romance 3.621879   3.621879 0.9902201  3204
# 13                     Comedy|Drama 3.627292   3.627292 0.9966671  3272
# 7                     Drama|Romance 3.659445   3.659445 1.0300998  3462
# 1                             Drama 3.709230   3.709230 0.9748923  7757
# 18             Crime|Drama|Thriller 3.714482   3.714482 0.9574963  1091
# 10                 Action|Drama|War 3.843474   3.843474 1.0096704  1019
# 15                      Documentary 3.864385   3.864385 0.9275705  1154
# 14                      Crime|Drama 4.007816   4.007816 0.9143603  2367
# 3                         Drama|War 4.012853   4.012853 0.9454718  1167

#
# Q9
#

# The plot you generated in Q8 shows strong evidence of a genre effect. Consider
# this plot as you answer the following question.

# If we define gu,i as the genre for user's u rating of movie i, which of the following
# models is most appropriate?

# Answer: Opeion 3, where a weighting is applied to each genre specifically.
