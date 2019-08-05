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

# Matrix factorisation is a widely used concept in machine learning. It is very
# much related to factor analysis, single value composition, and principal
# component analysis, or PCA. Here we describe the concept in the context of
# movie recommendation systems.

# We have previously described the following model which accounts for movie and
# movie differences through the parameters bi, and user reviews or differences
# through parameters bu. But this model leaves out an important source of
# variation related to the fact that groups of movies have similar rating
# patterns and groups of users have similar rating patterns as well.

# We will discover these patterns by studying the residuals obtained after
# fitting our model. These residuals.

# To study these residuals, we will convert the data into a matrix so that each
# user gets a row and each movie gets a column. So yui is the entry in row u and
# column i. User u, movie i.

# For illustration purposes, we will only consider a small subset of movies with
# many ratings and users that have rated many movies.

# We will use this code to generate our training data.

train_small <- movielens %>% 
     group_by(movieId) %>%
     filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% 
     group_by(userId) %>%
     filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
     select(userId, movieId, rating) %>%
     spread(movieId, rating) %>%
     as.matrix()

# To facilitate exploration we add row names and column names.

rownames(y)<- y[,1]
y <- y[,-1]

movie_titles <- movielens %>% 
     select(movieId, title) %>%
     distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

# The column names will be the movie names. And we convert these residuals by
# removing the column and row averages. Here's the code.

y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))

# OK, now. If the model we've been using describes all the signal and the extra
# ones are just noise, then the residuals for different movies should be
# independent of each other. But they are not. Here's an example.

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

# Here's a plot of the residuals for The Godfather and The Godfather II. They're
# very correlated. This plot says that users that liked the godfather more than
# what the model expects them to based on the movie and user effects also like
# The Godfather II more than expected. The same is true for The Godfather and
# Goodfellas.

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

# You can see it in this plot. Although not as strong, there still is a
# correlation. We see a correlation between other movies as well.

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

# For example, here's a correlation between You've Got Mail and Sleepless in
# Seattle. We can see a pattern.

x <- y[, c(m_1, m_2, m_3, m_4, m_5)]
colnames(x)[1:2] <- c("Godfather", "Godfather 2")
cor(x, use="pairwise.complete")
#>                      Godfather Godfather 2 Goodfellas You've Got Mail
#> Godfather                1.000       0.829      0.444          -0.440
#> Godfather 2              0.829       1.000      0.521          -0.331
#> Goodfellas               0.444       0.521      1.000          -0.481
#> You've Got Mail         -0.440      -0.331     -0.481           1.000
#> Sleepless in Seattle    -0.378      -0.358     -0.402           0.533
#>                      Sleepless in Seattle
#> Godfather                          -0.378
#> Godfather 2                        -0.358
#> Goodfellas                         -0.402
#> You've Got Mail                     0.533
#> Sleepless in Seattle                1.000

# If we look at the pairwise correlation for these five movies, we can see that
# there's a positive correlation between the gangster movies Godfathers and
# Goodfellas, and then there's a positive correlation between the romantic
# comedies You've Got Mail and Sleepless in Seattle. We also see a negative
# correlation between the gangster movies and the romantic comedies. This means
# that users that like gangster movies a lot tend to not like romantic comedies
# and vise versa. This result tells us that there is structure in the data that
# the model does not account for. So how do we model this? Here is where we use
# matrix factorization. We're going to define factors. Here's an illustration of
# how we could use some structure to predict the residuals. Suppose the
# residuals look like this.

round(r, 1)
#>    Godfather Godfather 2 Goodfellas You've Got Mail Sleepless in Seattle
#> 1        2.0         2.3        2.2            -1.8                 -1.9
#> 2        2.0         1.7        2.0            -1.9                 -1.7
#> 3        1.9         2.4        2.1            -2.3                 -2.0
#> 4       -0.3         0.3        0.3            -0.4                 -0.3
#> 5       -0.3        -0.4        0.3             0.2                  0.3
#> 6       -0.1         0.1        0.2            -0.3                  0.2
#> 7       -0.1         0.0       -0.2            -0.2                  0.3
#> 8        0.2         0.2        0.1             0.0                  0.4
#> 9       -1.7        -2.1       -1.8             2.0                  2.4
#> 10      -2.3        -1.8       -1.7             1.8                  1.7
#> 11      -1.7        -2.0       -2.1             1.9                  2.3
#> 12      -1.8        -1.7       -2.1             2.3                  2.0

# This is a simulation. There seems to be a pattern here.

cor(r) 
#>                      Godfather Godfather 2 Goodfellas You've Got Mail
#> Godfather                1.000       0.980      0.978          -0.974
#> Godfather 2              0.980       1.000      0.983          -0.987
#> Goodfellas               0.978       0.983      1.000          -0.986
#> You've Got Mail         -0.974      -0.987     -0.986           1.000
#> Sleepless in Seattle    -0.966      -0.992     -0.989           0.986
#>                      Sleepless in Seattle
#> Godfather                          -0.966
#> Godfather 2                        -0.992
#> Goodfellas                         -0.989
#> You've Got Mail                     0.986
#> Sleepless in Seattle                1.000

# It's based on what we saw with the real data. There's a gangster movie effect
# and there's a romantic comedy effect. In fact, we see a very strong
# correlation pattern, which we can see here. This structure could be explained
# using the following coefficients.

t(q) 
#>      Godfather Godfather 2 Goodfellas You've Got Mail Sleepless in Seattle
#> [1,]         1           1          1              -1                   -1

# We assign a 1 to the gangster movies and a minus one to the romantic comedies.
# In this case, we can narrow down movies to two groups, gangster and romantic
# comedy. Note that we can also reduce the users to three groups, those that
# like gangster movies but hate romantic comedies, the reverse, and those that
# don't care.

p
#>    [,1]
#> 1     2
#> 2     2
#> 3     2
#> 4     0
#> 5     0
#> 6     0
#> 7     0
#> 8     0
#> 9    -2
#> 10   -2
#> 11   -2
#> 12   -2

# The main point here is that we can reconstruct this data that has 60 values
# with a couple of vectors totaling 17 values. Those two vectors we just showed
# can be used to form the matrix with 60 values. We can model the 60 residuals
# with the 17 parameter model like this. And this is where the factorization
# name comes in. We have a matrix r and we factorised I used it into two things,
# the vector p, and the vector q.

# ru,i≈puqi

# Now we should be able to explain much more of the variance if we use a model
# like this one.

# Yu,i=μ+bi+bu+puqi+εu,i

# Now the structure in our movie data seems to be much more complicated than
# gangster movie versus romantic comedies. We have other factors. For example,
# and this is a simulation, let's suppose we had the movie Scent of a Woman, and
# now the data looks like this.

round(r, 1)
#>    Godfather Godfather 2 Goodfellas You've Got Mail Sleepless in Seattle
#> 1        0.5         0.6        1.6            -0.5                 -0.5
#> 2        1.5         1.4        0.5            -1.5                 -1.4
#> 3        1.5         1.6        0.5            -1.6                 -1.5
#> 4       -0.1         0.1        0.1            -0.1                 -0.1
#> 5       -0.1        -0.1        0.1             0.0                  0.1
#> 6        0.5         0.5       -0.4            -0.6                 -0.5
#> 7        0.5         0.5       -0.5            -0.6                 -0.4
#> 8        0.5         0.6       -0.5            -0.5                 -0.4
#> 9       -0.9        -1.0       -0.9             1.0                  1.1
#> 10      -1.6        -1.4       -0.4             1.5                  1.4
#> 11      -1.4        -1.5       -0.5             1.5                  1.6
#> 12      -1.4        -1.4       -0.5             1.6                  1.5
#>    Scent of a Woman
#> 1              -1.6
#> 2              -0.4
#> 3              -0.5
#> 4               0.1
#> 5              -0.1
#> 6               0.5
#> 7               0.4
#> 8               0.4
#> 9               0.9
#> 10              0.5
#> 11              0.6
#> 12              0.6

# Now we see another factor, a factor that divides users into those that love,
# those that hate, and those that don't care for Al Pacino. The correlation is a
# bit more complicated now. We can see it here.

cor(r)
#>                      Godfather Godfather 2 Goodfellas You've Got Mail
#> Godfather                1.000       0.997      0.562          -0.997
#> Godfather 2              0.997       1.000      0.577          -0.998
#> Goodfellas               0.562       0.577      1.000          -0.552
#> You've Got Mail         -0.997      -0.998     -0.552           1.000
#> Sleepless in Seattle    -0.996      -0.999     -0.583           0.998
#> Scent of a Woman        -0.571      -0.583     -0.994           0.558
#>                      Sleepless in Seattle Scent of a Woman
#> Godfather                          -0.996           -0.571
#> Godfather 2                        -0.999           -0.583
#> Goodfellas                         -0.583           -0.994
#> You've Got Mail                     0.998            0.558
#> Sleepless in Seattle                1.000            0.588
#> Scent of a Woman                    0.588            1.000

# Now to explain the structure, we need two factors. Here they are.

t(q) 
#>      Godfather Godfather 2 Goodfellas You've Got Mail Sleepless in Seattle
#> [1,]         1           1          1              -1                   -1
#> [2,]         1           1         -1              -1                   -1
#>      Scent of a Woman
#> [1,]               -1
#> [2,]                1

# The first one divides gangster movies from romantic comedies. The second
# factor divide Al Pacino movies and non Al Pacino movies. And we also have two
# sets of coefficients to describe the users. You can see it here.

p
#>    [,1] [,2]
#> 1     1 -0.5
#> 2     1  0.5
#> 3     1  0.5
#> 4     0  0.0
#> 5     0  0.0
#> 6     0  0.5
#> 7     0  0.5
#> 8     0  0.5
#> 9    -1  0.0
#> 10   -1 -0.5
#> 11   -1 -0.5
#> 12   -1 -0.5

# The model now has more parameters, but still less than the original data.

# Yu,i=μ+bi+bu+pu,1q1,i+pu,2q2,i+εu,i

# So we should be able to fit this model using, for example, the least squares
# method. However, for the Netflix challenge, they used regularization, and they
# penalize not just the user and movie effects, but also large values of the
# factors p or q. Now does this simulation match the actual data?

# Here are the correlation we get for the movies we just showed, but using the
# actual data. Notice that the structure is similar. However, if we want to find
# the structure using the data as opposed to constructing it ourselves as we did
# in the example, we need to fit models to data. So now we have to figure out
# how to estimate factors from the data as opposed to defining them ourselves.
# One way to do this is to fit models, but we can also use principle component
# analysis or equivalently, the singular reality composition to estimate factors
# from data. And we're going to show that in the next video.

