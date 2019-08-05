# --------------------------------------------------------------------------------
#
# Comprehension Check - SVA and Matrix Factorization
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

# In this exercise, we will see one of the ways that this decomposition can be
# useful. To do this, we will construct a dataset that represents grade scores
# for 100 students in 24 different subjects. The overall average has been
# removed so this data represents the percentage point each student received
# above or below the average test score. So a 0 represents an average grade (C),
# a 25 is a high grade (A+), and a -25 represents a low grade (F). You can
# simulate the data like this:
     
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

# Our goal is to describe the student performances as succinctly as possible.
# For example, we want to know if these test results are all just a random
# independent numbers. Are all students just about as good? Does being good in
# one subject  imply you will be good in another? How does the SVD help with all
# this? We will go step by step to show that with just three relatively small
# pairs of vectors we can explain much of the variability in this  dataset.

#
# Q1
#

#You can visualize the 24 test scores for the 100 students by plotting an image:
     
     my_image <- function(x, zlim = range(x), ...){
          colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
          cols <- 1:ncol(x)
          rows <- 1:nrow(x)
          image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
                xlab="", ylab="",  col = colors, zlim = zlim, ...)
          abline(h=rows + 0.5, v = cols + 0.5)
          axis(side = 1, cols, colnames(x), las = 2)
     }

my_image(y)

# How would you describe the data based on this figure? Ans: The students that
# test well are at the top of the image and there seem to be three groupings by
# subject. correct

#
# Q2
#

# You can examine the correlation between the test scores directly like this:
     
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Which of the following best describes what you see?

# Ans: There is correlation among all tests, but higher if the tests are in
# science and math and even higher within each subject. correct


     
# The matrix vectorization decomposition that we showed in the previous video
# that looks something like this is very much related to singular value
# composition and PCA. Singular value composition and principal component
# analysis are complicated concepts, but one way to understand them is to think
# of, for example, singular value decomposition as an algorithm that finds the
# vectors p and q that permit us to write the matrix of residuals r with m rows
# and n columns in the following way.

# ru,i=pu,1q1,i+pu,2q2,i+⋯+pu,mqm,i

# But with the added bonus that the variability of these terms is decreasing and
# also that the p's are uncorrelated to each other. The algorithm also computes
# these variabilities so that we can know how much of the matrix's total
# variability is explained as we add new terms. This may permit us to see that
# with just a few terms, we can explain most of the variability. Let's see an
# example with our movie data. To compute the decomposition, will make all DNA
# zero. So we will write this code.

y[is.na(y)] <- 0
pca <- prcomp(y)

# The vectors q are called the principal components and they are stored in this
# matrix.

dim(pca$rotation)
#> [1] 454 292

# While the p vectors which are the user's effects are stored in this matrix.

dim(pca$x)
#> [1] 292 292

# The PCA function returns a component with the variability of each of the
# principal components and we can access it like this and plot it.

pc <- 1:nrow(x)
qplot(pc, pca$sdev)

# We can also see that just with a few of these principal components we already
# explain a large percent of the data.

var_explained <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
qplot(pc, var_explained)

# So for example, with just 50 principal components we're already explaining
# about half the variability out of a total of over 300 principal components. To
# see that the principal components are actually capturing something important
# about the data, we can make a plot of for example, the first two principal
# components, but now label the points with the movie that each one of those
# points is related to.

library(ggrepel)

pcs <- data.frame(pca$rotation, name = str_trunc(colnames(y), 30))

highlight <- filter(pcs, PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1)

pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
     geom_text_repel(aes(PC1, PC2, label=name),
                     data = highlight, size = 2)

# Just by looking at the top three in each direction, we see meaningful patterns.

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)
#>                         name    PC1
#> 1               Pulp Fiction -0.161
#> 2       Seven (a.k.a. Se7en) -0.152
#> 3                      Fargo -0.139
#> 4      2001: A Space Odyssey -0.139
#> 5  Silence of the Lambs, The -0.134
#> 6        Clockwork Orange, A -0.128
#> 7                Taxi Driver -0.127
#> 8       Being John Malkovich -0.119
#> 9      Royal Tenenbaums, The -0.106
#> 10              Shining, The -0.106

# The first principle component shows the difference between critically
# acclaimed movies on one side. Here are the one extreme of the principal
# component. You can see Pulp Fiction, Seven, Fargo, Taxi Driver, and Hollywood
# blockbusters on the other. So this principle component has critically
# acclaimed movies on one side and blockbusters on the other.

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)
#>                              name    PC1
#> 1   Independence Day (a.k.a. ID4) 0.1523
#> 2                           Shrek 0.1304
#> 3                      Spider-Man 0.1131
#> 4                         Titanic 0.1118
#> 5                         Twister 0.1110
#> 6                      Armageddon 0.1077
#> 7  Harry Potter and the Sorcer... 0.1026
#> 8                    Forrest Gump 0.0987
#> 9  Lord of the Rings: The Retu... 0.0942
#> 10             Enemy of the State 0.0932

# It's separating out movies that have structure and they're determined by users
# that like these more than these and others that like these more than that. We
# can also see that the second principle component also seems to capture
# structure in the data. If we look at one extreme of this principle component,
# we see arts and independent films such as Little Miss Sunshine, the Truman
# Show, and Slumdog Millionaire.

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)
#>                              name     PC2
#> 1       Shawshank Redemption, The -0.0919
#> 2                Truman Show, The -0.0916
#> 3            Little Miss Sunshine -0.0901
#> 4             Slumdog Millionaire -0.0882
#> 5  Amelie (Fabuleux destin d'A... -0.0848
#> 6               Kill Bill: Vol. 1 -0.0831
#> 7                 American Beauty -0.0806
#> 8    City of God (Cidade de Deus) -0.0760
#> 9                   Mars Attacks! -0.0747
#> 10              Beautiful Mind, A -0.0743

# When we look at the other extreme, we see what I would call nerd favorites,
# The Lord of the Rings, Star Wars, The Matrix.

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)
#>                              name    PC2
#> 1  Lord of the Rings: The Two ... 0.3291
#> 2  Lord of the Rings: The Fell... 0.3216
#> 3  Lord of the Rings: The Retu... 0.2178
#> 4                     Matrix, The 0.2156
#> 5  Star Wars: Episode IV - A N... 0.2019
#> 6  Star Wars: Episode VI - Ret... 0.1804
#> 7  Star Wars: Episode V - The ... 0.1581
#> 8                    Spider-Man 2 0.1086
#> 9                Dark Knight, The 0.1021
#> 10                          Speed 0.0983

# So using principal component analysis, we have shown that a matrix
# factorisation approach can find important structure in our data. Now to
# actually fit the matrix factorization model that we presented earlier that
# takes into account that there is missing data, that there's missing cells in
# the matrix, is a bit more complicated. For those interested we recommend
# trying the recommended lab package which fits these models.

