# --------------------------------------------------------------------------------
#
# Regularization - comprehension check
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

# An education expert is advocating for smaller schools. The expert bases this
# recommendation on the fact that among the best performing schools, many are
# small schools. Let's simulate a dataset for 100 schools. First, let's simulate
# the number of students in each school, using the following code:
     
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely independent
# from size. This is the parameter we want to estimate in our analysis. The true
# quality can be assigned using the following code:

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# We can see the top 10 schools using this code: 

schools %>% top_n(10, quality) %>% arrange(desc(quality))

# Now let's have the students in the school take a test. There is random
# variability in test taking, so we will simulate the test scores as normally
# distributed with the average determined by the school quality with a standard
# deviation of 30 percentage points. This code will simulate the test scores:
     
set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
     scores <- rnorm(schools$size[i], schools$quality[i], 30)
     scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# Q1 - What are the top schools based on the average score?
# Show just the ID, size, and the average score.
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)
#
# Report the ID of the top school 67
#
# What is the ID of the top school? Note that the school IDs are given in the
# form "PS x" - where x is a number. Report the number only.
#
# What is the average score of the 10th school? 88.09490

# Q2 Median school size overall
median(schools$size)
# Median size of top 10 schools based on score
x <- schools %>% top_n(10, score) %>% select(id, size, score)
median(x$size)

# Q3 Repeat for worst 10 
x <- schools %>% top_n(-10, score) %>% arrange(desc(score)) %>% select(id, size, score)

x <- schools %>% top_n(-10, score) %>% select(id, size, score)
median(x$size) #146

# Q4 - From this analysis, we see that the worst schools are also small. Plot
# the average score versus school size to see what's going on. Highlight the top
# 10 schools based on the true quality. Use a log scale to transform for the
# size.
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)
head(schools)
x <- schools %>% group_by(size) %>% arrange(desc(size)) %>%
     mutate(avg_score = mean(score)) %>% 
     select(id, size, avg_score) %>%
     ggplot(aes(size, avg_score)) +
     geom_point() + scale_x_log10()

schools %>% ggplot(aes(size, score)) +
     geom_point(alpha = 0.5) +
     geom_point(data = filter(schools, rank<=10), col = 2) 

# The standard error of the score has larger variability when the school is
# smaller, which is why both the best and the worst schools are more likely to
# be small.

# Q5
#

# Let's use regularization to pick the best schools. Remember regularization
# shrinks deviations from the average towards 0. To apply regularization here,
# we first need to define the overall average for all schools, using the
# following code:
overall <- mean(sapply(scores, mean)) # 79.99628

# Then, we need to define, for each school, how it deviates from that average.
#
# Write code that estimates the score above the average for each school but
# dividing by n + a instead of n, with n the schools size and a a regularization
# parameters. Try 25.

reg_param <- 25
# STOPPED HERE
head(schools)
x <- schools %>% group_by(size) %>% 
     mutate(avg_score = mean(score), sata = ) %>% 
     select(id, size, avg_score) arrange(desc(size)) %>%
