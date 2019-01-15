#---------------------------------------------------------------------------
#
# Comprehension check on condditional probabilities (Part 4)
#
#---------------------------------------------------------------------------

library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)

# Question 3: You can generate data from a bivariate normal distrubution using
# the MASS package using the following code.

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
     data.frame() %>% setNames(c("x", "y"))

# And make a quick plot using plot(dat).

plot(dat)

# Using an approach similar to that used in the previous exercise, let's
# estimate the conditional expectations and make a plot. Part of the code has
# been provided for you:

# Trials for the missing code line
ps <- seq(0, 1, 0.1)
dat %>% 
# Missing code	#1
     mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
     group_by(g) %>%
     summarize(y = mean(y), x = mean(x)) %>%
     # End of missing code segment
     qplot(x, y, data =.)

# Does not include the lowest value
ps <- seq(0, 1, 0.1)
dat %>% 
     # Missing code	#2
     mutate(g = cut(x, quantile(x, ps))) %>%
     group_by(g) %>%
     summarize(y = mean(y), x = mean(x)) %>%
     # End of missing code segment
     qplot(x, y, data =.)

# This produces a single data point plot
ps <- seq(0, 1, 0.1)
dat %>% 
     # Missing code	#3
     mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
     summarize(y = mean(y), x = mean(x)) %>%
     # End of missing code segment
     qplot(x, y, data =.)

# Generates an error because y is not length 1
ps <- seq(0, 1, 0.1)
dat %>% 
     # Missing code	#4
     mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
     group_by(g) %>%
     summarize(y =(y), x =(x)) %>%
     # End of missing code segment
     qplot(x, y, data =.)
