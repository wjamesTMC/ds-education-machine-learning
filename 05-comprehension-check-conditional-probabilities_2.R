#---------------------------------------------------------------------------
#
# Comprehension check on condditional probabilities (Part 2)
#
#---------------------------------------------------------------------------

#
# Question 1: We are now going to write code to compute conditional
# probabilities for being male in the heights dataset. Round the heights to the
# closest inch. Plot the estimated conditional probability P(x) = Pr(Male |
# height = x) for each x.

library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)

# Missing code choices
heights %>% 
     group_by(height) %>%
     summarize(p = mean(sex == "Male")) %>%
#1 Comment: no rounding
     
heights %>% 
     mutate(height = round(height)) %>%
     group_by(height) %>%
     summarize(p = mean(sex == "Female")) %>%
#2 Comment: wrong sex
     
heights %>% 
     mutate(height = round(height)) %>%
     summarize(p = mean(sex == "Male")) %>%
#3 Comment: looks possible
     
heights %>% 
     mutate(height = round(height)) %>%
     group_by(height) %>%
     summarize(p = mean(sex == "Male")) %>%
#4 Comment: same as above but with group_by

# Test of #1:
library(dslabs)
data("heights")
heights %>% 
     group_by(height) %>%
     summarize(p = mean(sex == "Male")) %>%
     qplot(height, p, data =.)

# Test of #2:
library(dslabs)
data("heights")
heights %>% 
     mutate(height = round(height)) %>%
     group_by(height) %>%
     summarize(p = mean(sex == "Female")) %>%
     qplot(height, p, data =.)

# Test of #3:
library(dslabs)
data("heights")
heights %>% 
     mutate(height = round(height)) %>%
     summarize(p = mean(sex == "Male")) %>%
     qplot(height, p, data =.)

# Test of #4:
library(dslabs)
data("heights")
heights %>% 
     mutate(height = round(height)) %>%
     group_by(height) %>%
     summarize(p = mean(sex == "Male")) %>%
     qplot(height, p, data =.)

