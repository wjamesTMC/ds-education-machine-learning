#---------------------------------------------------------------------------
#
# Comprehension check on condditional probabilities (Part 3)
#
#---------------------------------------------------------------------------

library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)

# Question 2: In the plot we just made in Q1 we see high variability for low
# values of height. This is because we have few data points. This time use the
# quantile (0.1,0.2,...,0.9)and the cut function to assure each group has the
# same number of points. Note that for any numeric vector x, you can create
# groups based on quantiles like this:

#    cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).

# Part of the code is provided here:
     
ps <- seq(0, 1, 0.1)
heights %>% 
#    MISSING CODE
     group_by(g) %>%
     summarize(p = mean(sex == "Male"), height = mean(height)) %>%
     qplot(height, p, data =.)

# Code choices
# mutate(g = cut(male, quantile(height, ps), include.lowest = TRUE)) %>%
# mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
# mutate(g = cut(female, quantile(height, ps), include.lowest = TRUE)) %>%
# mutate(g = cut(height, quantile(height, ps))) %>%
     
# This does not work because "male" is not recognized inb the cut statement
ps <- seq(0, 1, 0.1)
heights %>% 
     mutate(g = cut(male, quantile(height, ps), include.lowest = TRUE)) %>%
     group_by(g) %>%
     summarize(p = mean(sex == "Male"), height = mean(height)) %>%
     qplot(height, p, data =.)     

# This is the correct one
ps <- seq(0, 1, 0.1)
heights %>% 
     mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
     group_by(g) %>%
     summarize(p = mean(sex == "Male"), height = mean(height)) %>%
     qplot(height, p, data =.) 

# This does not work because "female" is not recognized inb the cut statement
ps <- seq(0, 1, 0.1)
heights %>% 
     mutate(g = cut(female, quantile(height, ps), include.lowest = TRUE)) %>%
     group_by(g) %>%
     summarize(p = mean(sex == "Male"), height = mean(height)) %>%
     qplot(height, p, data =.)

# This is not correct because it does not include the lowest value clause
ps <- seq(0, 1, 0.1)
heights %>% 
     mutate(g = cut(height, quantile(height, ps))) %>%
     group_by(g) %>%
     summarize(p = mean(sex == "Male"), height = mean(height)) %>%
     qplot(height, p, data =.)
