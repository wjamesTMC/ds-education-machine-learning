# Setup 
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)
library(caret)
library(e1071)

rmse <- function(num_reps) {
     print(num_reps)
     x = num_reps * 2
     print(x)
}
set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, rmse)
