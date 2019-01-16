# --------------------------------------------------------------------------------
#
# Comprehension Check - Linear regression for prediction
#
# --------------------------------------------------------------------------------

# Setup 
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)
library(caret)
library(e1071)

# Question 1: Create a data set using the following code:
     
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
     data.frame() %>% setNames(c("x", "y"))

# Use the caret package to partition the dataset into test and training sets of
# equal size. 

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

# Train a linear model and calculate the RMSE. 
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)

The dependent variable is y and predictor is height.


Once lm_fit is found I was expecting y_hat will be calculated using the lm_fit like this:
     
     y_hat <- predict(lm_fit, test_set)

Instead, he calculated y_hat using the model and then calculated y_hat from p_hat. 


p_hat <- predict(lm_fit, test_set)

# The above code simply produced object "p_hat", which houses all of the
# "prediction" values.

y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()

# Now that we have all of our prediction values (Female or Male "dummified" into
# 0 or 1), the above code tells your machine to ultimately predict "Female" if
# p_hat (all predictions) is greater than 0.5. If you remember, Female = 1 and
# Male = 0. The last bit just makes y_hat a factor.

mean(RMSE) : two decimal places are sufficient
sd(RMSE): for a correct answer three decimal places are necessary.

# Repeat this exercise 100 times and report the mean and standard deviation of
# the RMSEs. (Hint: You can use the code shown in a previous course inside a
# call to replicate using a seed of 1).



