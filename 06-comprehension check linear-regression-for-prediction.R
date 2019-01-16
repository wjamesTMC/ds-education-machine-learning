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

# Question 1

# ******************
# Sample from LJames
# ******************
#Create data
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>% 
     data.frame() %>% setNames(c("x", "y"))

#Partition
train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
train <- dat[-train_index,]
test <- dat[train_index,] 
     
#Model
model <- lm(y ~ x, data = train)
model
# Call:
# lm(formula = y ~ x, data = train)
# 
# Coefficients:
#      (Intercept)            x  
# 43.6047       0.3678 

pred <- predict(model, test)
pred
# #        1        2        4        7       10       12       13       15       17       19 
# 68.72499 69.13358 70.41847 69.05170 67.76154 69.60851 67.59759 70.17063 69.14240 69.49340 
# 23       25       26       27       28       29       31       32       33       35 
# 69.17096 69.62886 68.53455 68.87306 67.59677 68.90051 70.24639 69.20800 69.05856 67.49642 
# 37       40       41       42       43       53       58       60       61       62 
# 68.77063 69.74195 69.88022 68.09017 70.56581 69.48275 68.32638 67.82107 71.04160 69.07550 
# 63       65       67       68       69       70       71       74       76       79 
# 69.05646 68.61275 67.39736 71.16754 69.20743 70.94281 68.16246 68.13136 69.27898 68.48559 
# 80       81       82       83       84       86       88       90       91       92 
# 67.75168 69.11725 68.30939 69.98569 68.33498 69.38706 69.11331 69.74746 68.56065 69.91370 
# 94       98 
# 69.19231 68.99398

#Performance
res <- test$y-pred
model_rmse <- sqrt(mean(res^2))
model_rmse
# [1] 2.45367     

# ************************************************
# L James code to replicate 100 times
# ************************************************
#Create data
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>% 
     data.frame() %>% setNames(c("x", "y"))

#Begin replication experiment
set.seed(1)
results <- replicate(100, expr = {
     
     #Partition
     train_index <- createDataPartition(dat$y, p = .5, list = FALSE, times = 1)
     train <- dat[-train_index,]
     test <- dat[train_index,] 
     
     #Model
     model <- lm(y ~ x, data = train)
     pred <- predict(model, test) 
     
     #Performance
     res <- test$y-pred
     model_rmse <- sqrt(mean(res^2))
     model_rmse
})

# ************************************************
# My code
# ************************************************
     
# Create a data set using the following code
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
     data.frame() %>% setNames(c("x", "y"))

# Use the caret package to partition the dataset into test and training sets of
# equal size. 
<<<<<<< HEAD
=======

>>>>>>> 6d4e6ae93b74750cfb4ae3c674090462e76ee544
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)

# Train a linear model 
model <- lm(y ~ x, data = train_set)
model_prediction <- predict(model, test_set) 

# Calculate the RMSE
model_result <- test_set$y-model_prediction
model_rmse <- sqrt(mean(model_result^2))
model_rmse

# Repeat this exercise 100 times and report the mean and standard deviation of
# the RMSEs. (Hint: You can use the code shown in a previous course inside a
# call to replicate using a seed of 1). Calculate the RMSE

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



