# --------------------------------------------------------------------------------
#
# Linear regression for prediction
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

# Linear regression can be considered a machine learning algorithm. As we will
# see, it is too rigid to be useful in general, but for some challenges it works
# rather well. It also serves as a baseline approach: if you can’t beat it with
# a more complex approach, you probably want to stick to linear regression. To
# quickly make the connection between regression and machine learning, we will
# reformulate Galton’s study with heights: a continuous outcome.

galton_heights <- GaltonFamilies %>%
     filter(childNum == 1 & gender == "male") %>%
     select(father, childHeight) %>%
     rename(son = childHeight)

# head(GaltonFamilies)
#   family father mother midparentHeight children childNum gender childHeight
# 1    001   78.5   67.0           75.43        4        1   male        73.2
# 2    001   78.5   67.0           75.43        4        2 female        69.2
# 3    001   78.5   67.0           75.43        4        3 female        69.0
# 4    001   78.5   67.0           75.43        4        4 female        69.0
# 5    002   75.5   66.5           73.66        4        1   male        73.5
# 6    002   75.5   66.5           73.66        4        2   male        72.5

# Suppose you are tasked with building a machine learning algorithm that
# predicts the son’s height Y using the father’s height   X. Let’s generate
# testing and training sets:
     
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

# In this case, if we were just ignoring the father’s height and guessing the
# son’s height we would guess the average height of sons.

avg <- mean(train_set$son)
avg
#> [1] 70.4625

# Our squared loss is:
     
mean((avg - test_set$son)^2)
#> [1] 6.242945

# Can we do better? In the regression chapter we learned that if the pair (X ,
# Y) follow a bivariate normal distribution, the conditional expectation (what
# we want to estimate) is equivalent to the regression line:

#         f(x)=E(Y∣X=x)=β0+β1x

# We also introduced least squares as a method for estimating the slope β0 and
# intercept β1

fit <- lm(son ~ father, data = train_set)
fit$coef
# (Intercept)      father 
#  34.8887042   0.5131032 

# This gives us an estimate of the conditional expectation:
     
#    f(x)=38+0.47x

# We can see that this does indeed provide an improvement over our guessing
# approach.

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)
# [1] 5.104337

# The predict function

# The predict function is very useful for machine learning applications. This
# function takes a fitted object from function such as lm or glm (we learn about
# glm soon) and a data frame with the new predictors for which to predict. So in
# our current example we would use predict like this:
     
y_hat <- predict(fit, test_set)

# Using predict we can get the same results as we did previously, because using
# predict is equivalent to using the regression line:
     
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)
# [1] 5.104337

# Predict does not always return objects of the same types; it depends on what
# type of object is sent to it. To learn about the specifics, you need to look
# at the help file specific for the type of fitted object that is being used.
# The predict is a actually a special type of function in R (called a generic
# function) that calls other functions depending on what kind of object it
# receives. So if predict receives an object coming out of the lm function, it
# will call predict.lm. If it receives an object coming out of glm, it calls
# predict.glm. These two functions are similar but different. You can learn more
# about the differences by reading the help files:

?predict.lm
# and
?predict.glm

# There are many other versions of predict with many machine learning algorithms
# having one. There'll be other examples like this, where the function is
# called, say, knn. Then we have to look at the help file for predict.knn.





