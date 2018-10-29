# --------------------------------------------------------------------------------
#
# Conditional probabilities and expectations
#
# --------------------------------------------------------------------------------

# Setup 
# Setup Libraries
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)
library(caret)
library(e1071)


# Linear regression for prediction

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

# We also introduced least squares as a method for estimating the slope β0β0 and
# intercept β1β1

fit <- lm(son ~ father, data = train_set)
fit$coef
# (Intercept)      father 
#  34.8887042   0.5131032 

# This gives us an estimate of the conditional expectation:
     
#         f(x)=38+0.47x

# We can see that this does indeed provide an improvement over our guessing approach.

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)
#> [1] 4.721657

# The predict function

# The predict function is very useful for machine learning applications. This
# function takes a fitted object from function such as lm or glm (we learn about
# glm soon) and a data frame with the new predictors for which to predict. So in
# our current example we would use predict like this:
     
y_hat <- predict(fit, test_set)

# Using predict we can get the same results as we did previously:
     
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)
# [1] 4.721657

# Predict does not always return objects of the same types; it depends on what
# type of object is sent to it. To learn about the specifics, you need to look
# at the help file specific for the type of fit object that is being used. The
# predict is a actually a special type of function in R (called a generic
# function) that calls other functions depending on what kind of object it
# receives. So if predict receives an object coming out of the lm function, it
# will call predict.lm. If it receives an object coming out of glm, it calls
# predict.glm. These two functions are similar but different. You can learn more
# about the differences by reading the help files:
     
?predict.lm
# and
?predict.glm

# There are many other versions of predict with many machine learning algorithms
# having one.

# Regression for a categorical outcome

# The regression approach can also be applied to categorical data. To illustrate
# this, we will apply it to our previous predicting sex example:

# If we define the outcome Y as 1 for females and 0 for males, and X as the
# height, in this case we are interested in the conditional probability:

#         Pr(Y=1∣X=x)Pr(Y=1∣X=x)

# As an example, let’s provide a prediction for a student that is 66 inches
# tall. What is the conditional probability of being female if you are 66 inches
# tall? In our dataset we can estimate this by rounding to the nearest inch and
# computing:

# IN THE CODE BELOW, train_set HAS NO COLUMN NAMED HEIGHT OR SEX <<<<<<<<<<<
train_set %>% 
filter(round(height)==66) %>%
summarize(mean(sex=="Female"))
#>   mean(sex == "Female")
#> 1                 0.242

# We will define Y=1 for females and Y=0 for males. To construct a prediction
# algorithm, we want to estimate the proportion of the population that is female
# for any given height X=x, which we write as the conditional probability
# described above: Pr(Y=1|X=x). Let’s see what this looks like for several
# values of xx (we will remove values of xx with few data points):

heights %>% 
mutate(x = round(height)) %>%
group_by(x) %>%
filter(n() >= 10) %>%
summarize(prop = mean(sex == "Female")) %>%
ggplot(aes(x, prop)) +
geom_point()

# Since the results from the plot above look close to linear, and it is the only
# approach we currently know, we will try regression. We assume that:

#         p(x)=Pr(Y=1|X=x)=β0+β1x

# Note: because p0(x)=1−p1(x), we will only estimate p1(x)) and drop the index.

# If we convert the factors to 0s and 1s, we can we can estimate β0β0 and β1β1
# with least squares.

# NOTE: THERE IS NO COLUMN "SEX" IN TRAIN_SET
# THE REST OF THE CODE PIECES WILL NOT WORK
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
     lm(y ~ height, data = .)

# Once we have estimates ^β0 and ^β1, we can obtain an actual prediction. Our
# estimate of the conditional probability p(x) is:

#         ^p(x)=^β0+^β1x

# To form a prediction we define a decision rule: predict female if
# ^p(x)>0.5p^(x)>0.5. We can compare our predictions to the outcomes using:

p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()

confusionMatrix(y_hat, test_set$sex)
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction Female Male
#     Female     20   15
#     Male       98  393
#                                        
#                Accuracy : 0.785        
#                  95% CI : (0.748, 0.82)
#     No Information Rate : 0.776        
#     P-Value [Acc > NIR] : 0.322        
#                                        
#                   Kappa : 0.177        
#  Mcnemar's Test P-Value : 1.22e-14     
#                                        
#             Sensitivity : 0.1695       
#             Specificity : 0.9632       
#          Pos Pred Value : 0.5714       
#          Neg Pred Value : 0.8004       
#              Prevalence : 0.2243       
#          Detection Rate : 0.0380       
#    Detection Prevalence : 0.0665       
#       Balanced Accuracy : 0.5664       
#                                        
#        'Positive' Class : Female       

# We see this method does substantially better than guessing.
