#---------------------------------------------------------------------------
#
# Logistic Regression
#
#---------------------------------------------------------------------------

library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(HistData)
library(caret)
library(e1071)

# Picking up code from prior program to set plots

library(dslabs)
data("heights")

y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
     filter(round(height) == 66) %>%
     summarize(mean(sex == "Female"))

heights %>% 
     mutate(x = round(height)) %>%
     group_by(x) %>%
     filter(n() >= 10) %>%
     summarize(prop = mean(sex == "Female")) %>%
     ggplot(aes(x, prop)) +
     geom_point()

# Note that the function beta 0 plus beta 1x can take any value, including
# negatives and values larger than 1. In fact, the estimate that we obtained for
# our conditional probability using linear regression goes from negative 0.4 to
# 1.12. But we're estimating a probability that's between 0 and 1. So can we
# avoid this?

# Logistic regression is an extension of linear regression that assures us the
# estimate of the conditional probability is, in fact, between 0 and 1.

# This approach makes use of the logistic transformation introduced in the data
# visualization course, which you can see here.

#    g(p) = log(p / (1 - p))

# The logistic transformation converts probabilities to log odds. As discussed
# in the data visualization course, the odds tells us how much more likely
# something will happen compared to not happen. So if p is equal to 0.5, this
# means that the odds are 1 to 1. Thus, the odds are 1.

# If p is 0.75, the odds are 3 to 1.

# A nice characteristic of this transformation is that it transforms
# probabilities to be symmetric around 0. Here's a plot of the logistic
# transformation versus the probability.

# Note: the graph is a reverse s curve startin near (0, -5.0), curving up then
# bending to the right and flattening, crossing at (.5, 0) and then bending up
# again to end at (1.0,5.0)

# Now, how do we fit this model? We can no longer use least squares. Instead, we
# compute something called the maximum likelihood estimate. You can learn more
# about this concept in a statistical theory textbook. In R, we can fit the
# logistic regression model with the function GLM, which stands for Generalized
# Linear Models.

# This function is more general than logistic regression, so we need to specify
# the model we want. We do this through the family parameter. Here's the code
# that fits a logistic regression model to our data.

glm_fit <- train_set %>%
     mutate(y = as.numeric(sex == "Female")) %>%
     glm(y ~ height, data=., family = "binomial")

# Just like with linear regression, we can obtain predictions using the predict
# function. However, once we read the help file predict.glm, we realize that
# when using predict with a GLM object, we have to specify that we want type
# equals response if we want the conditional probabilities. This is because the
# default is to return the logistic transform values.

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

# Now that we've done it, we can see how well our model fit. Note that this
# model fits the data slightly better than the line.

# Trend line version
heights %>% 
     mutate(x = round(height)) %>%
     group_by(x) %>%
     filter(n() >= 10) %>%
     summarize(prop = mean(sex == "Female")) %>%
     ggplot(aes(x, prop)) +
     geom_point() + geom_smooth(method = "lm")

# glm line version
heights %>% 
     mutate(x = round(height)) %>%
     group_by(x) %>%
     filter(n() >= 10) %>%
     summarize(prop = mean(sex == "Female")) %>%
     ggplot(aes(x, prop)) +
     geom_point() + geom_smooth(method = "glm",
                                method.args = list(family ="binomial"), 
                                se = FALSE)

# Because we have an estimate of the conditional probability, we can obtain
# predictions using code like this.

y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor

# And once we look at the confusion matrix, we see that our accuracy has
# increased slightly to about 80%.

confusionMatrix(y_hat_logit, test_set$sex)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Female Male
# Female     31   19
# Male       87  389
# 
# Accuracy : 0.7985         
# 95% CI : (0.7616, 0.832)
# No Information Rate : 0.7757         
# P-Value [Acc > NIR] : 0.1138         
# 
# Kappa : 0.2718         
# Mcnemar's Test P-Value : 7.635e-11      
# 
# Sensitivity : 0.26271        
# Specificity : 0.95343        
# Pos Pred Value : 0.62000        
# Neg Pred Value : 0.81723        
# Prevalence : 0.22433        
# Detection Rate : 0.05894        
# Detection Prevalence : 0.09506        
# Balanced Accuracy : 0.60807        
# 
# 'Positive' Class : Female  

# And once we look at the confusion matrix, we see that our accuracy has
# increased slightly to about 80%.

# Note that the resulting predictions are similar. This is because the two
# estimates of our conditional probability are larger than a half in roughly the
# same regions.

# You can see that in this plot.

# Both linear and logistic regression provide an estimate for the conditional
# expectation, which, in the case of binary data, is equivalent to a conditional
# probability.

# So we can use it in machine learning applications. However, once we move on to
# more complex examples, we will see that linear regression and logistic
# regression are limited and not flexible enough to be useful. The techniques we
# will learn are essentially approaches to estimating conditional probabilities
# or conditional expectations in ways that are more flexible.


library(dslabs)
data("heights")

y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# If we define the outcome Y as 1 for females and 0 for males, and X as the
# height, in this case we are interested in the conditional probability:

#    Pr(Y=1∣X=x)

# As an example, let’s provide a prediction for a student that is 66 inches
# tall. What is the conditional probability of being female if you are 66 inches
# tall? In our dataset we can estimate this by rounding to the nearest inch and
# computing:

train_set %>% 
     filter(round(height) == 66) %>%
     summarize(mean(sex == "Female"))
# mean(sex == "Female")
# 1             0.2424242

# Let's see what it looks like for several values of X. We'll repeat the same
# exercise but for 60 inches, 61 inches, et cetera. Note that we're removing X
# values for which we have very few data points. And if we do this, we get the
# following results. 

heights %>% 
     mutate(x = round(height)) %>%
     group_by(x) %>%
     filter(n() >= 10) %>%
     summarize(prop = mean(sex == "Female")) %>%
     ggplot(aes(x, prop)) +
     geom_point()

# Since the results from the plot above look close to linear, and it is the only
# approach we currently know, we will try regression. We assume that:

#         p(x) = Pr(Y=1|X=x) = β0+β1x

#
# We assume that the conditional probability of Y being 1 given X is a
# line--intercept plus slope times height.

# If we convert the factors to 0s and 1s, we can we can estimate β0 and β1
# with least squares using this piece of code:

lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
     lm(y ~ height, data = .)

# Once we have estimates ^β0 and ^β1, we can obtain an actual prediction. Our
# estimate of the conditional probability p(x) is:

#    ^p(x) = ^β0+^β1x

# To form a prediction we define a decision rule: predict female if
# the conditional probability is greater than 50%:

#    p^(x) > 0.5. 

# Now we can compare our predictions to the outcomes using:

p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()

# and use the confustion matrix to see how we did:

confusionMatrix(y_hat, test_set$sex)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Female Male
# Female     20   15
# Male       98  393
# 
# Accuracy : 0.7852          
# 95% CI : (0.7476, 0.8195)
# No Information Rate : 0.7757          
# P-Value [Acc > NIR] : 0.3218          
# 
# Kappa : 0.177           
# Mcnemar's Test P-Value : 1.22e-14        
# 
# Sensitivity : 0.16949         
# Specificity : 0.96324         
# Pos Pred Value : 0.57143         
# Neg Pred Value : 0.80041         
# Prevalence : 0.22433         
# Detection Rate : 0.03802         
# Detection Prevalence : 0.06654         
# Balanced Accuracy : 0.56636         
# 
# 'Positive' Class : Female        

# We see that we got an accuracy of 78.5%. We see this method does substantially
# better than guessing.
