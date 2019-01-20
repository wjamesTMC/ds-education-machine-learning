#---------------------------------------------------------------------------
#
# Regression for a categorical outcome
#
#---------------------------------------------------------------------------

library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(HistData)
library(caret)
library(e1071)

# The regression approach can also be applied to categorical data. To illustrate
# this, we will apply it to our previous predicting sex example using heights:

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
