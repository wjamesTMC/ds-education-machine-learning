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
