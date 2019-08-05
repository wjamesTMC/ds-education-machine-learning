# --------------------------------------------------------------------------------
#
# Balanced Accuracy and the F1 Score
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

# 
# Prior course section example set ups
#
data(heights)
y <- heights$sex
x <- heights$height

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
     factor(levels = levels(test_set$sex))
y_hat
mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
     y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
     mean(y_hat == train_set$sex)
})

max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
     factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)

mean(y_hat == test_set$sex)

table(predicted = y_hat, actual = test_set$sex)

test_set %>% 
     mutate(y_hat = y_hat) %>%
     group_by(sex) %>% 
     summarize(accuracy = mean(y_hat == sex))

prev <- mean(y == "Male")
confusionMatrix(data = y_hat, reference = test_set$sex)

#
# END OF CODE FROM Earlier Sections
#

# Balanced accuracy and F1 score

# Although, in general, we recommend studying both specificity and sensitivity,
# very often it is useful to have a one number summary, for example for
# optimization purposes. One metric that is preferred over overall accuracy is
# the average of specificity and sensitivity, referred to as balanced accuracy.
# Because specificity and sensitivity are rates, it is more appropriate to
# compute the harmonic average of specificity and sensitivity, like this: 

#    1 / (1/2) * ((1 / recall) + (1 / precision))

# In fact, the F1-score, a widely used one number summary, is the harmonic
# average of precision and recall. Because it is easier to write, you often see
# this harmonic average rewritten as:

#    2((precision - recall) / (precision + recall))

# ...when defining F1F1. Note: In mathematics, the harmonic mean (sometimes
# called the subcontrary mean) is one of several kinds of average, and in
# particular one of the Pythagorean means. Typically, it is appropriate for
# situations when the average of rates is desired.

#The harmonic mean can be expressed as the reciprocal of the arithmetic mean of
#the reciprocals of the given set of observations. As a simple example, the
#harmonic mean of 1, 4, and 4 is <see wikipedia example> 
#
#     https://en.wikipedia.org/wiki/Harmonic_mean
#
# Note that, depending on the context, some type of errors are more costly than
# others. For example, in the case of plane safety, it is much more important to
# maximize sensitivity over specificity: failing to predict a plane will
# malfunction before it crashes is a much more costly error than grounding a
# plane when, in fact, the plane is in perfect condition. In a capital murder
# criminal case, the opposite is true since a false positive can lead to killing
# an innocent person. The F1-score can be adapted to weigh specificity and
# sensitivity differently. To do this, we define β to represent how much more
# important sensitivity is compared to specificity and consider a weighted
# harmonic average using this formula:
#
#                                    1
#                  ____________________________________
#
#                      β2        1          1         1
#                    ------ * ------  +  ------ * ---------
#                    1 + β2   recall     1 + β2   precision
#                                     
# The F_meas function in the caret package computes this summary with beta
# defaulting to 1.

# Let’s rebuild our prediction algorithm, but this time maximizing the F-score
# instead of overall accuracy. We can do this by editing the code and using
# this instead:

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
     y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
          factor(levels = levels(test_set$sex))
     F_meas(data = y_hat, reference = factor(train_set$sex))
})

# As before, we can plot these F1 measures versus the cutoffs:
plot(cutoff, F_1)

# We see that it is maximized at F1 value of:

max(F_1)
#> [1] 0.6328125

# when we use cutoff of 66 inches:

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff
#> [1] 66

# A cutoff of 66 makes much more sense than 64. Furthermore, it balances the
# specificity and sensitivity of our confusion matrix:

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
     factor(levels = levels(test_set$sex))

confusionMatrix(data = y_hat, reference = test_set$sex)
#> Confusion Matrix and Statistics
#> 
# Reference
# Prediction Female Male
# Female     82   77
# Male       37  329
# 
# Accuracy : 0.7829          
# 95% CI : (0.7451, 0.8174)
# No Information Rate : 0.7733          
# P-Value [Acc > NIR] : 0.3221872       
# 
# Kappa : 0.4464          
# 
# Mcnemar's Test P-Value : 0.0002595       
# 
# Sensitivity : 0.6891          
# Specificity : 0.8103          
# Pos Pred Value : 0.5157          
# Neg Pred Value : 0.8989          
# Prevalence : 0.2267          
# Detection Rate : 0.1562          
# Detection Prevalence : 0.3029          
# Balanced Accuracy : 0.7497          
# 
# 'Positive' Class : Female        

# We now see that we do much better than guessing, that both sensitivity and
# specificity are relatively high, and that we have built our first machine
# learning algorithm. It takes height as a predictor and predicts female, if you
# are 66 inches or shorter.
