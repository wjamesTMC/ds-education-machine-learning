# --------------------------------------------------------------------------------
#
# The confusion matrix, prevalence, sensitivity and specificity
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

# Prior section set ups
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

#
# END OF CODE FROM SECTION 01 - caret_package_training_and_test_sets-overall_accuracy
#

# The Confusion Matrix

# The prediction rule we developed in the previous section predicts Male if the
# student is taller than 64 inches. Given that the average female is about 65
# inches, this prediction rule seems wrong. What happened? If a student is the
# height of the average female, shouldn’t we predict Female? 

# Generally speaking, overall accuracy can be a deceptive measure. To see this,
# we will start by constructing what is referred to as the confusion matrix,
# which basically tabulates each combination of prediction and actual value. We
# can do this in R using the function table:
      
table(predicted = y_hat, actual = test_set$sex)
#          actual
# predicted Female Male
#    Female     50   27
#    Male       69  379

# If we study this table closely, it reveals a problem. If we compute the
# accuracy separately for each sex, we get:
      
test_set %>% 
mutate(y_hat = y_hat) %>%
group_by(sex) %>% 
summarize(accuracy = mean(y_hat == sex))
# A tibble: 2 x 2
#   sex    accuracy
#   <fct>     <dbl>
# 1 Female    0.420
# 2 Male      0.933

# There is an imbalance in the accuracy for males and females: too many females
# are predicted to be male. In fact, we are calling almost half of the females,
# males! How can our overall accuracy be so high then? This is because the
# prevalence of males in this dataset is high. These heights were collected from
# three data sciences courses, two of which had more males enrolled:
      
prev <- mean(y == "Male")
prev
# [1] 0.7733333

# So when computing overall accuracy, the high percentage of mistakes made for
# females is outweighed by the gains in correct calls for men. This can actually
# be a big problem in machine learning. If your training data is biased in some
# way, you are likely to develop algorithms that are biased as well. The fact
# that we used a test set does not matter because it is also derived from the
# original, biased dataset. This is one of the reasons we look at metrics other
# than overall accuracy when evaluating a machine learning algorithm.
 
# There are several metrics that we can use to evaluate an algorithm in a way
# that prevalence does not cloud our assessment, and these can all be derived
# from the confusion matrix. A general improvement to using overall accuracy is
# to study sensitivity and specificity separately.

# Sensitivity and Specificity
#
# To define sensitivity and specificity, we need a binary outcome. When the
# outcomes are categorical, we can define these terms for a specific category.
# In the digits example, we can ask for the specificity in the case of correctly
# predicting 2 as opposed to some other digit. Once we specify a category of
# interest, then we can talk about positive outcomes, Y=1, and negative
# outcomes, Y=0. In general, sensitivity is defined as the ability of an
# algorithm to predict a positive outcome when the actual outcome is positive:
# call ^Y=1 whenever Y=1. Because an algorithm that calls everything positive
# (Y^=1 no matter what) has perfect sensitivity, this metric on its own is not
# enough to judge an algorithm. For this reason, we also examine specificity,
# which is generally defined as the ability of an algorithm to not to predict a
# positive Y^=0 when the actual outcome is not a positive Y=0. We can summarize
# in the following way:

#     High sensitivity: Y=1⟹^Y=1- predict a positive outcome when actual is positive
#     High specificity: Y=0⟹^Y=0 - not predict pos outcome when actual not positive

# Another way to define specificity is by the proportion of positive calls that
# are actually positive:

#     High specificity: ^Y=1⟹Y=1.

# To provide a precise definition, we name the four entries of the confusion
# matrix

#                        Actually Positive        Actually Negative
#                        --------------------     --------------------
# Predicted positive     True positives (TP)      False positives (FP)
# Predicted negative     False negatives (FN)     True negatives (TN)

# SENSITIVITY is typically quantified by TP/(TP+FN), or the proportion of actual
# positives (the first COLUMN or TP+FN) that are called positives TP. This
# quantity is referred to as the true positive rate(TPR) or Recall.

# SPECIFICITY is typically quantified as TN/(TN+FP) or the proportion of
# negatives (the second COLUMN or FP+TN) that are called negatives TN. This
# quantity is also called the true negative rate (TNR). 

# There is ANOTHER WAY OF QUANTIFYING SPECIFICITY,which is TP/(TP+FP) or the
# proportion of outcomes called positives (the first ROW or TP+FP that are
# actually positives TP. This quantity is referred to as PRECISION and also as
# POSITIVE PREDICTIVE VALUE (PPV). Note that unlike TPR and TNR, precision
# depends on THE prevalence since higher prevalence implies you can get higher
# precision even when guessing.

# The multiple names can be confusing, so we include a table to help us remember
# the terms. The table includes a column that shows the definition if we think
# of the proportions as probabilities.

# A measure of      Name 1                   Name 2    Definition     Probability representation
# ------------      ------------------------ --------- ----------     --------------------------
# Sensitivity       True positive rate (TPR) Recall    TP/(TP+FN)     Pr(^Y=1∣Y=1)
#
# Specificity       True negative rate (TNR) 1 minus   TN/(TN+FP)     Pr(^Y=0∣Y=0)
#                                            false 
#                                            positive 
#                                            rate (1-FPR)
#                   
# Specificity       Positive Predictive      Precision TP/(TP+FP)     Pr(Y=1∣^Y=1)
#                   value (PPV)

# The caret function confusionMatrix() computes all these metrics for us once we
# define what a positive is. The function expects factors as inputS and the first
# level is considered the “positive” outcome or Y=1 Y=1. In our example, Female
# is the first level because it comes before Male alphabetically: so if we type 
# this line of code for our prediictions, we get this confusion matrix information
# given to us all in one shot:

confusionMatrix(data = y_hat, reference = test_set$sex)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Female Male
# Female     50   27
# Male       69  379
# 
# Accuracy : 0.8171          
# 95% CI : (0.7814, 0.8493)
# No Information Rate : 0.7733          
# P-Value [Acc > NIR] : 0.008354        
# 
# Kappa : 0.4041          
# Mcnemar's Test P-Value : 2.857e-05       
# 
# Sensitivity : 0.42017         
# Specificity : 0.93350         
# Pos Pred Value : 0.64935         
# Neg Pred Value : 0.84598         
# Prevalence : 0.22667         
# Detection Rate : 0.09524         
# Detection Prevalence : 0.14667         
# Balanced Accuracy : 0.67683         
# 
# 'Positive' Class : Female         

# We can see that the high overall accuracy is possible despite relatively low
# sensitivity. As we hinted at above, the reason this happens is because of the
# low prevalence (0.23): the proportion of females is low. Because prevalence is
# low, failing to predict actual females as females (low sensitivity) does not
# lower the accuracy as much as failing to predict actual males as males (low
# specificity). This is an example of why it is important to examine sensitivity
# and specificity and not just accuracy. Before applying this algorithm to
# general datasets, we need to ask ourselves if prevalence will be the same.



