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

utoff <- seq(61, 70)
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
# are predicted to be male. We are calling almost half of the females, males!
# How can our overall accuracy be so high then? This is because the prevalence
# of males in this dataset is high. These heights were collected from three data
# sciences courses, two of which had more males enrolled:
      
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

#     * High sensitivity: Y=1⟹^Y=1
#     * High specificity: Y=0⟹^Y=0

# Another way to define specificity is by the proportion of positive calls that
# are actually positive:

#     * High specificity: ^Y=1⟹Y=1.

# To provide a precise definition, we name the four entries of the confusion
# matrix

#                        Actually Positive        Actually Negative
#                        --------------------     --------------------
# Predicted positive     True positives (TP)      False positives (FP)
# Predicted negative     False negatives (FN)     True negatives (TN)

# Sensitivity is typically quantified by TP/(TP+FN)TP/(TP+FN), or the proportion
# of actual positives (the first column or TP+FNTP+FN) that are called positives
# TP. This quantity is referred to as the true positive rate(TPR) or recall.

# Specificity is typically quantified as TN/(TN+FP)TN/(TN+FP) or the proportion
# of negatives (the second column or FP+TNFP+TN) that are called negatives TN.
# This quantity is also called the true negative rate (TNR). There is another
# way of quantifying specificity which is TP/(TP+FP)TP/(TP+FP) or the proportion
# of outcomes called positives (the first row or TP+FPTP+FP) that are actually
# positives TPTP. This quantity is referred to as precision and also as positive
# predictive value (PPV). Note that unlike TPR and TNR, precision depends on
# prevalence since higher prevalence implies you can get higher precision even
# when guessing. 

# The multiple names can be confusing, so we include a table to help us remember
# the terms. The table includes a column that shows the definition if we think
# of the proportions as probabilities.

# A measure of      Name 1                   Name 2    Definition     Probability representation
# ------------      ------------------------ --------- ----------     --------------------------
# Sensitivity       True positive rate (TPR) Recall    TP/(TP+FN)     Pr(^Y=1∣Y=1)
# Specificity       True negative rate (TNR) 1 minus   TN/(TN+FP)     Pr(^Y=0∣Y=0)
#                                            false 
#                                            positive 
#                                            rate (1-FPR)
#                   
# Specificity       Positive Predictive      Precision TP/(TP+FP)     Pr(Y=1∣^Y=1)
#                   value (PPV)

# The caret function confusionMatrix() computes all these metrics for us once we
# define what a positive is. The function expects factors as input and the first
# level is considered the “positive” outcome or Y=1 Y=1. In our example, Female
# is the first level because it comes before Male alphabetically:

confusionMatrix(data = y_hat, reference = test_set$sex)
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction Female Male
#     Female     81   67
#     Male       38  339
#                                         
#                Accuracy : 0.8           
#                  95% CI : (0.763, 0.833)
#     No Information Rate : 0.773         
#     P-Value [Acc > NIR] : 0.07819       
#                                         
#                   Kappa : 0.475         
#  Mcnemar's Test P-Value : 0.00629       
#                                         
#             Sensitivity : 0.681         
#             Specificity : 0.835         
#          Pos Pred Value : 0.547         
#          Neg Pred Value : 0.899         
#              Prevalence : 0.227         
#          Detection Rate : 0.154         
#    Detection Prevalence : 0.282         
#       Balanced Accuracy : 0.758         
#                                         
#        'Positive' Class : Female        

# We can see that the high overall accuracy is possible despite relatively low
# sensitivity. As we hinted at above, the reason this happens is because of the
# low prevalence (0.23): the proportion of females is low. Because prevalence is
# low, failing to predict actual females as females (low sensitivity) does not
# lower the accuracy as much as failing to predict actual males as males (low
# specificity). This is an example of why it is important to examine sensitivity
# and specificity and not just accuracy. Before applying this algorithm to
# general datasets, we need to ask ourselves if prevalence will be the same.

# Balanced accuracy and F1 score

# Although, in general, we recommend studying both specificity and sensitivity,
# very often it is useful to have a one number summary, for example for
# optimization purposes. One metric that is preferred over overall accuracy is
# the average of specificity and sensitivity, referred to as balanced accuracy.
# Because specificity and sensitivity are rates, it is more appropriate to
# compute the harmonic average of specificity and sensitivity. In fact, the
# F1F1-score, a widely used one number summary, is the harmonic average of
# precision and recall:

#    1 / (1/2) * ((1 / recall) + (1 / precision))
     
# Because it is easier to write, you often see this harmonic average rewritten as:
     
#    2((precision - recall) / (precision + recall))

# ...when defining F1F1. 

# Note that, depending on the context, some type of errors are more costly than
# others. For example, in the case of plane safety, it is much more important to
# maximize sensitivity over specificity: failing to predict a plane will
# malfunction before it crashes is a much more costly error than grounding a
# plane when, in fact, the plane is in perfect condition. In a capital murder
# criminal case, the opposite is true since a false positive can lead to killing
# an innocent person. The F1F1-score can be adapted to weigh specificity and
# sensitivity differently. To do this, we define ββ to represent how much more
# important sensitivity is compared to specificity and consider a weighted
# harmonic average:
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
# instead of overall accuracy:
     
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
     y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
          factor(levels = levels(test_set$sex))
     F_meas(data = y_hat, reference = factor(train_set$sex))
})

# As before, we can plot these F1 measures versus the cutoffs:

#     <<< Insert code when available >>>
     
# We see that it is maximized at F1 value of:
     
max(F_1)
#> [1] 0.614

# when we use cutoff:

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
#>           Reference
#> Prediction Female Male
#>     Female     81   67
#>     Male       38  339
#>                                         
#>                Accuracy : 0.8           
#>                  95% CI : (0.763, 0.833)
#>     No Information Rate : 0.773         
#>     P-Value [Acc > NIR] : 0.07819       
#>                                         
#>                   Kappa : 0.475         
#>  Mcnemar's Test P-Value : 0.00629       
#>                                         
#>             Sensitivity : 0.681         
#>             Specificity : 0.835         
#>          Pos Pred Value : 0.547         
#>          Neg Pred Value : 0.899         
#>              Prevalence : 0.227         
#>          Detection Rate : 0.154         
#>    Detection Prevalence : 0.282         
#>       Balanced Accuracy : 0.758         
#>                                         
#>        'Positive' Class : Female        

# We now see that we do much better than guessing, that both sensitivity and
# specificity are relatively high, and that we have built our first machine
# learning algorithm. It takes height as a predictor and predicts female, if you
# are 66 inches or shorter.

