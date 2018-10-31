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

# We start by briefly introducing the caret package, which has several useful
# functions for building and assessing machine learning methods.
#
# Later in this chapter we provide more details on this package.
#
# In this section, we focus on describing ways in which machine learning
# algorithms are evaluated. For our first introduction to machine learning
# concepts, we will start with a boring and simple example: how to predict sex
# using height. As we explain machine learning step by step, this example will
# let us set down the first building block. Soon enough we will be attacking
# more interesting challenges.

# For this first example, we use the height data in dslabs:
     
data(heights)

# We start by defining the outcome and predictors. In this case, we have only
# one predictor:
     
y <- heights$sex
x <- heights$height

# This is clearly a categorical outcome since Y can be Male or Female and we
# only have one predictor: height. We know that we will not be able to predict Y
# very accurately based on X because male and female average heights are not
# that different relative to within group variability. But can we do better than
# guessing? To answer this question, we need a quantitative definition of
# better.

# Training and test sets 
#
# Ultimately, a machine learning algorithm is evaluated on how it performs in
# the real world with completely new datasets. However, when developing an
# algorithm, we usually have a dataset for which we know the outcomes, as we do
# with the heights: we know the sex of every student in our dataset. Therefore,
# to mimic the ultimate evaluation process, we typically split the data into two
# and act as if we don’t know the outcome for one of these. We stop pretending
# we don’t know the outcome to evaluate the algorithm, but only after we are
# done constructing it. We refer to the group for which we know the outcome and
# use it to develop the algorithm as the training set, and the group for which
# we pretend we don’t know the outcome as the test set.
#
# A standard way of generating the training and test sets is by randomly
# splitting the data. The caret package includes the function
# createDataPartition that helps us generates indexes for randomly splitting the
# data into training and test sets:

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

# The argument times is used to define how many random samples of indexes to
# return, the argument p is used to define what proportion of the data is
# represented by the index, and the argument list is used to decide if we want
# the indexes returned as a list or not. We can use the result of the function
# call to define the training and test sets like this:
      
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# We will now develop an algorithm using only the training set. Once we are done
# developing the algorithm, we will freeze it and evaluate it using the test
# set. The simplest way to evaluate the algorithm when the outcomes are
# categorical is by simply reporting the proportion of cases that were correctly
# predicted in the test set. This metric is usually referred to as overall
# accuracy.

# Overall accuracy
#
# To demonstrate the use of overall accuracy, we will build two competing
# algorithms and compare them.
#
# Let’s start by developing the simplest possible machine algorithm: guessing
# the outcome.
 
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)

# Note that we are completely ignoring the predictor and simply guessing the
# sex.
 
# In machine learning applications, it is useful to use factors to represent the
# categorical outcomes because R functions developed for machine learning, such
# as those in the caret package, require or recommend that categorical outcomes
# be coded as factors. So convert y_hat to factors using the factor function:
      
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
     factor(levels = levels(test_set$sex))
y_hat
# [Output is a list of n entries (depnding on p) and the two factors, Male and
# Female]

# The overall accuracy is simply defined as the overall proportion that is
# predicted correctly:
      
mean(y_hat == test_set$sex)
# [1] 0.5238095

# Not surprisingly, our accuracy is about 50%. We are guessing!
#      
# Can we do better? Exploratory data analysis suggests we can because, on
# average, males are slightly taller than females:

heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
#   A tibble: 2 x 3
#   sex    `mean(height)` `sd(height)`
#   <fct>           <dbl>        <dbl>
# 1 Female           64.9         3.76
# 2 Male             69.3         3.61

# But how do we make use of this insight? Let’s try another simple approach:
# predict Male if height is within two standard deviations from the average
# male:
     
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))

# The accuracy goes up from 0.50 to about 0.80:
     
mean(y == y_hat)
# [1] 0.7933333

# But can we do even better? In the example above, we used a cutoff of 62, but
# we can examine the accuracy obtained for other cutoffs and then pick the value
# that provides the best results. But remember, it is important that we optimize
# the cutoff using only the training set: the test set is only for evaluation.
# Although for this simplistic example it is not much of a problem, later we
# will learn that evaluating an algorithm on the training set can lead to
# overfitting, which often results in dangerously over-optimistic assessments.

# Here we examine the accuracy of 10 different cutoffs and pick the one yielding
# the best result:
     
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
     y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
     mean(y_hat == train_set$sex)
})

# We can make a plot showing the accuracy on the training set for males and females

guessing %>% qplot(cutoff, accuracy, data =., xlab = "Cutoff", ylab = "Accuracy") + geom_line()
     
# We see that the maximum value is:
     
max(accuracy)
#> [1] 0.8361905

# which is much higher than 0.5. The cutoff resulting in this accuracy is:
     
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
#> [1] 64

# Now we can now test this cutoff on our test set to make sure our accuracy is
# not overly optimistic:
     
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
     factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)

mean(y_hat == test_set$sex)
#> [1] 0.8171429

# NOTE: The function factor is used to encode a vector as a factor (the terms
# ‘category’ and ‘enumerated type’ are also used for factors). If argument
# ordered is TRUE, the factor levels are assumed to be ordered. For
# compatibility with S there is also a function ordered.

# We see that it is a bit lower than the accuracy observed for the training set,
# but it is still better than guessing. And by testing on a dataset that we did
# not train on, we know it is not due to cherry-picking a good result.

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

