# --------------------------------------------------------------------------------
#
# Model Fitting and Recommendation Systems - comprehension check
#
# --------------------------------------------------------------------------------

# Setup
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(Lahman)
library(HistData)
library(caret)
library(e1071)
library(matrixStats)
library(Rborist)
library(randomForest)
library(gam)

#
# Q1
#

# Use the training set to build a model with several of the models available
# from the caret package. We will test out all of the following models in this
# exercise:
     
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

# We have not explained many of these, but apply them anyway using train with
# all the default parameters. You will likely need to install some packages.
# Keep in mind that you will probably get some warnings. Also, it will probably
# take a while to train all of the models - be patient!
     
# Run the following code to train the various models:
     
library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
     print(model)
     train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
# [1] "glm"            "lda"            "naive_bayes"    "svmLinear"      "gamboost"      
# [6] "gamLoess"       "qda"            "knn"            "kknn"           "loclda"        
# [11] "gam"            "rf"             "ranger"         "wsrf"           "Rborist"       
# [16] "avNNet"         "mlp"            "monmlp"         "adaboost"       "gbm"           
# [21] "svmRadial"      "svmRadialCost"  "svmRadialSigma"

# Did you train all of the models? ANSWER: YES

#
# Q2
#

# Now that you have all the trained models in a list, use sapply or map to
# create a matrix of predictions for the test set. You should end up with a
# matrix with length(mnist_27$test$y) rows and length(models).

# What are the dimensions of the matrix of predictions?

fits_predicts <- sapply(fits, function(fits){ 
     predict(fits,mnist_27$test) 
     })
# Answer: 200 rows and 23 columns - here is the instructor solution:

pred <- sapply(fits, function(object) 
     predict(object, newdata = mnist_27$test))
dim(pred)

#
# Q3
#

# Now compute accuracy for each model on the test set. Report the mean accuracy
# across all models.

y_test <- mnist_27$test$y

mvs <-  c(1:23)

confusion_mvs <- sapply(mvs, function(mvs){
     + confusionMatrix(factor(fits_predicts[,mvs]), y_test)$overall["Accuracy"]})

mean(confusion_mvs)
# [1] 0.8058696 - here is the instructor solution:

acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

#
# Q4
#

# Next, build an ensemble prediction by majority vote and compute the accuracy
# of the ensemble. What is the accuracy of the ensemble?

vote <- c(1:200)
for(i in 1:200) {
     num2s <- sum(fits_predicts[i,] == "2")
     num7s <- sum(fits_predicts[i,] == "7")
     vote[i]  <- ifelse(num2s > num7s, 2, 7)
}
# Accuracy 
# 0.84

confusionMatrix(factor(vote),  mnist_27$test$y)$overall["Accuracy"]  
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  2  7
# 2 92 18
# 7 14 76
# 
# Accuracy : 0.84            
# 95% CI : (0.7817, 0.8879)
# No Information Rate : 0.53            
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.6781          
# Mcnemar's Test P-Value : 0.5959          
# 
# Sensitivity : 0.8679          
# Specificity : 0.8085          
# Pos Pred Value : 0.8364          
# Neg Pred Value : 0.8444          
# Prevalence : 0.5300          
# Detection Rate : 0.4600          
# Detection Prevalence : 0.5500          
# Balanced Accuracy : 0.8382          
# 
# 'Positive' Class : 2

# Here is the instructor solution:
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#
# Q5
#

# In Q3, we computed the accuracy of each method on the training set and noticed
# that the individual accuracies varied.

# How many of the individual methods do better than the ensemble?
# My answer (87.5% correct) - 4: gam_oess, loclda, gam, and svmRadialCost
# The grader is incorrect, but accepts 1 and ANY higher performing model as the answer

# The comparison of the individual methods to the ensemble can be done using the
# following code:
     
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

#
# Q6
#

# It is tempting to remove the methods that do not perform well and re-do the
# ensemble. The problem with this approach is that we are using the test data to
# make a decision. However, we could use the accuracy estimates obtained from
# cross validation with the training data. Obtain these estimates and save them
# in an object. Report the mean accuracy of the new estimates.

# What is the mean accuracy of the new estimates?

y_test <- mnist_27$train$y

mvs <-  c(1:23)

confusion_mvs <- sapply(mvs, function(mvs){
     + confusionMatrix(factor(fits_predicts[,mvs]), y_test)$overall["Accuracy"]})

confusion_mvs
# Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
# 0.750    0.750    0.795    0.755    0.825    0.845    0.820    0.840    0.790    0.850    0.845 
# Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
# 0.780    0.785    0.795    0.765    0.830    0.750    0.830    0.790    0.815    0.840    0.850 
# Accuracy 
# 0.840 

mean(confusion_mvs)
# [1] 0.8058696 is the correct answer - instructor solution:

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

#
# Q7
#

# Now let's only consider the methods with an estimated accuracy of greater than
# or equal to 0.8 when constructing the ensemble. What is the accuracy of the
# ensemble now?

y_test <- mnist_27$test$y

# Create an index of the rows meeting the 0.8 criteria from the list in Q6 
nmv <- which(confusion_mvs >= 0.8)
# Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
# 5        6        7        8       10       11       16       18       20       21       22       23

# Apply the index to fits_predicts
new_df <- data.frame("gamboost" = fits_predicts[, nmv[1]], "gam_oess" = fits_predicts[, nmv[2]],
                     "qda" = fits_predicts[, nmv[3]], "knn" = fits_predicts[, nmv[4]],
                     "loclda" = fits_predicts[, nmv[5]], "gam" = fits_predicts[, nmv[6]],
                     "avNNet" = fits_predicts[, nmv[7]], "monmlp" = fits_predicts[, nmv[8]],
                     "gbm" = fits_predicts[, nmv[9]], "svmRadial" = fits_predicts[, nmv[10]],
                     "svmRadialCost" = fits_predicts[, nmv[11]], "svmRadialSigma" = fits_predicts[, nmv[12]])
                     
# Apply the same voting technique from # Q5

new_vote <- c(1:200)
for(i in 1:200) {
     num2s <- sum(new_df[i,] == "2")
     num7s <- sum(new_df[i,] == "7")
     new_vote[i]  <- ifelse(num2s > num7s, 2, 7)
}

# Apply the confusion matrix to determine the accuracy

confusionMatrix(factor(new_vote),  mnist_27$test$y)$overall["Accuracy"]
# Accuracy 
# 0.84 

# Instructor solution:
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)
