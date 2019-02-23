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

# Did you train all of the models?

