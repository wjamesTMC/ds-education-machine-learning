# --------------------------------------------------------------------------------
#
# TCaret Package - comprehension check
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

# In the exercise in Q6 from Comprehension Check: Trees and Random Forests, we
# saw that changing nodesize to 50 and setting maxnodes to 25 yielded smoother
# results. Let's use the train function to help us pick what the values of
# nodesize and maxnodes should be.

library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)

# From the caret description of methods, we see that we can't tune the maxnodes parameter or the nodesize argument with randomForests. So we will use the __Rborist__ package and tune the minNode argument. Use the train function to try values minNode <- seq(25, 100, 25). Set the seed to 1.

library(Rborist)
set.seed(1)
minNode <- seq(25, 100, 25)
fit <- train(y ~ ., method = "Rborist", 
             tuneGrid = data.frame(predFixed = 1, minNode),
             data = dat)
fit

#
# Q2
#

# Part of the code to make a scatterplot along with the prediction from the best
# fitted model is provided below. 

library(caret)
dat %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(x, y)) +
     #BLANK
     
# Which code correctly can be used to replace
# #BLANK in the code above?

# Option 1- don't think so because y_hat and x are reversed
library(caret)
dat %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(x, y)) +
     geom_step(aes(y_hat, x), col = 2)

# Option 2 - This is the correct answer - uses geom_step()
library(caret)
dat %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(x, y)) +
     geom_step(aes(x, y_hat), col = 2)
     
# Option 3 - doesn't use y_hat and it should, right? And is a weird plot
library(caret)
dat %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(x, y)) +
     geom_step(aes(x, y), col = 2)

# Option 4  -  generates an error / Error in FUN(X[[i]], ...) : object 'x_hat' not found
library(caret)
dat %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(x, y)) +
     geom_step(aes(x_hat, y_hat), col = 2)

# Option 5 - geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
library(caret)
dat %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(x, y)) +
     geom_smooth(aes(x, y_hat), col = 2)

# Option 6  - geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
#             Again, has y_hat and x reversed?
library(caret)
dat %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(x, y)) +
     geom_smooth(aes(y_hat, x), col = 2)


#
# Q3
#

# Use the rpart function to fit a classification tree to the
# tissue_gene_expression dataset. Use the train function to estimate the
# accuracy. Try out cp values of seq(0, 0.1, 0.01). Plot the accuracies to
# report the results of the best model. Set the seed to 1991.

# Which value of cp gives the highest accuracy?

data("tissue_gene_expression")

set.seed(1991)
class_tree <- train(tissue_gene_expression$x, tissue_gene_expression$y,
                    method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))

#
# Q4
#

# Study the confusion matrix for the best fitting classification tree from the
# exercise in Q3.

confusionMatrix(class_tree)
# Bootstrapped (25 reps) Confusion Matrix 
# 
# (entries are percentual average cell counts across resamples)
# 
# Reference
# Prediction    cerebellum colon endometrium hippocampus kidney liver placenta
# cerebellum        19.3   0.0         0.3         1.1    0.3   0.0      0.2
# colon              0.3  16.5         0.1         0.0    0.1   0.0      0.1
# endometrium        0.1   0.2         5.6         0.1    1.5   0.1      1.3
# hippocampus        0.2   0.0         0.0        15.3    0.1   0.0      0.0
# kidney             0.3   0.3         1.7         0.1   18.7   0.5      0.3
# liver              0.0   0.0         0.3         0.0    0.3  12.6      0.2
# placenta           0.2   0.0         0.4         0.0    0.5   0.1      0.9
# 
# Accuracy (average) : 0.8878

# What do you observe happening for the placenta samples?
# Placenta samples are all accurately classified.
# Placenta samples are being classified as two similar tissues.
# Placenta samples are being classified somewhat evenly across tissues. CORRECT
# Placenta samples not being classified into any of the classes.

#
# q5
#

# Note that there are only 6 placentas in the dataset. By default, rpart
# requires 20 observations before splitting a node. That means that it is
# difficult to have a node in which placentas are the majority. Rerun the
# analysis you did in the exercise in Q3, but this time, allow rpart to split
# any node by using the argument control = rpart.control(minsplit = 0). Look at
# the confusion matrix again to determine whether the accuracy increases. Again,
# set the seed to 1991.

data("tissue_gene_expression")

set.seed(1991)
class_tree <- train(tissue_gene_expression$x, tissue_gene_expression$y,
                    method = "rpart", control = rpart.control(minsplit = 0),
                    tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))

confusionMatrix(class_tree)
 
# What is the accuracy now?
# Bootstrapped (25 reps) Confusion Matrix 
# 
# (entries are percentual average cell counts across resamples)
# 
# Reference
# Prediction    cerebellum colon endometrium hippocampus kidney liver placenta
# cerebellum        19.5   0.0         0.2         0.9    0.4   0.0      0.1
# colon              0.3  16.5         0.1         0.0    0.1   0.0      0.1
# endometrium        0.1   0.2         6.4         0.1    0.9   0.1      0.5
# hippocampus        0.2   0.0         0.0        15.6    0.1   0.0      0.0
# kidney             0.3   0.3         0.9         0.1   19.1   0.5      0.3
# liver              0.0   0.0         0.3         0.0    0.3  12.6      0.2
# placenta           0.1   0.1         0.5         0.0    0.6   0.1      1.8
# 
# Accuracy (average) : 0.9141

#
# Q6
#

# Plot the tree from the best fitting model of the analysis you ran in Q5.

plot(class_tree$finalModel, margin = 0.1)
text(class_tree$finalModel, cex = 0.75)

# Which gene is at the first split? - answer is GPA33


#
# Q7
#

# We can see that with just seven genes, we are able to predict the tissue type.
# Now let's see if we can predict the tissue type with even fewer genes using a
# Random Forest. Use the train function and the rf method to train a Random
# Forest. Try out values of mtry ranging from seq(50, 200, 25) (you can also
# explore other values on your own). What mtry value maximizes accuracy? To
# permit small nodesize to grow as we did with the classification trees, use the
# argument: nodesize = 1.

# Note: This exercise will take some time to run. If you want to test out your
# code first, try using smaller values with ntree. Set the seed to 1991 again.
 
# What value of mtry maximizes accuracy?

