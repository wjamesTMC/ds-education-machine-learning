# Setup
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)
library(caret)
library(e1071)
library(matrixStats)

# Q1 - Previously, we used logistic regression to predict sex based on height.
# Now we are going to use knn to do the same. Use the code described in these
# videos to select the F_1 measure and plot it against k. Compare to the F_1 of
# about 0.6 we obtained with regression. Set the seed to 1.

# What is the max value of F_1?
# At what value of k does the max occur?

library(tidyverse)
library(caret)
library(dslabs)

data(heights)

ks <- seq(1, 101, 3)
y <-heights$sex
x <- heights$height
set.seed(1)

# Create the train and test sets
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set  <- heights %>% slice(-test_index,)
test_set   <- heights %>% slice(test_index,)

accuracy <- map_dbl(ks, function(k) {
     # print(k)
     knn_fit <- knn3(y ~ ., data = mnist_27$train, k = k)
})

accuracy %>% print(n=1000)
accuracy %>% ggplot(aes(k,F_val)) + geom_line()



# Working through the code in the chapter...
# Let’s use our logistic regression as the standard we need to beat.

fit_glm <- glm(y ~ x_1 + x_2, data = mnist_27$train, family = "binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]
#> Accuracy 
#>     0.76

# Now, lets compare to kNN. We will use the knn3 function from the caret
# package. But how do we pick the right value for k?

ks <- seq(3, 251, 2)

# we use the map_df function to repeat the above for each one. For comparative
# purposes, we will compute the accuracy by using both the training set
# (incorrect) and the test set (correct):

library(purrr)
accuracy <- map_df(ks, function(k){
     knnfit <- knn3(y ~ ., data = mnist_27$train, k = k)
     
     y_hat <- predict(knnfit, mnist_27$train, type = "class")
     cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
     train_error <- cm_train$overall["Accuracy"]
     
     y_hat <- predict(knnfit, mnist_27$test, type = "class")
     cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
     test_error <- cm_test$overall["Accuracy"]
     
     list(train = train_error, test = test_error)
})

# The final accuracy for this value of k is:
     
max(accuracy$test)
# [1] 0.86

#
# Code work
#

question posted 3 months ago by ashimkp

I get max value of F_1 at k=1 F_Val=0.186. Can someone please explain what is wrong with this code:
     
library(tidyverse)
library(caret)
library(dslabs)

data(heights)

ks <- seq(1, 101, 3)
y <-heights$sex
x <- heights$height
set.seed(1)

test_index<- createDataPartition(y, times=1, p=0.5, list=FALSE)

train_set<- heights[test_index,]

test_set<- heights[-test_index,]

accuracy <- map_df(ks, function(k) {
     
     fit<- knn3(sex~height, data=train_set, k=k)
     
     y_hat<-predict(fit, test_set, type="class")
     
     F_val <- F_meas(data = y_hat, reference = factor(test_set$sex))
     
     list(k=k, F_val=F_val)
     
})

accuracy

accuracy %>% ggplot(aes(k,F_val)) + geom_line()

# ---------------------------------------------------------------------------
# Other code snippets
# ---------------------------------------------------------------------------
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

library(tidyverse)
library(caret)
library(dslabs)
data("heights")

set.seed(1)

ind <- createDataPartition(heights$sex,times = 1, p = 0.5, list = FALSE)
train <- heights %>% slice(ind)
test <- heights %>% slice(-ind)
ks = seq(1,101,3)

f1 <- map_dbl(ks, function(x) {
})

# 
# Another
#
library(tidyverse)
library(caret)
library(dslabs)

data(heights)

ks <- seq(1, 101, 3)
y <-heights$sex
x <- heights$height

set.seed(1)

f1 <- sapply(ks, function(k) {
     
     test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
     train_set <- heights[test_index, ]   
     test_set <- heights[-test_index, ]
     
     fit <- knn3(sex ~ height, data = train_set, k = k)   
     y_hat <- predict(fit, test_set, type = "class") %>% factor(levels = levels(test_set$sex))   
     
     F_meas(data = y_hat, reference = factor(train_set$sex))    
})

max(f1)

plotdat <- cbind(as_data_frame(f1),data_frame(ks)) %>% rowid_to_column("id")
plotdat %>% ggplot() + geom_line(aes(x = ks, y=value), color = "blue") 
