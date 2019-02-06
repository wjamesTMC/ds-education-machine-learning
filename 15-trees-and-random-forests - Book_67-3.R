# --------------------------------------------------------------------------------
#
# Trees and Random Forests - Classification (Decision) Trees (Book 67.3)
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
library(rpart)

# When the outcome is continuous, we call these types of algorithms regression
# trees. We'll use a continuous case, the 2008 poll data introduced earlier, to
# describe the basic idea of how we build these algorithms. We'll try to
# estimate the conditional expectation-- we'll call it f of x, the expected
# value of y given x-- with y, the poll margin, and x, the day. Here's a graph
# of the data.

data("polls_2008")
qplot(day, margin, data = polls_2008)

# The general idea here is to build a decision tree. And at the end of each
# node, we'll have a different prediction Y hat. Here is how we do it. 

# First we partition the space into j non-overlapping regions, R1, R2, all the
# way up to Rj. For every observation that follows within a region, let's say,
# region Rj, we predict the Y hat with the average of all the training
# observations in that region. But how do we decide on the partitions R1, R2 and
# so on? And how do we decide how many? 

# Regression trees create partitions recursively. But what does this mean? OK,
# suppose we already have a partition. We then have to decide what predictor j
# to use to make the next partition and where to make it within that predictor.

# So suppose we already have a partition so that every observation i is in
# exactly one of these partitions. For each of these partitions, we will divide
# further using the following algorithm. 

# First we need to find a predictor j and a value s that define two new
# partitions. Let's call them R1 and R2. These two partitions will split our
# observations into the following two sets. 

#    R1(j,s)={X∣Xj<s} and R2(j,s)={X∣Xj≥s}

# Then, in each one of these two sets, we will define an average and use these
# as our predictions. The averages will be the averages of the observations in
# each of the two partitions. So we could do this for many Js and Ss. So how do
# we pick them? We pick the combination that minimizes the residual sum of
# squares defined by this formula.

#    ∑i:xi∈R1(j,s)(yi−^yR1)2+∑i:xi∈R2(j,s)(yi−^yR2)2

# This is then applied recursively. We keep finding new regions to split into
# two. Once we're done partitioning the predictor space into regions, in each
# region, a prediction is made by using the observations in that region. You
# basically calculate an average. Let's take a look at what this algorithm does
# on the 2008 presidential election poll data. We will use the rpart function in
# the rpart package. We simply type this.

fit <- rpart(margin ~ ., data = polls_2008)

# Here there's only one predictor. So we don't have to decide which predictor j
# to split by. We simply have to decide what value s we use to split. We can
# visually see where the splits were made by using this piece of code. Here's a
# tree.

plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# The first split is made on day 39.5. Then one of those two regions is split at
# day 86.5. The two resulting new partitions are split on day 49.5 and 117.5
# respectively and so on. We end up with eight partitions. The final estimate f
# hat of x looks like this.

polls_2008 %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

# Now, why did the algorithm stop partitioning at eight? There are some details
# of the algorithm we did not explain. Let's explain them now.

# Note that every time we split and define two new partitions, our training set
# residual sum of squares decreases. This is because with more partitions, our
# model has more flexibility to adapt to the training data. In fact, if you
# split until every point is its own partition, then the residual sum of squares
# goes all the way down to zero since the average of one value is that same
# value. 

# To avoid this overtraining, the algorithm sets a minimum for how much the
# residual sum of squares must improve for another partition to be added. This
# parameter is referred to as the Complexity Parameter, or CP. The residual sum
# of squares must improve by a factor of CP the new partition to be added.

# Another aspect of the algorithm we didn't describe is that it sets a minimum
# number of observations to be partitioned. In the rpart package, that rpart
# function has an argument called minsplit that lets you define this. The
# default is 20. 

# The algorithm also sets a minimum on the number of observations in each
# partition. In the rpart function, this argument is called minbucket. 

# So if the optimal split results in a partition with less observation than this
# minimum, it is not considered. The default for this parameter is minsplit
# divided by 3 rounded to the closest integer. OK, so let's see what happens if
# we set CP to 0 and minsplit to 2. What will happen then? Well, our prediction
# is our original data because the tree will keep splitting and splitting until
# the RSS is minimized to zero. Here's the data with the resulting fit.

fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

# Now, note that in this methodology, we can also prune trees by snipping off
# partitions that do not meet a CP criterion. So we can grow a tree very, very
# big and then prune off branches to make a smaller tree. Here is a code for how
# to do this.

pruned_fit <- prune(fit, cp = 0.01)
polls_2008 %>% 
     mutate(y_hat = predict(pruned_fit)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

# With the code that we just wrote, here is the resulting estimate. OK, but now
# is a default value of CP the best one? How do we pick CP? Well, we can use
# cross-validation, just like with any other tuning parameter. We can use the
# train function in the caret package, for example. We can write this code, then
# plot the result, and pick the best CP.

library(caret)
train_rpart <- train(margin ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)
ggplot(train_rpart)

# To see the resulting tree that minimizes the mean squared error, we can access
# it through the component finalmodel. If we plot it, we can see the tree. Here
# it is.

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# And because we only have one predictor, we can actually plot f hat x. Here it
# is.

polls_2008 %>% 
     mutate(y_hat = predict(train_rpart)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

# You can see that the fit looks reasonable.



