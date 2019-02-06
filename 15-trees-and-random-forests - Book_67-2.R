# --------------------------------------------------------------------------------
#
# Trees and Random Forests - Classification and Regression Trees 
# (CART) (Book 67.2)
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

# In this video, we will use a new dataset that includes the breakdown of the
# composition of olive into eight fatty acids. You can get the data like this.

data("olive")
head(olive)
#>           region         area palmitic palmitoleic stearic oleic linoleic
#> 1 Southern Italy North-Apulia    10.75        0.75    2.26  78.2     6.72
#> 2 Southern Italy North-Apulia    10.88        0.73    2.24  77.1     7.81
#> 3 Southern Italy North-Apulia     9.11        0.54    2.46  81.1     5.49
#> 4 Southern Italy North-Apulia     9.66        0.57    2.40  79.5     6.19
#> 5 Southern Italy North-Apulia    10.51        0.67    2.59  77.7     6.72
#> 6 Southern Italy North-Apulia     9.11        0.49    2.68  79.2     6.78
#>   linolenic arachidic eicosenoic
#> 1      0.36      0.60       0.29
#> 2      0.31      0.61       0.29
#> 3      0.31      0.63       0.29
#> 4      0.50      0.78       0.35
#> 5      0.50      0.80       0.46
#> 6      0.51      0.70       0.44
names(olive)
#>  [1] "region"      "area"        "palmitic"    "palmitoleic" "stearic"    
#>  [6] "oleic"       "linoleic"    "linolenic"   "arachidic"   "eicosenoic"

# For illustrative purpose, we'll try to predict the region using the fatty acid
# composition values as predictors.

table(olive$region)
#> 
#> Northern Italy       Sardinia Southern Italy 
#>            151             98            323

# It's either Northern Italy, Sardinia, or Southern Italy.
# We'll remove the area column because we don't use it as a 

olive <- select(olive, -area)

# Let's very quickly see how we do using k-nearest neighbors. We use the caret
# package to fit the model, and we get an accuracy of about 0.97, pretty good.

library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

# However, a bit of data exploration reveals that we should be able to do even
# better. For example, if we look at the distribution of each predictor
# stratified by region, we see that one of the fatty acids is only present in
# Southern Italy, and then another one separates Northern Italy from Sardinia.

olive %>% gather(fatty_acid, percentage, -region) %>%
     ggplot(aes(region, percentage, fill = region)) +
     geom_boxplot() +
     facet_wrap(~fatty_acid, scales = "free")

# This implies that we should be able to build an algorithm that perfectly
# predicts. We can see this clearly by plotting the values of these two fatty
# acids.

p <- olive %>% 
     ggplot(aes(eicosenoic, linoleic, color = region)) + 
     geom_point()
p

# We can, by eye, construct a prediction rule the partitions the predictor space
# like this.

p + geom_vline(xintercept = 0.065, lty = 2) + 
     geom_segment(x = -0.2, y = 10.535, xend = 0.065, yend = 10.535, color = "black", lty = 2)

# Specifically, we define the following decision rule. If the first predictor is
# larger than 0.065, B, predict Southern Italy. If not, then look at the second
# predictor. And if that's larger than 10.53, predict Sardinia, and Northern
# Italy otherwise.

# We can draw this as a decision tree like this. Decision trees like this one
# are often used in practice. For example, to decide if a person at risk of
# having a heart attack, a doctor will use a decision tree such as this one. The
# general idea is to define an algorithm that uses data to create trees such as
# the ones we've just shown. Regression and decision trees operate by predicting
# an outcome variable y by partitioning the predictor space.
