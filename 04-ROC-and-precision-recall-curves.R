# --------------------------------------------------------------------------------
#
# ROC and Precision-recall Curves [ROC = "Receiving Operator Characteristic"]
#
# --------------------------------------------------------------------------------

# Setup Libraries
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(Lahman)
library(HistData)
library(caret)
library(e1071)

# Setup Test Data
data(heights)
y <- heights$sex
x <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat
# When comparing the two methods: guessing versus using a height cutoff, we
# looked at accuracy and F1. The second method clearly outperformed. However,
# while we considered several cutoffs for the second method, for the first we
# only considered one approach: guessing with equal probability. Note that
# guessing Male with higher probability would give us higher accuracy due to the
# bias in the sample:
     
p <- 0.9
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% 
     factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)
#> [1] 0.718


# But, as described above, this would come at the cost of lower sensitivity. The
# curves we describe in this section will help us see this.

# Remember that for each of these parameters, we can get a different sensitivity
# and specificity. For this reason, a very common approach to evaluating methods
# is to compare them graphically by plotting both.

# A widely used plot that does this is the receiver operating characteristic
# (ROC) curve. If you are wondering where this name comes from, according to
# Wikipedia:
     
# The ROC curve was first used during World War II for the analysis of radar
# signals before it was employed in signal detection theory.[35] Following the
# attack on Pearl Harbor in 1941, the United States army began new research to
# increase the prediction of correctly detected Japanese aircraft from their
# radar signals. For this purpose they measured the ability of radar receiver
# operators to make these important distinctions, which was called the Receiver
# Operating Characteristics.

# The ROC curve plots sensitivity (TPR) versus 1 - specificity or the false
# positive rate (FPR). Here is an ROC curve for guessing sex but using different
# probabilities of guessing male:

probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
     y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Guessing",
          FPR = 1 - specificity(y_hat, test_set$sex),
          TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")




