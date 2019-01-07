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

# When comparing two or more methods, for example, guessing versus using a
# height cutoff, we looked at accuracy and F1. The second method - where we used
# height - clearly outperformed. However, while we considered several cutoffs
# for the second method, for the first we only considered one approach: guessing
# with equal probability. Note that guessing Male with higher probability would
# give us higher accuracy due to the bias in the sample:
     
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

# The ROC curve for guessing always looks like a straight line - the identiy
# line. A perfect algorithm would shoot straight to 1 and stay up there: perfect
# sensitivity for all values of specificity. So how does our second approach
# compare? We can construct an ROC curve for the height based approach using this code:
     
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Height cutoff",
          FPR = 1-specificity(y_hat, test_set$sex),
          TPR = sensitivity(y_hat, test_set$sex))
})

# By plotting both curves together we are able to compare sensitivity for
# different values of specificity:
     
bind_rows(guessing, height_cutoff) %>%
ggplot(aes(FPR, TPR, color = method)) +
geom_line() +
geom_point() +
xlab("1 - Specificity") +
ylab("Sensitivity")

# We can see that we obtain higher sensitivity with the height-based approach
# for all values of specificity, which implies it is in fact a better method.

# When making ROC curves it is often nice to add the cutoff used to the points.
# It would look like this:
     
map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
          factor(levels = c("Female", "Male"))
     list(method = "Height cutoff",
          cutoff = x, 
          FPR = 1-specificity(y_hat, test_set$sex),
          TPR = sensitivity(y_hat, test_set$sex))
}) %>%
ggplot(aes(FPR, TPR, label = cutoff)) +
geom_line() +
geom_point() +
geom_text(nudge_y = 0.01)

# ROC curves have one weakness and it is that neither of the measures plotted
# depend on prevalence. In cases in which prevalence matters, we may instead
# make a precision-recall plot. The idea is similar, but we instead plot
# precision against recall:
     
guessing <- map_df(probs, function(p){
     y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% factor(levels = c("Female", "Male"))
     list(method = "Guess",
          recall = sensitivity(y_hat, test_set$sex),
          precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% factor(levels = c("Female", "Male"))
     list(method = "Height cutoff",
          recall = sensitivity(y_hat, test_set$sex),
          precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
     ggplot(aes(recall, precision, color = method)) +
     geom_line() +
     geom_point()
# Warning: Removed 1 rows containing missing values (geom_path).
# Warning: Removed 1 rows containing missing values (geom_point).

# From this plot we immediately see that the precision of guessing is not high.
# This is because the prevalence is low.

# If we change positives to mean Male instead of Female, the ROC curve remains
# the same, but the precision recall plot changes, and it looks like this:

guessing <- map_df(probs, function(p){
     y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% factor(levels = c("Male", "Female"))
     list(method = "Guess",
          recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
          precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
     y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% factor(levels = c("Male", "Female"))
     list(method = "Height cutoff",
          recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
          precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
     ggplot(aes(recall, precision, color = method)) +
     geom_line() +
     geom_point()
#> Warning: Removed 1 rows containing missing values (geom_path).
#> Warning: Removed 1 rows containing missing values (geom_point).
     
