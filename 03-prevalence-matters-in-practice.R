# --------------------------------------------------------------------------------
#
# Prevalence
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

# Prevalence matters in practice
#
# A machine learning algorithm with very high sensitivity and specificity may
# not be useful in practice when prevalence is close to either 0 or 1. To see
# this, consider the case of a doctor that specializes in a rare disease and is
# interested in developing an algorithm for predicting who has the disease. The
# doctor shares data with you and you then develop an algorithm with very high
# sensitivity. You explain that this means that if a patient has the disease,
# the algorithm is very likely to predict correctly. You also tell the doctor
# that you are also concerned because, based on the dataset you analyzed, 1/2
# the patients have the disease:

#         Pr(Y^=1)

# The doctor is neither concerned nor impressed and explains that what is
# important is the precision of the test:

#         Pr(Y=1|^Y=1)

# Using Bayes theorem, we can connect the two measures:

#         Pr(Y∣^Y=1)=Pr(^Y=1∣Y=1) * Pr(Y=1)/ Pr(^Y=1)

# The doctor knows that the prevalence of the disease is 5 in 1000 which implies
# that Pr(Y=1)/Pr(^Y=1)=1/100Pr(Y=1)/Pr(Y^=1)=1/100 and therefore the precision
# of your algorithm is less than 0.01. The doctor does not have much use for
# your algorithm.

# The doctor knows that the prevalence of the disease is 5 in 1000 which implies
# that Pr(Y=1)/Pr(^Y=1)=1/100 and therefore the precision of your algorithm is
# less than 0.01. The doctor does not have much use for your algorithm.
