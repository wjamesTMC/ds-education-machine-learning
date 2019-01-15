#---------------------------------------------------------------------------
#
# Comprehension check on condditional probabilities
#
#---------------------------------------------------------------------------

# In a previous module, we covered Bayes' theorem and the Bayesian paradigm.
# Conditional probabilities are a fundamental part of this previous covered
# rule. We first review a simple example to go over conditional probabilities.
# Note: links to explanation of Bayes' theorem:
#
#    https://betterexplained.com/articles/an-intuitive-and-short-explanation-of-bayes-theorem/
#    http://vassarstats.net/bayes.html
#

#
# Question 1
#

# Assume a patient comes into the doctor’s office to test whether they have a
# particular disease. The test is positive 85% of the time when tested on a
# patient with the disease (high sensitivity). The test is negative 90% of the
# time when tested on a healthy patient (high specificity). The disease is
# prevalent in about 2% of the community:

# Using Bayes' theorem, calculate the probability that you have the disease if
# the test is positive.

# Answer: the chances of a true positive - that you have the disease AND the
# test is positive is the multiplication of these two odds:

#    2% * 85% = .017
    
# Now what we want is to determine the chances that we get a true positive out
# of all the other possibilities, so we divide by all possibilities for a
# positive test:
     
# All possibilities = odds of a true positive + odds of a false positive:
#
#    2% * 85% + 98% * 10%  [Where 98% = 100% - 2% and 10% = 100% - 90%
#    or .017 + .098 = 0.115
               
# So then Prob we test positive AND have the disease = prob of a positive test /
# all possibilities to get positive test = prob of pos test / (prob of pos test
# + prob of false pos)
#
#    or .017 / 0.115 = 0.1478 = .15

# Second interpretation:
#
#    P(B) = [P(test + | disease) x P(disease(] + [P(test+ | healthy) x P(healthy)]
#
#    or
#
#    P(B) = (85% * 2%) + (10% * 98%) = ,115
#
#    Plugging these into the formula:

#
# Questions 2-5
#
# The following 4 questions (Q2-Q5) all relate to implementing this calculation
# using R. We have a hypothetical population of 1 million individuals with the
# following conditional probabilities as described below:
     
#     The test is positive 85% of the time when tested on a patient with the disease (high sensitivity):
#         P(test+ | disease) = 85%
#   
#     The test is negative 90% of the time when tested on a healthy patient (high specificity): 
#         P(test- | healthy) = 90
#
#     The disease is prevalent in about 2% of the community: 
#         P(disease) = 2%
     
#
# Code for Questions 2-5
#
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
mean(test[disease==0])  # [1] 0.09964064
mean(test[disease==1])  # [1] 0.8461191

#
# Question 2: What is the probability that a test is positive?
#

# First, determine the odds of a positive test if you have the disease
p_postest_dis <- mean(test[disease==1]) * .02
# [1] 0.01692238

# Next, determine the odds of a positive test if you do not have the disease
p_postest_hea <- mean(test[disease==0]) * (1 - .02)
# [1] 0.09764783

# Next, add the two probabilities together to get the probability that the test is positive
p_testpos <- p_postest_dis + p_postest_hea
# [1] 0.1145702

#
# Question 3: What is the probability that an individual has the disease if the
# test is negative?
#
p_hasdis_testneg <- (1 - mean(test[disease==1])) * .02
# [1] 0.003077618

#
# Question 4: What is the probability that you have the disease if the test is
# positive?
# 

# First, calculate the odds of a positive test if you have the disease
p_hasdis_testpos <- mean(test[disease==1]) * .02
# [1] 0.01692238

# Next, calculate odds of a positive test if you do not have the disease
p_healthy_testpos <- mean(test[disease==0]) * .98
# [1] 0.09764783

# Next, add the two together to get the all positive probability
pos_test_prob <- p_hasdis_testpos + p_healthy_testpos
# [1] 0.1145702

# Finally, divide odds of positive test by all possibilities
odds_pos_test_if_dis <- p_hasdis_testpos / pos_test_prob
# [1] 0.1477032

#
# Question 5: If the test is positive, what is the relative risk of having the
# disease? First calculate the probability of having the disease given a
# positive test, then normalize it against the disease prevalence.
#
# RR = P(disease | test+) / P(disease)
a1 <- odds_pos_test_if_dis / .02
# [1] 7.385158


