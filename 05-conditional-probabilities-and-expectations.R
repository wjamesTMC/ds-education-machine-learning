# --------------------------------------------------------------------------------
#
# Conditional probabilities and expectations
#
# --------------------------------------------------------------------------------

# No setup required (no code)

# In machine learning applications, we rarely can predict outcomes perfectly.
# For example, spam detectors often miss emails that are clearly spam, Siri
# often misunderstands the words we are saying, and your bank at times thinks
# your card was stolen when it was not. The most common reason for not being
# able to build prefect algorithms is that it is impossible. To see this, note
# that most datasets will include groups of observations with the same exact
# observed values for all predictors, but with different outcomes. Because our
# prediction rules are functions, equal inputs (the predictors) implies equal
# outputs (the predictions). 

# Therefore, for a challenge in which the same predictors are associated with
# different outcomes across different individual observations, it is impossible
# to predict correctly for all these cases. We saw a simple example of this in
# the previous section: for any given height xx, you will have both males and
# females that are xx inches tall. However, none of this means that we can’t
# build useful algorithms that are much better than guessing, and in some cases
# better than expert opinions. To achieve this in an optimal way, we make use of
# probabilistic representations of the problem. Observations with the same
# observed values for the predictors may not all be the same, but we can assume
# that they all have the same probability of this class or that class. We will
# write this idea out mathematically for the case of categorical data.

# Conditional probabilities

# We use the notation (X1=x1,…,Xp=xp)to represent the fact that we have observed
# values x1,…,xn for covariates X1,…,Xpp. This does not imply that the outcome
# YY will take a specific value. Instead, it implies a specific probability. In
# particular, we denote the conditional probabilities for each class k:

#         Pr(Y=k∣X1=x1,…,Xp=xp),for k=1,…,K

# To avoid writing out all the predictors, we will use the following bold
# letters: X≡(X1,…,Xp) and x≡(x1,…,xp). We will also use the following notation
# for the conditional probability of being class k.

#         pk(x)=Pr(Y=k∣X=x),fork=1,…,K

# Note: We will be using the p(x)p(x) notation to represent conditional
# probabilities as functions of the predictors. Do not confuse it with the pp
# that represents the number of predictors.

# Knowing these probabilities can guide the construction of an algorithm that
# makes the best prediction: for any given xx, we will predict the class kk with
# the largest probability among p1(x),p2(x),…pK(x). In mathematical notation, we
# write it like this:

#         ^Y=maxpk(x) / k

# In machine learning we refer to this as Bayes’ Rule. But keep in mind that
# this is a theoretical rule since in practice we don’t know pk(x),k=1,…,K. In
# fact, estimating these conditional probabilities can be thought of as the main
# challenge of machine learning. The better our estimate ^pk(x), the better our
# predictor:

#         ^Y=maxk^pk(x) / k

# So what we will predict depends on two things 1) how closemaxkpk(x)maxkpk(x)is
# to 1 and 2) how close our estimate ^pk(x) is to pk(x). We can’t do anything
# about the first restriction as it is determined by the nature of the problem,
# so our energy goes into finding ways to best estimate conditional
# probabilities. The first restriction does imply that we have limits as to how
# well even the best possible algorithm can perform. You should get used to the
# idea that while in some challenges we will be able to achieve almost perfect
# accuracy, digit readers for example, in others our success is restricted by
# the randomness of the process, movie recommendations for example.

# Before we continue it is important to remember that defining our prediction by
# maximizing the probability is not always optimal in practice and depends on
# the context. As discussed above, sensitivity and specificity may differ in
# importance. But, even in these cases, having a good estimate of the
# pk(x),k=1,…,K will suffice for us to build optimal prediction models since we
# can control the balance between specificity and sensitivity however we wish.
# For example, we can simply change the cutoffs used to predict one outcome or
# the other. In the plane example, we may ground the plane anytime the
# probability of malfunction is higher than 1 in a million as opposed to the
# default 1/2 used when error types are equally undesired.

# Conditional expectations

# For binary data, you can think of the probability Pr(Y=1∣X=x) as the
# proportion of 1s in the stratum of the population for which X=x. Many of the
# algorithms we will learn can be applied to both categorical and continuous
# data due to the connection between conditional probabilities and conditional
# expectations.

# Because the expectation is the average of values y1,…,yn in the
# population, in the case in which the yys are 0 or 1, the expectation is
# equivalent to the probability of randomly picking a one since the average is
# simply the proportion of ones:

#         E(Y∣X=x)=Pr(Y=1∣X=x)

# We therefore often only use the expectation to denote both the conditional
# probability and conditional expectation.

# Just like with categorical outcomes, in most applications the same observed
# predictors do not guarantee the same continuous outcomes. Instead, we assume
# that the outcome follows the same conditional distribution and we will now
# explain why we use the conditional expectation to define our predictors.

# The Loss Function

# Before we start describing approaches to optimizing the way we build
# algorithms for continuous data, we first need to define what we mean when we
# say one approach is better than another. Specifically, we need to quantify
# what we mean by “better”.

# With binary outcomes we have already described how sensitivity, specificity,
# accuracy and F1F1 can be used as quantification. However, these metrics are
# not useful for continuous outcomes. The general approach to defining “best” in
# machine learning is to define a loss function. The most commonly used one is
# the squared loss function: if ^y is our predictor and y is the observed
# outcome, the squared loss function is simply:

#         (^y−y)2

# Because we often have a test set with many observations, say N, we use the
# mean squared error (MSE):

#         MSE = 1/N*RSS =1/N N∑i=1(^yi−yi)2

# In practice, we often report the root mean squared error because it is in the
# same units as the outcomes:

#      RMSE = √MSE = √ 1/N NN∑ i =1(^yi−yi)2

# But doing the math is often easier with the MSE and is therefore more commonly
# used in text books, since these usually describe theoretical properties of
# algorithms.

# If the outcomes are binary, both RMSE and MSE are equivalent to accuracy since
# (^y−y)2 is 1 if the prediction was correct and 0 otherwise. In general, our
# goal is to build an algorithm that minimizes the loss so it is as close to 0
# as possible.

# Because our data is usually a random sample, we can think of the MSE as a
# random variable and the observed MSE can be thought of as an estimate of the
# expected MSE, which in mathematical notation we write like this:

#      E{1NN∑i=1(^Yi−Yi)2}

# This is a theoretical concept because in practice we only have one dataset to
# work with. But, in theory, we think of having a very, very large number, call
# it BB, of random samples, apply our algorithm to each, obtain an MSE for each
# random sample, and think of the expected MSE as:

#      1BB∑b=11NN∑i=1(^ybi−ybi)2

# with ybiyib denoting the iith observation in the bb-th random sample and
# ^ybiy^ib the resulting prediction obtained from applying the exact same
# algorithm to the bb-th random sample. But again, in practice, we only observe
# one random sample so the expected MSE is only theoretical. However, in a later
# section we describe an approach to estimating the MSE that tries to mimic this
# theoretical quantity.

# Before we continue, note that the there are loss functions other that the
# squared loss. For example, the Mean Absolute Error uses absolute values
# instead of squaring the errors:

#         E{1/N N∑i=1|^Yi−Yi|}

# However, in this book we focus on minimizing square loss since it is the most
# widely used.

# Conditional expectation minimizes squared loss function

# So why do we care about the conditional expectation in machine learning? This
# is because the expected value has an attractive mathematical property: it
# minimized the MSE. Specifically, of all possible predictors ^Y,

#         ^Y=E(Y∣X=x) minimizes E{(^Y−Y)2∣X=x}}

# Due to this property, a succinct description of the main task of Machine
# Learning is that we use data to estimate:

#         f(x)≡E(Y∣X=x)

# ...for any set of features x=(x1,…,xp). Of course this is easier said than
# done, since this function can take any shape and pp can be very large.
# Consider a case in which we only have one predictor xx. The expectation
# E{Y∣X=x}E{Y∣X=x} can be any function of x: a line, a parabola, a sine wave, a
# step function, anything. It gets even more complicated when we consider
# instances with large pp, in which case f(x)f(x) is a function of a
# multidimensional vector xx. For example, in our digit reader example
# p=784p=784! The main way in which competing Machine Learning algorithms differ
# is in the approach to estimating this expectation.
