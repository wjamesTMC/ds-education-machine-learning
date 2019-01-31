# --------------------------------------------------------------------------------
#
# Bootstrap
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
library(matrixStats)

data(income)

# In this video we describe the bootstrap. We're going to use a very simple
# example to do it. hist(log10(income)) Suppose the income distribution of a
# population is as follows.

hist(log10(income))

# HISTOGRAM - normally distributed between 3 and 6, mean at 4.5 or so

# The population median is, in this case, about 45,000. median(income) Suppose
# we don't have access to the entire population, but want to estimate the
# median, let's call it M. We take a sample of 250 and estimate the population
# median, M, with the sample medium big M, like this.

set.seed(1)
N <- 250
X <- sample(income, N)
M <- median(X)
M
#> [1] 48503

# Now, can we construct a confidence interval? What's the distribution of the
# sample median? From an Monte Carlo simulation, we see that the distribution of
# the sample median is approximately normal with the following expected value
# and standard errors. You can see it here.
 
B <- 10^5
Ms <- replicate(B, {
     X <- sample(income, N)
     M <- median(X)
})
par(mfrow=c(1,2))
hist(Ms)
qqnorm(Ms)
qqline(Ms)
mean(Ms)
#> [1] 45517
sd(Ms)
#> [1] 3670

# The problem here is that, as we have described before, in practice, we do not
# have access to the distribution. In the past, we've used the central limit
# theorem, but the central limit theorem we studied applies to averages and here
# we're interested in the median. The bootstrap permits us to approximate a
# Monte Carlo simulation without access to the entire distribution.

# The general idea is relatively simple. We act as if the sample is the entire
# population and sample with replacement data sets of the same size. Then we
# compute the summary statistic, in this case, the median, on what is called the
# bootstrap sample.

# There is theory telling us that the distribution of the statistic obtained
# with bootstrap samples approximate the distribution of our actual statistic.
# This is how we construct bootstrap samples in an approximate distribution.
# This simple code.

B <- 10^5
M_stars <- replicate(B, {
     X_star <- sample(X, N, replace = TRUE)
     M_star <- median(X_star)
})

# Now we can check how close it is to the actual distribution.
# We can see it's relatively close.

qqplot(Ms, M_stars)
abline(0,1)  

# We see it's not perfect, but it provides a decent approximation. In
# particular, look at the quantities we need to form a 95% confidence interval.
# They are quite close.

quantile(Ms, c(0.05, 0.95))
#>    5%   95% 
#> 39727 51778
quantile(M_stars, c(0.05, 0.95))
#>    5%   95% 
#> 40471 53080

# This is much better than what we get if we mindlessly use the central limit
# theorem, which would give us this confidence interval, which
# is entirely wrong.

median(X) + 1.96 * sd(X)/sqrt(N) * c(-1,1)
#> [1] 36801 60205

# If we know the distribution is normal, we can use a bootstrap to estimate the
# mean, the standard error, and then form a confidence interval that way.

mean(Ms) + 1.96*sd(Ms)*c(-1,1)
#> [1] 38325 52710
mean(M_stars) + 1.96*sd(M_stars)*c(-1,1)
#> [1] 40170 55350
