# --------------------------------------------------------------------------------
#
# Local Weighted Regressioin (loess)
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

# A limitation of the bin smoother approach we just described is that we need
# small windows for the approximately constant assumption to hold.

# As a result, we end up with a small number of data points to average. And as a
# result of this, we obtain imprecise estimates of our trend. Here, we describe
# how local weighted regression or loess permits us to consider larger windows.

# To do this, we will use a mathematical result referred to as Taylor's theorem,
# which tells us that if you look close enough at any smooth function f, it
# looks like a line.

# To see why this makes sense, consider the curved edges gardeners make. They
# make these using spades which are straight lines so they can generate curves
# that are locally straight lines.

# So instead of assuming the function is approximately constant in a window, we
# assume the function is locally linear. With the linear assumption, we can
# consider larger window sizes than with a constant.

# So instead of the one-week window, we will instead consider a larger window in
# which the trend is approximately linear. We start with a three-week window and
# later consider enabling other options. So the model for points that are in a
# three-week window looks like this. 

#    E[Yi|Xi = xi] = β0+β1(xi−x0)   if   |xi−x0| ≤ 10.5

# We assume that Y given X in that window is a line. Now, for every point x0
# loess defines a window and then fits a line within that window. So here's an
# example of those fits for x0 equals negative 125 and x0 equals negative 55.

# The fitted values at x0 become our estimate of the trend. In this animation,
# we demonstrate the idea. The final result is a smoother fit than the bin
# smoother since we used larger sample sizes to estimate our local parameters.
# You can get the final estimate using this code, and it looks like this.

total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(day, smooth), color="red")

# Now, note that different spans give us different estimates. We can see how
# different window sizes lead to different estimates with this animation.

# Here are the final estimates. We can see that with 0.1 the line is quite
# wiggly. With 0.15, it's slightly less. Now, with 0.25, we get a rather smooth
# estimate. And with 0.66, it almost looks like a straight line. There are three
# other differences between loess and the typical bin smoother which we describe
# here.

# The first is that rather than keeping the bin size the same, loess keeps the
# number of points used in the local fit the same. This number is controlled via
# the span argument which expects a proportion. So for example, if N is a number
# of data points, and the span is 0.5, then for any given X, loess will use 0.5
# times N closest points to X for the fit. Another difference is that when
# fitting a line locally, loess uses a weighted approach.

# Basically, instead of least squares, we minimize a weighted version. So we
# would minimize this equation. 

#    N∑i=1w0(xi)[Yi−{β0+β1(xi−x0)}]2

# However, instead of the Gaussian kernel, loess
# uses a function called the Tukey tri-weight which you can see here. 

#    W(u)=(1−|u|3)3 if |u|≤1 and W(u)=0 if |u|>1

# And to define weights, we use this formula.

#    w0(xi) = W((xi−x0) / h)

# The kernel for the tri-weight looks like this.
 
# The third difference is that loess has the option of fitting the local model
# robustly. An iterative algorithm is implemented in which, after fitting a
# model in one iteration, outliers are detected and down-weighted for the next
# iteration. To use this option, use the argument:

#    family = "symmetric"

# One more important point about loess. Taylor's theorem also tells us that if
# you look at a function close enough, it looks like a parabola and that you
# don't have to look as close as you do for the linear approximation. This means
# we can make our windows even larger and fit parabolas instead of lines, so the
# local model would look like this. 

#    E[Yi|Xi=xi]=β0+β1(xi−x0)+β2(xi−x0)2 if |xi−x0|≤h

# This is actually the default procedure for the function loess. You may have
# noticed that when we show the code for loess, we set a parameter degree equals
# 1. This tells loess to fit polynomials of degree 1, a fancy name for lines. If
# you read the help page for loess, you'll see that the argument degree defaults
# to 2. So by default, loess fits parabolas not lines. Here is a comparison of
# fitting lines, the red dashed, and fitting parabolas, the orange solid.

total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(day, smooth_1), color="red", lty = 2) +
     geom_line(aes(day, smooth_2), color="orange", lty = 1) 

# Notice that degree equals 2 gives us a more wiggly result. I personally prefer
# degree equals 1 as it is less prone to this kind of noise. Now, one final
# note. This relates to ggplot. Note that ggplot uses loess and the geom smooth
# function. So if you type this, you get your points and fitted loess line. 

polls_2008 %>% ggplot(aes(day, margin)) +
     geom_point() + 
     geom_smooth()

# But be careful with the default table, as they are rarely optimal. However,
# you can change these quite easily as is demonstrated in this code, and now we
# get a better fit.

polls_2008 %>% ggplot(aes(day, margin)) +
     geom_point() + 
     geom_smooth(color="red",  span = 0.15,
                 method.args = list(degree=1))



