# --------------------------------------------------------------------------------
#
# bin smoothing and kernels
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

# The general idea of bin smoothing is to group data points into strata in which
# the value of f of x can be assumed to be constant. We can make this assumption
# because we think f of x changes slowly. And as a result, f of x is almost
# constant in small windows of time. An example of this idea is to assume for
# the poll data that public opinion remain approximately the same within a
# week's time. With this assumption in place, we have several data points with
# the same expected value.

# So if we fix a day to be in the center of our week-- call it x0-- then for any
# day x, such that the absolute value of |x - x0| is less than 3.5, we assume
# that f(x) is a constant. Let's call it mu.

# This assumption implies that the expected value of y given x is approximately
# mu when the distance between xi and x0 is less than 3.5. In smoothing, we call
# the size of the interval satisfying the condition the distance between xi and
# x0 is less than 3.5 the window size, the bandwidth, or the span. These are all
# synonyms.

# Now, this assumption implies that a good estimate for f(x) is the average of
# the y values in the window. If we define A0 to be the set of indexes i such
# that xi minus x0 is less than 3.5 in absolute value and N0 as the number of
# indexes in A0, then our estimate is given by this formula.

#    A0 set if undexes i such that |xi - x0| < 3.5
#    N0 number of indexes in A0

#    ^f(x0)=1/N0 ∑i∈A0 Yi

# *** It's just simply the average in the window ***

# The idea behind bin smoothing is to make this calculation for each value of x.
# So we make each value of x the center and recompute that average. So in the
# poll example, for each day, we would compute the average for the values within
# a week of the day that we're considering.

# Let's look at two examples.

# Let's set the center at negative 125 and then also set it at negative 55.
# Here's what the data looks like. The black points are the points that are used
# to compute the average at those two points. The blue line represents the
# average that was computed.

# By computing this average for every point, we form an estimate of the
# underlying curve f(x). In this animation, we see the procedure happening,
# starting at negative 155 all the way up to election day - day 0. At each value
# of x0, we keep the estimate f hat of x0 and move on to the next point.
 
# The final result, which you can compute using this code, looks like this. 

span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, x.points = day, kernel="box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
     ggplot(aes(polls_2008$day, polls_2008$margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") + 
     geom_line(aes(polls_2008$day, smooth), color="red")

# Note that the final result for the bin smoother is quite wiggly. One reason
# for this is that each time the window moves, two points change. So if we start
# with seven points and change two, that's a substantial percentage of points
# that are changing. We can attenuate this somewhat by taking weighted averages
# that give the center of a point more weight than those that are far away from
# the center, with the two points at the edges receiving very little weight.

# We call the functions from which we compute these weights the kernel. Note
# that you can think of the bin smoother as an approach that uses a kernel. The
# formula looks like this.

#    ^f(x0) = N∑i=1 w0(xi)Yi

# Each point receives a weight, in the case of bin smoothers, between 0 for
# points that are outside the window and 1 divided by N0 for points inside the
# window, with N0 the number of points in that week. In the code we showed, we
# told the function k-smooth to use the kernel "box."

# That is because the kernel that gives us bin smoother using this formulation
# looks like a box. Here's a picture.

# Now, the k-smooth function provides a way to obtain a smoother estimate. This
# is by using the normal or Gaussian density to assign weights. So the kernel
# will be the normal density, which we can see here. In this animation, the size
# of the points represent the weights they get in the weighted average. You can
# see that the points near the edges receive little weight.
 
# The final result, which you can get using this code, looks like this. 

span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
     ggplot(aes(polls_2008$day, polls_2008$margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") + 
     geom_line(aes(polls_2008$day, smooth), color="red")

# Note that the final estimate looks smoother now. Now, there are several
# functions in R that implement bin smoothers or kernel approaches.

# One example, the one we showed, is k-smooth. However, in practice, we
# typically prefer methods that use slightly more complex models than fitting a
# constant. The final result that we saw for the smooth bin smoother is still
# somewhat wiggly.

# You can see in some parts-- for example, from negative 125 to negative 75, we
# see that the function is more wiggly than we really expect. We're now going to
# learn about approaches that improve on this.




