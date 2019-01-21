# --------------------------------------------------------------------------------
#
# Case study: Is it a 2 or a 7?
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
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(RColorBrewer)

data("mnist_27")

# In the simple examples we've examined up to now, we only had one predictor.
# We actually do not consider these machine learning challenges, which are
# characterized by having many predictors. So let's go back to the digit
# example, in which we had 784 predictors. However, for illustrative purpose, we
# would look at a subset of this data set, where we only have two predictors and
# two we will start by simpli.ing this problem to one with 2 predictors and two
# categories (classes).

# We are not quite ready to build algorithms with 784 predictors so we will
# extract two simple predictors from the 784: the proportion of dark pixels that
# are in the upper left quadrant (X1) and the lower right quadrant (X2). We want
# to build an algorithm that can determine if a digit is a two or a seven from
# the two predictors.

# To have a more manageable data set, we will select a random sample of 1,000
# digits from the training set that has 60,000 digits. 500 will be in the
# training set, and 500 will be in the test set. We actually include these
# examples in the DS Lab package.

# We can explore this data by plotting the two predictors and use colors to
# denote the labels. You can see them here.
     
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) +
geom_point()

# We can immediately see some patterns. For example, if X1 (the upper left
# panel) is very large, then the digit is probably a 7. Also, for smaller values
# of X1, the 2s appear to be in the mid range values of X2.

# Also, for smaller values of the second predictor, the lower right panel, the
# twos appear to be in the mid-range values. To connect these insights to the
# original data, let's look at the images of the digits with the largest and
# smallest values of x1. Here are the images.

# IMAGES

# This makes a lot of sense. The image on the left, which is a seven, has a lot
# of dark in the upper left quadrant. So x1 is big. The digit on the right,
# which is a two, has no black on the upper left quadrant. So x1 is small.

# We can start getting a sense for why these predictors are useful, but also why
# the problem will be somewhat challenging.

Now let's look at the original images corresponding
to the largest and smallest values of the second predictor, x2, which
represents the lower right quadrant.
Here we see that they're both sevens.
The seven on the left has a lot of black on the lower right quadrant.
The seven on the right has very little black on the lower right quadrant.
So we can start getting a sense for why these predictors are informative,
but also why the problem will be somewhat challenging.

# So let’s try building a machine learning algorithm. We haven’t really learned
# any algorithms yet, so let’s start with logistic regression. The model is
# simply:

#    p(x1,x2)=Pr(Y=1∣X1=x1,X2=x2)=g−1(β0+β1x1+β2x2)
                                  
# with g−1g−1 the inverse of the logistic function: g−1(x)=exp(x)/{1+exp(x)}. We
# can fit it using the glm function like this.:

fit <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")

# # And now we can build a decision rule based on the estimate of the
# conditional probability  ^p(x1,x2). Whenever it is bigger than 0.5, we predict
# a seven. Whenever it's not, we predict a two. So we write this code.

p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))

# Then we compute the confusion matrix, and we see that we achieve an accuracy
# of 79%.

confusionMatrix(data = y_hat, reference = mnist_27$test$y)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  2  7
#>          2 92 34
#>          7 14 60
#>                                         
#>                Accuracy : 0.76          
#>                  95% CI : (0.695, 0.817)
#>     No Information Rate : 0.53          
#>     P-Value [Acc > NIR] : 1.67e-11      
#>                                         
#>                   Kappa : 0.512         
#>  Mcnemar's Test P-Value : 0.0061        
#>                                         
#>             Sensitivity : 0.868         
#>             Specificity : 0.638         
#>          Pos Pred Value : 0.730         
#>          Neg Pred Value : 0.811         
#>              Prevalence : 0.530         
#>          Detection Rate : 0.460         
#>    Detection Prevalence : 0.630         
#>       Balanced Accuracy : 0.753         
#>                                         
#>        'Positive' Class : 2             

# We get an accuracy of 0.79! Not bad for our first try. But can we do better?
# # Now before we continue, I want to point out that, for this particular data
# set, I know the true conditional probability. This is because I constructed
# this example using the entire set of 60,000 digits. I use this to build the
# true conditional probability p of x1, x2. Now note that this is something we
# don't have access to in practice, but included here in this example because it
# lets us compare estimates to our true conditional probabilities. And this
# teaches us the limitations of the different algorithms. So let's do that here.
# We can access and plot p(x1,x2) like this:
     
mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
geom_raster() 

# IMAGE

# We will choose better colors and draw a curve that separates pairs (X1,X2) for
# which P(X1,X2) > 0.5 and cases for which P(X1,X2) > 0.5:
     
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
geom_raster() +
scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
stat_contour(breaks=c(0.5),color="black")

# IMAGE

# So above you see a plot of the true P(X,Y). To start understanding the
# limitations of logistic regression here, first, note that with logistic
# regression P(X,Y) has to be a plane and as a result the boundary defined by
# the decision rule is given by:

#    P(X,Y) = 0.5

# which implies the boundary can’t be anything other than a straight line:

#    g−1(^β0+^β1x1+^β2x2)=0.5⟹^β0+^β1x1+^β2x2=g(0.5)=0⟹x2=−^β0/^β2+−^β1/^β2x1

# which implies our logistic regression approach has no chance of capturing the
# non-linear nature of the true p(x1,x2)p(x1,x2). Here is a visual
# representation of ^p(x1,x2)p^(x1,x2):

p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
     ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
     geom_raster() +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(breaks=c(0.5),color="black") 

# IMAGE

# Now to see where the mistakes were made, we can again plot the test data with
# x1 and x2 plotted against each other and color used to show the label. If we
# do this, we can see where the mistakes are made. Because logistic regression
# divides the sevens and the twos with a line, we will miss several points that
# can't be captured by this shape. Logistic regression can’t catch this.

p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
     ggplot() +
     stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
     geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test) 

# IMAGE

# So we need something more flexible. Logistic regression forces our estimates
# to be a plane and our boundary to be a line. We need a method that permits
# other shapes. We will start by describing the nearest neighbor algorithm and
# some kernel approaches. To introduce the concepts behind these approaches, we
# will again start with a simple one-dimensional example and describe the
# concept of smoothing.

# ---


So let's try building a machine learning algorithm with what we have.
We haven't really learned any algorithm yet.
So let's start with logistic regression.
The model will be simply like this.
The conditional probability of being a seven given the two predictors x1
and x2 will be a linear function of x1 and x2
after the logistic transformation.




We can access and plot the true conditional probability.
We can use this code.
And it looks like this.
We will improve this plot by choosing better colors.
And we'll also draw a curve that separates the pairs, x1, x2,
for which the conditional probably is bigger than 0.5 and lower than 0.5.
We use this code.
And now the plot looks like this.
So we can see the true conditional probability.
So to start understanding the limitations of logistic regression,
we can compare the true conditional probability
to the estimated conditional probability.
Let's compute the boundary that divides the values of x1 and x2
that make the estimated conditional probably lower than 0.5
and larger than 0.5.
So at this boundary, the conditional probability
is going to be equal to 0.5.
Now we can do a little bit of math, shown here.
And if we do this, we will see that the boundary can't
be anything other than a straight line, which
implies that our logistic regression approach has
no chance of capturing the non-linear nature
of our true conditional probability.
You can see that the boundary of the true conditional probability
is a curve.



