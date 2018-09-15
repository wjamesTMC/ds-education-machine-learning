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

# In the two simple examples above, we only had one predictor. We actually do not consider these machine learning challenges, which are characterized by cases with many predictors. Let’s go back to the digits example in which we had 784 predictors. For illustrative purposes, we will start by simplifying this problem to one with 2 predictors and two classes. Specifically, we define the challenge as building an algorithm that can determine if a digit is a 2 or 7 from the predictors. We are not quite ready to build algorithms with 784 predictors so we will extract two simple predictors from the 784: the proportion of dark pixels that are in the upper left quadrant (X1) and the lower right quadrant (X2).

# We then select a random sample of 1,000 digits, 500 in the training set and
# 500 in the test set and provide them here:
     
data("mnist_27")

# We can explore this data by plotting the two predictors and by using colors to
# denote the labels:
     
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) +
geom_point()

# We can immediately see some patterns. For example, if X1 (the upper left
# panel) is very large, then the digit is probably a 7. Also, for smaller values
# of X1, the 2s appear to be in the mid range values of X2.

# These are the images of the digits with the largest and smallest values for X1
# x1: (image)

# And here are the original images corresponding to the largest and smallest
# value of X2: (IMAGE)

# We can start getting a sense for why these predictors are useful, but also why
# the problem will be somewhat challenging.

# So let’s try building a machine learning algorithm. We haven’t really learned
# any algorithms yet, so let’s start with logistic regression. The model is
# simply:

#    p(x1,x2)=Pr(Y=1∣X1=x1,X2=x2)=g−1(β0+β1x1+β2x2
                                  
# with g−1g−1 the inverse of the logistic function: g−1(x)=exp(x)/{1+exp(x)}. We
# fit it like this:

fit <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")

# We can now build a decision rule based on the estimate of ^p(x1,x2).

p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))

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
#> 

# We get an accuracy of 0.79! Not bad for our first try. But can we do better?
     
# Because we constructed the mnist_27 example and we had at our disposal 60,000
# digits in just the MNIST dataset, we used this to build the true conditional
# distribution p(x1,x2) Keep in mind that this is something we don’t have access
# to in practice, but we include it in this example because it lets us compare
# ^p(x1,x2) to the true p(x1,x2) which teaches us the limitations of different
# algorithms. Let’s do that here. We can access and plot p(x1,x2) like this:
     
mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
geom_raster() 

# IMAGE

We will choose better colors and draw a curve that separates pairs (X1,X2) for which P(X1,X2) > 0.5 and cases for which P(X1,X2) > 0.5:
     
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
geom_raster() +
scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
stat_contour(breaks=c(0.5),color="black")

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

# We can see where the mistakes were made mainly come from low values X1 that have either high or low value of X2. Logistic regression can’t catch this.

p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
     ggplot() +
     stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
     geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test) 

# We need something more flexible. A method that permits estimates with shapes other than a plane.

# We are going to learn a few new algorithms based on different ideas and concepts. But what they all have in common is that they permit more flexible approaches. We will start by describing nearest neighbor and kernel approaches. To introduce the concepts behinds these approaches, we will again start with a simple one dimensional example and describe the concept of smoothing.
