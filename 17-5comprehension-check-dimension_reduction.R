# --------------------------------------------------------------------------------
#
# Model Fitting and Recommendation Systems - comprehension check - dimension reduction
#
# --------------------------------------------------------------------------------

# Setup
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(Lahman)
library(HistData)
library(caret)
library(e1071)
library(matrixStats)
library(Rborist)
library(randomForest)
library(gam)

#
# Q1
#

# We want to explore the tissue_gene_expression predictors by plotting them.

data("tissue_gene_expression")
dim(tissue_gene_expression$x) # dimension of the matrix
[1] 189 500

# We want to get an idea of which observations are close to each other, but, as
# you can see from the dimensions, the predictors are 500-dimensional, making
# plotting difficult. Plot the first two principal components (PC) with color
# representing tissue type.

tissue_gene_expression$x[1,]         # This is the first row
length(tissue_gene_expression$x[1,]) # 500 is the number of predictors
tissue_gene_expression$x[,1]         # This is the first column
length(tissue_gene_expression$x[,1]) # 189 is the number of observations by tissue
tissue_gene_expression$x[1,1]        # 9.82568 (for cerebellum_1, MAML1 predictor)

ftf <- cbind(tissue_gene_expression$x[,1], tissue_gene_expression$x[,2])
dim(ftf)
# [1] 189   
# [,1]      [,2]
# cerebellum_1    9.825680  8.327163
# cerebellum_2    9.631247  8.542827
# cerebellum_3    9.690548  8.476486
# ...

# This is the correct code - taken from book after getting right answer
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
     ggplot(aes(pc_1, pc_2, color = tissue)) +
     geom_point()

# Which tissue is in a cluster by itself? ANSWER: Liver

#
# Q2
#

# The predictors for each observation are measured using the same device and
# experimental procedure. This introduces biases that can affect all the
# predictors from one observation. For each observation, compute the average
# across all predictors, and then plot this against the first PC with color
# representing tissue. Report the correlation.

# Instructor answer - I did not get it right
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
     ggplot(aes(avgs, pc_1, color = tissue)) +
     geom_point()
cor(avgs, pc$x[,1])

# What is the correlation?
# [1] 0.5969088

#
# Q3
#

# We see an association with the first PC and the observation averages. Redo the
# PCA but only after removing the center. Part of the code is provided for you.

#BLANK
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
     ggplot(aes(pc_1, pc_2, color = tissue)) +
     geom_point()

# Which line of code should be used to replace #BLANK in the code block above?
x <- with(tissue_gene_expression, sweep(x, 1, mean(x)))
x <- sweep(x, 1, rowMeans(tissue_gene_expression$x))
x <- tissue_gene_expression$x - mean(tissue_gene_expression$x)
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))

x <- with(tissue_gene_expression, sweep(x, 1, mean(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
     ggplot(aes(pc_1, pc_2, color = tissue)) +
     geom_point()

x <- sweep(x, 1, rowMeans(tissue_gene_expression$x))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
     ggplot(aes(pc_1, pc_2, color = tissue)) +
     geom_point()

x <- tissue_gene_expression$x - mean(tissue_gene_expression$x)
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
     ggplot(aes(pc_1, pc_2, color = tissue)) +
     geom_point()

# This is the correct answer
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
     ggplot(aes(pc_1, pc_2, color = tissue)) +
     geom_point()


#
# Q4
#

# For the first 10 PCs, make a boxplot showing the values for each tissue.

# For the 7th PC, which two tissues have the greatest median difference?
# Select the TWO tissues that have the greatest median difference.

for(i in 1:10){
     boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

# Answer: Placenta and Liver

#
# Q5
#

# Plot the percent variance explained by PC number. Hint: use the summary
# function.

# How many PCs are required to reach a cumulative percent variance explained
# greater than 50%?

df <- summary()
     
# Create a data frame from the summary() function to get importance. I also
# modified it from wide to long (ggplot like it better) and added a few more
# fields ... the cumulative sum and an index for the PCs.

importance_df <- data.frame(summary(pca)$importance)
importance_df <- importance_df[2,] %>% 
     gather(key = pc, value = importance)
importance_df <- importance_df %>% mutate(pc_index = as.integer(str_remove(importance_df$pc, "PC")))
importance_df$pc <- factor(importance_df$pc, levels = importance_df$pc[order(importance_df$pc_index)])
importance_df <- importance_df %>% mutate(cum_sum = cumsum(importance))

# Then it's just a matter of using ggplot(), with the PCs on the x and the
# cumulative sum on the y.

importance_df %>% 
filter(pc_index < 20) %>% 
arrange(pc_index, cum_sum) %>% 
ggplot(aes(x = pc, y = cum_sum, fill=pc)) +
geom_col() +
scale_y_continuous(breaks = seq(0,1,0.1)) +
theme_grey()

I do have one slight tweak to your code above which might save some work.

See what you think about

importance_df <- data.frame(Sum_Exp = summary(pca)$importance[3, ]) %>%
     rownames_to_column("PCA")
Which gives this

nada Etc.

Which simplifies things a bit.

#
# Q6
#


