# --------------------------------------------------------------------------------
#
# Comprehension check - Distance
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

# Q1 - Load the following dataset:

library(dslabs)
data("tissue_gene_expression")

# This dataset includes a matrix x:
     
dim(tissue_gene_expression$x)

# This matrix has the gene expression levels of 500 genes from 189 biological
# samples representing seven different tissues. The tissue type is stored in y:
     
table(tissue_gene_expression$y)

# Which of the following lines of code computes the Euclidean distance between
# each observation and stores it in the object d?

d <- dist(tissue_gene_expression$x, distance='maximum')
d <- dist(tissue_gene_expression)
d <- dist(tissue_gene_expression$x) # Correct answer
d <- cor(tissue_gene_expression$x)

# Q2 - Compare the distances between observations 1 and 2 (both cerebellum),
# observations 39 and 40 (both colon), and observations 73 and 74 (both
# endometrium).

# The vector of predictors for each of these observations are going to be saved
# in these three objects.

x_1  <- d[1]
x_2  <- d[2]
x_39 <- d[39]
x_40 <- d[40]
x_73 <- d[73]
x_74 <- d[74]

# The distances

sqrt(sum((x_1-x_2)^2))
# [1] 0.3706502
sqrt(sum((x_39-x_40)^2))
# [1] 0.4358396
sqrt(sum((x_73-x_74)^2))
# [1] 0.1077056

sqrt(sum((x_74-x_1)^2))
# [1] 14.88258
sqrt(sum((x_74-x_39)^2))
# [1] 0.811257
sqrt(sum((x_40-x_1)^2))
# [1] 16.12967
sqrt(sum((x_40-x_74)^2))
# [1] 1.247097

# No, the samples from the same tissue type are not necessarily closer.
# The two colon samples are closest to each other, but the samples from the other two tissues are not.
# The two cerebellum samples are closest to each other, but the samples from the other two tissues are not.
# CORRECT: Yes, the samples from the same tissue type are closest to each other.

# Q3 - Make a plot of all the distances using the image function to see if the
# pattern you observed in Q2 is general. Which code would correctly make the
# desired plot?

image(d)
image(as.matrix(d))   # CORRECT ANSWER
d
image()

