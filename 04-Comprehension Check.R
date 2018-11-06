# --------------------------------------------------------------------------------
#
# Comprehension Check for the Unit on Confusion Matrix
#
# --------------------------------------------------------------------------------

# Setup Libraries
library(dslabs)
library(dplyr)
library(lubridate)
library(caret)

#--------------------------------------------------------------------------------------
# Set up for the questions
#--------------------------------------------------------------------------------------

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
     filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
     mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
     select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))  # Sex, what we want to predict
x <- dat$type                              # inclass or online, the predictor

#--------------------------------------------------------------------------------------
# The questions
#--------------------------------------------------------------------------------------

# Question 1: What is the propotion of females in class and online? (That is,
# calculate the proportion of the in class students who are female and the
# proportion of the onlinx students who are female.)

# Get the basic data as shown
tns <- length(y)                 # total number in class
pic <- mean(x == "inclass")      # percentage in class
pol <- mean(x == "online")       # percentage online
nic <- tns * pic                 # number in class
nol <- tns * pol                 # number online

# Create a vector fol which lists all femails in dat who are in class
# Then create the ratio of that number (length(fol)) and the total in class (nic)
fol <- which(dat$sex == "Female" & dat$type == "inclass")
pfic <- length(fol) / nic
pfic

# Do the same for femails who are online
fic <- which(dat$sex == "Female" & dat$type == "online")
pfol <- length(fic) / nol
pfol

# Answers:
# inclass = 0.6666667  (note the grader accepts .67)
# online  = 0.3783784  (note the grader accepts .38)

# Question 2: If you used the type variable to predict sex, what would the
# prediction accuracy be?

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)  
test_set <- y[test_index]
train_set <- y[-test_index]

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
     factor(levels = levels(test_set))

# GOOD TO THIS POINT ---------------------------------------------

# Read the # "textbook" that comes with the course:
#
# https://rafalab.github.io/dsbook/the-confusion-matrix-prevalence-sensitivity-and-specificity.html
#
# Section 54 deals with our problem. The "Textbook" is really exactly what the
# instructor is saying in the videos. The exercises seem to be the problems we
# are asked to solve.

# You have to create a model using the data in the "type" column. So, you are
# using type (online or offline) to predict the gender of the student. Once you
# have that model (e.g. y_hat = some model) you can use mean(y == y_hat) to find
# the answers to the next several questions (or just use confusionMatrix(),
# since it will display all the stats you need - you know you correct if your
# confidence interval is: 95% CI : (0.5508, 0.7104)).

# Section 54.2 goes over another model using the Heights dataset. They use
# "Male" vs "Female" to predict height. You want to do the same thing only use
# our "type" column data. Remember that in our dat dataset, "y" is the actual,
# or the data we use to determine how good our "model" is (i.e. compare y_hat
# against y)

y <- factor(dat$sex, c("Female", "Male"))   # What we want to predict (sex)
x <- dat$type                               # The predictor (based on inclass or online)

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

y_hat <- sample(c("Female", "Male"), length(test_index), replace = TRUE) %>%
     factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# Question 3: Write a line of code using the table function to show the
# confusion matrix, assuming the prediction is y_hat and the truth is y.

# Question 4: What is the sensitivity of this prediction?

# Question 5: What is the specificity of this prediction?

# Question 6: What is the prevalence (% of females) in the dat dataset defined
# above?


# Working area - might be useful
psm_ic <- ifelse(dat$type == "inclass", "Male", "Female")
psf_ic <- ifelse(dat$type == "inclass", "Female", "Male")
psm_ol <- ifelse(dat$type == "online", "Male", "Female")
psf_ol <- ifelse(dat$type == "online", "Female", "Male")

mean(psm_ic == dat$sex)
mean(psf_ic == dat$sex)
mean(psm_ol == dat$sex)
mean(psf_ol == dat$sex)
#
# Working area - probably all wrong
mean(y_hat == test_set$sex)
# [1] 0.5066667

mean(y == y_hat)

table(predicted = y_hat, actual = test_set$sex)

test_set %>% 
     mutate(y_hat = y_hat) %>%
     group_by(sex) %>% 
     summarize(accuracy = mean(y_hat == sex))

prev <- mean(y == "Male")
prev

confusionMatrix(data = y_hat, reference = test_set$sex)
