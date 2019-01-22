# --------------------------------------------------------------------------------
#
# COmprehension check - Smoothing - Question 1
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
library(purrr)
library(pdftools)
library(lubridate)

# In the Wrangling course of this series, PH125.6x, we used the following code
# to obtain mortality counts for Puerto Rico for 2015-2018:

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
     s <- str_trim(s)
     header_index <- str_which(s, "2015")[1]
     tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
     month <- tmp[1]
     header <- tmp[-1]
     tail_index  <- str_which(s, "Total")
     n <- str_count(s, "\\d+")
     out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
     s[-out] %>%
          str_remove_all("[^\\d\\s]") %>%
          str_trim() %>%
          str_split_fixed("\\s+", n = 6) %>%
          .[,1:5] %>%
          as_data_frame() %>% 
          setNames(c("day", header)) %>%
          mutate(month = month,
                 day = as.numeric(day)) %>%
          gather(year, deaths, -c(day, month)) %>%
          mutate(deaths = as.numeric(deaths))
}) %>%
     mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                           "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
     mutate(date = make_date(year, month, day)) %>%
     filter(date <= "2018-05-01")

# Use the loess function to obtain a smooth estimate of the expected number of
# deaths as a function of date. Plot this resulting smooth function. Make the
# span about two months long.

# See the video lecture and literally copy that code when they use the loess
# function, just change the variables to match your variable names.
total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(day, smooth_1), color="red", lty = 2) +
     geom_line(aes(day, smooth_2), color="orange", lty = 1) 

