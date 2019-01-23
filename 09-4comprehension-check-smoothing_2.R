# --------------------------------------------------------------------------------
#
# COmprehension check - Smoothing - Question 2
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
library(stringr)

# Data set
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

# *** resulting data set dat ***

# A tibble: 1,205 x 5
#      day month year  deaths date      
#  <dbl> <dbl> <chr>    <dbl> <date>    
#  1     1     1 2015     107 2015-01-01
#  2     2     1 2015     101 2015-01-02
#  3     3     1 2015      78 2015-01-03
#  4     4     1 2015     121 2015-01-04
#  5     5     1 2015      99 2015-01-05
#  6     6     1 2015     104 2015-01-06
#  7     7     1 2015      79 2015-01-07
#  8     8     1 2015      73 2015-01-08
#  9     9     1 2015      90 2015-01-09
# 10    10     1 2015      75 2015-01-10
# ... with 1,195 more rows

# ********************************************************************
#
# Q2 Work with the same data as in Q1 to plot smooth estimates against day of
# the year, all on the same plot, but with different colors for each year.
# Which code produces the desired plot?
#
# ********************************************************************

# dat %>% 
#      mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
#      ggplot(aes(day, smooth, col = year)) +
#      geom_line(lwd = 2)
# 
# dat %>% 
#      mutate(smooth = predict(fit, as.numeric(date)), day = mday(date), year = as.character(year(date))) %>%
#      ggplot(aes(day, smooth, col = year)) +
#      geom_line(lwd = 2)
# 
# dat %>% 
#      mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
#      ggplot(aes(day, smooth)) +
#      geom_line(lwd = 2)
# 
# dat %>% 
#      mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
#      ggplot(aes(day, smooth, col = year)) +
#      geom_line(lwd = 2)

# As before, remove the NA from the deaths column
dat <- dat[!is.na(dat$deaths),]

# Base code from the prior comprehension check
total_days <- diff(range(as.numeric(dat$date)))
span <- 62/total_days

fit <- loess(deaths ~ as.numeric(date), degree=1, span = span, data=dat)

# Base option (plot by year)
dat %>% mutate(smooth = fit$fitted, date) %>%
     ggplot(aes(as.numeric(date), deaths)) +
     geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(as.numeric(date), smooth), color="red")

# Option 1 - POSSIBLE (shows data for 365 days, with a colored line for each year)
dat %>% 
     mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
     ggplot(aes(day, smooth, col = year)) +
     geom_line(lwd = 2)

# Option 2 - INCORRECT (shows one month)
dat %>% 
     mutate(smooth = predict(fit, as.numeric(date)), day = mday(date), year = as.character(year(date))) %>%
     ggplot(aes(day, smooth, col = year)) +
     geom_line(lwd = 2)

# Option 3 - INCORRECT (shows a solid black filled area for a year)
dat %>% 
     mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
     ggplot(aes(day, smooth)) +
     geom_line(lwd = 2)

# Option 4 - POSSIBLE (shows data for 365 days, with a colored line for each year)
dat %>% 
     mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
     ggplot(aes(day, smooth, col = year)) +
     geom_line(lwd = 2)

# Answer: Option 4
