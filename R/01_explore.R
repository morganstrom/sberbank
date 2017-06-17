# Sberbank housing data
# 01_explore.R

# Load libraries
library(dplyr)
library(readr)

# Set environment variables
setwd("~/gitarchives/sberbank/R")

# Load data
train <- read_csv("../data/train.csv")
test <- read_csv("../data/test.csv")
macro <- read_csv("../data/macro.csv")

# Apply pre processing
source("00_pre_process.R")
train_clean <- pre_process(train)
test_clean <- pre_process(test)

# Look at missing data
p_missing <- function(df) {
  p <- sapply(df, function(x) sum(is.na(x)) / length(x))
  sort(round(p, 4)[p > 0])
}
p_missing(train_clean)
p_missing(test_clean)

# Look at classes
table(sapply(train_clean, class))

# Make exploratory graphs and summaries

# Outcome variable
hist(train_clean$price_doc)
hist(log(train_clean$price_doc) + 1)
