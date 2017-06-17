# Sberbank housing data
# 02_feature_engineering.R

# Load libraries
library(dplyr)
library(Matrix)

# Set environment variables
setwd("~/gitarchives/sberbank/R")
source("00_pre_process.R")

# Load data
train <- read_csv("../data/train.csv")
test <- read_csv("../data/test.csv")
macro <- read_csv("../data/macro.csv")


# Function that extracts outcome vector y from training set df
get_outcome_vector <- function(df) {
  log(df$price_doc + 1)
}

# Function that maps prediction to the original scale
map_prediction <- function(y_hat) {
  exp(y_hat) - 1
}

# Assert
#assertthat::are_equal(train$price_doc, 
#                      map_prediction(get_outcome_vector(train)))

# Get list of one hot encoders for categorical variables, based on training data
train_clean <- pre_process(train)

source("get_one_hot_encoder.R")
cat_var <- c("state", "material", "product_type", "sub_area", "ecology")
ohe <- sapply(train_clean[,cat_var], get_one_hot_encoder)

# Get list of imputer functions for numerical variables, based on training data
# Median is default function
get_imputer <- function(x, f = median) {
  val <- f(x, na.rm = TRUE)
  
  function(x) {
    x[is.na(x)] <- val
    x
  }
}

imputer <- sapply(train_clean, get_imputer, f = median)


# Function that extracts a feature matrix from df
get_feature_matrix <- function(df, impute = FALSE) {
  
  # Apply pre processing
  df <- pre_process(df)
  
  # Impute missing variables
  if (impute) {
    for (c in names(imputer)) {
      df[, c] <- imputer[[c]](df[, c])
    }
  }

  # Apply one hot encoders
  for (c in names(ohe)) {
    df <- cbind(df, ohe[[c]](df[, c], c))
  }
  # Remove old variables
  df <- df %>%
    select(-one_of(names(ohe)))
  
  # Calculate new features
  # Date, day of week, month, year
  # building age 
  
  # Select relevant features
  
  # Function to normalize numerical values
  normalize <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  
  # Output sparse matrix for modelling
  X <- Matrix(as.matrix(df), sparse = TRUE)
  X
}





