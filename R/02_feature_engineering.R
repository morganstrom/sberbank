# Sberbank housing data
# 02_feature_engineering.R

# Load libraries
library(dplyr)
library(Matrix)

# Set environment variables
setwd("~/gitarchives/sberbank/R")

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
source("get_one_hot_encoder.R")
cat_var <- c("state", "material", "product_type", "sub_area", "ecology")
ohe <- sapply(train[,cat_var], get_one_hot_encoder)


# Function that extracts a feature matrix from df
get_feature_matrix <- function(df) {
  
  # Apply pre processing
  source("00_pre_process.R")
  df <- pre_process(df)

  # Apply one hot encoders
  for (c in names(ohe)) {
    df <- cbind(df, ohe[[c]](df[[c]], c))
  }
  # Remove old variables
  df <- df %>%
    select(-one_of(names(ohe)))
  
  # Calculate new features
  # Date, day of week, month, year
  # building age 
  
  # Select relevant features
  
  # Drop timestamp, id and price_doc columns (if exists)
  df <- df %>% select(one_of(setdiff(names(df), c("id", "timestamp", "price_doc"))))
  
  
  # Function to normalize numerical values
  normalize <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  
  # Output sparse matrix for modelling
  X <- Matrix(as.matrix(df), sparse = TRUE)
  X
}





