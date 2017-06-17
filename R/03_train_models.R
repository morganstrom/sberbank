# Sberbank housing data
# 03_train_models.R

# Load libraries
library(dplyr)
library(xgboost)
library(glmnet)

# Set environment variables
setwd("~/gitarchives/sberbank/R")

# Load data, apply pre-processing and define feature extraction
source("02_feature_engineering.R")

# Extract outcome for training
y <- get_outcome_vector(train)

# Extract features for training
X <- get_feature_matrix(train)

# Split data into training and validation sets by sampling
#set.seed(42341)
#rand_val <- runif(length(y), 0, 1)
#y_train <- y[rand_val <= 0.7]
#y_val <- y[rand_val > 0.7]
#X_train <- X[rand_val <= 0.7, ]
#X_val <- X[rand_val > 0.7, ]
#rm(X, y)

# Train multiple regression models with regularization using glmnet


# Train xgboost model
# -------------------

# Correct data format
dtrain <- xgb.DMatrix(data = X, label = y)

gc()

# Params for xgboost
param <- list(objective="reg:linear",
              eval_metric = "rmse",
              booster = "gbtree",
              eta = .05,
              gamma = 1,
              max_depth = 4,
              min_child_weight = 1,
              subsample = .7,
              colsample_bytree = .7
)

# Train model
xgb_model <- xgb.train(data = dtrain,
                       params = param,
                       watchlist = list(train = dtrain),
                       nrounds = 501,
                       verbose = 1,
                       print_every_n = 5,
                       missing=NA
)

# Save model
save(xgb_model, file = "xgb_model1.RData")

# Look at feature importance
imp <- xgb.importance(feature_names = X@Dimnames[[2]], model = xgb_model)
xgb.plot.importance(imp, top_n = 50)

# Prune?
# Make predictions on validation set?
