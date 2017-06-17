# Sberbank housing data
# 03_train_models.R

# Load libraries
library(dplyr)
library(xgboost)
library(glmnet)
library(doMC)

# Set environment variables
setwd("~/gitarchives/sberbank/R")

# Load data, apply pre-processing and define feature extraction
source("02_feature_engineering.R")

# Extract outcome for training
y <- get_outcome_vector(train)

# Extract features for training
X <- get_feature_matrix(train, impute = TRUE)

# Split data into training and validation sets by sampling
set.seed(42341)
rand_val <- runif(length(y), 0, 1)
y_train <- y[rand_val <= 0.7]
y_val <- y[rand_val > 0.7]
X_train <- X[rand_val <= 0.7, ]
X_val <- X[rand_val > 0.7, ]
rm(X, y)

# Train multiple regression models
# --------------------------------

registerDoMC(cores=4)
glm_lasso <- cv.glmnet(X_train, y_train, family = "gaussian",
                    alpha = 1, parallel = T)

registerDoMC(cores=4)
glm_elast <- cv.glmnet(X_train, y_train, family = "gaussian",
                    alpha = 0, parallel = T)

registerDoMC(cores=4)
glm_mix <- cv.glmnet(X_train, y_train, family = "gaussian",
                  alpha = 0.5, parallel = T)

# Look at cross validation
plot.cv.glmnet(glm_lasso)
plot.cv.glmnet(glm_elast)
plot.cv.glmnet(glm_mix)

# Save models
save(glm_lasso, glm_elast, glm_mix, file = "glm1.RData")

# Train xgboost model
# -------------------

# Correct data format
dtrain <- xgb.DMatrix(data = X_train, label = y_train)

gc()

# Params for xgboost
param <- list(objective="reg:linear",
              eval_metric = "rmse",
              booster = "gbtree",
              eta = .08,
              gamma = 1,
              max_depth = 6,
              min_child_weight = 1,
              subsample = .7,
              colsample_bytree = .35
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
save(xgb_model, file = "xgb_model2.RData")

# Look at feature importance
imp <- xgb.importance(feature_names = X@Dimnames[[2]], model = xgb_model)
xgb.plot.importance(imp, top_n = 50)


# Return list of predict functions
models <- list(
  glm_lasso = function(X) predict(glm_lasso, X, s = "lambda.1se"),
  glm_elast = function(X) predict(glm_elast, X, s = "lambda.1se"),
  glm_mix = function(X) predict(glm_mix, X, s = "lambda.1se"),
  xgb = function(X) predict(xgb_model, xgb.DMatrix(X))
)

save(models, file = "models_list.RData")
