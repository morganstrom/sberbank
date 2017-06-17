# Sberbank housing data
# 04_validate_models.R

# Load libraries and data


# Set environment variables

# Load data and models
load("models_list.RData")

validate_models <- function(predfun, X, y) {
  # Map actuals to real scale
  act <- map_prediction(y)
  
  # Make predictions using predfun
  pred <- map_prediction(predfun(X))
  
  # Plot predictions vs actuals
  plot(pred, act, main = "real scale")
  abline(0, 1, col="red")

  # Estimate RMSE
  sqrt(mean((pred - act)^2))
  
  # Return RMSLE
  sqrt(mean((log(pred + 1) - log(act + 1))^2))
}

rmsle <- sapply(models, function(fun) validate_models(fun, X = X_val, y = y_val))

# Compare predictions
preds <- sapply(models, function(x) x(X_val))
pairs(preds)

# Make ensemble of glm_mix and xgb?
ens_pred <- rowMeans(preds[, c(3,4)])
ens_pred <- apply(preds[, c(3,4)], 1, max)

#RMSLE
sqrt(mean((ens_pred - y_val)^2))

# Plot
plot(ens_pred, y_val)
abline(0, 1, col = "red")
