# Sberbank housing data
# 05_predict.R

# Load libraries

# Set environment variables

# Load data and model


# Extract features from test set
X_test <- get_feature_matrix(test)
dtest <- xgb.DMatrix(X_test)

# Apply model
y_hat_test <- predict(xgb_model, dtest)

# Create submission file
sub <- data.frame(id = test$id, price_doc = map_prediction(y_hat_test))
write_csv(sub, "../submission/xgbmodel1.csv")
