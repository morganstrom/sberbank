# Sberbank housing data
# 05_predict.R

# Load libraries

# Set environment variables

# Load data and models
test_clean <- pre_process(test)
load("models_list.RData")

# Extract features from test set
X_test <- get_feature_matrix(test, impute = TRUE)

# Apply models, use mean
y_hat_test <- rowMeans(cbind(models$glm_mix(X_test),
                             models$xgb(X_test)))

# Apply models, use max

# Create submission file
sub <- data.frame(id = test$id, price_doc = map_prediction(y_hat_test))
write_csv(sub, "../submission/ensemble1.csv")
