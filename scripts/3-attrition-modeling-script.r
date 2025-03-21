# ---------------------------------------------------------------
# Predictive Modeling for HR Attrition
# ---------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(caret)
library(randomForest)

# Load cleaned data (run 01_data_preparation.R first)
hr_data_clean <- readRDS("data/hr_data_clean.rds")

# Prepare data for modeling
set.seed(123) # For reproducibility
model_data <- hr_data_clean

# Split data into training and testing sets
training_index <- createDataPartition(model_data$Attrition, p = 0.7, list = FALSE)
train_data <- model_data[training_index, ]
test_data <- model_data[-training_index, ]

# Check class balance
train_balance <- table(train_data$Attrition)
train_balance_pct <- prop.table(train_balance) * 100
cat("Training data class balance:\n")
print(train_balance)
cat("Percentage:", round(train_balance_pct, 1), "%\n")

# ----------------------------------------------
# Logistic Regression Model
# ----------------------------------------------

# Build a logistic regression model
log_model <- glm(Attrition ~ ., data = train_data, family = "binomial")

# Make predictions on test data
log_predictions <- predict(log_model, newdata = test_data, type = "response")
log_pred_class <- ifelse(log_predictions > 0.5, "Yes", "No")

# Evaluate logistic regression model
log_conf_matrix <- confusionMatrix(factor(log_pred_class, levels = levels(test_data$Attrition)), 
                                  test_data$Attrition)

cat("\nLogistic Regression Model Performance:\n")
print(log_conf_matrix)

# ----------------------------------------------
# Random Forest Model
# ----------------------------------------------

# Build a random forest model
rf_model <- randomForest(Attrition ~ ., data = train_data, ntree = 100, importance = TRUE)

# Make predictions with random forest
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate random forest performance
rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$Attrition)

cat("\nRandom Forest Model Performance:\n")
print(rf_conf_matrix)

# ----------------------------------------------
# Feature Importance Analysis
# ----------------------------------------------

# Extract and analyze feature importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# Sort by importance
importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini))

# Plot variable importance
importance_plot <- ggplot(importance_df[1:15,], aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 15 Variables by Importance for Attrition Prediction",
       x = "Features",
       y = "Importance (Mean Decrease in Gini)")

print(importance_plot)
ggsave("visualizations/feature_importance.png", plot = importance_plot, width = 10, height = 6, dpi = 300)

cat("\nTop 10 most important features:\n")
print(importance_df[1:10, c("Variable", "MeanDecreaseGini")])

# Save models for later use
saveRDS(log_model, "data/logistic_model.rds")
saveRDS(rf_model, "data/random_forest_model.rds")
