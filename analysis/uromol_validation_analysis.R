# Generate predictions
validation_prob <- predict(eau_predictor, newdata = uromol_scores, type = "prob")
validation_response <- predict(eau_predictor, newdata = uromol_scores, type = "response")

# Combine actual labels, predicted labels, and probabilities
results <- data.frame(
  sample_id = uromol_eau$sample_id,
  actual = uromol_eau$eau_risk_category,
  predicted = validation_response,
  prob_high_risk = validation_prob[, "High risk"],
  prob_low_risk = validation_prob[, "Low risk"]
)

####################################################################################################
# Create a confusion matrix
conf_matrix <- confusionMatrix(
  factor(results$predicted, levels = c("Low risk", "High risk")),  # Predicted labels
  factor(results$actual, levels = c("Low risk", "High risk"))  # Actual labels
)

# View the confusion matrix and metrics
print(conf_matrix)

####################################################################################################
# Convert actual labels to binary (e.g., 1 for "High risk", 0 for "Low risk")
results$binary_actual <- ifelse(results$actual == "High risk", 1, 0)

# Create an ROC curve for "High risk"
roc_curve <- roc(results$binary_actual, results$prob_high_risk)

# Plot the ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve for High Risk")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal line for random guessing

# Calculate and display the AUC
auc_value <- auc(roc_curve)
legend(x = 0.6, y = 0.35, legend = paste("AUC =", round(auc_value, 3)), col = "blue", 
       lwd = 5,
       bty = "n", 
       cex = 0.7)


####################################################################################################
# Apply a custom threshold (e.g., 0.5 for "High risk")
results$custom_predicted <- ifelse(results$prob_high_risk > 0.55, "High risk", "Low risk")

# Create a confusion matrix for the custom threshold
conf_matrix_custom <- confusionMatrix(
  factor(results$custom_predicted, levels = c("Low risk", "High risk")),
  factor(results$actual, levels = c("Low risk", "High risk"))
)

# View the confusion matrix and metrics for the custom threshold
print(conf_matrix_custom)

####################################################################################################
# Find misclassified samples
misclassified <- results[results$predicted != results$actual, ]

# View misclassified samples
head(misclassified)

####################################################################################################
# Plot predicted probabilities for "High risk"
ggplot(results, aes(x = prob_high_risk, fill = actual)) +
  geom_density(alpha = 0.5) +
  labs(title = "Predicted Probabilities for High Risk", x = "Probability", y = "Density") +
  theme_minimal()

ggplot(results, aes(x = prob_low_risk, fill = actual)) +
  geom_density(alpha = 0.5) +
  labs(title = "Predicted Probabilities for High Risk", x = "Probability", y = "Density") +
  theme_minimal()

####################################################################################################
# Create an ROC curve
roc_curve <- roc(results$binary_actual, results$prob_high_risk)

# Find the optimal threshold using Youden's Index
optimal_threshold <- coords(roc_curve, "best", ret = "threshold", best.method = "youden")

# Print the optimal threshold
print(paste("Optimal Threshold (Youden's Index):", round(optimal_threshold, 3)))

# Find misclassified samples
misclassified <- results[results$custom_predicted != results$actual, ]

# View misclassified samples
head(misclassified)

