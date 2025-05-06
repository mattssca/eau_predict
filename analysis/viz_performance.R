## roc curve
#get prediction probabilities for the "High risk" class
probabilities <- predict(eau_predictor, newdata = selected_data, type = "prob")

#generate the ROC curve for the "High risk" class
roc_high <- roc(selected_data$eau_risk_category, probabilities[, "High risk"])

#plot the ROC curve
plot(roc_high, col = "darkgreen", lwd = 2, main = "ROC Curve for High Risk")

#add the AUC value to the plot
auc_high <- auc(roc_high)
legend(x = 0.6, y = 0.35, legend = paste("AUC =", round(auc_high, 3)), col = "darkgreen", 
       lwd = 5,
       bty = "n", 
       cex = 0.7)


## precision/recal
#create a precision-recall curve
pr <- pr.curve(scores.class0 = probabilities[, "High risk"], 
               weights.class0 = as.numeric(selected_data$eau_risk_category == "High risk"),
               curve = TRUE)

# Plot the precision-recall curve
plot(pr, main = "Precision-Recall Curve")

## calibration plot
# Create a calibration plot
calibration_data <- data.frame(
  Predicted = probabilities[, "High risk"],
  Actual = as.numeric(selected_data$eau_risk_category == "High risk")
)

ggplot(calibration_data, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Calibration Plot", x = "Predicted Probability", y = "Actual Outcome") +
  theme_minimal()


## variable importance
varImpPlot(eau_predictor, n.var = 38)

