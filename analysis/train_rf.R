#train the model
rf_model <- randomForest(eau_risk_category ~ .,
                         data = selected_data,
                         ntree = 1750,
                         mtry = 9,
                         importance = TRUE,
                         keep.forest = TRUE)

#print performance of the model
print(rf_model)

#look for OOB error rate
oob_predictions <- rf_model$predicted
table(Predicted = oob_predictions, Actual = selected_data$eau_risk_category)

#plot OOB vs n trees
plot(rf_model, main = "OOB Error vs. Number of Trees")
