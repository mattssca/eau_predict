#generate a confusion matrix
confusion_matrix <- confusionMatrix(selected_data$predicted_class, selected_data$eau_risk_category)

#print the confusion matrix
print(confusion_matrix)
