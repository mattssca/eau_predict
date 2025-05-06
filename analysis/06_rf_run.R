#generate predictions on the validation dataset
predictions <- predict(eau_predictor, 
                       newdata = selected_data)

#view the predictions
head(predictions)

#add predictions to the validation dataset
selected_data$predicted_class <- predictions

#print head
head(selected_data)

#subset the actual eau categories and predicted
selected_data_sub = selected_data %>% 
  select(eau_risk_category, predicted_class)
