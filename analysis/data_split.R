#subset to variables that might be informative
selected_data <- full_scores %>%
  remove_rownames() %>% 
  column_to_rownames("sample_id") %>% 
  select(eau_risk_category, all_of(unique_top_variables$var))

#create a training index (e.g., 70% training, 30% validation)
train_index <- createDataPartition(selected_data$eau_risk_category, p = 0.7, list = FALSE)

#split the data
training_data <- selected_data[train_index, ]  # 70% for training
validation_data <- selected_data[-train_index, ]  # 30% for validation

#check the dimensions
cat("Training data:", nrow(training_data), "rows\n")
cat("Validation data:", nrow(validation_data), "rows\n")
