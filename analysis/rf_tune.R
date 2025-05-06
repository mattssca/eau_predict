#tune RF to find optimal mtry
tuneRF(selected_data[, -which(names(selected_data) == "eau_risk_category")],
       selected_data$eau_risk_category,
       stepFactor = 1.5,
       improve = 0.01,
       ntreeTry = 3000)

#plot variable importance
varImpPlot(rf_model, n.var = 38)

#retrain based on the tuning
eau_predictor <- randomForest(
  eau_risk_category ~ .,
  data = selected_data,
  ntree = 3000,  # Number of trees
  mtry = 6,     # Number of variables randomly sampled at each split
  importance = TRUE,  # Calculate variable importance
  keep.forest = TRUE  # Keep the forest for predictions
)

#print performance of the model
print(eau_predictor)

#save predictor
save(eau_predictor, file = "../eau__predictor/predictor/eau_predictor_trained_full.Rdata")
