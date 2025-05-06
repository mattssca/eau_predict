#tune RF to find optimal mtry
tuneRF(selected_data[, -which(names(selected_data) == "eau_risk_category")],
       selected_data$eau_risk_category,
       stepFactor = 1.5,
       improve = 0.01,
       ntreeTry = 1750)

#plot variable importance
varImpPlot(rf_model, n.var = 20)

#retrain based on the tuning
eau_predictor <- randomForest(
  eau_risk_category ~ .,
  data = selected_data, 
  proximity = TRUE,
  ntree = 1750,  # Number of trees
  mtry = 3,     # Number of variables randomly sampled at each split
  importance = TRUE,  # Calculate variable importance
  keep.forest = TRUE  # Keep the forest for predictions
)

#print performance of the model
print(eau_predictor)

#save predictor
save(eau_predictor, file = "predictor/eau_predictor_trained_full.Rdata")
