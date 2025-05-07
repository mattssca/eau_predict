#load data
load("C:/Users/matts/Desktop/eau_predict/data/full_scores.Rdata")

#tracabillity
set.seed(42)

#get info from meta data
meta_df = UROSCANSEQMetadata2025_01_16 %>% 
  filter(EAU_Risk_Use == "Yes") %>% 
  filter(ClinTest_Progression_Patschan_TaT1 == "Yes") %>% 
  select(EAU_Risk_Score_Class_AdjustedOtherFactor) %>% 
  rownames_to_column("sample_id") %>% 
  rename(eau_risk_category = EAU_Risk_Score_Class_AdjustedOtherFactor) %>% 
  select(sample_id, eau_risk_category)

#subset to minimum columns
sub_scores = full_scores %>% 
  select(sample_id, proliferation_score, progression_score, 
         molecular_grade_who_2022_score, molecular_grade_who_1999_score,
         subtypeUroA, subtypeUroB, subtypeUroC, subtypeGU)

#get names for columns to normalize
norm_signatures <- c("proliferation_score", "progression_score", 
                     "molecular_grade_who_2022_score", 
                     "molecular_grade_who_1999_score")

#normalize the scores for the chosen signatures
sub_scores <- sub_scores %>%
  mutate(across(all_of(norm_signatures), ~ . / rowSums(across(all_of(norm_signatures)), na.rm = TRUE)))

#add EAU categories
sub_scores = sub_scores %>% 
  left_join(meta_df, by = "sample_id")

sub_scores = sub_scores %>% 
  remove_rownames() %>% 
  column_to_rownames("sample_id")

#ensure the target variable is a factor
sub_scores$eau_risk_category <- as.factor(sub_scores$eau_risk_category)


####################################################################################################
#train the model (tmp)
tmp_model <- randomForest(eau_risk_category ~ .,
                          data = sub_scores,
                          ntree = 1750,
                          mtry = 2,
                          importance = TRUE,
                          keep.forest = TRUE)

#print performance of the model
print(tmp_model)

#look for OOB error rate
oob_predictions <- tmp_model$predicted
table(Predicted = oob_predictions, Actual = sub_scores$eau_risk_category)

#plot OOB vs n trees
plot(tmp_model, main = "OOB Error vs. Number of Trees")

#tune RF to find optimal mtry
tuneRF(sub_scores[, -which(names(sub_scores) == "eau_risk_category")],
       sub_scores$eau_risk_category,
       stepFactor = 1.5,
       improve = 0.01,
       ntreeTry = 1750)

#plot variable importance
varImpPlot(tmp_model)

#retrain based on the tuning
eau_predictor_test <- randomForest(
  eau_risk_category ~ .,
  data = sub_scores, 
  proximity = TRUE,
  ntree = 1000,  # Number of trees
  mtry = 2,     # Number of variables randomly sampled at each split
  importance = TRUE,  # Calculate variable importance
  keep.forest = TRUE  # Keep the forest for predictions
)

#print performance of the model
print(eau_predictor_test)


####################################################################################################
library(pROC)
library(pROC)

# Extract true labels and OOB probabilities
true_labels <- sub_scores$eau_risk_category  # True class labels
oob_probabilities <- eau_predictor_test$votes  # OOB predicted probabilities

# Define the risk categories
risk_categories <- colnames(oob_probabilities)

# Loop through each risk category and compute the ROC curve
roc_curves <- list()
auc_values <- list()

for (category in risk_categories) {
  # Convert true labels to binary (1 for the current category, 0 for others)
  binary_labels <- ifelse(true_labels == category, 1, 0)
  
  # Use OOB probabilities for the current category
  oob_prob <- oob_probabilities[, category]
  
  # Compute the ROC curve
  roc_curve <- roc(binary_labels, oob_prob)
  
  # Store the ROC curve and AUC value
  roc_curves[[category]] <- roc_curve
  auc_values[[category]] <- auc(roc_curve)
  
  # Plot the ROC curve
  plot(roc_curve, col = "blue", main = paste("ROC Curve for", category), print.auc = TRUE)
}


custom_colors = c("#86D293","#F3C623","#FF6600","#B8001F")

# Plot all ROC curves in one plot with custom colors
plot(roc_curves[[1]], col = custom_colors[1], main = "ROC Curves for All Risk Categories")
for (i in 2:length(risk_categories)) {
  plot(roc_curves[[i]], col = custom_colors[i], add = TRUE)
}

legend(x = 0.6, y = 0.35, legend = paste(risk_categories, "AUC =", round(unlist(auc_values), 2)), col = custom_colors,
       lwd = 5,
       bty = "n", 
       cex = 0.7)

# Add a legend with custom colors
legend("bottomright", legend = paste(risk_categories, "AUC =", round(unlist(auc_values), 2)), 
       col = custom_colors, lwd = 2)

# Plot all ROC curves in one plot
plot(roc_curves[[1]], col = "red", main = "ROC Curves for All Risk Categories")
for (i in 2:length(risk_categories)) {
  plot(roc_curves[[i]], col = i + 1, add = TRUE)
}
legend(x = 0.6, y = 0.35, legend = paste(risk_categories, "AUC =", round(unlist(auc_values), 2)), col = 2:(length(risk_categories) + 1),
       lwd = 5,
       bty = "n", 
       cex = 0.7)
legend("bottomright", legend = paste(risk_categories, "AUC =", round(unlist(auc_values), 2)), col = 2:(length(risk_categories) + 1), lwd = 2)
