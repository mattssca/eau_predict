#load predictions for uromol
load("C:/Users/matts/Desktop/eau_predict/data/uromol/pred_uromol_fixed_grade.Rdata")
load("C:/Users/matts/Desktop/eau_predict/data/uromol/uromol_eau_adjsuted.Rdata")

#subset scores
uromol_scores = pred_uromol_fixed_grade_score$scores %>% 
  rownames_to_column("sample_id")

#get subtype information
subtypes_df = as.data.frame(pred_uromol_fixed_grade_score$predictions_7classes) %>% 
  rownames_to_column("sample_id") %>% 
  rename(subtype = `pred_uromol_fixed_grade_score$predictions_7classes`)

#join the data
uromol_scores = uromol_scores %>% 
  left_join(subtypes_df, by = "sample_id")

#create one-hot encoded matrix for the 'subtype' column
subtype_matrix <- model.matrix(~ subtype - 1, data = uromol_scores)

#convert the matrix to a data frame and bind it to the original data
uromol_scores <- cbind(uromol_scores, as.data.frame(subtype_matrix))

#subset to samples of interest
uromol_scores = uromol_scores %>% 
  filter(sample_id %in% uromol_eau$sample_id)

#save object
save(uromol_scores, file = "data/uromol/uromol_validation.Rdata")
