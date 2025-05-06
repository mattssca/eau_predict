#load data
load("../eau__predictor/data/uroscanseq/pred_uroscanseq_high_low_fixed_grade.Rdata")
load("../eau__predictor/data/uroscanseq/UROSCANSEQMetadata2025_01_16.Rdata")

#get info from meta data
meta_df = UROSCANSEQMetadata2025_01_16 %>% 
  filter(EAU_Risk_Use == "Yes") %>% 
  filter(ClinTest_Progression_Patschan_TaT1 == "Yes") %>% 
  select(EAU_Risk_Score_Class_AdjustedOtherFactor) %>% 
  rownames_to_column("sample_id") %>% 
  rename(eau_risk_category = EAU_Risk_Score_Class_AdjustedOtherFactor) %>% 
  select(sample_id, eau_risk_category)

#subset scores
full_scores = pred_uroscanseq_high_low_fixed_grade_score$scores %>% 
  rownames_to_column("sample_id")

#get subtype information
subtypes_df = as.data.frame(pred_uroscanseq_high_low_fixed_grade_score$predictions_7classes) %>% 
  rownames_to_column("sample_id") %>% 
  rename(subtype = `pred_uroscanseq_high_low_fixed_grade_score$predictions_7classes`)

#join the data
full_scores = full_scores %>% 
  left_join(subtypes_df, by = "sample_id")

#add EAU risk category information
full_scores = full_scores %>% 
  right_join(meta_df, by = "sample_id")

#mutate the risk category group
full_scores$eau_condensed <- ifelse(
  full_scores$eau_risk_category %in% c("Low risk", "Intermediate risk"), 
  "Low risk", "High risk")

#replace 4 level EAU categories with 2 level
full_scores = full_scores %>% 
  select(-eau_risk_category) %>% 
  rename(eau_risk_category = eau_condensed)

#create one-hot encoded matrix for the 'subtype' column
subtype_matrix <- model.matrix(~ subtype - 1, data = full_scores)

#convert the matrix to a data frame and bind it to the original data
full_scores <- cbind(full_scores, as.data.frame(subtype_matrix))

#save data
save(full_scores, file = "../eau__predictor/data/full_scores.Rdata")
