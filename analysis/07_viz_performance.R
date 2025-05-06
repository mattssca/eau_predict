#load packages 
library(pROC) 
library(PRROC) 
library(ComplexHeatmap)

## roc curve
#extract the OOB probabilities for the "High risk" class
oob_probabilities <- eau_predictor$votes[, "High risk"]

#generate the OOB ROC curve for the "High risk" class
roc_oob <- roc(selected_data$eau_risk_category, oob_probabilities)

#extract the AUC value
auc_value <- auc(roc_oob)

#plot the OOB ROC curve
plot(roc_oob, col = "#C95792", lwd = 3, main = "OOB ROC Curve for High Risk")
legend(x = 0.6, y = 0.35, legend = paste("AUC =", round(auc_value, 3)), col = "#C95792", 
       lwd = 5,
       bty = "n", 
       cex = 0.7)


## predision / recall curve
#compute the PR curve for the OOB predictions
pr <- pr.curve( curve = TRUE, scores.class0 = oob_probabilities, weights.class0 = as.numeric(selected_data$eau_risk_category == "High risk"))

#plot the PR curve
plot(pr, main = "Precision-Recall Curve (OOB Predictions)", col = "#C95792", lwd = 3,)


####################################################################################################
## confusion matrix
#get OOB predicted classes
oob_predicted_classes <- predict(eau_predictor, type = "response")

#create a confusion matrix
confusion_matrix <- table(Predicted = oob_predicted_classes, Actual = selected_data$eau_risk_category)
print(confusion_matrix)

#convert confusion matrix to a data frame for plotting
confusion_df <- as.data.frame(as.table(confusion_matrix))

#plot the confusion matrix as a heatmap
ggplot(confusion_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "#C95792", high = "#EF9651") +
  labs(title = "Confusion Matrix (OOB Predictions)", x = "Predicted", y = "Actual") +
  theme_minimal()


####################################################################################################
## varaible importance plot
# Plot variable importance
varImpPlot(eau_predictor, main = "Variable Importance", n.var = 20)


####################################################################################################
## calibration curve
#create a calibration curve
calibration_data <- data.frame(
  Predicted = oob_probabilities,
  Actual = as.numeric(selected_data$eau_risk_category == "High risk")
)

#bin the predicted probabilities
calibration_data$Bin <- cut(calibration_data$Predicted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)

#compute mean predicted probability and actual proportion for each bin
calibration_summary <- calibration_data %>%
  group_by(Bin) %>%
  summarise(
    Mean_Predicted = mean(Predicted),
    Proportion_Actual = mean(Actual)
  )

#plot the calibration curve
ggplot(calibration_summary, aes(x = Mean_Predicted, y = Proportion_Actual)) +
  geom_line(color = "#C95792", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Calibration Curve (OOB Predictions)",
    x = "Mean Predicted Probability",
    y = "Observed Proportion"
  ) +
  theme_minimal()


####################################################################################################
## multidimensional scaling plot
#perform multidimensional scaling (MDS) on the proximity matrix
mds <- cmdscale(1 - eau_predictor$proximity, k = 2)

#create a data frame for plotting
mds_data <- data.frame(
  Dim1 = mds[, 1],
  Dim2 = mds[, 2],
  RiskCategory = selected_data$eau_risk_category
)

#plot the MDS results
ggplot(mds_data, aes(x = Dim1, y = Dim2, color = RiskCategory)) +
  geom_point(size = 3, alpha = 1) +
  scale_color_manual(values = c("High risk" = "#D76C82" , "Low risk" = "#7ED4AD")) +
  labs(
    title = "MDS Plot Based on Proximity Matrix",
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal()


####################################################################################################
## outlier detection
#compute outlier scores
outlier_scores <- outlier(eau_predictor)

#add outlier scores to the dataset
selected_data$outlier_score <- outlier_scores

#plot outlier scores
ggplot(selected_data, aes(x = seq_along(outlier_score), y = outlier_score)) +
  geom_bar(stat = "identity", fill = "#C95792") +
  labs(
    title = "Outlier Scores",
    x = "Sample Index",
    y = "Outlier Score"
  ) +
  theme_minimal()


####################################################################################################
## ranked plot with OOB values
#convert OOB probabilities to a data frame
oob_df <- data.frame(
  sample_id = names(oob_probabilities),
  Probability = oob_probabilities
)

subtypes_5_class = as.data.frame(pred_uroscanseq_high_low_fixed_grade_score$predictions_5classes) %>% 
  rename(subtype = `pred_uroscanseq_high_low_fixed_grade_score$predictions_5classes`) %>% 
  rownames_to_column("sample_id")

#merge with full_scores to include the subtype column
ranked_data <- oob_df %>%
  left_join(subtypes_5_class, by = "sample_id") %>%
  arrange(Probability) %>%
  mutate(Rank = row_number())

#plot the ranked probabilities with points colored by subtype
ggplot(ranked_data, aes(x = Rank, y = Probability, color = subtype)) +
  geom_point(size = 2, alpha = 1) +
  geom_segment(aes(x = Rank, xend = Rank, y = -0.05, yend = 0, color = subtype), size = 2) +  # Add bar segments
  scale_color_manual(values = lund_colors$lund_colors) +
  labs(
    title = "Ranked Plot of OOB Probabilities by Subtype",
    x = "Rank",
    y = "OOB Probability",
    color = "Subtype"
  ) +
  theme_minimal()


####################################################################################################
#more ranked plots
# Convert OOB probabilities to a data frame
oob_df <- data.frame(
  sample_id = names(oob_probabilities),
  Probability = oob_probabilities
)

extra_variables = full_scores %>% 
  select(sample_id, molecular_grade_who_2022, molecular_grade_who_1999, progression_risk)

meta_vars = UROSCANSEQMetadata2025_01_16 %>% 
  rownames_to_column("sample_id") %>% 
  select(sample_id, QC_removal, EAU_Risk_Score_Class_AdjustedOtherFactor, Stage, Grade_1_2_3_X)

extra_variables = extra_variables %>% 
  left_join(meta_vars, by = "sample_id")

# Merge with full_scores to include the subtype column
ranked_data <- oob_df %>%
  left_join(extra_variables, by = "sample_id") %>%
  arrange(Probability) %>%
  mutate(Rank = row_number())

#add in the predicted 2 class EAU category
eau_2_class = full_scores %>% 
  select(eau_risk_category, sample_id)

#add information to ranked data
ranked_data = ranked_data %>% 
  left_join(eau_2_class, by = "sample_id")

#add subtypes
subtypes_7_class = as.data.frame(pred_uroscanseq_high_low_fixed_grade_score$predictions_7classes) %>% 
  rownames_to_column("sample_id") %>% 
  rename(subtype_7_class = `pred_uroscanseq_high_low_fixed_grade_score$predictions_7classes`) %>% 
  select(sample_id, subtype_7_class)

subtypes_5_class = as.data.frame(pred_uroscanseq_high_low_fixed_grade_score$predictions_5classes) %>% 
  rownames_to_column("sample_id") %>% 
  rename(subtype_5_class = `pred_uroscanseq_high_low_fixed_grade_score$predictions_5classes`) %>% 
  select(sample_id, subtype_5_class)

ranked_data = ranked_data %>% 
  left_join(subtypes_5_class, by = "sample_id") %>% 
  left_join(subtypes_7_class, by = "sample_id")

ranked_data$progression_risk <- factor(ranked_data$progression_risk, levels = c("HR", "LR"))
ranked_data$QC_removal <- factor(ranked_data$QC_removal, levels = c("Bad_batches", "Low_Quality", "High_Quality"))
ranked_data$EAU_Risk_Score_Class_AdjustedOtherFactor <- factor(ranked_data$EAU_Risk_Score_Class_AdjustedOtherFactor, levels = c("Low risk", "Intermediate risk", "High risk", "Very high risk"))
ranked_data$Stage <- factor(ranked_data$Stage, levels = c("Ta", "T1"))
ranked_data$eau_risk_category <- factor(ranked_data$eau_risk_category, levels = c("Low risk", "High risk"))

####################################################################################################
#ranked molecular grade 2022
ggplot(ranked_data, aes(x = Rank, y = Probability, color = molecular_grade_who_2022)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("HG" = "#F0100C", "LG" = "#0C90F0")) +
  geom_segment(aes(x = Rank, xend = Rank, y = -0.05, yend = 0, color = molecular_grade_who_2022), size = 2) +  # Add bar segments
  labs(
    title = "Ranked Plot of OOB \nProbabilities by Mol. Grade 2022",
    x = "Rank",
    y = "OOB Probability",
    color = "Mol. Grade 2022 \n(LundTax)"
  ) +
  theme_minimal()

####################################################################################################
#ranked molecular grade 1999
ggplot(ranked_data, aes(x = Rank, y = Probability, color = molecular_grade_who_1999)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("G1_2" = "#0C90F0", "G3" = "#F0100C")) +
  geom_segment(aes(x = Rank, xend = Rank, y = -0.05, yend = 0, color = molecular_grade_who_1999), size = 2) +  # Add bar segments
  labs(
    title = "Ranked Plot of OOB \nProbabilities by Mol. Grade 1999",
    x = "Rank",
    y = "OOB Probability",
    color = "Mol. Grade 1999 \n(LundTax)"
  ) +
  theme_minimal()

####################################################################################################
#ranked progression risk
ggplot(ranked_data, aes(x = Rank, y = Probability, color = progression_risk)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("LR" = "#0C90F0", "HR" = "#F0100C")) +
  geom_segment(aes(x = Rank, xend = Rank, y = -0.05, yend = 0, color = progression_risk), size = 2) +  # Add bar segments
  labs(
    title = "Ranked Plot of OOB \nProbabilities by Progression Risk",
    x = "Rank",
    y = "OOB Probability",
    color = "Progression Risk \n(LundTax)"
  ) +
  theme_minimal()

####################################################################################################
#ranked QC removal
ggplot(ranked_data, aes(x = Rank, y = Probability, color = QC_removal)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("Bad_batches" = "#2A3335", "Low_Quality" = "#F0100C", "High_Quality" = "#72BF78")) +
  geom_segment(aes(x = Rank, xend = Rank, y = -0.05, yend = 0, color = QC_removal), size = 2) +  # Add bar segments
  labs(
    title = "Ranked Plot of OOB \nProbabilities by QC Removal",
    x = "Rank",
    y = "OOB Probability",
    color = "QC Group"
  ) +
  theme_minimal()

####################################################################################################
#ranked 4 class EAU
ggplot(ranked_data, aes(x = Rank, y = Probability, color = EAU_Risk_Score_Class_AdjustedOtherFactor)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("Low risk" = "#86D293", "Intermediate risk" = "#F3C623", "High risk" = "#FF6600", "Very high risk" = "#B8001F")) +
  geom_segment(aes(x = Rank, xend = Rank, y = -0.05, yend = 0, color = EAU_Risk_Score_Class_AdjustedOtherFactor), size = 2) +  # Add bar segments
  labs(
    title = "Ranked Plot of OOB \nProbabilities by EAU Risk Category",
    x = "Rank",
    y = "OOB Probability",
    color = "EAU Risk \nCategory"
  ) +
  theme_minimal()

#ranked 2 class EAU
ggplot(ranked_data, aes(x = Rank, y = Probability, color = eau_risk_category)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("Low risk" = "#86D293", "High risk" = "#B8001F")) +
  geom_segment(aes(x = Rank, xend = Rank, y = -0.05, yend = 0, color = eau_risk_category), size = 2) +  # Add bar segments
  labs(
    title = "Ranked Plot of OOB \nProbabilities by EAU Risk Category",
    x = "Rank",
    y = "OOB Probability",
    color = "EAU Risk \nCategory"
  ) +
  theme_minimal()

####################################################################################################
#ranked 4 class stage
ggplot(ranked_data, aes(x = Rank, y = Probability, color = Stage)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("Ta" = "#72B133", "T1" = "#547EB6")) +
  geom_segment(aes(x = Rank, xend = Rank, y = -0.05, yend = 0, color = Stage), size = 2) +  # Add bar segments
  labs(
    title = "Ranked Plot of OOB \nProbabilities by Stage",
    x = "Rank",
    y = "OOB Probability",
    color = "Stage \n(Patological)"
  ) +
  theme_minimal()

####################################################################################################
#ranked grade
ggplot(ranked_data, aes(x = Rank, y = Probability, color = Grade_1_2_3_X)) +
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(values = c("1" = "#0C90F0", "2" = "#EFEF0D", "3" = "#F0100C")) +
  geom_segment(aes(x = Rank, xend = Rank, y = -0.05, yend = 0, color = Grade_1_2_3_X), size = 2) +  # Add bar segments
  labs(
    title = "Ranked Plot of OOB \nProbabilities by Grade",
    x = "Rank",
    y = "OOB Probability",
    color = "Grade \n(Patological)"
  ) +
  theme_minimal()


####################################################################################################
## heatmap
#prepare the data
#extract the probability scores and order by rank
heatmap_data <- as.matrix(t(ranked_data$Probability))  # Transpose the data for vertical orientation
colnames(heatmap_data) <- ranked_data$sample_id

#create annotation tracks as column annotations
col_annotations <- HeatmapAnnotation(
  `Subtype 5 Class` = ranked_data$subtype_5_class,
  `Subtype 7 Class` = ranked_data$subtype_7_class,
  `Mol. Grade 2022 (LundTax)` = ranked_data$molecular_grade_who_2022,
  `Mol. Grade 1999 (LundTax)` = ranked_data$molecular_grade_who_1999,
  `Progression Risk (LundTax)` = ranked_data$progression_risk,
  `Stage (Pathological)` = ranked_data$Stage,
  `Grade (Pathological)` = ranked_data$Grade_1_2_3_X,
  `EAU Risk 4 Class` = ranked_data$EAU_Risk_Score_Class_AdjustedOtherFactor,
  `EAU Risk 2 Class` = ranked_data$eau_risk_category,
  `QC Removal` = ranked_data$QC_removal,
  col = list(
    `Subtype 5 Class` = c("Uro" = "#3cb44b", "GU" = "#4363d8", "BaSq" = "#CD2626", "Mes" = "#f58231", "ScNE" = "#A020F0"),
    `Subtype 7 Class` = c("UroA" = "#3cb44b", "UroB" = "#8B1A1A", "UroC" = "#006400", "GU" = "#4363d8", "BaSq" = "#CD2626", "Mes" = "#f58231", "ScNE" = "#A020F0"),
    `Mol. Grade 2022 (LundTax)` = c("LG" = "#0C90F0", "HG" = "#F0100C"),
    `Mol. Grade 1999 (LundTax)` = c("G1_2" = "#0C90F0", "G3" = "#F0100C"),
    `Progression Risk (LundTax)` = c("LR" = "#0C90F0", "HR" = "#F0100C"),
    `Stage (Pathological)` = c("Ta" = "#72B133", "T1" = "#547EB6"),
    `Grade (Pathological)` = c("1" = "#0C90F0", "2" = "#EFEF0D", "3" = "#F0100C"),
    `EAU Risk 4 Class` = c("Low risk" = "#86D293", "Intermediate risk" = "#F3C623", "High risk" = "#FF6600", "Very high risk" = "#B8001F"),
    `EAU Risk 2 Class` = c("Low risk" = "#86D293", "High risk" = "#B8001F"),
    `QC Removal` = c("Bad_batches" = "#2A3335", "High_Quality" = "#72BF78", "Low_Quality" = "#F0100C")
  ),
  annotation_name_side = "left"
)

#create the heatmap
ht = Heatmap(
  heatmap_data,
  name = "OOB Probability",
  row_title = "OOB Probability",
  row_title_gp = gpar(fontsize = 12, fontface = "bold"),
  row_names_gp = gpar(fontsize = 10),
  row_title_rot = 0,
  heatmap_legend_param = list(title = "OOB Probability", title_gp = gpar(fontsize = 12)),
  col = colorRamp2(c(0, 1), c("#183B4E", "#72BF78")),
  height = unit(2, "cm"),
  column_title = "OOB Probability for EAU 2 Class Prediction",
  show_column_names = FALSE,
  cluster_columns = FALSE, show_heatmap_legend = FALSE,
  cluster_rows = FALSE,
  top_annotation = col_annotations
)

#draw the heatmap
draw(ht, heatmap_legend_side = "right", annotation_legend_side = "right")



