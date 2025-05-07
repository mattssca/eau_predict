# Recode the levels of eau_risk_category and convert to numeric
sub_scores$eau_risk_category <- as.numeric(factor(sub_scores$eau_risk_category, 
                                                  levels = c("Low risk", "Intermediate risk", "High risk", "Very high risk"), 
                                                  labels = c(1, 2, 3, 4)))

# View the updated data frame
head(sub_scores)

new_pred <- randomForest(
  eau_risk_category ~ ., type = "regression",
  data = sub_scores, 
  proximity = TRUE,
  ntree = 1000,  # Number of trees
  mtry = 2,     # Number of variables randomly sampled at each split
  importance = TRUE,  # Calculate variable importance
  keep.forest = TRUE  # Keep the forest for predictions
)


low_risk_samples = meta_df %>% 
  filter(eau_risk_category == "Low risk") %>% 
  pull(sample_id)

int_risk_samples = meta_df %>% 
  filter(eau_risk_category == "Intermediate risk") %>% 
  pull(sample_id)

high_risk_samples = meta_df %>% 
  filter(eau_risk_category == "High risk") %>% 
  pull(sample_id)

veryhigh_risk_samples = meta_df %>% 
  filter(eau_risk_category == "Very high risk") %>% 
  pull(sample_id)

low_out = as.data.frame(new_pred$predicted) %>% 
  rownames_to_column("sample_id") %>% 
  rename(pred = `new_pred$predicted`) %>% 
  filter(sample_id %in% low_risk_samples) %>% 
  mutate(eau_actual = "Low risk")

int_out = as.data.frame(new_pred$predicted) %>% 
  rownames_to_column("sample_id") %>% 
  rename(pred = `new_pred$predicted`) %>% 
  filter(sample_id %in% int_risk_samples) %>% 
  mutate(eau_actual = "Intermediate risk")

high_out = as.data.frame(new_pred$predicted) %>% 
  rownames_to_column("sample_id") %>% 
  rename(pred = `new_pred$predicted`) %>% 
  filter(sample_id %in% high_risk_samples) %>% 
  mutate(eau_actual = "High risk")

veryhigh_out = as.data.frame(new_pred$predicted) %>% 
  rownames_to_column("sample_id") %>% 
  rename(pred = `new_pred$predicted`) %>% 
  filter(sample_id %in% veryhigh_risk_samples) %>% 
  mutate(eau_actual = "Very high risk")

pred_cons = rbind(low_out, int_out, high_out, veryhigh_out)

pred_cons$eau_actual <- factor(pred_cons$eau_actual, levels = c("Low risk","Intermediate risk", "High risk", "Very high risk"))


ggplot(pred_cons, aes(x = eau_actual, y = pred, fill = eau_actual)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  labs(title = "Distribution of Pred Scores by EAU Actual Category",
       x = "EAU Actual Category",
       y = "Pred Score") +
  theme_minimal() +
  theme(legend.position = "none")
