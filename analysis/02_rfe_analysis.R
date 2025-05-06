#load packages
library(caret) 
library(randomForest)  

#perform Recursive Feature Elimination (RFE)
set.seed(42)

#ensure the target variable is a factor
full_scores$eau_risk_category <- as.factor(full_scores$eau_risk_category)

#subset to numeric columns
pred_cols <- full_scores[, sapply(full_scores, is.numeric)]

#define the control for RFE
control <- rfeControl(functions = rfFuncs,
                      method = "cv",
                      number = 10)

#perform RFE
rfe_results <- rfe(pred_cols,
                   full_scores$eau_risk_category,
                   sizes = c(5, 10, 15, 20, 25, 30, 35, 40),
                   rfeControl = control)

#view results
print(rfe_results)

#plot the results
plot(rfe_results)

#extract the unique top 25 variables based on the highest Overall scores
unique_top_variables <- rfe_results$variables %>%
  filter(Variables == 20) %>%
  group_by(var) %>%
  summarize(Overall = mean(Overall, na.rm = TRUE)) %>%
  arrange(desc(Overall)) %>%
  slice_head(n = 20)

#print the unique top 38 variables
print(unique_top_variables$var)
