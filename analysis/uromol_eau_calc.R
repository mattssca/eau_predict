#read uromola ge supp.
uromol_age = read.csv("data/uromol/age_id.txt", sep = '\t')

#format the uromol age object
uromol_age = uromol_age %>% 
  rename(sample_id = UROMOL.ID, age = Age)

#load uromol metadata
load("C:/Users/matts/Desktop/eau_predict/data/uromol/meta_uromol.Rdata")

#subset to columns of interest
meta_uromol = meta_uromol %>%
  rename(sample_id = UROMOL.ID) %>% 
  select(sample_id, Tumor.stage, Concomitant.CIS, Tumor.size, Tumor.grade) %>% 
  rename(tumor_stage = Tumor.stage, cis = Concomitant.CIS, tumor_size = Tumor.size, tumor_grade = Tumor.grade) %>% 
  left_join(uromol_age, by = "sample_id")

#tmp n tumors
meta_uromol$tumor_n = "single"

#remove NAs
meta_uromol = meta_uromol %>% 
  drop_na()

#calcualte score
meta_uromol$eau_score <- with(meta_uromol, 
                              (ifelse(age > 70, 55, 0)) + 
                              (ifelse(tumor_n == "multiple", 55, 0)) + 
                              (ifelse(tumor_size == ">= 3 cm", 65, 0)) + 
                              (ifelse(cis == "Yes", 100, 0)) + 
                              (ifelse(tumor_grade == "High grade", 85, 0)))

#assign risk category
meta_uromol$eau_risk_category <- with(meta_uromol, 
                                      ifelse(eau_score <= 80, "Low risk",
                                      ifelse(eau_score <= 150, "Intermediate risk",
                                      ifelse(eau_score <= 305, "High risk", "Very high risk"))))

#identify samples 50 points within being another risk category
meta_uromol$near_risk_boundary <- with(meta_uromol, 
                                       ifelse(
                                         (eau_score >= 30 & eau_score <= 80) |  
                                           (eau_score >= 135 & eau_score <= 150) |  
                                           (eau_score >= 255 & eau_score <= 305) |  
                                           (eau_score >= 315 & eau_score <= 365),
                                         "Yes", 
                                         "No"))

#filter out samples that are intermediate and close to tipping point
meta_uromol_filtered <- meta_uromol %>%
  filter(!(eau_risk_category == "Intermediate risk" & near_risk_boundary == "Yes"))

#adjust risk category into two levels
uromol_eau <- meta_uromol_filtered %>%
  mutate(eau_category = ifelse(eau_risk_category %in% c("Low risk", "Intermediate risk"), "Low risk", eau_risk_category))

#subset to columns of interest
uromol_eau = uromol_eau %>% 
  select(sample_id, eau_category) %>% 
  rename(eau_risk_category = eau_category)

#save object
save(uromol_eau, file = "data/uromol/uromol_eau_adjsuted.Rdata")
