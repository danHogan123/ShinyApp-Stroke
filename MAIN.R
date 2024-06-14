library(readr)
library(VIM)
library(tidyverse)

strokeDataSet <- read.csv("healthcare-dataset-stroke-data.csv", na.strings = c("N/A"))


df_for_imputation <- strokeDataSet

# Perform KNN imputation
df_imputed <- kNN(df_for_imputation, variable = "bmi", k = 5, imp_var = FALSE)

# Replace only the NA values in the original bmi column with the imputed values
strokeDataSet$bmi[is.na(strokeDataSet$bmi)] <- df_imputed$bmi[is.na(strokeDataSet$bmi)]
strokeDataSet <- df_imputed
strokeDataSet <- strokeDataSet %>% 
  filter(gender != "Other") %>% # Removing the 1 other
  filter(age >= 18) %>%  # 18+
  mutate(obesity_level = case_when(
    bmi < 18.5 ~ "underweight",
    between(bmi, 18.5, 24.9) ~ "healthy_weight",
    between(bmi, 25, 29.9) ~ "overweight",
    bmi >= 30 ~ "obese",)) %>% # Turning BMI's into weight categories
  mutate(across(c(smoking_status, work_type, ever_married,
                  Residence_type, gender, obesity_level), factor)) %>% # Turning into factors
  select(-id, -bmi) # removing stuff


df_nw <- strokeDataSet %>% filter(work_type == "Never_worked") # 4 people

mod1 <- glm(data = strokeDataSet, stroke ~ ., family = binomial)
summary(mod1)
