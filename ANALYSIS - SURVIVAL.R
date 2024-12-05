################ Linked CPES analysis - survival ###################

#Analysis of 1 and 5 year survival by sexuality and language status using linked 
#CPES-registry data

#Created January 2024 by Lizzie Augarde 
############################################################################# 

library(survival)
library(gtsummary)
library(ggplot2)
library(survminer)
library(tidyverse)
library(rsample)
library(tidymodels)
library(yardstick)
library(car)

load("surv_1yr_data.RData")
load("surv_5yr_data.RData")

#data prep
surv_1yr_data <- surv_1yr_data |>
  select(DIAGNOSISDATEBEST, DEATHDATEBEST, daystodeath, status_1yr, sexuality, sexuality_bin, lang_stat) |>
  ungroup() |>
  mutate(daystodeath = ifelse(is.na(daystodeath), 366, 
                              ifelse(daystodeath > 366, 366, daystodeath)))

surv_5yr_data <- surv_5yr_data |>
  select(DIAGNOSISDATEBEST, DEATHDATEBEST, daystodeath, status_5yr, sexuality, sexuality_bin, lang_stat) |>
  ungroup() |>
  mutate(daystodeath = ifelse(is.na(daystodeath), 1826, 
                              ifelse(daystodeath > 1825, 1826, daystodeath)))


######## 1 year separate population analyses ########
#overall survival Kaplan-Meier, not by sexuality or lang
surv_1yr <- Surv(surv_1yr_data$daystodeath, surv_1yr_data$status_1yr)
s1 <- survfit(Surv(daystodeath, status_1yr) ~ 1, data = surv_1yr_data)
str(s1)
summ_1yr <- survfit(Surv(daystodeath, status_1yr) ~ 1, data = surv_1yr_data)
summary(summ_1yr, times = 365) #overall 1yr survival probability 98.5% CI 98.5-98.6

#sexual minority pop
surv_1yr_data_sm <- surv_1yr_data |> filter(sexuality_bin == "Sexual minority")
summ_1yr_sm <- survfit(Surv(daystodeath, status_1yr) ~ 1, data = surv_1yr_data_sm)
summary(summ_1yr_sm, times = 365)
#98.8% 1yr survival probability for sexual minority, CI 98.5-99.2 

#heterosexaul pop
surv_1yr_data_het <- surv_1yr_data |> filter(sexuality_bin == "Heterosexual")
summ_1yr_het <- survfit(Surv(daystodeath, status_1yr) ~ 1, data = surv_1yr_data_het)
summary(summ_1yr_het, times = 365)
#98.5% 1yr survival probability for heterosexual, CI 98.5-98.6 

#plot KM curves for the two pops
surv_fits <- list(Group_A = summ_1yr_het, Group_B = summ_1yr_sm)

# Plot the Kaplan-Meier curves ######## this is weird, maybe redo but possibly not needed
ggsurvplot(surv_fits, combine = TRUE, 
           legend.title = "Group", 
           legend.labs = c("Group A", "Group B"),
           xlab = "Time (days)",
           ylab = "Survival Probability",
           ggtheme = theme_minimal())

#NNES pop
surv_1yr_data_nnes <- surv_1yr_data |> filter(lang_stat == "Non-native English speaker")
summ_1yr_nnes <- survfit(Surv(daystodeath, status_1yr) ~ 1, data = surv_1yr_data_nnes)
summary(summ_1yr_nnes, times = 365)
#99.1% 1yr survival probability for NNES, CI 99.0-99.3  

#NES pop
surv_1yr_data_nes <- surv_1yr_data |> filter(lang_stat == "Native English speaker")
summ_1yr_nes <- survfit(Surv(daystodeath, status_1yr) ~ 1, data = surv_1yr_data_nes)
summary(summ_1yr_nes, times = 365)
#98.5% 1yr survival probability for NES, CI 98.5-98.6  

#plot KM curves for the two pops
surv_fits <- list(Group_A = summ_1yr_nes, Group_B = summ_1yr_nnes)

# Plot the Kaplan-Meier curves ######## this is weird, maybe redo but possibly not needed
ggsurvplot(surv_fits, combine = TRUE, 
           legend.title = "Group", 
           legend.labs = c("Group A", "Group B"),
           xlab = "Time (days)",
           ylab = "Survival Probability",
           ggtheme = theme_minimal())


######## 1 year comparative analyses ########
#sexuality 
surv_1yr_data_sex <- surv_1yr_data |> filter(sexuality_bin != "Missing") 

#difference by sexuality
survdiff(Surv(daystodeath, status_1yr) ~ sexuality_bin, data = surv_1yr_data_sex) #log rank comparison no difference chisq = 1.9, p = 0.2
survfitsex <- survfit(Surv(daystodeath, status_1yr) ~ sexuality_bin, data = surv_1yr_data_sex)
ggsurvplot(survfitsex, data = surv_1yr_data_sex, pval = TRUE, conf.int = TRUE, risk.table = TRUE,
           legend.title = "Sexuality", legend.labs = c("Heterosexual", "Sexual minority"),
           xlab = "Time (days)", ylab = "Survival probability",
           ggtheme = theme_minimal()) #not useful

#language
surv_1yr_data_lang <- surv_1yr_data |> filter(lang_stat != "Missing") 

#difference by language 
survdiff(Surv(daystodeath, status_1yr) ~ lang_stat, data = surv_1yr_data_lang) #log rank comparison true difference chisq = 25.1, p = <0.0001, surv prob lower for NES
survfitlang <- survfit(Surv(daystodeath, status_1yr) ~ lang_stat, data = surv_1yr_data_lang)
ggsurvplot(survfitlang, data = surv_1yr_data_lang, pval = TRUE, conf.int = TRUE, risk.table = TRUE,
           legend.title = "Sexuality", legend.labs = c("Native English-speaking", "Non-native English-speaking"),
           xlab = "Time (days)", ylab = "Survival probability",
           ggtheme = theme_minimal()) #need to adapt this 

#regression to account for age -----
surv_1yr_lang_reg <- surv_1yr_data |>
  select(PATIENTID, status_1yr, lang_stat, age_at_diag, IMD19_DECILE_LSOAS,SITE_ICD10_3CHAR, ETHNICITY, STAGE_BEST) |>
  ungroup() |>
  #making dependent variable binary
  mutate(status_1yr = factor(status_1yr)) |> 
  mutate(age_at_diag = as.numeric(age_at_diag)) |>
  
  #excluding observations with missing values for any variables
  filter(lang_stat != "Missing",
         !is.na(ETHNICITY), !is.na(IMD19_DECILE_LSOAS), !is.na(age_at_diag), 
         !is.na(SITE_ICD10_3CHAR), !is.na(STAGE_BEST) ) 

levels(surv_1yr_lang_reg$status_1yr) #checking if death is considered as the event, should be "1" "0"
surv_1yr_lang_reg$status_1yr <- relevel(surv_1yr_lang_reg$status_1yr, "1")

#splitting into training and testing sets
set.seed(123)
surv_1yr_lang_reg_split <- initial_split(surv_1yr_lang_reg)
train_data <- training(surv_1yr_lang_reg_split)
test_data <- testing(surv_1yr_lang_reg_split)

surv_1yr_lang <- 
  recipe(status_1yr ~ lang_stat + AGE + ETHNICITY + IMD19_DECILE_LSOAS + SITE_ICD10_3CHAR + STAGE_BEST, data = train_data) |>
  step_dummy(all_nominal_predictors())

#model
model1 <- logistic_reg() |>
  set_engine("glm") |>
  fit(status_1yr ~ lang_stat + AGE + ETHNICITY + IMD19_DECILE_LSOAS + SITE_ICD10_3CHAR + STAGE_BEST, data = train_data)

tidy(model1)

model1_spec <- logistic_reg() |>
  set_engine("glm") 

model1_wflow <- 
  workflow() |> 
  add_model(model1_spec) |> 
  add_recipe(surv_1yr_lang)

model1_fit <- 
  model1_wflow |> 
  fit(data = train_data)

model1_fit |> 
  extract_fit_parsnip() |> 
  tidy()

model1_aug <- augment(model1_fit, test_data)
model1_aug |> 
  roc_curve(truth = status_1yr, .pred_1) |> 
  autoplot() 

model1_aug |> roc_auc(truth = status_1yr, .pred_1)

#final model on full dataset 
model1 <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(status_1yr ~ lang_stat + AGE + ETHNICITY + IMD19_DECILE_LSOAS + SITE_ICD10_3CHAR + STAGE_BEST, data = surv_1yr_lang_reg)

model1_results <- tidy(model1) |>
  mutate(conf.low = exp(estimate - 1.96*std.error), 
         conf.high = exp(estimate + 1.96*std.error)) |>
  mutate(estimate = exp(estimate)) |>
  mutate(estimate = sprintf("%s", estimate),
         conf.low = sprintf("%s", conf.low),
         conf.high = sprintf("%s", conf.high),
         p.value = sprintf("%s", p.value)) |>
  
  #convert to probability 
  mutate(prob = (as.numeric(estimate) / (1+as.numeric(estimate)))*100) |>
  mutate(prob = sprintf("%s", prob))
#no association after accounting for age, p=0.3

######## 5 year separate population analyses ########
#overall survival Kaplan-Meier, not by sexuality or lang
surv_5yr <- Surv(surv_5yr_data$daystodeath, surv_5yr_data$status_5yr)
s5 <- survfit(Surv(daystodeath, status_5yr) ~ 1, data = surv_5yr_data)
str(s5)
summ_5yr <- survfit(Surv(daystodeath, status_5yr) ~ 1, data = surv_5yr_data)
summary(summ_5yr, times = 1825) #overall 5yr survival probability 78.3% CI 78-78.6

#sexual minority pop
surv_5yr_data_sm <- surv_5yr_data |> filter(sexuality_bin == "Sexual minority")
summ_5yr_sm <- survfit(Surv(daystodeath, status_5yr) ~ 1, data = surv_5yr_data_sm)
summary(summ_5yr_sm, times = 1825)
#80.8% 5yr survival probability for sexual minority, CI 78.7-82.9 

#heterosexual pop
surv_5yr_data_het <- surv_5yr_data |> filter(sexuality_bin == "Heterosexual")
summ_5yr_het <- survfit(Surv(daystodeath, status_5yr) ~ 1, data = surv_5yr_data_het)
summary(summ_5yr_het, times = 1825)
#78.4% 5yr survival probability for heterosexual, CI 78.1-78.7 

#plot KM curves for the two pops
surv_fits <- list(Group_A = summ_5yr_het, Group_B = summ_5yr_sm)

# Plot the Kaplan-Meier curves
ggsurvplot(surv_fits, combine = TRUE, 
           legend.title = "Sexuality", 
           legend.labs = c("Heterosexual", "Sexual minority"),
           xlab = "Time (days)",
           ylab = "Survival probability",
           ggtheme = theme_minimal()) 

#NNES pop
surv_5yr_data_nnes <- surv_5yr_data |> filter(lang_stat == "Non-native English speaker")
summ_5yr_nnes <- survfit(Surv(daystodeath, status_5yr) ~ 1, data = surv_5yr_data_nnes)
summary(summ_5yr_nnes, times = 1825)
#83.0% 5yr survival probability for NNES, CI 81.8-84.3  

#NES pop
surv_5yr_data_nes <- surv_5yr_data |> filter(lang_stat == "Native English speaker")
summ_5yr_nes <- survfit(Surv(daystodeath, status_5yr) ~ 1, data = surv_5yr_data_nes)
summary(summ_5yr_nes, times = 1825)
#78.0% 5yr survival probability for NES, CI 77.8-78.3

#plot KM curves for the two pops
surv_fits <- list(Group_A = summ_5yr_nes, Group_B = summ_5yr_nnes)

# Plot the Kaplan-Meier curves 
ggsurvplot(surv_fits, combine = TRUE, 
           legend.title = "Language status", 
           legend.labs = c("Native English-speaking", "Non-native English-speaking"),
           xlab = "Time (days)",
           ylab = "Survival probability",
           ggtheme = theme_minimal())


######## 5 year comparative analyses ########
#sexuality ----
surv_5yr_data_sex <- surv_5yr_data |> filter(sexuality_bin != "Missing") 

#difference by sexuality
survdiff(Surv(daystodeath, status_5yr) ~ sexuality_bin, data = surv_5yr_data_sex) #true difference chisq = 3.1, p = 0.08
survfitsex <- survfit(Surv(daystodeath, status_5yr) ~ sexuality_bin, data = surv_5yr_data_sex)
plot <- ggsurvplot(survfitsex, data = surv_5yr_data_sex, pval = TRUE, conf.int = TRUE, risk.table = TRUE,
           legend.title = "Sexuality", legend.labs = c("Heterosexual", "Sexual minority"),
           xlab = "Time (days)", ylab = "Survival probability",
           ggtheme = theme_minimal()) #this doesn't look great, not sure how to plot better, ylim doesn't work

#regression to account for age -----
surv_5yr_sex_reg <- surv_5yr_data |>
  select(PATIENTID, status_5yr, sexuality_bin, AGE, IMD19_DECILE_LSOAS,SITE_ICD10_3CHAR, ETHNICITY, STAGE_BEST) |>
  ungroup() |>
  #making dependent variable binary
  mutate(status_5yr = factor(status_5yr)) |> 
  mutate(AGE = as.numeric(AGE)) |>
  
  #excluding observations with missing values for any variables
  filter(sexuality_bin != "Missing",
         !is.na(ETHNICITY), !is.na(IMD19_DECILE_LSOAS), !is.na(AGE), 
         !is.na(SITE_ICD10_3CHAR), !is.na(STAGE_BEST) ) 

levels(surv_5yr_sex_reg$status_5yr) #checking if death is considered as the event, should be "1" "0"
surv_5yr_sex_reg$status_5yr <- relevel(surv_5yr_sex_reg$status_5yr, "1")

#splitting into training and testing sets
set.seed(123)
surv_5yr_sex_reg_split <- initial_split(surv_5yr_sex_reg)
train_data <- training(surv_5yr_sex_reg_split)
test_data <- testing(surv_5yr_sex_reg_split)

surv_5yr_sex <- 
  recipe(status_5yr ~ sexuality_bin + AGE + ETHNICITY + IMD19_DECILE_LSOAS + SITE_ICD10_3CHAR + STAGE_BEST, data = train_data) |>
  step_dummy(all_nominal_predictors())

#model
model1 <- logistic_reg() |>
  set_engine("glm") |>
  fit(status_5yr ~ sexuality_bin + AGE + ETHNICITY + IMD19_DECILE_LSOAS + SITE_ICD10_3CHAR + STAGE_BEST, data = train_data)

tidy(model1)

model1_spec <- logistic_reg() |>
  set_engine("glm") 

model1_wflow <- 
  workflow() |> 
  add_model(model1_spec) |> 
  add_recipe(surv_5yr_sex)

model1_fit <- 
  model1_wflow |> 
  fit(data = train_data)

model1_fit |> 
  extract_fit_parsnip() |> 
  tidy()

model1_aug <- augment(model1_fit, test_data)
model1_aug |> 
  roc_curve(truth = status_5yr, .pred_1) |> 
  autoplot() 

model1_aug |> roc_auc(truth = status_5yr, .pred_1)

#final model on full dataset 
model1 <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(status_5yr ~ sexuality_bin + AGE + ETHNICITY + IMD19_DECILE_LSOAS + SITE_ICD10_3CHAR + STAGE_BEST, data = surv_5yr_sex_reg)

model1_results <- tidy(model1) |>
  mutate(conf.low = exp(estimate - 1.96*std.error), 
         conf.high = exp(estimate + 1.96*std.error)) |>
  mutate(estimate = exp(estimate)) |>
  mutate(estimate = sprintf("%s", estimate),
         conf.low = sprintf("%s", conf.low),
         conf.high = sprintf("%s", conf.high),
         p.value = sprintf("%s", p.value)) |>
  
  #convert to probability 
  mutate(prob = (as.numeric(estimate) / (1+as.numeric(estimate)))*100) |>
  mutate(prob = sprintf("%s", prob))
#no association after accounting for age, p=0.8

#language------
surv_5yr_data_lang <- surv_5yr_data |> filter(lang_stat != "Missing") 

#difference by language 
survdiff(Surv(daystodeath, status_5yr) ~ lang_stat, data = surv_5yr_data_lang) #log rank comparison true difference chisq = 47.7, p = <0.0001, surv prob lower for NES
survfitlang <- survfit(Surv(daystodeath, status_5yr) ~ lang_stat, data = surv_5yr_data_lang)
ggsurvplot(survfitlang, data = surv_5yr_data_lang, pval = TRUE, conf.int = TRUE, risk.table = TRUE,
           legend.title = "Sexuality", legend.labs = c("Native English-speaking", "Non-native English-speaking"),
           xlab = "Time (days)", ylab = "Survival probability",
           ggtheme = theme_minimal()) #this doesn't look great, not sure how to plot better, ylim doesn't work

#regression to account for age -----
surv_5yr_lang_reg <- surv_5yr_data |>
  select(PATIENTID, status_5yr, lang_stat, AGE, IMD19_DECILE_LSOAS,SITE_ICD10_3CHAR, ETHNICITY, STAGE_BEST) |>
  ungroup() |>
  #making dependent variable binary
  mutate(status_5yr = factor(status_5yr)) |> 
  mutate(AGE = as.numeric(AGE)) |>
  
  #excluding observations with missing values for any variables
  filter(lang_stat != "Missing",
         !is.na(ETHNICITY), !is.na(IMD19_DECILE_LSOAS), !is.na(AGE), 
         !is.na(SITE_ICD10_3CHAR), !is.na(STAGE_BEST) ) 

levels(surv_5yr_lang_reg$status_5yr) #checking if death is considered as the event, should be "1" "0"
surv_5yr_lang_reg$status_5yr <- relevel(surv_5yr_lang_reg$status_5yr, "1")

#splitting into training and testing sets
set.seed(123)
surv_5yr_lang_reg_split <- initial_split(surv_5yr_lang_reg)
train_data <- training(surv_5yr_lang_reg_split)
test_data <- testing(surv_5yr_lang_reg_split)

surv_5yr_lang <- 
  recipe(status_5yr ~ lang_stat + AGE + ETHNICITY + IMD19_DECILE_LSOAS + SITE_ICD10_3CHAR + STAGE_BEST, data = train_data) |>
  step_dummy(all_nominal_predictors())

#model
model1 <- logistic_reg() |>
  set_engine("glm") |>
  fit(status_5yr ~ lang_stat + AGE + ETHNICITY + IMD19_DECILE_LSOAS + SITE_ICD10_3CHAR + STAGE_BEST, data = train_data)

tidy(model1)

model1_spec <- logistic_reg() |>
  set_engine("glm") 

model1_wflow <- 
  workflow() |> 
  add_model(model1_spec) |> 
  add_recipe(surv_5yr_lang)

model1_fit <- 
  model1_wflow |> 
  fit(data = train_data)

model1_fit |> 
  extract_fit_parsnip() |> 
  tidy()

model1_aug <- augment(model1_fit, test_data)
model1_aug |> 
  roc_curve(truth = status_5yr, .pred_1) |> 
  autoplot() 

model1_aug |> roc_auc(truth = status_5yr, .pred_1)

#final model on full dataset 
model1 <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(status_5yr ~ lang_stat + AGE + ETHNICITY + IMD19_DECILE_LSOAS + SITE_ICD10_3CHAR + STAGE_BEST, data = surv_5yr_lang_reg)

model1_results <- tidy(model1) |>
  mutate(conf.low = exp(estimate - 1.96*std.error), 
         conf.high = exp(estimate + 1.96*std.error)) |>
  mutate(estimate = exp(estimate)) |>
  mutate(estimate = sprintf("%s", estimate),
         conf.low = sprintf("%s", conf.low),
         conf.high = sprintf("%s", conf.high),
         p.value = sprintf("%s", p.value)) |>
  
  #convert to probability 
  mutate(prob = (as.numeric(estimate) / (1+as.numeric(estimate)))*100) |>
  mutate(prob = sprintf("%s", prob))

#no association after accounting for age, p=0.1