#https://www.tidymodels.org/start/recipes/#recipe
#https://argoshare.is.ed.ac.uk/healthyr_book/fitting-logistic-regression-models-in-base-r.html
#https://www.r-bloggers.com/2023/04/a-tidymodels-tutorial/#google_vignette
#https://www.thomasvanhoey.com/post/2021-10-12-tidymodels-interactions/
#https://www.datacamp.com/tutorial/logistic-regression-R

library(rsample)
library(tidymodels)
library(yardstick)
library(car)
library(MASS)
library(rms)

load("stage_data.RData")
load("rtd_data.RData")

######### SEXUALITY AND STAGE AT DIAGNOSIS #########
#dataset prep
stage_data_sexuality_reg <- stage_data |>
  #making dependent variable binary
  filter(STAGE_2LEVEL != "Not available") |>
  mutate(STAGE_2LEVEL = factor(ifelse(STAGE_2LEVEL == "Late", 1, 0))) |> 
  mutate(AGE = as.numeric(AGE)) |>
  
  #excluding observations with missing values for any variables
  filter(sexuality_bin != "Missing",
         !is.na(ETHNICITY), !is.na(IMD19_DECILE_LSOAS), !is.na(AGE), 
         !is.na(SITE_ICD10_3CHAR))  

levels(stage_data_sexuality_reg$STAGE_2LEVEL) #checking if late stage is considered as the reference/event, should be "1" "0"

#splitting into training and testing sets
set.seed(123)
stage_data_sexuality_reg_split <- initial_split(stage_data_sexuality_reg, prop = 0.75)
train_data <- training(stage_data_sexuality_reg_split)
test_data <- testing(stage_data_sexuality_reg_split)

stage_sexuality <- 
  recipe(STAGE_2LEVEL ~ sexuality_bin + age_10yr_band + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = train_data) |>
  step_dummy(all_nominal_predictors()) 

#model
model1 <- logistic_reg() |>
  set_engine("glm") |>
  fit(STAGE_2LEVEL ~ sexuality_bin + age_10yr_band + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = train_data)

tidy(model1)

model1_spec <- logistic_reg() |>
  set_engine("glm") 

model1_wflow <- 
  workflow() |> 
  add_model(model1_spec) |> 
  add_recipe(stage_sexuality)

model1_fit <- 
  model1_wflow |> 
  fit(data = train_data)

model1_fit |> 
  extract_fit_parsnip() |> 
  tidy()

model1_aug <- augment(model1_fit, test_data)
model1_aug |> 
  roc_curve(truth = STAGE_2LEVEL, .pred_1) |> 
  autoplot() 

model1_aug |> roc_auc(truth = STAGE_2LEVEL, .pred_1)

#final model on full dataset 
model1 <- logistic_reg() |>
  set_engine("glm") |>
  fit(STAGE_2LEVEL ~ sexuality_bin + age_10yr_band + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = stage_data_sexuality_reg)

model1_results <- tidy(model1) |>
  mutate(conf.low = exp(estimate - 1.96*std.error), 
         conf.high = exp(estimate + 1.96*std.error)) |>
  mutate(estimate = exp(estimate)) |>
  mutate(estimate = sprintf("%s", estimate),
         conf.low = sprintf("%s", conf.low),
         p.value = sprintf("%s", p.value),
         conf.high = sprintf("%s", conf.high))

#checking multicollinearity in categorical variables
model1_mc <- glm(STAGE_2LEVEL ~ sexuality_bin + age_binary + IMD19_DECILE_LSOAS, data = stage_data_sexuality_reg, family = binomial)
class(model1_mc)
vif(model1_mc) ##no significant multicollinearity


######### LANGUAGE STATUS AND STAGE AT DIAGNOSIS #########
#dataset prep
stage_data_language_reg <- stage_data |>
  #making dependent variable binary
  filter(STAGE_2LEVEL != "Not available") |>
  mutate(STAGE_2LEVEL = factor(ifelse(STAGE_2LEVEL == "Late", 1, 0))) |> 
  mutate(AGE = as.numeric(AGE)) |>
  
  #excluding observations with missing values for any variables
  filter(lang_stat != "Missing",
         !is.na(ETHNICITY), !is.na(IMD19_DECILE_LSOAS), !is.na(AGE), 
         !is.na(SITE_ICD10_3CHAR)) 

levels(stage_data_language_reg$STAGE_2LEVEL) #checking if late stage is considered as the event, should be "1" "0"

#splitting into training and testing sets
set.seed(123)
stage_data_language_reg_split <- initial_split(stage_data_language_reg)
train_data <- training(stage_data_language_reg_split)
test_data <- testing(stage_data_language_reg_split)

stage_language <- 
  recipe(STAGE_2LEVEL ~ lang_stat + age_10yr_band + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = train_data) |>
  step_dummy(all_nominal_predictors())

#model
model2 <- logistic_reg() |>
  set_engine("glm") |>
  fit(STAGE_2LEVEL ~ lang_stat + age_10yr_band + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = train_data)

tidy(model2)

model2_spec <- logistic_reg() |>
  set_engine("glm") 

model2_wflow <- 
  workflow() |> 
  add_model(model2_spec) |> 
  add_recipe(stage_language)

model2_fit <- 
  model2_wflow |> 
  fit(data = train_data)

model2_fit |> 
  extract_fit_parsnip() |> 
  tidy()

model2_aug <- augment(model2_fit, test_data)
model2_aug |> 
  roc_curve(truth = STAGE_2LEVEL, .pred_1) |> 
  autoplot() 

model2_aug |> roc_auc(truth = STAGE_2LEVEL, .pred_1)

#final model on full dataset 
model2 <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(STAGE_2LEVEL ~ lang_stat + age_binary + ETHNICITY + IMD19_DECILE_LSOAS, data = stage_data_language_reg)

model2_results <- tidy(model2) |>
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

#checking multicollinearity in categorical variables
model2_mc <- glm(STAGE_2LEVEL ~ lang_stat + age_binary + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = stage_data_language_reg, family = binomial)
class(model2_mc)
vif(model2_mc)

#checking association between IMD and age
table_imd_age <- table(stage_data_language_reg$age_10yr_band, stage_data_language_reg$IMD19_DECILE_LSOAS)
chisq.test(table_imd_age) #p<0.05, strong evidence for association between IMD and age 
model_full <- glm(STAGE_2LEVEL ~ lang_stat + age_10yr_band + IMD19_DECILE_LSOAS, data = stage_data_language_reg, family = binomial)
model_red <- glm(STAGE_2LEVEL ~ lang_stat + age_10yr_band, data = stage_data_language_reg, family = binomial)
lr_test <- anova(model_red, model_full, test = "LRT")
print(lr_test) #adding IMD significantly improves model 
vif_model <- glm(STAGE_2LEVEL ~ lang_stat + age_10yr_band + IMD19_DECILE_LSOAS, data = stage_data_language_reg, family = binomial)
vif(vif_model) #no evidence of multicollinearity 

model_interaction <- glm(STAGE_2LEVEL ~ lang_stat * age_10yr_band + lang_stat * IMD19_DECILE_LSOAS,
                         data = stage_data_language_reg,
                         family = binomial)
summary(model_interaction)

#checking full model 
stage_lang_reg_clean <- stage_data_language_reg[complete.cases(stage_data_language_reg[, c("lang_stat", "age_10yr_band", "ETHNICITY", "IMD19_DECILE_LSOAS", "NDRS_MAIN")]), ]
model_full <- glm(STAGE_2LEVEL ~ lang_stat + age_10yr_band + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = stage_lang_reg_clean, family = binomial)
model_red <- glm(STAGE_2LEVEL ~ lang_stat + age_10yr_band, data = stage_lang_reg_clean, family = binomial)
lr_test <- anova(model_red, model_full, test = "LRT")
print(lr_test) #did this stepwise, adding each variable significantly improves the model

model_interaction <- glm(STAGE_2LEVEL ~ lang_stat * age_10yr_band + lang_stat * IMD19_DECILE_LSOAS
                         + lang_stat * ETHNICITY + lang_stat * NDRS_MAIN,
                         data = stage_data_language_reg,
                         family = binomial)
summary(model_interaction) #high level of interaction, little evidence for language status influencing independently 



######### SEXUALITY AND EMERGENCY ROUTE TO DIAGNOSIS #########
#dataset prep
rtd_ep_sexuality_reg <- rtd_data |>
  #making dependent variable binary
  filter(!FINAL_ROUTE %in% c("Missing", "Unknown")) |>
  mutate(FINAL_ROUTE = factor(ifelse(FINAL_ROUTE == "Emergency presentation", 1, 0))) |> 
  mutate(AGE = as.numeric(AGE)) |>
  
  #excluding observations with missing values for any variables
  filter(sexuality_bin != "Missing",
         !is.na(ETHNICITY), !is.na(IMD19_DECILE_LSOAS), !is.na(AGE), 
         !is.na(SITE_ICD10_3CHAR)) 

levels(rtd_ep_sexuality_reg$FINAL_ROUTE) #checking if late stage is considered as the event, should be "1" "0"
rtd_ep_sexuality_reg$FINAL_ROUTE <- relevel(rtd_ep_sexuality_reg$FINAL_ROUTE, "1")

#splitting into training and testing sets
set.seed(123)
rtd_ep_sexuality_reg_split <- initial_split(rtd_ep_sexuality_reg)
train_data <- training(rtd_ep_sexuality_reg_split)
test_data <- testing(rtd_ep_sexuality_reg_split)

rtd_ep_sexuality <- 
  recipe(FINAL_ROUTE ~ sexuality_bin + age_10yr_band + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = train_data) |>
  step_dummy(all_nominal_predictors())

#model
model3 <- logistic_reg() |>
  set_engine("glm") |>
  fit(FINAL_ROUTE ~ sexuality_bin + age_10yr_band + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = train_data)

tidy(model3)

model3_spec <- logistic_reg() |>
  set_engine("glm") 

model3_wflow <- 
  workflow() |> 
  add_model(model3_spec) |> 
  add_recipe(rtd_ep_sexuality)

model3_fit <- 
  model3_wflow |> 
  fit(data = train_data)

model3_fit |> 
  extract_fit_parsnip() |> 
  tidy()

model3_aug <- augment(model3_fit, test_data)
model3_aug |> 
  roc_curve(truth = FINAL_ROUTE, .pred_1) |> 
  autoplot() 

model3_aug |> roc_auc(truth = FINAL_ROUTE, .pred_1)

#final model on full dataset 
model3 <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(FINAL_ROUTE ~ sexuality_bin + age_10yr_band + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = rtd_ep_sexuality_reg)

model3_results <- tidy(model3) |>
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

#checking multicollinearity in categorical variables
model3_mc <- glm(FINAL_ROUTE ~ lang_stat + age_10yr_band + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = rtd_ep_sexuality_reg, family = binomial)
class(model2_mc)
rms::vif(model2_mc)


######### SEXUALITY AND SCREENING ROUTE TO DIAGNOSIS #########
#dataset prep
rtd_sc_sexuality_reg <- rtd_data |>
  #making dependent variable binary
  filter(!FINAL_ROUTE %in% c("Missing", "Unknown")) |>
  mutate(FINAL_ROUTE = factor(ifelse(FINAL_ROUTE == "Screening", 1, 0))) |> 
  mutate(AGE = as.numeric(AGE)) |>
  
  #excluding observations with missing values for any variables
  filter(sexuality_bin != "Missing",
         !is.na(ETHNICITY), !is.na(IMD19_DECILE_LSOAS), !is.na(AGE), 
         !is.na(SITE_ICD10_3CHAR)) 

levels(rtd_sc_sexuality_reg$FINAL_ROUTE) #checking if late stage is considered as the event, should be "1" "0"
rtd_sc_sexuality_reg$FINAL_ROUTE <- relevel(rtd_sc_sexuality_reg$FINAL_ROUTE, "1")

#splitting into training and testing sets
set.seed(123)
rtd_sc_sexuality_reg_split <- initial_split(rtd_sc_sexuality_reg)
train_data <- training(rtd_sc_sexuality_reg_split)
test_data <- testing(rtd_sc_sexuality_reg_split)

rtd_sc_sexuality <- 
  recipe(FINAL_ROUTE ~ sexuality_bin + age_binary + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = train_data) |>
  step_dummy(all_nominal_predictors())

#model
model4 <- logistic_reg() |>
  set_engine("glm") |>
  fit(FINAL_ROUTE ~ sexuality_bin + age_binary + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = train_data)

tidy(model4)

model4_spec <- logistic_reg() |>
  set_engine("glm") 

model4_wflow <- 
  workflow() |> 
  add_model(model4_spec) |> 
  add_recipe(rtd_sc_sexuality)

model4_fit <- 
  model4_wflow |> 
  fit(data = train_data)

model4_fit |> 
  extract_fit_parsnip() |> 
  tidy()

model4_aug <- augment(model4_fit, test_data)
model4_aug |> 
  roc_curve(truth = FINAL_ROUTE, .pred_1) |> 
  autoplot() 

model4_aug |> roc_auc(truth = FINAL_ROUTE, .pred_1)

#final model on full dataset 
model4 <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(FINAL_ROUTE ~ sexuality_bin + age_binary + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = rtd_sc_sexuality_reg)

model4_results <- tidy(model4) |>
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

#checking all confounders model
table_age_imd <- table(rtd_sc_sexuality_reg$age_10yr_band, rtd_sc_sexuality_reg$IMD19_DECILE_LSOAS)
chisq.test(table_age_imd) #p<0.05, strong evidence for association between age and ethnicity

rtd_sc_sex_reg_clean <- rtd_sc_sexuality_reg[complete.cases(rtd_sc_sexuality_reg[, c("lang_stat", "age_binary", "age_10yr_band", "ETHNICITY", "IMD19_DECILE_LSOAS", "NDRS_MAIN")]), ]
model_full <- glm(FINAL_ROUTE ~ sexuality_bin + age_binary + ETHNICITY, data = rtd_sc_sex_reg_clean, family = binomial)
model_red <- glm(FINAL_ROUTE ~ sexuality_bin + age_binary, data = rtd_sc_sex_reg_clean, family = binomial)
lr_test <- anova(model_red, model_full, test = "LRT")
print(lr_test) #adding each variable significantly improves model 
vif_model <- glm(FINAL_ROUTE ~ sexuality_bin + age_binary + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = rtd_sc_sexuality_reg, family = binomial)
vif(vif_model) #no evidence of multicollinearity 

model_interaction <- glm(FINAL_ROUTE ~ sexuality_bin * age_binary + sexuality_bin * ETHNICITY
                         + sexuality_bin * IMD19_DECILE_LSOAS + sexuality_bin * NDRS_MAIN,
                         data = rtd_sc_sexuality_reg,
                         family = binomial)
summary(model_interaction)


######### LANGUAGE STATUS AND EMERGENCY ROUTE TO DIAGNOSIS #########
#dataset prep
rtd_ep_language_reg <- rtd_data |>
  #making dependent variable binary
  filter(!FINAL_ROUTE %in% c("Missing", "Unknown")) |>
  mutate(FINAL_ROUTE = factor(ifelse(FINAL_ROUTE == "Emergency presentation", 1, 0))) |> 
  mutate(AGE = as.numeric(AGE)) |>
  
  #excluding observations with missing values for any variables
  filter(lang_stat != "Missing",
         !is.na(ETHNICITY), !is.na(IMD19_DECILE_LSOAS), !is.na(AGE), 
         !is.na(SITE_ICD10_3CHAR)) 

levels(rtd_ep_language_reg$FINAL_ROUTE) #checking if late stage is considered as the event, should be "1" "0"
rtd_ep_language_reg$FINAL_ROUTE <- relevel(rtd_ep_language_reg$FINAL_ROUTE, "1")

#splitting into training and testing sets
set.seed(123)
rtd_ep_language_reg_split <- initial_split(rtd_ep_language_reg)
train_data <- training(rtd_ep_language_reg_split)
test_data <- testing(rtd_ep_language_reg_split)

rtd_ep_language <- 
  recipe(FINAL_ROUTE ~ lang_stat + ETHNICITY, data = train_data) |>
  step_dummy(all_nominal_predictors())

#model
model5 <- logistic_reg() |>
  set_engine("glm") |>
  fit(FINAL_ROUTE ~ lang_stat + ETHNICITY, data = train_data)

tidy(model5)

model5_spec <- logistic_reg() |>
  set_engine("glm") 

model5_wflow <- 
  workflow() |> 
  add_model(model5_spec) |> 
  add_recipe(rtd_ep_sexuality)

model5_fit <- 
  model5_wflow |> 
  fit(data = train_data)

model5_fit |> 
  extract_fit_parsnip() |> 
  tidy()

model5_aug <- augment(model5_fit, test_data)
model5_aug |> 
  roc_curve(truth = FINAL_ROUTE, .pred_1) |> 
  autoplot() 

model5_aug |> roc_auc(truth = FINAL_ROUTE, .pred_1)

#final model on full dataset 
model5 <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(FINAL_ROUTE ~ lang_stat + age_binary + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = rtd_ep_language_reg)

model5_results <- tidy(model5) |>
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


######### LANGUAGE STATUS AND SCREENING ROUTE TO DIAGNOSIS #########
#dataset prep
rtd_sc_language_reg <- rtd_data |>
  #making dependent variable binary
  filter(!FINAL_ROUTE %in% c("Missing", "Unknown")) |>
  mutate(FINAL_ROUTE = factor(ifelse(FINAL_ROUTE == "Screening", 1, 0))) |> 
  mutate(AGE = as.numeric(AGE)) |>
  
  #excluding observations with missing values for any variables
  filter(lang_stat != "Missing",
         !is.na(ETHNICITY), !is.na(IMD19_DECILE_LSOAS), !is.na(AGE), 
         !is.na(SITE_ICD10_3CHAR)) 

levels(rtd_sc_language_reg$FINAL_ROUTE) #checking if late stage is considered as the event, should be "1" "0"
rtd_sc_language_reg$FINAL_ROUTE <- relevel(rtd_sc_language_reg$FINAL_ROUTE, "1")

#splitting into training and testing sets
set.seed(123)
rtd_sc_language_reg_split <- initial_split(rtd_sc_language_reg)
train_data <- training(rtd_sc_language_reg_split)
test_data <- testing(rtd_sc_language_reg_split)

rtd_sc_language <- 
  recipe(FINAL_ROUTE ~ lang_stat + ETHNICITY, data = train_data) |>
  step_dummy(all_nominal_predictors())

#model
model6 <- logistic_reg() |>
  set_engine("glm") |>
  fit(FINAL_ROUTE ~ lang_stat + ETHNICITY, data = train_data)

tidy(model6)

model6_spec <- logistic_reg() |>
  set_engine("glm") 

model6_wflow <- 
  workflow() |> 
  add_model(model6_spec) |> 
  add_recipe(rtd_sc_language)

model6_fit <- 
  model6_wflow |> 
  fit(data = train_data)

model6_fit |> 
  extract_fit_parsnip() |> 
  tidy()

model6_aug <- augment(model6_fit, test_data)
model6_aug |> 
  roc_curve(truth = FINAL_ROUTE, .pred_1) |> 
  autoplot() 

model6_aug |> roc_auc(truth = FINAL_ROUTE, .pred_1)

#final model on full dataset 
model6 <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(FINAL_ROUTE ~ lang_stat + ETHNICITY + IMD19_DECILE_LSOAS, data = rtd_sc_language_reg)

model6_results <- tidy(model6) |>
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

#checking all confounders model
table_eth_imd <- table(rtd_sc_language_reg$ETHNICITY, rtd_sc_language_reg$IMD19_DECILE_LSOAS)
chisq.test(table_eth_imd) #p<0.05, strong evidence for association between IMD and ethnicity

model_full <- glm(FINAL_ROUTE ~ sexuality_bin + IMD19_DECILE_LSOAS + ETHNICITY, data = rtd_sc_language_reg, family = binomial)
model_red <- glm(FINAL_ROUTE ~ sexuality_bin + IMD19_DECILE_LSOAS, data = rtd_sc_language_reg, family = binomial)
lr_test <- anova(model_red, model_full, test = "LRT")
print(lr_test) #adding each variable significantly improves model 
vif_model <- glm(FINAL_ROUTE ~ sexuality_bin + IMD19_DECILE_LSOAS + ETHNICITY, data = rtd_sc_language_reg, family = binomial)
vif(vif_model) #no evidence of multicollinearity 

model_interaction <- glm(FINAL_ROUTE ~ sexuality_bin * IMD19_DECILE_LSOAS + sexuality_bin * ETHNICITY,
                         data = rtd_sc_language_reg,
                         family = binomial)
summary(model_interaction)
