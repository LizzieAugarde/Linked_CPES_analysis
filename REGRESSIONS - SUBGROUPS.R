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


######### LANGUAGE STATUS AND STAGE AT DIAGNOSIS #########
#limiting to older patients (older than median age)
stage_data_language_reg_old <- stage_data_language_reg |>
  filter(age_at_diag >66)

#splitting into training and testing sets
set.seed(123)
stage_data_language_reg_split <- initial_split(stage_data_language_reg_old)
train_data <- training(stage_data_language_reg_split)
test_data <- testing(stage_data_language_reg_split)

stage_language <- 
  recipe(STAGE_2LEVEL ~ lang_stat + age_10yr_band + IMD19_DECILE_LSOAS, data = train_data) |>
  step_dummy(all_nominal_predictors())

#model
model2 <- logistic_reg() |>
  set_engine("glm") |>
  fit(STAGE_2LEVEL ~ lang_stat + age_10yr_band + IMD19_DECILE_LSOAS, data = train_data)

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
  fit(STAGE_2LEVEL ~ lang_stat + IMD19_DECILE_LSOAS, data = stage_data_language_reg_old)

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


######### SEXUALITY AND SCREENING ROUTE TO DIAGNOSIS - SCREENABLE CANCERS #########
rtd_sc_sexuality_reg_lim <- rtd_sc_sexuality_reg |>
  filter((NDRS_MAIN == "Cervix" & age_at_diag >= 25 & age_at_diag <= 64 & GENDER == 2) |
         (NDRS_MAIN == "Breast" & age_at_diag >= 50 & age_at_diag <= 70 & GENDER == 2) |
         (NDRS_MAIN == "Colorectal" & age_at_diag >= 50))

#splitting into training and testing sets
model4 <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(FINAL_ROUTE ~ sexuality_bin + age_binary + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = rtd_sc_sexuality_reg_lim)

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


######### LANGUAGE STATUS AND SCREENING ROUTE TO DIAGNOSIS - SCREENABLE CANCERS #########
rtd_sc_language_reg_lim <- rtd_sc_language_reg |>
  filter((NDRS_MAIN == "Cervix" & age_at_diag >= 25 & age_at_diag <= 64 & GENDER == 2) |
           (NDRS_MAIN == "Breast" & age_at_diag >= 50 & age_at_diag <= 70 & GENDER == 2) |
           (NDRS_MAIN == "Colorectal" & age_at_diag >= 50)) |>
  filter(sexuality_bin != "Missing") #added to check influence of missing data, no influence

#splitting into training and testing sets
model4 <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(FINAL_ROUTE ~ sexuality_bin + ETHNICITY + IMD19_DECILE_LSOAS + NDRS_MAIN, data = rtd_sc_language_reg_lim)

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
