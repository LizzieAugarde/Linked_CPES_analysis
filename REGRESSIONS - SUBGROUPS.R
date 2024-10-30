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
