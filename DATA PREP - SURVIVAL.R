################ Linked CPES analysis - survival ###################

#Prep of survival dataset by sexuality and language status using linked 
#CPES-registry data

#Created June 2024 by Lizzie Augarde 
############################################################################# 

########### Creating survival variables ########### 
surv_data <- resp_data |>
  mutate(DIAGNOSISDATEBEST = as.Date(DIAGNOSISDATEBEST),
         DEATHDATEBEST = as.Date(DEATHDATEBEST)) |>
  mutate(daystodeath = difftime(as.Date(DEATHDATEBEST), as.Date(DIAGNOSISDATEBEST), units = "days")) |>
  mutate(date1yrpost = DIAGNOSISDATEBEST + 365, date5yrspost = DIAGNOSISDATEBEST + 1825) |>
  mutate(survival_1yr = ifelse(daystodeath > 365 | is.na(daystodeath), "Yes", "No")) |>
  mutate(survival_5yr = ifelse(daystodeath > 1825 | is.na(daystodeath), "Yes", "No"),
         status_1yr = ifelse(daystodeath > 365 | is.na(daystodeath), 0, 1),
         status_5yr = ifelse(daystodeath > 1825 | is.na(daystodeath), 0, 1))


########### 1yr survival dataset for descriptive analysis ###########
surv_1yr_data <- surv_data |>
  select(-starts_with("Q")) |>
  filter((DIAGNOSISDATEBEST + 365) < "2023-01-01") #filtering to just those with at least 1 year 'available' from diag date'

#multiple responses 
surv_1yr_data <- surv_1yr_data |> 
  group_by(PATIENTID) |>
  arrange(datayear) |>
  mutate(rownum = row_number()) |>
  filter(rownum == 1) |>
  select(-rownum)

#check 
length(unique(surv_1yr_data$PATIENTID))


########### 5yr survival dataset for descriptive analysis ###########
surv_5yr_data <- surv_data |>
  select(-starts_with("Q")) |>
  filter((DIAGNOSISDATEBEST + 1826) < "2023-01-01") #filtering to just those with at least 5 years 'available' from diag date

#multiple responses 
surv_5yr_data <- surv_5yr_data |> 
  group_by(PATIENTID) |>
  arrange(datayear) |>
  mutate(rownum = row_number()) |>
  filter(rownum == 1) |>
  select(-rownum)

#check 
length(unique(surv_5yr_data$PATIENTID))


########### Saving data ########### 
save(surv_1yr_data, file = "surv_1yr_data.RData")
save(surv_5yr_data, file = "surv_5yr_data.RData")
