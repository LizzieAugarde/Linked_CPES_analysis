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
  mutate(survival_1yr = ifelse(daystodeath > 365 | is.na(daystodeath), "Yes", "No")) |>
  mutate(survival_5yr = ifelse(daystodeath > 1825 | is.na(daystodeath), "Yes", "No"),
         status_1yr = ifelse(daystodeath > 365 | is.na(daystodeath), 0, 1),
         status_5yr = ifelse(daystodeath > 1825 | is.na(daystodeath), 0, 1))


########### 1yr survival dataset for descriptive analysis ###########
surv_1yr_data <- surv_data |>
  select(-starts_with("Q")) |>
  filter((DIAGNOSISDATEBEST + 365) < "2023-01-01") #filtering to just those with at least 1 year 'available' from diag date'

#multiple responses 
surv_1yr_data_dups_check <- surv_1yr_data  |>
  group_by(PATIENTID) |> 
  filter(n() > 1) 

surv_1yr_data_single_response <- surv_1yr_data |>
  group_by(PATIENTID) |> 
  filter(n() == 1) 

source("multiple_responses_functions.R")

#checking for matching death dates in multiple responses
surv_1yr_data_dups_check <- surv_1yr_data_dups_check |>
  group_by(PATIENTID) |>
  mutate(deathdate_match = n_distinct(DEATHDATEBEST))

unique(surv_1yr_data_dups_check$deathdate_match) #no mismatching death dates

#ranking
surv_1yr_data_dups_rank <- rank_multiple_responses(surv_1yr_data_dups_check, "PATIENTID", "datayear")

surv_1yr_data_dups_rank <- surv_1yr_data_dups_rank |> 
  filter(rank == 1) |>
  select(-rank, -deathdate_match)

surv_1yr_data <- rbind(surv_1yr_data_single_response, surv_1yr_data_dups_rank) 

#check 
length(unique(surv_1yr_data$PATIENTID))

#write.csv(surv_1yr_data, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/Survival 1yr dataset for analysis.csv")


########### 5yr survival dataset for descriptive analysis ###########
surv_5yr_data <- surv_data |>
  select(-starts_with("Q")) |>
  filter((DIAGNOSISDATEBEST + 1825) < "2023-01-01") #filtering to just those with at least 5 years 'available' from diag date

#multiple responses 
surv_5yr_data_dups_check <- surv_5yr_data  |>
  group_by(PATIENTID) |> 
  filter(n() > 1) 

surv_5yr_data_single_response <- surv_5yr_data |>
  group_by(PATIENTID) |> 
  filter(n() == 1) 

source("multiple_responses_functions.R")

#checking for matching death dates in multiple responses
surv_5yr_data_dups_check <- surv_5yr_data_dups_check |>
  group_by(PATIENTID) |>
  mutate(deathdate_match = n_distinct(DEATHDATEBEST))

unique(surv_5yr_data_dups_check$deathdate_match) #no mismatching death dates

#ranking
surv_5yr_data_dups_rank <- rank_multiple_responses(surv_5yr_data_dups_check, "PATIENTID", "datayear")

surv_5yr_data_dups_rank <- surv_5yr_data_dups_rank |> 
  filter(rank == 1) |>
  select(-rank, -deathdate_match)

surv_5yr_data <- rbind(surv_5yr_data_single_response, surv_5yr_data_dups_rank) 

#check 
length(unique(surv_5yr_data$PATIENTID))

#write.csv(surv_5yr_data, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/Survival 5yr dataset for analysis.csv")


########### Saving env ########### 
#saving the environment so objects can be loaded and used in Quarto doc
save(surv_1yr_data, file = "surv_1yr_data.RData")
save(surv_5yr_data, file = "surv_5yr_data.RData")
