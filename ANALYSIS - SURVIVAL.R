################ Linked CPES analysis - survival ###################

#Analysis of 1 and 5 year survival by sexuality and language status using linked 
#CPES-registry data

#Created January 2024 by Lizzie Augarde 
#Change log:
#14/02/2024 redoing survival datasets to use all patients with 1 and 5 years post-diag
#available in COSD
############################################################################# 

library(xlsx)

########### 1yr survival dataset for analysis ###########
surv_1yr_data <- resp_data %>%
  select(-starts_with("Q")) %>%
  filter((DIAGNOSISDATEBEST + 365) < "2022-01-01") #filtering to just those with at least 1 year 'available' from diag date

#multiple responses 
surv_1yr_data_dups_check <- surv_1yr_data  %>%
  group_by(PATIENTID) %>% 
  filter(n() > 1) 

surv_1yr_data_single_response <- surv_1yr_data %>%
  group_by(PATIENTID) %>% 
  filter(n() == 1) 

source("multiple_responses_functions.R")

#checking for matching death dates in multiple responses
surv_1yr_data_dups_check <- surv_1yr_data_dups_check %>%
  group_by(PATIENTID) %>%
  mutate(deathdate_match = n_distinct(DEATHDATEBEST))

unique(surv_1yr_data_dups_check$deathdate_match) #no mismatching death dates

#ranking
surv_1yr_data_dups_rank <- rank_multiple_responses(surv_1yr_data_dups_check, "PATIENTID", "datayear")

surv_1yr_data_dups_rank <- surv_1yr_data_dups_rank %>% 
  filter(rank == 1) %>%
  select(-rank, -deathdate_match)

surv_1yr_data <- rbind(surv_1yr_data_single_response, surv_1yr_data_dups_rank) 

#check 
length(unique(surv_1yr_data$PATIENTID))

write.csv(surv_1yr_data, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/Survival 1yr dataset for analysis.csv")


########### Overall 1yr survival descriptive stats ########### 
surv_1yr_composition <- surv_1yr_data %>%
  group_by(survival_1yr) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(surv_1yr_composition), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "1yr survival", row.names = FALSE, append = TRUE)

surv_1yr_sexuality <- surv_1yr_data %>%
  group_by(sexuality_bin, survival_1yr) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(surv_1yr_sexuality), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "1yr survival sexuality", row.names = FALSE, append = TRUE)

surv_1yr_language <- surv_1yr_data %>%
  group_by(lang_stat, survival_1yr) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(surv_1yr_language), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "1yr survival language", row.names = FALSE, append = TRUE)


########### 5yr survival dataset for analysis ###########
surv_5yr_data <- resp_data %>%
  select(-starts_with("Q")) %>%
  filter((DIAGNOSISDATEBEST + 1825) < "2022-01-01") #filtering to just those with at least 5 years 'available' from diag date

#multiple responses 
surv_5yr_data_dups_check <- surv_5yr_data  %>%
  group_by(PATIENTID) %>% 
  filter(n() > 1) 

surv_5yr_data_single_response <- surv_5yr_data %>%
  group_by(PATIENTID) %>% 
  filter(n() == 1) 

source("multiple_responses_functions.R")

#checking for matching death dates in multiple responses
surv_5yr_data_dups_check <- surv_5yr_data_dups_check %>%
  group_by(PATIENTID) %>%
  mutate(deathdate_match = n_distinct(DEATHDATEBEST))

unique(surv_5yr_data_dups_check$deathdate_match) #no mismatching death dates

#ranking
surv_5yr_data_dups_rank <- rank_multiple_responses(surv_5yr_data_dups_check, "PATIENTID", "datayear")

surv_5yr_data_dups_rank <- surv_5yr_data_dups_rank %>% 
  filter(rank == 1) %>%
  select(-rank, -deathdate_match)

surv_5yr_data <- rbind(surv_5yr_data_single_response, surv_5yr_data_dups_rank) 

#check 
length(unique(surv_5yr_data$PATIENTID))

write.csv(surv_5yr_data, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/Survival 5yr dataset for analysis.csv")


########### Overall 5yr survival descriptive stats ########### 
surv_5yr_composition <- surv_5yr_data %>%
  group_by(survival_5yr) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(surv_5yr_composition), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "5yr survival", row.names = FALSE, append = TRUE)

surv_5yr_sexuality <- surv_5yr_data %>%
  group_by(sexuality_bin, survival_5yr) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(surv_5yr_sexuality), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "5yr survival sexuality", row.names = FALSE, append = TRUE)

surv_5yr_language <- surv_5yr_data %>%
  group_by(lang_stat, survival_5yr) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(surv_5yr_language), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "5yr survival language", row.names = FALSE, append = TRUE)


########### Regression ########### 