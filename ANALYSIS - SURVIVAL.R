################ Linked CPES analysis - survival ###################

#Analysis of 1 and 5 year survival by sexuality and language status using linked 
#CPES-registry data

#Created January 2024 by Lizzie Augarde 
#Change log:
#14/02/2024 redoing survival datasets to use all patients with 1 and 5 years post-diag
#available in COSD
############################################################################# 

library(xlsx)

########### Creating survival variables ########### 
resp_data <- resp_data %>%
  mutate(DIAGNOSISDATEBEST = as.Date(DIAGNOSISDATEBEST),
         DEATHDATEBEST = as.Date(DEATHDATEBEST)) %>%
  mutate(daystodeath = difftime(as.Date(DEATHDATEBEST), as.Date(DIAGNOSISDATEBEST), units = "days")) %>%
  mutate(survival_1yr = ifelse(daystodeath > 365 | is.na(daystodeath), "Yes", "No")) %>%
  mutate(survival_5yr = ifelse(daystodeath > 1825 | is.na(daystodeath), "Yes", "No"),
         status_1yr = ifelse(daystodeath > 365 | is.na(daystodeath), 0, 1),
         status_5yr = ifelse(daystodeath > 1825 | is.na(daystodeath), 0, 1))

########### 1yr survival dataset for descriptive analysis ###########
surv_1yr_data <- resp_data %>%
  select(-starts_with("Q")) %>%
  filter((DIAGNOSISDATEBEST + 365) < "2023-01-01") #filtering to just those with at least 1 year 'available' from diag date

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


########### 5yr survival dataset for descriptive analysis ###########
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


########### Surv package analysis ########### 
library(survival)
library(ggsurvfit)
library(gtsummary)
library(ggplot2)
library(survminer)

surv_data <- resp_data %>%
  select(DIAGNOSISDATEBEST, DEATHDATEBEST, daystodeath, status_1yr, status_5yr, sexuality, sexuality_bin, lang_stat)

surv_1yr <- Surv(surv_data$daystodeath, surv_data$status_1yr)

s1 <- survfit(Surv(daystodeath, status_1yr) ~ 1, data = surv_data)
str(s1)

summ_1yr <- survfit(Surv(daystodeath, status_1yr) ~ 1, data = surv_data)
summ_5yr <- survfit(Surv(daystodeath, status_5yr) ~ 1, data = surv_data)

summary(summ_1yr, times = 365)
summary(summ_5yr, times = 1825)

ggplot2::autoplot(summ_5yr)

survdiff(Surv(daystodeath, status_1yr) ~ sexuality_bin, data = filter(surv_data, sexuality_bin != "Missing"))
survdiff(Surv(daystodeath, status_5yr) ~ sexuality_bin, data = filter(surv_data, sexuality_bin != "Missing"))
survdiff(Surv(daystodeath, status_1yr) ~ lang_stat, data = filter(surv_data, lang_stat != "Missing"))
survdiff(Surv(daystodeath, status_5yr) ~ lang_stat, data = filter(surv_data, lang_stat != "Missing"))

sexuality <- survfit(Surv(daystodeath, status_5yr) ~ sexuality_bin, data = filter(surv_data, sexuality_bin != "Missing"))
language <- survfit(Surv(daystodeath, status_5yr) ~ lang_stat, data = filter(surv_data, lang_stat != "Missing"))

ggsurvplot(sexuality, pval = TRUE, conf.int = TRUE,
           risk.table = TRUE)

summary(sexuality)$table

ggsurvplot(language, pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, xlim = c(0, 2000))

summary(language)$table


https://www.sthda.com/english/wiki/survival-analysis-basics#compute-survival-curves-survfit
https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#Part_1:_Introduction_to_Survival_Analysis
