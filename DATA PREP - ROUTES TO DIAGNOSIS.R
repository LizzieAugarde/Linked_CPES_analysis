################ Linked CPES analysis - route to diagnosis ###################

#Analysis of routes to diagnosis by sexuality and language status using linked 
#CPES-registry data

#Created January 2024 by Lizzie Augarde 
#Change log:
#08/01/2024 added ranking function for multiple responses
#10/06/2024 adapted to include 2019 RTD data. changed to a data prep script only 
############################################################################# 

########### Dataset for RTD analysis ########### 
rtd_data <- resp_data %>%
  filter(datayear %in% c(2017, 2018, 2019)) %>%
  select(-starts_with("Q")) %>%
  mutate(FINAL_ROUTE = ifelse(FINAL_ROUTE == "TWW", "Urgent suspected cancer referral", FINAL_ROUTE)) %>%
  mutate(FINAL_ROUTE = ifelse(is.na(FINAL_ROUTE), "Missing", FINAL_ROUTE))

#multiple responses 
rtd_data_dups_check <- rtd_data %>%
  group_by(PATIENTID) %>% 
  filter(n() > 1) 

rtd_data_single_response <- rtd_data %>%
  group_by(PATIENTID) %>% 
  filter(n() == 1) 

source("multiple_responses_functions.R")

rtd_data_dups_rank <- rank_multiple_responses(rtd_data_dups_check, "PATIENTID", "datayear")
rtd_data_dups_rank <- rtd_data_dups_rank %>% 
  filter(rank == 1) %>%
  select(-rank)

rtd_data <- rbind(rtd_data_single_response, rtd_data_dups_rank) 

#check 
length(unique(rtd_data$PATIENTID))

#write.csv(rtd_data, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/RTD dataset for analysis.csv")

########### Saving env ########### 
#saving the environment so objects can be loaded and used in Quarto doc
save(rtd_data, file = "rtd_data.RData")

