################ Linked CPES analysis - route to diagnosis ###################

#Analysis of routes to diagnosis by sexuality and language status using linked 
#CPES-registry data

#Created January 2024 by Lizzie Augarde 
#Change log:
#08/01/2024 added ranking function for multiple responses
############################################################################# 

########### Dataset for RTD analysis ########### 
rtd_data <- resp_data %>%
  filter(datayear %in% c(2017, 2018)) %>%
  select(-starts_with("Q"))

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

write.csv(rtd_data, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/RTD dataset for analysis.csv")


########### Overall RTD descriptive stats ########### 
rtd_composition <- rtd_data %>%
  group_by(datayear, FINAL_ROUTE) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(rtd_composition), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "RTD", row.names = FALSE, append = TRUE)

rtd_sexuality <- rtd_data %>%
  group_by(datayear, sexuality_bin, FINAL_ROUTE) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100) 

write.xlsx(as.data.frame(rtd_sexuality), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "RTD sexuality", row.names = FALSE)

rtd_language <- rtd_data %>%
  group_by(datayear, lang_stat, FINAL_ROUTE) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100) 

write.xlsx(as.data.frame(rtd_language), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "RTD language", row.names = FALSE)


########### Regression ########### 