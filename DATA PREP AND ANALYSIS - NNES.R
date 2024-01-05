################ Linked CPES analysis - NNES ###################

#Prep and analysis of CPES data linked to registry data, looking at respondents
#who are non-native English speakers, clinical characteristics

#Created December 2023 by Lizzie Augarde 
#Change log:
#05/01/2024 converted this script to do the basic population stats for sexuality,
#rather than the characteristics analysis which will happen in a different script
############################################################################# 

#prep
library(tidyverse)

#data prep
resp_data <- resp_data %>% ####different question in different years
  mutate(lang_stat = case_when(datayear == 2021 & Q70 == 1 ~ "Native English speaker", 
                                   datayear == 2021 & Q70 == 2 ~ "Non-native English speaker",
                                   datayear == 2019 & Q71 == 1 ~ "Native English speaker", 
                                   datayear == 2019 & Q71 == 2 ~ "Non-native English speaker",
                                   datayear %in% c(2018, 2017) & Q68 == 1 ~ "Native English speaker", 
                                   datayear %in% c(2018, 2017) & Q68 == 2 ~ "Non-native English speaker",
                                   TRUE ~ "Missing"))


########### NNES population ########### 
language_denoms <- resp_data %>% 
  count(lang_stat) %>%
  mutate(n = as.numeric(n)) %>%
  mutate(percent = (n/sum(n))*100)


########### 1 year survival ###########
lang_resp_1ysurv <- resp_data %>%
  mutate(diag_to_death = difftime(as.Date(DEATHDATEBEST), as.Date(DIAGNOSISDATEBEST))) %>%
  mutate(survival_1y = ifelse(diag_to_death > 364 | is.na(diag_to_death), "Y", "N")) %>%
  group_by(lang_stat, survival_1y) %>%
  summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)
