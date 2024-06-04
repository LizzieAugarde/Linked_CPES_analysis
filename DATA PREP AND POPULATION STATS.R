################ Linked CPES analysis - data preparation ###################

#Prep and analysis of CPES data linked to registry data, looking at respondents
#of sexual minorities and non-native English speakers, clinical characteristics

#Created December 2023 by Lizzie Augarde 
#Change log:
#05/01/2024 converted this script to do the basic population stats rather than
#the characteristics analysis which will happen in a different script. Added 
#cleaning confounders, missing data analysis and creation of survival variables
#04/06/2024 moved survival variables to ANALYSIS-SURVIVAL script
############################################################################# 

#prep
library(tidyverse)
library(dplyr)
library(xlsx)

resp_data <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/Raw respondent data with IMD 20240213.csv")

#data prep
#sexuality variable to binary
resp_data <- resp_data %>% ####different question in different years
  mutate(sexuality = case_when(datayear %in% c(2021,2022) & Q66 == 1 ~ "Heterosexual", 
                                    datayear %in% c(2021,2022) & Q66 == 2 ~ "Gay or lesbian", 
                                    datayear %in% c(2021,2022) & Q66 == 3 ~ "Bisexual", 
                                    datayear %in% c(2021,2022) & Q66 == 4 ~ "Other", 
                                    datayear %in% c(2021,2022) & Q66 == 5 ~ "Prefer not to say", 
                                    datayear %in% c(2021,2022) & Q66 == 6 ~ "Don't know/not sure", 
                                    datayear %in% c(2021,2022) & Q66 == 999 ~ "Missing", 
                                    datayear == 2019 & Q67 == 1 ~ "Heterosexual",
                                    datayear == 2019 & Q66 == 2 ~ "Gay or lesbian", 
                                    datayear == 2019 & Q66 == 3 ~ "Bisexual", 
                                    datayear == 2019 & Q66 == 4 ~ "Other", 
                                    datayear == 2019 & Q66 == 5 ~ "Prefer not to say", 
                                    datayear == 2019 & Q66 == 6 ~ "Don't know/not sure", 
                                    datayear == 2019 & Q66 == 999 ~ "Missing", 
                                    datayear %in% c(2018, 2017) & Q65 == 1 ~ "Heterosexual",
                                    datayear %in% c(2018, 2017) & Q65 == 2 ~ "Gay or lesbian", 
                                    datayear %in% c(2018, 2017) & Q65 == 3 ~ "Bisexual", 
                                    datayear %in% c(2018, 2017) & Q65 == 4 ~ "Other", 
                                    datayear %in% c(2018, 2017) & Q65 == 5 ~ "Prefer not to say", 
                                    datayear %in% c(2018, 2017) & Q65 == 6 ~ "Don't know/not sure", 
                                    datayear %in% c(2018, 2017) & Q65 == 999 ~ "Missing", 
                                    TRUE ~ "Missing")) %>%
  mutate(sexuality_bin = case_when(sexuality %in% c("Gay or lesbian", "Bisexual", "Other") ~ "Sexual minority", 
                                   sexuality == "Heterosexual" ~ "Heterosexual", 
                                   TRUE ~ "Missing")) 
#language status variable to binary
resp_data <- resp_data %>% ####different question in different years
  mutate(lang_stat = case_when(datayear %in% c(2021,2022) & Q70 == 1 ~ "Native English speaker", 
                               datayear %in% c(2021,2022) & Q70 == 2 ~ "Non-native English speaker",
                               datayear == 2019 & Q71 == 1 ~ "Native English speaker", 
                               datayear == 2019 & Q71 == 2 ~ "Non-native English speaker",
                               datayear %in% c(2018, 2017) & Q68 == 1 ~ "Native English speaker", 
                               datayear %in% c(2018, 2017) & Q68 == 2 ~ "Non-native English speaker",
                               TRUE ~ "Missing"))

#total responses by year
resp_data %>%
  group_by(datayear) %>%
  summarise(count = n())


########### Sexuality population ########### 
sexuality_comp <- resp_data %>%
  group_by(datayear, sexuality_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(sexuality_comp), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240517.xlsx",
           sheetName = "Sexuality", row.names = FALSE, append = TRUE)


########### NNES population ########### 
language_comp <- resp_data %>%
  group_by(datayear, lang_stat) %>%
  summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(language_comp), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240517.xlsx",
           sheetName = "Language status", row.names = FALSE, append = TRUE)


########### Confounders missing data ########### 
resp_data %>%
  filter(is.na(IMD19_QUINTILE_LSOAS)) %>%
  group_by(datayear) %>%
  summarise(count = n()) 


########### Cleaning confounder data ########### 
resp_data <- resp_data %>%
  mutate(ETHNICITY = ifelse(ETHNICITY == "0", NA, ETHNICITY))



  






