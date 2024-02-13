################ Linked CPES analysis - sexual minorities ###################

#Prep and analysis of CPES data linked to registry data, looking at respondents
#of sexual minorities, clinical characteristics

#Created December 2023 by Lizzie Augarde 
#Change log:
#05/01/2024 converted this script to do the basic population stats for sexuality,
#rather than the characteristics analysis which will happen in a different script
############################################################################# 

#prep
library(tidyverse)

#data prep
resp_data <- resp_data %>% ####different question in different years
  mutate(sexuality = case_when(datayear == 2021 & Q66 == 1 ~ "Heterosexual", 
                                    datayear == 2021 & Q66 == 2 ~ "Gay or lesbian", 
                                    datayear == 2021 & Q66 == 3 ~ "Bisexual", 
                                    datayear == 2021 & Q66 == 4 ~ "Other", 
                                    datayear == 2021 & Q66 == 5 ~ "Prefer not to say", 
                                    datayear == 2021 & Q66 == 6 ~ "Don't know/not sure", 
                                    datayear == 2021 & Q66 == 999 ~ "Missing", 
                                    datayear == 2019 & Q67 == 1 ~ "Heterosexual",
                                    datayear == 2019 & Q66 == 2 ~ "Gay or lesbian", 
                                    datayear == 2019 & Q66 == 3 ~ "Bisexual", 
                                    datayear == 2019 & Q66 == 4 ~ "Other", 
                                    datayear == 2019 & Q66 == 5 ~ "Prefer not to say", 
                                    datayear == 2019 & Q66 == 6 ~ "Don't know/not sure", 
                                    datayear == 2019 & Q66 == 999 ~ "Missing", 
                                    datayear %in% c(2018, 2017) & Q65 == 1 ~ "Heterosexual",
                                    datayear %in% c(2018, 2017) & Q66 == 2 ~ "Gay or lesbian", 
                                    datayear %in% c(2018, 2017) & Q66 == 3 ~ "Bisexual", 
                                    datayear %in% c(2018, 2017) & Q66 == 4 ~ "Other", 
                                    datayear %in% c(2018, 2017) & Q66 == 5 ~ "Prefer not to say", 
                                    datayear %in% c(2018, 2017) & Q66 == 6 ~ "Don't know/not sure", 
                                    datayear %in% c(2018, 2017) & Q66 == 999 ~ "Missing", 
                                    TRUE ~ "Missing")) %>%
  mutate(sexuality_bin = case_when(sexuality %in% c("Gay or lesbian", "Bisexual", "Other") ~ "Sexual minority", 
                                   sexuality == "Heterosexual" ~ "Heterosexual", 
                                   TRUE ~ "Missing")) 

########### Sexuality population ########### 
sexuality_denoms <- resp_data %>% 
  count(sexuality) %>%
  mutate(n = as.numeric(n)) %>%
  mutate(percent = (n/sum(n))*100)

sexuality_bin_denoms <- resp_data %>% 
  count(sexuality_bin) %>%
  mutate(n = as.numeric(n)) %>%
  mutate(percent = (n/sum(n))*100)


########### 1 year survival ########### 
sex_resp_1ysurv <- resp_data %>%
  mutate(diag_to_death = difftime(as.Date(DEATHDATEBEST), as.Date(DIAGNOSISDATEBEST))) %>%
  mutate(survival_1y = ifelse(diag_to_death > 364 | is.na(diag_to_death), "Y", "N"))
  


