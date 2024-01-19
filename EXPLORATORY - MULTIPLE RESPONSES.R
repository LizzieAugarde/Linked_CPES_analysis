################ Linked CPES analysis - multiple responses ###################

#Exploratory analysis of patients responding in multiple years

#Created January 2024 by Lizzie Augarde 
#Change log:
############################################################################# 

library(plyr)
library(xlsx)

resp_data_dups <- resp_data %>%
  group_by(PATIENTID) %>% 
  filter(n() > 1)

#write.csv(resp_data_dups, "Multi-year respondents 20231213.csv", row.names = FALSE)
#resp_data_dups_check <- read.xlsx("Multi-year respondents 20231213.xlsx", sheetIndex = 2)

source("multiple_responses_compare_rows_function.R")

#order by patientid and year to mark earliest response with lowest row number
resp_data_dups_check <- resp_data_dups_check[order(resp_data_dups_check$PATIENTID, resp_data_dups_check$datayear), ] 

resp_data_dups_check$row_number <- seq_len(nrow(resp_data_dups_check))

resp_data_dups_check <- compare_rows(resp_data_dups_check)


#compare sexuality in each record
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

sexuality_changes <- resp_data %>%
  select(sexuality_bin, PATIENTID) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = "sexuality_bin", values_from = "count", values_fn = list)

sexuality_changes <- sexuality_changes %>%
  mutate(sexuality_multi_year_check = case_when(Heterosexual != "NULL" & Missing == "NULL" & `Sexual minority` == "NULL" ~ "Heterosexual only",
                                                Heterosexual == "NULL" & Missing != "NULL" & `Sexual minority` == "NULL" ~ "Missing only",
                                                Heterosexual == "NULL" & Missing == "NULL" & `Sexual minority` != "NULL" ~ "Sexual minority only",
                                                Heterosexual != "NULL" & Missing != "NULL" & `Sexual minority` != "NULL" ~ "Reported as all 3",
                                                Heterosexual != "NULL" & Missing != "NULL" & `Sexual minority` == "NULL" ~ "Heterosexual and Missing",
                                                Heterosexual != "NULL" & Missing == "NULL" & `Sexual minority` != "NULL" ~ "Heterosexual and Sexual Minority",
                                                Heterosexual == "NULL" & Missing != "NULL" & `Sexual minority` != "NULL" ~ "Missing and Sexual Minority",
                                                TRUE ~ "Other")) 
sexuality_changes <- sexuality_changes %>%
  unnest_wider(col = Heterosexual, names_sep = "_") %>%
  unnest_wider(col = Missing, names_sep = "_") %>%
  unnest_wider(col = `Sexual minority`, names_sep = "_") %>%
  mutate(sexuality_multi_year_check = case_when((Heterosexual_1 == 1 | Heterosexual_2 == 1 | Heterosexual_3 == 1 | Heterosexual_4 == 1)))



#compare language status in each record
resp_data <- resp_data %>% ####different question in different years
  mutate(lang_stat = case_when(datayear == 2021 & Q70 == 1 ~ "Native English speaker", 
                               datayear == 2021 & Q70 == 2 ~ "Non-native English speaker",
                               datayear == 2019 & Q71 == 1 ~ "Native English speaker", 
                               datayear == 2019 & Q71 == 2 ~ "Non-native English speaker",
                               datayear %in% c(2018, 2017) & Q68 == 1 ~ "Native English speaker", 
                               datayear %in% c(2018, 2017) & Q68 == 2 ~ "Non-native English speaker",
                               TRUE ~ "Missing"))

language_changes <- resp_data %>%
  select(lang_stat, PATIENTID) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = "lang_stat", values_from = "count", values_fn = list)

language_changes <- language_changes %>%
  mutate(language_multi_year_check = case_when(`Native English speaker` != "NULL" & Missing == "NULL" & `Non-native English speaker` == "NULL" ~ "NES only",
                                               `Native English speaker` == "NULL" & Missing != "NULL" & `Non-native English speaker` == "NULL" ~ "Missing only",
                                               `Native English speaker` == "NULL" & Missing == "NULL" & `Non-native English speaker` != "NULL" ~ "NNES only",
                                               `Native English speaker` != "NULL" & Missing != "NULL" & `Non-native English speaker` != "NULL" ~ "Reported as all 3",
                                               `Native English speaker` != "NULL" & Missing != "NULL" & `Non-native English speaker` == "NULL" ~ "NES and Missing",
                                               `Native English speaker` != "NULL" & Missing == "NULL" & `Non-native English speaker` != "NULL" ~ "NES and NNES",
                                               `Native English speaker` == "NULL" & Missing != "NULL" & `Non-native English speaker` != "NULL" ~ "Missing and NNES",
                                               TRUE ~ "Other")) 



