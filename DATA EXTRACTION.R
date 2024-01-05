################ Linked CPES analysis - data extraction ###################

#Extract of linked CPES data

#Created December 2023 by Lizzie Augarde 
#Change log:
#14/12/2023 added analysis of patients responding in multiple years
#05/01/2024 moved multiple responses analysis to separate exploratory script
############################################################################# 

library(tidyverse)
library(NDRSAfunctions)
library(plyr)
library(xlsx)

casref01 <- NDRSAfunctions::createConnection()

#data extract - 2021
resp2021_query <- "SELECT *
                  FROM CPES.NC_2021_RESPONDENTS@casref01 cpes, CPES.NC_21_LINK_RES_CAS2306@casref01 ln, AV2021.AT_TUMOUR_ENGLAND@casref01 at
                  WHERE cpes.cpes_prn = ln.cpes_prn
                  AND ln.patientid = at.patientid
                  AND ln.tumourid = at.tumourid
                  AND ln.final_unique =  1"

resp2021 <- dbGetQueryOracle(casref01, resp2021_query, rowlimit = NA)
resp2021 <- resp2021[, !duplicated(colnames(resp2021))]

#data extract - 2019
resp2019_query <- "select *
                  from cpes.nc_2019_respondents@casref01 cpes, cpes.nc_2019_linkage_cas2009@casref01 ln, av2021.at_tumour_england@casref01 at
                  where cpes.cpes_prn = ln.prn
                  and ln.patientid = at.patientid
                  and ln.tumourid = at.tumourid
                  and ln.final_unique =  1"

resp2019 <- dbGetQueryOracle(casref01, resp2019_query, rowlimit = NA)
resp2019 <- resp2019[, !duplicated(colnames(resp2019))]

#data extract - 2018 (includes RTD link)
resp2018_query <- "select *
                  from cpes.nc_2018_respondents@casref01 cpes
                  join cpes.nc_2018_linkage_cas1907@casref01 ln on cpes.cpes_psid = ln.psid 
                  join av2021.at_tumour_england@casref01 at on ln.patientid = at.patientid and ln.tumourid = at.tumourid
                  left join av2018.rtd2018@casref01 rtd on ln.tumourid = rtd.tumourid
                  where ln.final_unique = 1"

resp2018 <- dbGetQueryOracle(casref01, resp2018_query, rowlimit = NA)
resp2018 <- resp2018[, !duplicated(colnames(resp2018))]

#data extract - 2017 (includes RTD link)
resp2017_query <- "select *
                  from cpes.nc_2017_respondents@casref01 cpes
                  join cpes.nc_2017_linkage_cas1907@casref01 ln on cpes.cpes_psid = ln.psid 
                  join av2021.at_tumour_england@casref01 at on ln.patientid = at.patientid and ln.tumourid = at.tumourid
                  left join av2017.rtd2017@casref01 rtd on ln.tumourid = rtd.tumourid
                  where ln.final_unique = 1"

resp2017 <- dbGetQueryOracle(casref01, resp2017_query, rowlimit = NA)
resp2017 <- resp2017[, !duplicated(colnames(resp2017))]

#add year labels and bind rows 
resp2021$datayear <- 2021
resp2019$datayear <- 2019
resp2018$datayear <- 2018
resp2017$datayear <- 2017

resp_data <- rbind.fill(resp2021, resp2019, resp2018, resp2017)


############ Patients responding in multiple years ############ 
resp_data_dups <- resp_data %>%
  group_by(PATIENTID) %>% 
  filter(n() > 1)

#write.csv(resp_data_dups, "Multi-year respondents 20231213.csv", row.names = FALSE)
#resp_data_dups_check <- read.xlsx("Multi-year respondents 20231213.xlsx", sheetIndex = 2)

compare_rows <- function(data_frame) {
  data_frame$match_type <- NA
  unique_patient_ids <- unique(data_frame$PATIENTID)
  
  #loop through each patientid
  for (patient_id in unique_patient_ids) {
    patient_data <- data_frame[data_frame$PATIENTID == patient_id, ]
    
    #identify first response per patient
    first_response <- patient_data[1, 'row_number']
    
    #identify full duplicates
    duplicates <- all(apply(patient_data[, !names(patient_data) %in% c("row_number", "PATIENTID")], 1, function(x) all(x == patient_data[1, !names(patient_data) %in% c("row_number", "PATIENTID")])))
    
    #identify records where patient is responding in more than 1 year but diagnosis is the same in both years
    same_tumour_diff_year <- any(!is.na(patient_data$datayear[-1]) & patient_data$datayear[-1] != patient_data$datayear[1])
    
    #mark rows
    data_frame$match_type[first_response] <- "First response"
    
    if (exact_match_all && !differences_datayear) {
      data_frame$match_type[patient_data$row_number[-1]] <- "Duplicate record"
    } else if (!exact_match_all && differences_datayear) {
      data_frame$match_type[patient_data$row_number[-1]] <- "Same tumour different datayear"
    } else {
      data_frame$match_type[patient_data$row_number[-1]] <- "Different tumour different datayear"
    }
  }
  
  return(data_frame)
}

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



