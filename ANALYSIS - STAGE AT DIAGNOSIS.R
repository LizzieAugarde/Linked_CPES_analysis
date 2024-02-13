################ Linked CPES analysis - stage at diagnosis ###################

#Analysis of stage at diagnosis by sexuality and language status using linked 
#CPES-registry data

#Created February 2024 by Lizzie Augarde 
#Change log:
############################################################################# 

library(xlsx) 

########### Dataset for stage analysis ########### 
#prep staging functions 
source("stage_functions_tidy.R")

stage_data <- stage_table(resp_data) 

#combining into 2 level stage variable 
stage_data <- stage_data %>%
  mutate(STAGE_2LEVEL = case_when(STAGE %in% c("1", "2", "Staged - other early") ~ "Early",
                                  STAGE %in% c("3", "4", "Staged - other advanced") ~ "Late",
                                  STAGE %in% c("Error", "Unstageable", "Missing") ~ "Not available")) %>%
  select(-starts_with("Q"))

#trans patient fix from 02_data_tidying_tidy.R received from Chloe Bright
stage_data <- stage_data %>% mutate(
  STAGE_PI = if_else(
    condition = (
      (
        (GENDER == 2 & SITE_ICD10R4_O2_3CHAR_FROM2013 %in% c("C60", "C61", "C62", "C63")) |
          (GENDER == 1 & SITE_ICD10R4_O2_3CHAR_FROM2013 %in% c("C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58"))
      ) &
        !(STAGE_BEST == "?" | is.na(STAGE_BEST))
    ),
    true = "Y",
    false = STAGE_PI
  ),
  STAGE_PI_DETAIL = if_else(
    condition = (
      (
        (GENDER == 2 & SITE_ICD10R4_O2_3CHAR_FROM2013 %in% c("C60", "C61", "C62", "C63")) |
          (GENDER == 1 & SITE_ICD10R4_O2_3CHAR_FROM2013 %in% c("C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58"))
      ) &
        !(STAGE_BEST == "?" | is.na(STAGE_BEST))
    ),
    true = "Y",
    false = STAGE_PI_DETAIL
  )
)

#removing unstageable tumours
stage_data <- stage_data %>%
  filter(STAGE_PI_DETAIL == "Y")

#multiple responses 
stage_data_dups_check <- stage_data %>%
  group_by(PATIENTID) %>% 
  filter(n() > 1) 

stage_data_single_response <- stage_data %>%
  group_by(PATIENTID) %>% 
  filter(n() == 1) 

source("multiple_responses_functions.R")

stage_data_dups_rank <- rank_multiple_responses(stage_data_dups_check, "PATIENTID", "datayear")
stage_data_dups_rank <- stage_data_dups_rank %>% 
  filter(rank == 1) %>%
  select(-rank)

stage_data <- rbind(stage_data_single_response, stage_data_dups_rank)

#check 
length(unique(stage_data$PATIENTID))

write.csv(stage_data, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/Stage dataset for analysis.csv")


########### Overall stage descriptive stats ########### 
stage_all_composition <- stage_data %>%
  group_by(datayear, STAGE) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(stage_all_composition), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "Stage - all", row.names = FALSE, append = TRUE)

stage_lim_composition <- stage_data %>%
  group_by(datayear, STAGE_2LEVEL) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100)

write.xlsx(as.data.frame(stage_lim_composition), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "Stage - limited", row.names = FALSE, append = TRUE)

stage_sexuality <- stage_data %>%
  group_by(datayear, sexuality_bin, STAGE_2LEVEL) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100) 

write.xlsx(as.data.frame(stage_sexuality), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "Stage sexuality2", row.names = FALSE, append = TRUE)

stage_language <- stage_data %>%
  group_by(datayear, lang_stat, STAGE_2LEVEL) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = (count/sum(count))*100) 

write.xlsx(as.data.frame(stage_language), 
           "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Results/Basic characteristics composition 20240105.xlsx",
           sheetName = "Stage language2", row.names = FALSE, append = TRUE)


########### Regression ########### 