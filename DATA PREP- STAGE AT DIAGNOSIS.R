################ Linked CPES analysis - stage at diagnosis ###################

#Analysis of stage at diagnosis by sexuality and language status using linked 
#CPES-registry data

#Created February 2024 by Lizzie Augarde 
############################################################################# 


########### Dataset for stage analysis ########### 
#prep staging functions 
source("stage_functions_tidy.R")

#trans patient fix from 02_data_tidying_tidy.R received from Chloe Bright
stage_data <- resp_data |> mutate(
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

#running staging function
stage_data <- stage_table(stage_data) 

#combining into 2 level stage variable 
stage_data <- stage_data |>
  mutate(STAGE_2LEVEL = case_when(STAGE %in% c("1", "2", "Staged - other early") ~ "Early",
                                  STAGE %in% c("3", "4", "Staged - other advanced") ~ "Late",
                                  STAGE %in% c("Error", "Unstageable", "Missing") ~ "Not available")) |>
  select(-starts_with("Q"))

#removing unstageable tumours
stage_data <- stage_data |>
  filter(STAGE_PI_DETAIL == "Y")

#multiple responses 
stage_data <- stage_data |> 
  group_by(PATIENTID) |>
  arrange(datayear) |>
  mutate(rownum = row_number()) |>
  filter(rownum == 1) |>
  select(-rownum)

#check 
length(unique(stage_data$PATIENTID))

########### Saving data ########### 
save(stage_data, file = "stage_data.RData")

