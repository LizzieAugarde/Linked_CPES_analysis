################ Linked CPES analysis - route to diagnosis ###################

#Analysis of routes to diagnosis by sexuality and language status using linked 
#CPES-registry data

#Created January 2024 by Lizzie Augarde 
############################################################################# 

########### Dataset for RTD analysis ########### 
rtd_data <- resp_data |>
  filter(datayear %in% c(2017, 2018, 2019)) |> #RTD currently only available for these years 
  select(-starts_with("Q")) |>
  mutate(FINAL_ROUTE = ifelse(FINAL_ROUTE == "TWW", "Urgent suspected cancer referral", FINAL_ROUTE)) |>
  mutate(FINAL_ROUTE = ifelse(is.na(FINAL_ROUTE), "Missing", FINAL_ROUTE))

#multiple responses 
rtd_data <- rtd_data |> 
  group_by(PATIENTID) |>
  arrange(datayear) |>
  mutate(rownum = row_number()) |>
  filter(rownum == 1) |>
  select(-rownum)


#check 
length(unique(rtd_data$PATIENTID))


########### Saving data ########### 
save(rtd_data, file = "rtd_data.RData")

