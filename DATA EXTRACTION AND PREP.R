################ Linked CPES analysis - data extraction ###################

#Extract of linked CPES data

#Created December 2023 by Lizzie Augarde 
############################################################################# 

library(tidyverse)
library(NDRSAfunctions)

casref01 <- NDRSAfunctions::createConnection()

#data extract - 2022
resp2022_query <- "SELECT *
                  FROM CPES.NC_2022_RESPONDENTS@casref01 cpes,
                  CPES.NC_2022_LINKAGE_RES_CAS2407@casref01 ln, 
                  AV2022.AT_TUMOUR_ENGLAND@casref01 at
                  left join analysispollyjeffrey.at_site_england@casref01 s on at.tumourid = s.tumourid
                  WHERE cpes.cpes_prn = ln.cpes_prn
                  AND ln.avtum_patientid = at.patientid
                  AND ln.avtum_tumourid = at.tumourid
                  AND ln.final_unique =  1"

resp2022 <- dbGetQueryOracle(casref01, resp2022_query, rowlimit = NA)
resp2022 <- resp2022[, !duplicated(colnames(resp2022))]

#data extract - 2021
resp2021_query <- "SELECT *
                  FROM CPES.NC_2021_RESPONDENTS@casref01 cpes, 
                  CPES.NC_21_LINK_RES_CAS2306@casref01 ln, 
                  AV2022.AT_TUMOUR_ENGLAND@casref01 at
                  left join analysispollyjeffrey.at_site_england@casref01 s on at.tumourid = s.tumourid
                  WHERE cpes.cpes_prn = ln.cpes_prn
                  AND ln.patientid = at.patientid
                  AND ln.tumourid = at.tumourid
                  AND ln.final_unique =  1"

resp2021 <- dbGetQueryOracle(casref01, resp2021_query, rowlimit = NA)
resp2021 <- resp2021[, !duplicated(colnames(resp2021))]

#data extract - 2019 (includes RTD link)
resp2019_query <- "select *
                  from cpes.nc_2019_respondents@casref01 cpes
                  join cpes.nc_2019_linkage_cas2009@casref01 ln on cpes.cpes_prn = ln.prn
                  join av2022.at_tumour_england@casref01 at on ln.patientid = at.patientid and ln.tumourid = at.tumourid
                  left join av2020.rtd2020@casref01 rtd on ln.tumourid = rtd.tumourid
                  left join analysispollyjeffrey.at_site_england@casref01 s on at.tumourid = s.tumourid
                  where ln.final_unique =  1"

resp2019 <- dbGetQueryOracle(casref01, resp2019_query, rowlimit = NA)
resp2019 <- resp2019[, !duplicated(colnames(resp2019))]

#data extract - 2018 (includes RTD link)
resp2018_query <- "select *
                  from cpes.nc_2018_respondents@casref01 cpes
                  join cpes.nc_2018_linkage_cas1907@casref01 ln on cpes.cpes_psid = ln.psid 
                  join av2022.at_tumour_england@casref01 at on ln.patientid = at.patientid and ln.tumourid = at.tumourid
                  left join av2020.rtd2020@casref01 rtd on ln.tumourid = rtd.tumourid
                  left join analysispollyjeffrey.at_site_england@casref01 s on at.tumourid = s.tumourid
                  where ln.final_unique = 1"

resp2018 <- dbGetQueryOracle(casref01, resp2018_query, rowlimit = NA)
resp2018 <- resp2018[, !duplicated(colnames(resp2018))]

#data extract - 2017 (includes RTD link)
resp2017_query <- "select *
                  from cpes.nc_2017_respondents@casref01 cpes
                  join cpes.nc_2017_linkage_cas1907@casref01 ln on cpes.cpes_psid = ln.psid 
                  join av2022.at_tumour_england@casref01 at on ln.patientid = at.patientid and ln.tumourid = at.tumourid
                  left join av2020.rtd2020@casref01 rtd on ln.tumourid = rtd.tumourid
                  left join analysispollyjeffrey.at_site_england@casref01 s on at.tumourid = s.tumourid
                  where ln.final_unique = 1"

resp2017 <- dbGetQueryOracle(casref01, resp2017_query, rowlimit = NA)
resp2017 <- resp2017[, !duplicated(colnames(resp2017))]

#add year labels and bind rows 
resp2022$datayear <- 2022
resp2021$datayear <- 2021
resp2019$datayear <- 2019
resp2018$datayear <- 2018
resp2017$datayear <- 2017

resp2022 <- resp2022 |> mutate_all(as.character)
resp2021 <- resp2021 |> mutate_all(as.character)
resp2019 <- resp2019 |> mutate_all(as.character)
resp2018 <- resp2018 |> mutate_all(as.character)
resp2017 <- resp2017 |> mutate_all(as.character)

resp_data <- bind_rows(resp2022, resp2021, resp2019, resp2018, resp2017)


########### Adding in deprivation ########### 
#2022 IMD data extract-----
imd2022_query <- "select 
                      at.patientid, 
                      at.tumourid,
                      g.lsoa11_code,
                      d.imd19_decile_lsoas,
                      d.imd19_quintile_lsoas
                  from cpes.nc_2022_respondents@casref01 cpes
                  join cpes.nc_2022_linkage_res_cas2407@casref01 ln on cpes.cpes_prn = ln.cpes_prn
                  join av2022.at_tumour_england@casref01 at on ln.avtum_patientid = at.patientid and ln.avtum_tumourid = at.tumourid
                  left join av2022.at_geography_england@casref01 g on at.tumourid = g.tumourid
                  left join imd.imd2019_equal_lsoas@casref01 d on g.lsoa11_code = d.lsoa11_code
                  where ln.final_unique = 1"

imd2022 <- dbGetQueryOracle(casref01, imd2022_query, rowlimit = NA)

#2021 IMD data extract-----
imd2021_query <- "select 
                      at.patientid, 
                      at.tumourid,
                      g.lsoa11_code,
                      d.imd19_decile_lsoas,
                      d.imd19_quintile_lsoas
                  from cpes.nc_2021_respondents@casref01 cpes
                  join cpes.nc_21_link_res_cas2306@casref01 ln on cpes.cpes_prn = ln.cpes_prn
                  join av2022.at_tumour_england@casref01 at on ln.patientid = at.patientid and ln.tumourid = at.tumourid
                  left join av2022.at_geography_england@casref01 g on at.tumourid = g.tumourid
                  left join imd.imd2019_equal_lsoas@casref01 d on g.lsoa11_code = d.lsoa11_code
                  where ln.final_unique = 1"

imd2021 <- dbGetQueryOracle(casref01, imd2021_query, rowlimit = NA)

#2019 IMD data extract----
imd2019_query <- "select 
                      at.patientid, 
                      at.tumourid,
                      g.lsoa11_code,
                      d.imd19_decile_lsoas,
                      d.imd19_quintile_lsoas
                  from cpes.nc_2019_respondents@casref01 cpes
                  join cpes.nc_2019_linkage_cas2009@casref01 ln on cpes.cpes_prn = ln.prn
                  join av2022.at_tumour_england@casref01 at on ln.patientid = at.patientid and ln.tumourid = at.tumourid
                  left join av2022.at_geography_england@casref01 g on at.tumourid = g.tumourid
                  left join imd.imd2019_equal_lsoas@casref01 d on g.lsoa11_code = d.lsoa11_code
                  where ln.final_unique = 1"

imd2019 <- dbGetQueryOracle(casref01, imd2019_query, rowlimit = NA)

#2018 IMD data extract-----
imd2018_query <- "select 
                      at.patientid, 
                      at.tumourid,
                      g.lsoa11_code,
                      d.imd19_decile_lsoas,
                      d.imd19_quintile_lsoas
                  from cpes.nc_2018_respondents@casref01 cpes
                  join cpes.nc_2018_linkage_cas1907@casref01 ln on cpes.cpes_psid = ln.psid
                  join av2022.at_tumour_england@casref01 at on ln.patientid = at.patientid and ln.tumourid = at.tumourid
                  left join av2022.at_geography_england@casref01 g on at.tumourid = g.tumourid
                  left join imd.imd2019_equal_lsoas@casref01 d on g.lsoa11_code = d.lsoa11_code
                  where ln.final_unique = 1"

imd2018 <- dbGetQueryOracle(casref01, imd2018_query, rowlimit = NA)

#2017 IMD data extract-----
imd2017_query <- "select 
                      at.patientid, 
                      at.tumourid,
                      g.lsoa11_code,
                      d.imd19_decile_lsoas,
                      d.imd19_quintile_lsoas
                  from cpes.nc_2017_respondents@casref01 cpes
                  join cpes.nc_2017_linkage_cas1907@casref01 ln on cpes.cpes_psid = ln.psid
                  join av2022.at_tumour_england@casref01 at on ln.patientid = at.patientid and ln.tumourid = at.tumourid
                  left join av2022.at_geography_england@casref01 g on at.tumourid = g.tumourid
                  left join imd.imd2019_equal_lsoas@casref01 d on g.lsoa11_code = d.lsoa11_code
                  where ln.final_unique = 1"

imd2017 <- dbGetQueryOracle(casref01, imd2017_query, rowlimit = NA)

#add year labels and bind rows 
imd2022$datayear <- 2022
imd2021$datayear <- 2021
imd2019$datayear <- 2019
imd2018$datayear <- 2018
imd2017$datayear <- 2017

imd_data <- bind_rows(imd2022, imd2021, imd2019, imd2018, imd2017)

#checking for LSOA variation in different responses for the same tumour (linking IMD based on tumour not patient as looking at IMD at diag)
imd_data_check <- imd_data |>
  group_by(TUMOURID) |>
  dplyr::summarise(unique_lsoas = n_distinct(LSOA11_CODE))

inconsistent_lsoa <- imd_data_check |>
  filter(unique_lsoas > 1) #no inconsistent LSOAs by tumourid

imd_data <- imd_data |>
  dplyr::select(-c(PATIENTID, datayear)) |>
  distinct()

#link to resp_data table 
resp_data <- left_join(resp_data, imd_data, by = c("TUMOURID" = "TUMOURID"), relationship = "many-to-one")


########### Converting variables to analysable formats ########### 
#sexuality variable to binary
resp_data <- resp_data |> ####different question in different years
  mutate(sexuality = case_when(datayear %in% c(2021,2022) & Q66 == 1 ~ "Heterosexual", 
                               datayear %in% c(2021,2022) & Q66 == 2 ~ "Gay or lesbian", 
                               datayear %in% c(2021,2022) & Q66 == 3 ~ "Bisexual", 
                               datayear %in% c(2021,2022) & Q66 == 4 ~ "Other", 
                               datayear %in% c(2021,2022) & Q66 == 5 ~ "Prefer not to say", 
                               datayear %in% c(2021,2022) & Q66 == 6 ~ "Don't know/not sure", 
                               datayear %in% c(2021,2022) & Q66 == 999 ~ "Missing", 
                               datayear == 2019 & Q67 == 1 ~ "Heterosexual",
                               datayear == 2019 & Q67 == 2 ~ "Gay or lesbian", 
                               datayear == 2019 & Q67 == 3 ~ "Bisexual", 
                               datayear == 2019 & Q67 == 4 ~ "Other", 
                               datayear == 2019 & Q67 == 5 ~ "Prefer not to say", 
                               datayear == 2019 & Q67 == 6 ~ "Don't know/not sure", 
                               datayear == 2019 & Q67 == 999 ~ "Missing", 
                               datayear %in% c(2018, 2017) & Q65 == 1 ~ "Heterosexual",
                               datayear %in% c(2018, 2017) & Q65 == 2 ~ "Gay or lesbian", 
                               datayear %in% c(2018, 2017) & Q65 == 3 ~ "Bisexual", 
                               datayear %in% c(2018, 2017) & Q65 == 4 ~ "Other", 
                               datayear %in% c(2018, 2017) & Q65 == 5 ~ "Prefer not to say", 
                               TRUE ~ "Missing")) |>
  mutate(sexuality_bin = case_when(sexuality %in% c("Gay or lesbian", "Bisexual", "Other") ~ "Sexual minority", 
                                   sexuality == "Heterosexual" ~ "Heterosexual", 
                                   TRUE ~ "Missing")) 
#language status variable to binary
resp_data <- resp_data |> ####different question in different years
  mutate(lang_stat = case_when(datayear %in% c(2021,2022) & Q70 == 1 ~ "Native English speaker", 
                               datayear %in% c(2021,2022) & Q70 == 2 ~ "Non-native English speaker",
                               datayear == 2019 & Q71 == 1 ~ "Native English speaker", 
                               datayear == 2019 & Q71 == 2 ~ "Non-native English speaker",
                               datayear %in% c(2018, 2017) & Q68 == 1 ~ "Native English speaker", 
                               datayear %in% c(2018, 2017) & Q68 == 2 ~ "Non-native English speaker",
                               TRUE ~ "Missing"))


########### Creating age variables ########### 
#derive age at diagnosis from birth and diagnosis dates due to weird age responses in CPES 
resp_data <- resp_data |>
  mutate(age_at_diag = round((as.numeric(difftime(DIAGNOSISDATEBEST, BIRTHDATEBEST, units = "days")))/365.25)) |>
  mutate(age_10yr_band = case_when(age_at_diag < 10 ~ "0-9", 
                                   age_at_diag > 9 & age_at_diag <20 ~ "10-19",
                                   age_at_diag > 19 & age_at_diag <30 ~ "20-29",
                                   age_at_diag > 29 & age_at_diag <40 ~ "30-39",
                                   age_at_diag > 39 & age_at_diag <50 ~ "40-49",
                                   age_at_diag > 49 & age_at_diag <60 ~ "50-59",
                                   age_at_diag > 59 & age_at_diag <70 ~ "60-69",
                                   age_at_diag > 69 & age_at_diag <80 ~ "70-79",
                                   age_at_diag > 79 & age_at_diag <90 ~ "80-89",
                                   age_at_diag > 89 ~ "90+")) |>
  mutate(age_binary = ifelse(age_at_diag < 68, "Young", "Old")) #based on median age at diagnosis which for this cohort is 67


########### Saving env ########### 
#saving the environment so objects can be loaded and used in Quarto doc
save(resp_data, file = "resp_data.RData")

