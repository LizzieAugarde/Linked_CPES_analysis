################ Linked CPES analysis - univariate ###################

#Univariate analysis of confounding variables, comparing to 2022 total cancer pop

#Created August 2024 by Lizzie Augarde 
############################################################################# 

library(tidyverse)
library(NDRSAfunctions)

casref01 <- NDRSAfunctions::createConnection()

all_pts_query <- "SELECT 
                       p.patientid,
                       p.birthdatebest,
                       p.ethnicity,
                       t.diagnosisdatebest,
                       t.diagnosisyear,
                       s.ndrs_main,
                       i.imd19_decile_lsoas,
                       i.imd19_quintile_lsoas,
                       rank () over (partition by t.patientid order by t.diagnosisdatebest, t.tumourid asc) as rank
                       from av2022.at_tumour_england@casref01 t
                       left join av2022.at_patient_england@casref01 p on t.patientid = p.patientid
                       left join av2022.at_geography_england@casref01 g on t.tumourid = g.tumourid --adding IMD
                       left join imd.imd2019_equal_lsoas@casref01 i on g.lsoa11_code = i.lsoa11_code --adding IMD
                       left join analysispollyjeffrey.at_site_england@casref01 s on t.tumourid = s.tumourid --including site from new site table 
                       left join av2020.rtd2020 r on t.tumourid = r.tumourid --adding route to diagnosis
                       where (t.diagnosisyear > 2016 and t.diagnosisyear < 2023)
                       and t.cascade_inci_flag = 1 --standard CAS exclusions
                       and t.dco = 'N' --excluding those diagnosed on death certificate only 
                       and t.dedup_flag = 1 --excluding duplicate records
                       and substr(t.site_icd10_o2_3char, 1, 1) = 'C' --include C codes only 
                       and t.site_icd10_o2_3char <> 'C44' --exclude skin cancer"

all_pts <- dbGetQueryOracle(casref01, all_pts_query, rowlimit = NA)


######## Age at diagnosis
median(resp_data$age_at_diag)
min(resp_data$age_at_diag)
max(resp_data$age_at_diag)
IQR(resp_data$age_at_diag)
sd(resp_data$age_at_diag)

boxplot(resp_data$age_at_diag)
table(resp_data$age_10yr_band)

all_pts <- all_pts |>
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
                                   age_at_diag > 89 ~ "90+"))

median(all_pts$age_at_diag)
min(all_pts$age_at_diag)
max(all_pts$age_at_diag)
IQR(all_pts$age_at_diag)
sd(all_pts$age_at_diag)

boxplot(all_pts$age_at_diag)
table(all_pts$age_10yr_band)


######## Ethnicity
table(resp_data$ETHNICITY)
table(all_pts$ETHNICITY)

######## IMD
table(resp_data$IMD19_DECILE_LSOAS)
table(all_pts$IMD19_DECILE_LSOAS)

######## Cancer site
resps_site <- as.data.frame(table(resp_data$NDRS_MAIN))
all_pts_site <- as.data.frame(table(all_pts$NDRS_MAIN))
