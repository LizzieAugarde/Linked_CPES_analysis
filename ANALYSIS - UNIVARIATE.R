################ Linked CPES analysis - univariate ###################

#Univariate analysis of confounding variables, by sexuality/language and
#overall, comparing to 2022 total cancer pop

#Created August 2024 by Lizzie Augarde 
############################################################################# 

library(tidyverse)
library(NDRSAfunctions)

casref01 <- NDRSAfunctions::createConnection()


################ COMPARING NCPES RESPONDENTS TO GENERAL CANCER POP ############
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


################ COMPARING COMPOSITION BY SEXUALITY ############
resp_data |> group_by(sexuality_bin) |> summarise(median_age = median(age_at_diag))
resp_data |> group_by(sexuality_bin) |> summarise(min_age = min(age_at_diag))
resp_data |> group_by(sexuality_bin) |> summarise(max_age = max(age_at_diag))
resp_data |> group_by(sexuality_bin) |> summarise(iqr_age = IQR(age_at_diag))
resp_data |> group_by(sexuality_bin) |> summarise(sd_age = sd(age_at_diag))

table(resp_data$sexuality_bin, resp_data$GENDER)
table(resp_data$sexuality_bin, resp_data$ETHNICITY)
table(resp_data$sexuality_bin, resp_data$IMD19_DECILE_LSOAS)
site_sex <- as.data.frame(table(resp_data$sexuality_bin, resp_data$NDRS_MAIN))


################ COMPARING COMPOSITION BY LANGUAGE STATUS ############
resp_data |> group_by(lang_stat) |> summarise(median_age = median(age_at_diag))
resp_data |> group_by(lang_stat) |> summarise(min_age = min(age_at_diag))
resp_data |> group_by(lang_stat) |> summarise(max_age = max(age_at_diag))
resp_data |> group_by(lang_stat) |> summarise(iqr_age = IQR(age_at_diag))
resp_data |> group_by(lang_stat) |> summarise(sd_age = sd(age_at_diag))

table(resp_data$lang_stat, resp_data$GENDER)
table(resp_data$lang_stat, resp_data$ETHNICITY)
table(resp_data$lang_stat, resp_data$IMD19_DECILE_LSOAS)
site_lang <- as.data.frame(table(resp_data$lang_stat, resp_data$NDRS_MAIN))


################ POPULATION PYRAMIDS ############
resp_data |> 
  filter(sexuality_bin == "Sexual minority", GENDER != 0) |>
  group_by(GENDER, age_10yr_band) |>
  summarise(population = n()) |>
  mutate(population = ifelse(GENDER == 1, 
                             population*(-1), 
                             population*1))|>
  mutate(GENDER = ifelse(GENDER == 1, "Male", "Female")) |>
  ggplot(aes(x = age_10yr_band, y = population, fill = GENDER)) +  
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(y = "", x = "Age group", fill = "Gender", title = "Sexual minority respondents") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12, face = "bold"))

resp_data |> 
  filter(lang_stat == "Non-native English speaker", GENDER != 0) |>
  group_by(GENDER, age_10yr_band) |>
  summarise(population = n()) |>
  mutate(population = ifelse(GENDER == 1, 
                             population*(-1), 
                             population*1))|>
  mutate(GENDER = ifelse(GENDER == 1, "Male", "Female")) |>
  ggplot(aes(x = age_10yr_band, y = population, fill = GENDER)) +  
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(y = "", x = "Age group", fill = "Gender", title = "Non-native English-speaking respondents") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12, face = "bold"))


################ DEPRIVATION GRAPHS ############
resp_data |> 
  filter(sexuality_bin != "Missing", !is.na(IMD19_DECILE_LSOAS)) |>
  mutate(IMD19_DECILE_LSOAS = factor(IMD19_DECILE_LSOAS, 
                                     levels = c("1 - most deprived", "2", "3",
                                                "4", "5", "6", "7", "8", "9",
                                                "10 - least deprived"))) |>
  group_by(sexuality_bin, IMD19_DECILE_LSOAS) |>
  summarise(count = n()) |>
  mutate(prop = (count/sum(count))*100) |>
  ggplot(aes(x = IMD19_DECILE_LSOAS, y = prop, fill = sexuality_bin)) +  
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Deprivation decile", y = "Proportion of respondents", 
       fill = "Sexuality") +
  coord_flip() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  scale_y_continuous(limits = c(0,15))

resp_data |> 
  filter(lang_stat != "Missing", !is.na(IMD19_DECILE_LSOAS)) |>
  mutate(IMD19_DECILE_LSOAS = factor(IMD19_DECILE_LSOAS, 
                                     levels = c("1 - most deprived", "2", "3",
                                                "4", "5", "6", "7", "8", "9",
                                                "10 - least deprived"))) |>
  group_by(lang_stat, IMD19_DECILE_LSOAS) |>
  summarise(count = n()) |>
  mutate(prop = (count/sum(count))*100) |>
  ggplot(aes(x = IMD19_DECILE_LSOAS, y = prop, fill = lang_stat)) +  
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Deprivation decile", y = "Proportion of respondents", 
       fill = "Language status") +
  coord_flip() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  scale_y_continuous(limits = c(0,15))