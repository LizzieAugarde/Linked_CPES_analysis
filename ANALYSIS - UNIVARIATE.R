################ Linked CPES analysis - univariate ###################

#Univariate analysis of confounding variables, comparing to 2022 total cancer pop

#Created August 2024 by Lizzie Augarde 
############################################################################# 

library(tidyverse)
library(NDRSAfunctions)

casref01 <- NDRSAfunctions::createConnection()

all_pts_2022_query <- "SELECT * from AV2022.AT_PATIENT_ENGLAND"

resp2022 <- dbGetQueryOracle(casref01, resp2022_query, rowlimit = NA)
resp2022 <- resp2022[, !duplicated(colnames(resp2022))]



######## Age at diagnosis
median(resp_data$age_at_diag)
min(resp_data$age_at_diag)
max(resp_data$age_at_diag)
IQR(resp_data$age_at_diag)
sd(resp_data$age_at_diag)

boxplot(resp_data$age_at_diag)
table(resp_data$age_10yr_band)


######## Ethnicity
table(resp_data$ETHNICITY)

######## IMD
table(resp_data$IMD19_DECILE_LSOAS)

######## Cancer site
