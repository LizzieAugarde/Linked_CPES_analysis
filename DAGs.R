################ Linked CPES analysis - DAGs ###################

#Creating DAGs for assessing confounders to include in regression models

#Created January 2024 by Lizzie Augarde 
#Change log:
############################################################################# 

#prep
library(dagitty)
library(tidyverse)

##### Survival and sexuality
surv_1yr_data <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/Survival 1yr dataset for analysis.csv")

dag1 <- dagitty("dag {
                      sexuality -> survival_1yr
                      AGE -> survival_1yr
                      AGE -> sexuality
                      ETHNICITY <-> sexuality 
                      ETHNICITY <-> AGE
                      IMD19_DECILE_LSOAS -> survival_1yr
                      sexuality <-> IMD19_DECILE_LSOAS 
                      stage -> survival_1yr
                      AGE -> SITE_ICD10_O2_3CHAR
                      ETHNICITY -> SITE_ICD10_O2_3CHAR
                      SITE_ICD10_O2_3CHAR -> survival_1yr 
                }")

plot(dag1)
adjustmentSets(dag1, exposure = "sexuality", outcome = "survival_1yr")

#stage not included - analysis will determine if stage is associated with sexuality but can't assume a priori


##### Stage at diagnosis and sexuality
stage_data <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/Stage dataset for analysis.csv")

dag1 <- dagitty("dag {
                      sexuality -> stage
                      AGE -> survival_1yr
                      AGE -> sexuality
                      ETHNICITY <-> sexuality 
                      ETHNICITY <-> AGE
                      AGE -> SITE_ICD10_O2_3CHAR
                      ETHNICITY -> SITE_ICD10_O2_3CHAR
                      SITE_ICD10_O2_3CHAR -> survival_1yr 
                      IMD19_DECILE_LSOAS -> survival_1yr
                      sexuality <-> IMD19_DECILE_LSOAS 
                }")

plot(dag1)
adjustmentSets(dag1, exposure = "sexuality", outcome = "survival_1yr")

