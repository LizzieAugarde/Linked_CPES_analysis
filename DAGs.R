################ Linked CPES analysis - DAGs ###################

#Creating DAGs for assessing confounders to include in regression models

#Created January 2024 by Lizzie Augarde 
#Change log:
############################################################################# 

#prep
library(dagitty)
library(tidyverse)

##### Survival
surv_1yr_data <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/Survival 1yr dataset for analysis.csv")

dag1 <- dagitty("dag {
                      AGE -> survival_1yr
                      sexuality -> survival_1yr
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

#stage not included - analysis will determine if stage is associated with sexuality but can't assume a priori

#sexuality independently associated with deprivation? 
imd_table <- table(surv_1yr_data$sexuality_bin, surv_1yr_data$IMD19_DECILE_LSOAS, exclude = NA)
prop.table(imd_table, 1) %>% {. * 100} %>% round(2)

imd_check <- surv_1yr_data %>% 
  filter(sexuality_bin != "Missing") %>%
  mutate(sexuality_bin = ifelse(sexuality_bin == "Heterosexual", 0, 1)) %>%
  mutate(IMD19_DECILE_LSOAS = factor(IMD19_DECILE_LSOAS, levels = c("1 - most deprived", "2", "3", "4", "5", "6", "7", "8", "9", "10 - least deprived")))
  
sex_by_imd <- table(imd_check$IMD19_DECILE_LSOAS, imd_check$sexuality_bin)
chisq.test(sex_by_imd) #p v small
model <- glm(imd_check$sexuality_bin ~ imd_check$IMD19_DECILE_LSOAS, family = binomial (link=logit))
summary(model)
exp(model$coefficients)
#sexuality independent linear association with deprivation
####odds of being minority sexuality decrease with increasing deprivation, p values almost all small 
