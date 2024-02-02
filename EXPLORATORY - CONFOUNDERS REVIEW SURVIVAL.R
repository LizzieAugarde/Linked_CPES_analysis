################ Linked CPES analysis - investigating confounders for survival ###################

#Assessing relationships/assumptions for confounding of sexuality and survival relationship

#Created January 2024 by Lizzie Augarde 
#Change log:
############################################################################# 

#prep
library(tidyverse)

#read in
surv_1yr_data <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Linked CPES-registry analysis/Data/Survival 1yr dataset for analysis.csv")

#converting variables to numerical binary for regression checks 
surv_1yr_data_num <- surv_1yr_data %>% 
  filter(sexuality_bin != "Missing") %>% #removing missing sexuality records
  mutate(survival_1yr = ifelse(survival_1yr == "Yes", 1, 0)) %>%
  mutate(sexuality_bin = ifelse(sexuality_bin == "Sexual minority", 1, 0)) 

surv_1yr_data <- surv_1yr_data %>% 
  filter(sexuality_bin != "Missing")


############ Reviewing each confounding relationship 
#age and survival
surv_by_age <- table(surv_1yr_data$AGE, surv_1yr_data$survival_1yr)
props_surv_by_age <- prop.table(surv_by_age, margin = 1) #frequencies of survival to 1 yr by age
logodds <- log(props_surv_by_age[, "Yes"]/props_surv_by_age[, "No"]) #log odds of survival
plot(rownames(props_surv_by_age), logodds) #weird relationship...

model <- glm(surv_1yr_data_num$survival_1yr ~ surv_1yr_data_num$AGE, family = binomial (link=logit))
summary(model)
exp(model$coefficients)


#age and survival with age as a categorical variable 
surv_1yr_data_num <- surv_1yr_data_num %>% 
  mutate(age_grouped = case_when(AGE < 45 ~ "Under 45",
                                 AGE >= 45 & AGE < 65 ~ "45-64", 
                                 AGE >= 65 ~ "65+")) %>%
  mutate(age_grouped = factor(age_grouped, levels = c("Under 45", "45-64", "65+")))

surv_by_age_cat <- table(surv_1yr_data_num$age_grouped, surv_1yr_data_num$survival_1yr)
chisq.test(surv_by_age_cat)
model <- glm(surv_1yr_data_num$survival_1yr ~ surv_1yr_data_num$age_grouped, family = binomial (link=logit))
summary(model)
exp(model$coefficients)


#age and sexuality
sexuality_by_age <- table(surv_1yr_data$AGE, surv_1yr_data$sexuality_bin)
props_sex_by_age <- prop.table(sexuality_by_age, margin = 1) #frequencies of sexuality status by age
logodds <- log(props_sex_by_age[, "Sexual minority"]/props_sex_by_age[, "Heterosexual"]) #log odds of sexual minority
plot(rownames(props_sex_by_age), logodds)

chisq.test(sexuality_by_age)
model <- glm(surv_1yr_data_num$sexuality_bin ~ surv_1yr_data_num$AGE, family = binomial (link=logit))
summary(model)
exp(model$coefficients)


#ethnicity and sexuality - converting ethnicity to grouped categories
surv_1yr_data <- surv_1yr_data %>%
  mutate(ETHNICITY = case_when(ETHNICITY %in% c("A", "B", "C") ~ "White",
                               ETHNICITY %in% c("D", "E", "F", "G") ~ "Mixed",
                               ETHNICITY %in% c("H", "J", "K", "L") ~ "Asian",
                               ETHNICITY %in% c("M", "N", "P") ~ "Black",
                               ETHNICITY == "R" ~ "Chinese",
                               ETHNICITY == "S" ~ "Any other ethnic group",
                               ETHNICITY == "Z" ~ "Not stated")) %>%
  mutate(ETHNICITY = factor(ETHNICITY, levels = c("White", "Mixed", "Asian", "Black", "Chinese", "Any other ethnic group", "Not stated"))) 

surv_1yr_data_num <- surv_1yr_data_num %>%
  mutate(ETHNICITY = case_when(ETHNICITY %in% c("A", "B", "C") ~ "White",
                               ETHNICITY %in% c("D", "E", "F", "G") ~ "Mixed",
                               ETHNICITY %in% c("H", "J", "K", "L") ~ "Asian",
                               ETHNICITY %in% c("M", "N", "P") ~ "Black",
                               ETHNICITY == "R" ~ "Chinese",
                               ETHNICITY == "S" ~ "Any other ethnic group",
                               ETHNICITY == "Z" ~ "Not stated")) %>%
  mutate(ETHNICITY = factor(ETHNICITY, levels = c("White", "Mixed", "Asian", "Black", "Chinese", "Any other ethnic group", "Not stated"))) 


sexuality_by_eth <- table(surv_1yr_data$ETHNICITY, surv_1yr_data$sexuality_bin)
props_sex_by_eth <- prop.table(sexuality_by_eth, margin = 1) #frequencies of sexuality status by ethnicity

chisq.test(sexuality_by_eth)
model <- glm(surv_1yr_data_num$sexuality_bin ~ surv_1yr_data_num$ETHNICITY, family = binomial (link=logit))
summary(model)
exp(model$coefficients)

#TODO
#AGE -> SITE_ICD10_O2_3CHAR
#ETHNICITY -> SITE_ICD10_O2_3CHAR
#SITE_ICD10_O2_3CHAR -> survival_1yr 
#IMD19_DECILE_LSOAS -> survival_1yr

#sexuality and deprivation
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
