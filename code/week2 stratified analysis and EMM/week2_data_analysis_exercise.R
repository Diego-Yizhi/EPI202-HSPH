#Load packages
pacman::p_load(tidyverse, patchwork, ggsignif,
               gtsummary, janitor, rstatix,
               scales, flextable, here,rio,modelsummary,
               epiR)
#Source the epicalc package- set this to the file path where you saved the epicalc_v3 file
source(here("code","epicalc_v3.R"))
# source("code/epicalc_v3.R")

#Load data
# evansData<-rio::import("data/evans_example_dat.csv", header=T)
mi_onset_10 <- import(here("data","MI_Onset_10.rdata"))
glimpse(mi_onset_10)
skimr::skim(mi_onset_10)
mi_onset_10 <- mi_onset_10 %>% mutate(sedendm = case_when(
  phys_activity==0 & dm==1 ~ 1,
  phys_activity!=0 & dm==1 ~ 2,
  phys_activity==0 & dm!=1 ~ 3,
  phys_activity!=0 & dm!=1 ~ 4
))


# adjust factor variables
fac_vars <- names(mi_onset_10)[c(1,3:10,12:14)]
mi_onset_10[fac_vars] <- lapply(mi_onset_10[fac_vars],factor)

mi_onset_10_cat <- mi_onset_10 %>%
  mutate(age_cat = fct_recode(age_cat,
                              "<50yrs"="1","50-64yrs"="2","65+yrs"="3"),
         female_cat=fct_recode(female,"F"="1","M"="0"),
         married_cat=fct_recode(married, "yes"="1","no"="0"),
         educ_cat=fct_recode(educ,"<HS"="1","HS"="2",">HS"="3"),
         dm_cat=fct_recode(dm,"yes"="1","no"="0"),
         htn_cat=fct_recode(htn,"yes"="1","no"="0"),
         phys_cat=fct_recode(phys_activity,"<1/wk"="0","1-3/wk"="1","4+/wk"="2"),
         evermarj_cat=fct_recode(evermarj,"yes"="1","no"="0"),
         dead_cat=fct_recode(dead,"yes"="1","no"="0"),
         cvdeath_cat=fct_recode(cvdeath,"CVD death"="1","not CVD death"="0"),
         sedentary = fct_recode(phys_activity, "yes"="0", "no"="1","no"="2"),
         sedendm_cat = fct_recode(sedendm, "sedentary and diabetes"="1",
                                  "no sedentary and diabetes"="2", "sedentary and no diabetes"="3",
                                  "no sedentary and no diabetes"="4")
         ) %>%
  mutate(dead_cat = fct_rev(dead_cat),
         sedentary = fct_rev(sedentary),
         sedendm_cat = fct_rev(sedendm_cat))

# 1 Classroom -----------------------------------------------------------------------------------------------------

  # 1 prevalence of evermarj ----------------------------------------------------------------------------------------
  # Hint: ctrl+shift+R
  mi_onset_10_cat %>%
    tabyl(evermarj_cat) %>%
    adorn_rounding(digits = 4)
  # n=113, p=113/3712=0.0304

  # 2 crude ------------------------------------------------------------------------------------------------
  crude_mi_rate <- as.data.frame(mi_onset_10_cat %>%
                                   group_by(evermarj_cat) %>%
                                   summarise(Ncase=sum(cvdeath_cat=="CVD death"),
                                             PY=sum(follow_up)))
  (crude_mi_rateT <- as.rateTable.new(crude_mi_rate$Ncase,
                                     crude_mi_rate$PY))
  summary(crude_mi_rateT, alpha=0.05)

  mi_onset_10_cat %>%
    group_by(age_cat) %>%
    count(evermarj_cat) %>%
    mutate(percent = n/sum(n))

  mi_onset_10_cat %>%
    tabyl(age_cat, evermarj_cat) %>%
    adorn_percentages("col") %>%
    adorn_rounding(digits = 4)

  # 3 stratified by age ---------------------------------------------------------------------------------------------
  mi_onset_10_cat %>%
    group_by(age_cat, evermarj_cat) %>%
    count(cvdeath_cat) %>%
    mutate(prop=n/sum(n))

  mi_onset_10_cat %>%
    tabyl(cvdeath_cat,evermarj_cat,age_cat) %>%
    adorn_percentages("col")

  # create new age categories variable (<50, ≥50)
  mi_onset_10_cat %>%
    mutate(age50 = case_when(
      age<50 ~ "<50yrs",
      age>=50 ~ "≥50yrs"
    ),
    evermarj_cat=fct_relevel(evermarj_cat,"yes","no"),
    cvdeath_cat=fct_relevel(cvdeath_cat,"CVD death","not CVD death")) %>%
    tabyl(cvdeath_cat, evermarj_cat, age50) %>%
    adorn_percentages("col")

  mi_onset_10_cat <- mi_onset_10_cat %>%
    mutate(age50 = case_when(
      age >= 50 ~ 1,
      TRUE ~ 0
    ))
  mi_onset_10_cat$age50 <-  factor(mi_onset_10_cat$age50,
                                   levels = c(1,0),
                                   labels = c("yes","no"))

  stratified_mi_rate <- as.data.frame(mi_onset_10_cat %>%
                                        group_by(age50, evermarj_cat) %>%
                                        summarise(Ncase=sum(cvdeath_cat=="CVD death"),
                                                  PY=sum(follow_up)))
  stratified_mi_rateT <- as.rateTable.new(stratified_mi_rate$Ncase,
                                          stratified_mi_rate$PY)
  summary(stratified_mi_rateT, alpha=0.05)


# 2 A2 ------------------------------------------------------------------------------------------------------------



crude_mi_ci <- as.data.frame(mi_onset_10_cat %>% group_by(sedentary) %>%
                               summarise(Ncase=sum(dead_cat=="yes"),
                                         Noncase=sum(dead_cat=="no")))
(crude_mi_ci_T <- as.riskTable.new(crude_mi_ci$Ncase, crude_mi_ci$Noncase))
summary(crude_mi_ci_T, alpha=0.05)

stratified_mi_ci <- as.data.frame(mi_onset_10_cat %>% group_by(age_cat,sedentary) %>%
                                    summarise(Ncase=sum(dead_cat=="yes"),
                                              Noncase=sum(dead_cat=="no")))
(stratified_mi_ci_T <- as.riskTable.new(stratified_mi_ci$Ncase, stratified_mi_ci$Noncase))
summary(stratified_mi_ci_T,alpha=0.05)

mi_onset_10_cat %>%
  group_by(age_cat) %>%
  count(sedentary) %>%
  mutate(prop=n/sum(n))

crude_mi_ci <- as.data.frame(mi_onset_10_cat %>%
                               filter(age_cat == "<50yrs") %>%
                               group_by(sedentary) %>%
                               summarise(Ncase=sum(dead_cat=="yes"),
                                         Noncase=sum(dead_cat=="no")))
(crude_mi_ci_T <- as.riskTable.new(crude_mi_ci$Ncase, crude_mi_ci$Noncase))
summary(crude_mi_ci_T, alpha=0.05)

crude_mi_ci <- as.data.frame(mi_onset_10_cat %>%
                               filter(age_cat == "50-64yrs") %>%
                               group_by(sedentary) %>%
                               summarise(Ncase=sum(dead_cat=="yes"),
                                         Noncase=sum(dead_cat=="no")))
(crude_mi_ci_T <- as.riskTable.new(crude_mi_ci$Ncase, crude_mi_ci$Noncase))
summary(crude_mi_ci_T, alpha=0.05)

crude_mi_ci <- as.data.frame(mi_onset_10_cat %>%
                               filter(age_cat == "65+yrs") %>%
                               group_by(sedentary) %>%
                               summarise(Ncase=sum(dead_cat=="yes"),
                                         Noncase=sum(dead_cat=="no")))
(crude_mi_ci_T <- as.riskTable.new(crude_mi_ci$Ncase, crude_mi_ci$Noncase))
summary(crude_mi_ci_T, alpha=0.05)


# 3 A3 ---------------------------------------------------------------------------------------------------------------
# descriptive table
mi_onset_10_cat %>%
  select(age, age_cat, female_cat, married_cat,
         educ_cat, dm_cat, htn_cat, sedentary) %>%
  tbl_summary(
    by = sedentary,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 3,
    type = all_categorical() ~ "categorical",
    label = list(
      sedentary ~ "Sendentary",
      age ~ "Age (years)",
      age_cat ~ "Age category",
      female_cat ~ "Gender",
      married_cat ~ "Marrige status",
      educ_cat ~ "Education level",
      dm_cat ~ "Diabetes",
      htn_cat ~ "Hypertension"
    )
  ) %>% add_p()

vars_mi <- names(mi_onset_10_cat)[c(2,3,15:19)]
fac_vars_mi <- names(mi_onset_10_cat)[c(3,15:19)]
table1 <- CreateTableOne(vars = vars_mi,
                         strata = "sedentary",
                         data = mi_onset_10_cat,
                         addOverall = T, includeNA = T)
table1_print <- print(table1,showAllLevels = T)
export(table1_print, "HW/HW3/A3_tableone.xlsx", rowNames = T,
       colNames = T)
write.csv(table1_print, "HW/HW3/A3_tableone.csv")

mi_onset_10_cat %>%
  group_by(sedentary) %>%
  count(dead_cat) %>%
  mutate(prop = n/sum(n))

mi_onset_10_cat %>%
  filter(dm==1) %>%
  group_by(sedentary) %>%
  count(dead_cat) %>%
  mutate (prop = n/sum(n))

mi_onset_10_cat %>%
  filter(dm==0) %>%
  group_by(sedentary) %>%
  count(dead_cat) %>%
  mutate (prop = n/sum(n))


# 4 A4 regression -------------------------------------------------------------------------------------------------

log_1 <- mi_onset_10_cat %>%
  glm(dead~sedentary+dm_cat+htn_cat+female_cat+age,
      data = .,
      family=binomial(link = "logit"))
exp(5*(log_1 %>% tidy(conf.int = T))[6,2])
exp(5*(log_1 %>% tidy(conf.int = T))[6,6])
exp(5*(log_1 %>% tidy(conf.int = T))[6,7])


log_2 <- mi_onset_10_cat %>%
  glm(dead~sedentary*dm_cat+htn_cat+female_cat+age,
      data = .,
      family=binomial(link = "logit"))
(log_2 %>% tidy(conf.int = T))
exp((log_2 %>% tidy(conf.int = T))[2,2]+(log_2 %>% tidy(conf.int = T))[7,2])
exp((log_2 %>% tidy(conf.int = T))[2,2])

log_crude <- mi_onset_10_cat %>%
  glm(dead~sedentary,
      data = .,
      family=binomial(link = "logit"))
(log_crude %>% tidy(exponentiate=T,conf.int = T))

log_agesex <- mi_onset_10_cat %>%
  glm(dead~sedentary+age+female_cat,
      data = .,
      family=binomial(link = "logit"))
(log_agesex %>% tidy(exponentiate=T,conf.int = T))

log_full <- mi_onset_10_cat %>%
  glm(dead~sedentary+age+female_cat+dm_cat+htn_cat,
      data = .,
      family=binomial(link = "logit"))
(log_full %>% tidy(exponentiate=T,conf.int = T))

modelsummary(list(log_crude, log_agesex, log_full),
             exponentiate = T,
             estimate  = "{estimate} ({conf.low}, {conf.high})",
             statistic = NULL,
             output = "huxtable")

log_full <- mi_onset_10_cat %>%
  glm(dead~sedentary+age+female_cat+dm_cat+htn_cat,
      data = .,
      family=binomial(link = "logit"))
(log_full %>% tidy(conf.int = T))
exp(5*(log_full %>% tidy(conf.int = T))[3,2])
exp(5*(log_full %>% tidy(conf.int = T))[3,6])
exp(5*(log_full %>% tidy(conf.int = T))[3,7])

log_interaction <- mi_onset_10_cat %>%
  glm(dead~sedentary*dm_cat+age+female_cat+htn_cat,
      data = .,
      family=binomial(link = "logit"))
(log_interaction %>% tidy(exponentiate = T, conf.int = T))
beta_sen_dm <- log_interaction$coefficients[2]+log_interaction$coefficients[7]
CI_LB_sen_dm <- beta_sen_dm-
  1.96*sqrt(vcov(log_interaction)[2,2]+
  vcov(log_interaction)[7,7]+
  2*vcov(log_interaction)[2,7])
CI_UB_sen_dm <- beta_sen_dm+
  1.96*sqrt(vcov(log_interaction)[2,2]+
              vcov(log_interaction)[7,7]+
              2*vcov(log_interaction)[2,7])
rbind(beta_sen_dm, CI_LB_sen_dm, CI_UB_sen_dm)
exp(rbind(beta_sen_dm, CI_LB_sen_dm, CI_UB_sen_dm))

epi.interaction(model = log_interaction,
                param = "product",
                coef = c(2,3,7),
                conf.level = 0.95)
