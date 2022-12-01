#Load packages
pacman::p_load(tidyverse, patchwork, ggsignif,
               gtsummary, janitor, rstatix,
               scales, flextable, here,rio,modelsummary)
#Source the epicalc package- set this to the file path where you saved the epicalc_v3 file
source(here("code","epicalc_v3.R"))
# source("code/epicalc_v3.R")

#Load data
# evansData<-rio::import("data/evans_example_dat.csv", header=T)
mi_onset_10 <- import(here("data","MI_Onset_10.rdata"))
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

# overview the dataset
skimr::skim(mi_onset_10_cat)

# 2. 2*2 table
(crosstable <- mi_onset_10_cat %>%
  group_by(dm_cat) %>%
  count(cvdeath_cat) %>%
  mutate(prop = n/sum(n)))
crosstable[4,3]*crosstable[1,3]/(crosstable[2,3]*crosstable[3,3])

# 3. create a logit regre
log_1 <- glm(cvdeath_cat ~ dm_cat,
             data = mi_onset_10_cat,
             family = binomial(link = "logit"))
(log_1 %>% tidy(conf.int=T))
(log_1 %>% tidy(exponentiate=T,conf.int=T))

# 5 cvdeath~dm+age
log_2 <- glm(cvdeath_cat ~ dm_cat + age,
             data = mi_onset_10_cat, family = binomial(link = "logit"))
(log_2 %>%  tidy(conf.int=T))
(log_2 %>% tidy(exponentiate=T, conf.int=T))

# 6 10-year increament
exp(10*(log_2 %>%  tidy(conf.int=T))[3,2])

# 7 cvdeath~dm+age+htn+female+phys_activity+educ
log_3 <- glm(cvdeath_cat ~ dm_cat+age+htn_cat+female_cat+phys_cat+educ_cat,
             data = mi_onset_10_cat,
             family = binomial(link = "logit"))
(log_3 %>%  tidy(conf.int=T))
(log_3 %>% tidy(exponentiate=T, conf.int=T))

modelsummary(list(log_1, log_2, log_3),
             exponentiate = T,
             stars = T,
             output = "huxtable") %>%
  export("weekly materials/6 Logistic regression/regression.xlsx")

# 8 htn*dm
log_4 <- glm(cvdeath_cat ~ dm_cat*htn_cat+age+female_cat+phys_cat+educ_cat,
             data = mi_onset_10_cat,
             family = binomial(link = "logit"))
modelsummary(list(log_3, log_4),
             estimate  = "{estimate} ({conf.low}, {conf.high})",
             statistic = NULL,
             exponentiate = T,output = "huxtable")

modelsummary(list(log_3, log_4),
             estimate  = "{estimate} ({conf.low}, {conf.high})",
             statistic = NULL,output = "huxtable")


