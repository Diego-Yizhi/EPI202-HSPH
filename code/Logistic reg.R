#Load packages
pacman::p_load(tidyverse, patchwork, ggsignif,
               gtsummary, janitor, rstatix,
               scales, flextable, here,rio,modelsummary)
#Source the epicalc package- set this to the file path where you saved the epicalc_v3 file
source(here("code","epicalc_v3.R"))
# source("code/epicalc_v3.R")

#Load data
# evansData<-rio::import("data/evans_example_dat.csv", header=T)
cvdeath <- import(here("data","CVdeath_Case_control.rdata"))

# adjust factor variables
fac_vars <- names(cvdeath)[c(3:10)]
cvdeath[fac_vars] <- lapply(cvdeath[fac_vars],factor)

cvdeath_cat <- cvdeath %>%
  mutate(age_cat = fct_recode(age_cat,
                              "<50yrs"="1","50-64yrs"="2","65+yrs"="3"),
         female_cat=fct_recode(female,"F"="1","M"="0"),
         married_cat=fct_recode(married, "yes"="1","no"="0"),
         educ_cat=fct_recode(educ,"<HS"="1","HS"="2",">HS"="3"),
         dm_cat=fct_recode(dm,"yes"="1","no"="0"),
         htn_cat=fct_recode(htn,"yes"="1","no"="0"),
         phys_cat=fct_recode(phys_activity,"<1/wk"="0","1-3/wk"="1","4+/wk"="2"),
         case_cat = fct_recode(case, "case"="1","non-case"="0")
  )

# overview the dataset
skimr::skim(cvdeath_cat)

# 2. 2*2 table
(crosstable <- cvdeath_cat %>%
  group_by(dm_cat) %>%
  count(case_cat) %>%
  mutate(prop = n/sum(n)))
crosstable[4,3]*crosstable[1,3]/(crosstable[2,3]*crosstable[3,3])

# 3. create a logit regre
log_1 <- glm(case_cat ~ dm_cat,
             data = cvdeath_cat,
             family = binomial(link = "logit"))
(log_1 %>% tidy(conf.int=T))
(log_1 %>% tidy(exponentiate=T,conf.int=T))

# 5 cvdeath~dm+age
log_2 <- glm(case_cat ~ dm_cat + age,
             data = cvdeath_cat, family = binomial(link = "logit"))
(log_2 %>%  tidy(conf.int=T))
(log_2 %>% tidy(exponentiate=T, conf.int=T))

# 6 10-year increament
exp(10*(log_2 %>%  tidy(conf.int=T))[3,2])

# 7 cvdeath~dm+age+htn+female+phys_activity+educ
log_3 <- glm(case_cat ~ dm_cat+age+htn_cat+female_cat+phys_cat+educ_cat,
             data = cvdeath_cat,
             family = binomial(link = "logit"))
(log_3 %>%  tidy(conf.int=T))
(log_3 %>% tidy(exponentiate=T, conf.int=T))

modelsummary(list(log_1, log_2, log_3),
             exponentiate = T,
             stars = T,
             output = here("weekly materials","6 Logistic regression",
                           "log_1 to log_3.docx"))
modelsummary(list(log_1, log_2, log_3),
             exponentiate = T,
             stars = T,
             output = "huxtable")

# 8 htn*dm
log_4 <- glm(case_cat ~ dm_cat*htn_cat+age+female_cat+phys_cat+educ_cat,
             data = cvdeath_cat,
             family = binomial(link = "logit"))
modelsummary(list(log_3, log_4),
             estimate  = "{estimate} ({conf.low}, {conf.high})",
             statistic = NULL,
             exponentiate = T,
             output = here("weekly materials","6 Logistic regression",
                                            "log_3 and 4.docx"))
modelsummary(list(log_3,log_4),
             estimate  = "{estimate} ({conf.low}, {conf.high})",
             exponentiate = T,
             statistic = NULL,
             output = "huxtable")


