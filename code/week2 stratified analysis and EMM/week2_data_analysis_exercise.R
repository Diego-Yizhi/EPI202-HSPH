#Load packages
pacman::p_load(tidyverse, patchwork, ggsignif,
               gtsummary, janitor, rstatix,
               scales, flextable, here,rio)
#Source the epicalc package- set this to the file path where you saved the epicalc_v3 file
source(here("code","epicalc_v3.R"))
# source("code/epicalc_v3.R")

#Load data
# evansData<-rio::import("data/evans_example_dat.csv", header=T)
mi_onset_10 <- import(here("data","MI_Onset_10.rdata"))
glimpse(mi_onset_10)
skimr::skim(mi_onset_10)

# adjust factor variables
fac_vars <- names(mi_onset_10)[c(1,3:10,12:13)]
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
         sedentary = fct_recode(phys_activity, "yes"="0", "no"="1","no"="2")
         ) %>%
  mutate(dead_cat = fct_rev(dead_cat),
         sedentary = fct_rev(sedentary))

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

log_model <- glm(dead_cat ~ sedentary + dm_cat +
                   htn_cat + female_cat + age,
                 data = mi_onset_10_cat,
                 family = binomial(link = 'logit'))
summary(log_model)
tidy(log_model)

log_model %>%
  tbl_regression(exponentiate = T,
                 label = list(
                   sedentary ~ "Sedentary lifestyle at baseline",
                   dm_cat ~ "Diabetes",
                   htn_cat ~ "Hypertension",
                   female_cat ~ "Gender",
                   age ~ "Age (years)"
                 )) %>%
  bstfun::add_inline_forest_plot()

# Q1b
log_model_emm <- glm(dead_cat~sedentary*dm_cat+htn_cat+female_cat+age,
                     data = mi_onset_10_cat,
                     family = binomial(link = "logit"))
summary(log_model_emm)
tidy(log_model_emm)

log_model_emm %>%
  tbl_regression(exponentiate = T,
                 label = list(
                   sedentary ~ "Sedentary lifestyle at baseline",
                   dm_cat ~ "Diabetes",
                   htn_cat ~ "Hypertension",
                   female_cat ~ "Gender",
                   age ~ "Age (years)"
                 )) %>%
  bstfun::add_inline_forest_plot()

log_model_emm %>% bruceR::model_summary()














