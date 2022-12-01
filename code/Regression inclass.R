#Load packages
pacman::p_load(tidyverse, patchwork, ggsignif,
               gtsummary, janitor, rstatix,
               scales, flextable, here,rio,modelsummary)
#Source the epicalc package- set this to the file path where you saved the epicalc_v3 file
source(here("code","epicalc_v3.R"))
# source("code/epicalc_v3.R")

#Load data
# evansData<-rio::import("data/evans_example_dat.csv", header=T)
dat <- import(here("data","HeightWeight.rdata"))
glimpse(dat)
skimr::skim(dat)

dat <- dat %>%
  mutate(female_cat = factor(female,
                             levels = c(0,1),
                             labels = c("Male","Female")))

# 2 height vs age
dat %>%
  ggplot(aes(x=age, y=height))+
  geom_point()

# 3 delete outliers
dat_new <- dat %>%
  filter(age <= 21)

# 4 table 1
dat_new %>%
  mutate(age_cat = case_when(
    age<15 ~ 1,
    age>=15 & age<=18 ~ 2,
    age>18 ~ 3
  )) %>%
  select(age, age_cat, female) %>%
  tbl_summary(by = female,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 3,
              type = all_categorical() ~ "categorical",
              label = list(
                female ~ "Sex",
                age ~ "Age (years)",
                age_cat ~ "Age category"
              )
  ) %>% add_p()

dat_new <- dat_new %>% mutate(age_cat = case_when(
  age<15 ~ 1,
  age>=15 & age<=18 ~ 2,
  age>18 ~ 3
))

datasummary_balance(~female,
                    data = dat_new[c("age","age_cat","female")],
                    output = "huxtable",
                    fmt = 3,
                    stars = T,
                    dinm_statistic = "p.value")

# 5 height ~ age
dat_new %>%
  lm(height~age, data = .) %>%
  tidy(conf.int = T) %>%
  mutate(across(where(is.numeric),round,digits=3))

# 7 expected height for a 16-year-old
dat_new %>%
  lm(height~age, data=.) %>%
  predict(newdata = data.frame(age = 16))

# 8 scatter of height as precited sex
dat_new %>%
  ggplot(aes(x=female,y=height))+geom_point()

# 10 height~age+female
dat_new %>%
  lm(height~age+female, data = .) %>%
  tidy(conf.int = T) %>%
  mutate(across(where(is.numeric),round,digits=3))

# 15 and 16 association
dat_new %>%
  lm(height~age*female, data=.) %>%
  tidy(conf.int=T) %>%
  mutate(across(where(is.numeric),round,digits=3))


