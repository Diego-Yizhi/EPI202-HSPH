#Load packages
pacman::p_load(tidyverse, patchwork, ggsignif,
               gtsummary, janitor, rstatix,
               scales, flextable, here,rio)
#Source the epicalc package- set this to the file path where you saved the epicalc_v3 file
source(here("code","epicalc_v3.R"))
# source("code/epicalc_v3.R")

#Load data
# evansData<-rio::import("data/evans_example_dat.csv", header=T)
dat <- import(here("data","HeightWeight.rdata"))
glimpse(dat)
skimr::skim(dat)

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

# 5 height ~ age
dat_new %>%
  lm(height~age, data = .) %>%
  tidy(conf.int = T) %>%
  mutate(across(where(is.numeric),round,digits=3))

dat_new %>%
  lm(height~age, data=.) %>%
  report::report()

# 7 predict age=16



