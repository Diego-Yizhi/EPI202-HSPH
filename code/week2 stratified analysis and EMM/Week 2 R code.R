#Load packages
pacman::p_load(tidyverse, patchwork, ggsignif, 
               gtsummary, janitor, rstatix,
               scales, flextable, here,rio)
#Source the epicalc package- set this to the file path where you saved the epicalc_v3 file 
source(here("code","epicalc_v3.R"))
# source("code/epicalc_v3.R")

#Load data
# evansData<-rio::import("data/evans_example_dat.csv", header=T)
MI_Onset_10 <- import(here("data","MI_Onset_10.rdata"))

## 1.	What is the prevalence of having a history of reporting ever using marijuana 
##    at the time of the baseline interview. 

## Show the table result
table(MI_Onset_10$evermarj)



## you can save the table and ask R to display the entries as percentages too:
marjtable <- table(MI_Onset_10$evermarj)
prop.table(marjtable)

##  2a.	Compute the incidence rate of death from cardiovascular causes among those 
##      with and without a history of reporting ever using marijuana at baseline and 
##      describe the relationship on the multiplicative scale.

##  2b.	Calculate and interpret the incidence rate ratio and its 95% confidence 
##      interval. 

##  2c.	Calculate and interpret the incidence rate difference and its 95% confidence
##      interval for the association between reporting a history of ever reporting 
##      marijuana on the baseline interview and the incidence rate of death from 
##      cardiovascular causes.


## This command will produce a table with the summary of the person-time results
crude.MImarj <-as.data.frame(MI_Onset_10 %>% group_by(evermarj)%>%
                                  summarise(Ncase=sum(cvdeath=='1'),
                                            PY=sum(follow_up)))
print (crude.MImarj)

## These commands will produce the rate analysis table from the summary data you produced 

crude.MI_Onset_10.rateTable<-as.rateTable.new(crude.MImarj$Ncase, crude.MImarj$PY)
print (crude.MI_Onset_10.rateTable)
summary(crude.MI_Onset_10.rateTable, alpha=0.05)

##  3a.	Evaluate whether the crude association you computed above is confounded 
##      by age. Based on your subject matter knowledge, illustrate in a directed acyclic 
##      graph (DAG) the relationship between age, history of ever using marijuana and 
##      the incidence rate of death from cardiovascular causes. 

##  3b.	Does age have the characteristics of a confounder of the marijuana --> 
##      cardiovascular mortality relationship?  Evaluate the relevant associations.  
##      Note that the variable age_cat is defined as age_cat=1 if age<50; age_cat=2 
##      if age >=50 to <65; age_cat=3 if age>=65.

table(MI_Onset_10$age_cat, MI_Onset_10$evermarj)

## Note the positvity violation - there are no marijuana users in age_cat 3 (above 65)

## Now conduct a stratified analysis

stratified.MImarj<-as.data.frame(MI_Onset_10 %>% group_by(age_cat, evermarj)%>%
                                       summarise(Ncase=sum(cvdeath==1),
                                                 PY=sum(follow_up)))
print (stratified.MImarj)

# drop age_cat == 3, because of the positivity violation
stratified.MImarj2 <- subset(stratified.MImarj, age_cat<3)
print (stratified.MImarj2)

# Now run stratified analysis on the two remaining strata
stratified.MImarj.rateTable <-as.rateTable.new(stratified.MImarj2$Ncase, stratified.MImarj2$PY)
print (stratified.MImarj.rateTable)
summary(stratified.MImarj.rateTable, alpha=0.05)