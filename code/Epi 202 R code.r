#Epi 202 R Code
#August 26, 2018
#Samantha Molsberry
#Updated to epicalc_v3 on October 27, 2020

#Set Working Directory - set this to the file path where you saved your dataset 
#setwd("C:/Users/Sam/Dropbox/Epi202 TA Materials Fall 2018/Homework/Sam Code") 

#Load packages
pacman::p_load(tidyverse, patchwork, ggsignif, 
               gtsummary, janitor, rstatix,
               scales, flextable, here,rio)
#Source the epicalc package- set this to the file path where you saved the epicalc_v3 file 
source(here("code","epicalc_v3.R"))
# source("code/epicalc_v3.R")
 
#Load data
# evansData<-rio::import("data/evans_example_dat.csv", header=T)
demo <- import(here("data","MI_Onset_10.rdata"))
glimpse(demo)

demo %>% 
  tabyl(evermarj) %>% 
  adorn_percentages('row') #%>% 
  # adorn_pct_formatting(digits = 2) %>% 
  # adorn_totals(c("row","col"))
  
  
#Reminders before beginning:
# 1) Exposure must be binary & exposed must have a higher value of your indicator than the unexposed
# 2) Outcome must be binary, need to know values that indicates case v. non-case/control
# 3) Stratification factors must be categorical - transform any continuous variables as necessary
# 4) In stratified analyses, you must list all of the stratification factors before the exposure variable in the 'group_by' command
# 5) You do not need to change the names of any of the outputted objects, but if you do so  make sure to also change any subsequent references to those objects

##################################
### Count data: Crude Analysis ###
##################################

crude.evans<-as.data.frame(evansData %>% #change this to your dataset name
                             group_by(HTN)%>% #change HTN to your exposure of interest
                             summarise(Ncase=sum(CHD=='1'), #change CHD to your outcome of interest
                                       Nnoncase=sum(CHD=='0'))) #change CHD to your outcome of interest
crude.evans.riskTable<-as.riskTable.new(crude.evans$Ncase, crude.evans$Nnoncase) #change variable names as necessary
summary(crude.evans.riskTable, alpha=0.05) #changes names & alpha level as necessary

#######################################
### Count data: Stratified Analysis ###
#######################################

stratified.evans<-as.data.frame(evansData %>% #change evansData to your dataset name
                                  group_by(SMK,HTN)%>% #change SMK to your modifier(s), change HTN to your exposure
                                  summarise(Ncase=sum(CHD=='1'), #change CHD to your outcome, change '1' to the level that indicates an event
                                            Nnoncase=sum(CHD=='0'))) #change CHD to your outcome, change '0' to the level that indicates no event
stratified.evans.riskTable<-as.riskTable.new(stratified.evans$Ncase, stratified.evans$Nnoncase) #change names as necessary
summary(stratified.evans.riskTable, alpha=0.05) #changes names & alpha level as necessary

########################################
### Person-time data: Crude Analysis ###
########################################

crude.evans.rate<-as.data.frame(evansData %>% group_by(HTN)%>% #change 'evansData' to your dataset name, change HTN to the exposure of interest
                            summarise(Ncase=sum(CHD=='1'),#change CHD to your outcome, change '1' to the level that indicates an event
                                      PY=sum(person_time))) #change person_time to the variable name containing follow-up time 
crude.evans.rateTable<-as.rateTable.new(crude.evans.rate$Ncase, crude.evans.rate$PY) #change names as necessary
summary(crude.evans.rateTable, alpha=0.05) #change names & alpha level as necessary

#############################################
### Person-time data: Stratified Analysis ###
#############################################

stratified.evans.rate<-as.data.frame(evansData %>% group_by(SMK, HTN)%>% #change evansData to your dataset name, #change SMK to your modifier(s), change HTN to your exposure
                                  summarise(Ncase=sum(CHD=='1'), #change CHD to your outcome, change '1' to the level that indicates an event
                                            PY=sum(person_time))) #change person_time to the variable name containing follow-up time
stratified.evans.rateTable<-as.rateTable.new(stratified.evans.rate$Ncase, stratified.evans.rate$PY) #change names as necessary
summary(stratified.evans.rateTable, alpha=0.05) #change names & alpha level as necessary

########################################
### Case-control data: Crude Analysis ##
########################################

evansCC<-evansData%>%filter(!is.na(caco)) #change dataset name & outcome variable name as necessary
crude.evans.cc<-as.data.frame(evansCC %>% #change evansData to your dataset name
                          group_by(HTN)%>% #change HTN to your exposure of interest
                          summarise(Ncase=sum(caco=='case'), #change caco to your case-control status indicator variable, change 'case' the value that indicates a case event 
                                    Ncontrol=sum(caco=='control'))) #change caco to your case-control status indicator variable, change 'control' the value that indicates a control observation

crude.evans.ccTable<-as.ccTable.new(crude.evans.cc$Ncase, crude.evans.cc$Ncontrol)#change names as necessary
summary(crude.evans.ccTable, alpha=0.05) #change names & alpha level as necessary

##############################################
### Case-control data: Stratified Analysis ###
##############################################

stratified.evans.cc<-as.data.frame(evansCC %>% #change evansData to your dataset name
                                group_by(SMK,HTN)%>% #change SMK to your stratification factor(s) name(s) of interest, HTN to your exposure of interest
                                summarise(Ncase=sum(caco=='case'), #change caco to your case-control status indicator variable, change 'case' the value that indicates a case event
                                          Ncontrol=sum(caco=='control'))) #change caco to your case-control status indicator variable, change 'control' the value that indicates a control observation

stratified.evans.ccTable<-as.ccTable.new(stratified.evans.cc$Ncase, stratified.evans.cc$Ncontrol) #change names as necessary
summary(stratified.evans.ccTable, alpha=0.05) #change names & alpha level as necessary

