
setwd("C:/Users/MittlemanMurrayA/Dropbox (Harvard University)/EPI202 Fall/EPI202 Fall 2021/Regression data sets/Framingham")
 
# load the epicalc code to run tabular analyses
library(dplyr)
source("C:/Users/MittlemanMurrayA/Dropbox (Harvard University)/EPI202 Fall/EPI202 Fall 2021/Statistical Software/R/epicalc_v3.R")

# Open the FHS csv file
FHS <- read.csv("FHS_ALL.csv")
head(FHS)

# define hypertension based on measured blood pressure at baseline
FHS$highbp <- ifelse(FHS$sysbp1>=140 | FHS$diabp1>=90,1,0)
table(FHS$highbp)

# show the probability of death
table(FHS$death)

# to create the intercept only model, I created a column of 1's
FHS$ones <- 1
model1 <- glm(FHS$death ~ FHS$ones, family=binomial(link='logit'))
summary(model1)

exp(-0.62092)/(1+exp(-0.62092))

# describe the relationship between hypertension and death during follow-up

# To conduct the tabular analysis and estimate the OR, rather than teh CIR
# you can use the code for tabular analysis of case-control data

crude.FHS <- as.data.frame(FHS %>% group_by(highbp)%>%
          summarise(Ncase=sum(death==1), Ncontrol=sum(death==0)))
crude.FHS.Table<-as.ccTable.new(crude.FHS$Ncase, crude.FHS$Ncontrol)
crude.FHS.Table
summary(crude.FHS.Table, alpha=0.05)

model2 <- glm(FHS$death ~ FHS$highbp, family=binomial(link='logit'))
summary(model2)
exp(coefficients(model2))
exp(confint(model2))

# What is the median age? 
# Define old as over the median age at baseline

median(FHS$age1)
FHS$old <- ifelse(FHS$age1 > median(FHS$age1),1,0)

# Is old associated with hypertension at baseline?
crude.FHS <- as.data.frame(FHS %>% group_by(old)%>%
                             summarise(Ncase=sum(highbp==1), Ncontrol=sum(highbp==0)))
crude.FHS.Table<-as.ccTable.new(crude.FHS$Ncase, crude.FHS$Ncontrol)
crude.FHS.Table
summary(crude.FHS.Table, alpha=0.05)

# Is death assocaited with old among the non-expsoed?
# First filter the dataset to only operate on those without highbp

crude.FHS <- as.data.frame(FHS %>% filter(highbp == 0) %>% group_by(old)%>%
                             summarise(Ncase=sum(death==1), Ncontrol=sum(death==0)))
crude.FHS.Table<-as.ccTable.new(crude.FHS$Ncase, crude.FHS$Ncontrol)
crude.FHS.Table
summary(crude.FHS.Table, alpha=0.05)

# Crude association between hypertension and death
crude.FHS <- as.data.frame(FHS %>% group_by(highbp)%>%
                             summarise(Ncase=sum(death==1), Ncontrol=sum(death==0)))
crude.FHS.Table<-as.ccTable.new(crude.FHS$Ncase, crude.FHS$Ncontrol)
crude.FHS.Table
summary(crude.FHS.Table, alpha=0.05)

# Stratum-specific assoications between hypertension and death stratified on old
# Estimate for old == 0
young.FHS <- as.data.frame(FHS %>% filter(old == 0) %>% group_by(highbp)%>%
                             summarise(Ncase=sum(death==1), Ncontrol=sum(death==0)))
young.FHS.Table<-as.ccTable.new(young.FHS$Ncase, young.FHS$Ncontrol)
young.FHS.Table
summary(young.FHS.Table, alpha=0.05)

# Estimate for old == 1
old.FHS <- as.data.frame(FHS %>% filter(old == 1) %>% group_by(highbp)%>%
                             summarise(Ncase=sum(death==1), Ncontrol=sum(death==0)))
old.FHS.Table<-as.ccTable.new(old.FHS$Ncase, old.FHS$Ncontrol)
old.FHS.Table
summary(old.FHS.Table, alpha=0.05)

# Estimate the association between hypertension and death adjusted for old
stratified.FHS<-as.data.frame(FHS %>%
                group_by(old,highbp)%>%
                summarise(Ncase=sum(death==1),
                Ncontrol=sum(death==0)))
stratified.FHS.Table<-as.ccTable.new(stratified.FHS$Ncase, stratified.FHS$Ncontrol)
stratified.FHS.Table
summary(stratified.FHS.Table, alpha=0.05)

# now fit the adjusted logistic model
model3 <- glm(FHS$death ~ FHS$highbp + FHS$old, family=binomial(link='logit'))
summary(model3)
exp(coefficients(model3))
exp(confint(model3))

# association between hypertension and death adjusted for age (continuous)
model4 <- glm(FHS$death ~ FHS$highbp + FHS$age1, family=binomial(link='logit'))
summary(model4)
exp(coefficients(model4))
exp(confint(model4))

# association between 5 year increment in age and death adjusted for hypertension
1.112769^5

# center age to at the median get a meaningful intercept
FHS$age_c <- FHS$age1-49

model5 <- glm(FHS$death ~ FHS$highbp + FHS$age_c, family=binomial(link='logit'))
summary(model5)
exp(coefficients(model5))
exp(confint(model5))

# use the intercept to estimate the probability of death
# at the median age among those without hypertension
0.343597/(1 + 0.343597)

# Create a categorical age variable
FHS$age4050 <- ifelse(FHS$age1 > 40 & FHS$age1 <= 50,1,0)
FHS$age5060 <- ifelse(FHS$age1 > 50 & FHS$age1 <= 60,1,0)
FHS$agegt60 <- ifelse(FHS$age1 > 60,1,0)

table(FHS$age4050)
table(FHS$age5060)
table(FHS$agegt60)

# Fit logistic model with categorical age variables
model6 <- glm(FHS$death ~ FHS$highbp + FHS$age4050 + FHS$age5060 + FHS$agegt60, family=binomial(link='logit'))
summary(model6)
exp(coefficients(model6))
exp(confint(model6))

# Now adjust for glucose (continuous)
model7 <- glm(FHS$death ~ FHS$highbp + FHS$glucose1, family=binomial(link='logit'))
summary(model7)
exp(coefficients(model7))
exp(confint(model7))

# Create a categorical variable to encode glucose level
FHS$gluccat <- as.factor(ifelse(FHS$glucose1 <  76, 1, 
                             ifelse(FHS$glucose1 <= 99, 2,
                             ifelse(FHS$glucose1 <=126, 3,
                             ifelse(FHS$glucose1 > 126, 4, NA)))))
table(FHS$gluccat)


# Now adjust for glucose (in categories)
model8 <- glm(FHS$death ~ FHS$highbp + as.factor(FHS$gluccat), family=binomial(link='logit'))
summary(model8)
exp(coefficients(model8))
exp(confint(model8))

# stratify by gender -- first create a variable that equals 1 if a woman and 0 if a man
FHS$women <- ifelse(FHS$sex1 == 2,1,0)
table(FHS$women)

# Conduct stratum-specific and stratified tabular analysis

# Estimate in the men only
men.FHS <- as.data.frame(FHS %>% filter(women == 0) %>% group_by(highbp)%>%
                             summarise(Ncase=sum(death==1), Ncontrol=sum(death==0)))
men.FHS.Table<-as.ccTable.new(men.FHS$Ncase, men.FHS$Ncontrol)
men.FHS.Table
summary(men.FHS.Table, alpha=0.05)

# Estimate in the women only
women.FHS <- as.data.frame(FHS %>% filter(women == 1) %>% group_by(highbp)%>%
                           summarise(Ncase=sum(death==1), Ncontrol=sum(death==0)))
women.FHS.Table<-as.ccTable.new(women.FHS$Ncase, women.FHS$Ncontrol)
women.FHS.Table
summary(women.FHS.Table, alpha=0.05)

# Now look at the crdue nd stratified analysis, including the H test for EMM
stratified.FHS<-as.data.frame(FHS %>%
                group_by(women,highbp)%>%
                summarise(Ncase=sum(death==1),
                Ncontrol=sum(death==0)))
stratified.FHS.Table<-as.ccTable.new(stratified.FHS$Ncase, stratified.FHS$Ncontrol)
stratified.FHS.Table
summary(stratified.FHS.Table, alpha=0.05) 
 
# Create an interaction term between hypertension and being a woman
FHS$highbpwomen <- FHS$highbp*FHS$women

# Run the logistic model for highbp adjusted for women and allowing for EMM by including the interaction term
model9 <- glm(FHS$death ~ FHS$highbp + FHS$women + FHS$highbpwomen, family=binomial(link='logit'))
summary(model9)
exp(coefficients(model9))
exp(confint(model9))

# Now, display the point estimate and 95% CI for highbp among men (women=0)
# we can read this directly from the model output using the term for highbp
# or we can calculate it using the variance covariance matrix.
# Note that in the output for Model9 highbp is the second term
# we refer to elements in the variance covariance matrix using the syntax
# vcov(Modelname)[row, col] where Modelname is the model that you are working 
# with and [row, col] specifies which element of the variance covariance matrix 
# you are accessing

# View the variance-covaraince matrix
vcov(model9)

beta_highbp_male <- model9$coefficients[2]
OR_highbp_male <- exp(beta_highbp_male)
CI_LB_highbp_male <- exp(beta_highbp_male - 1.96 * sqrt(vcov(model9)[2,2]))
CI_UB_highbp_male <- exp(beta_highbp_male + 1.96 * sqrt(vcov(model9)[2,2]))
rbind(OR_highbp_male, CI_LB_highbp_male, CI_UB_highbp_male)

# Now display the point estimate and 95% CI for height among women (women=1)
# Note that we need to add the coefficients for women and the interaction term (highbpwomen)
# to obtain the point estimate.
# We need to calculate the variance of the sum of the coefficients as:
# var(women) + var(highbpwomen) + 2*covariance(highbp, highbpwomen)

beta_highbp_female <- model9$coefficients[2] + model9$coefficients[4]
OR_highbp_female <- exp(beta_highbp_female)
CI_LB_highbp_female <- exp(beta_highbp_female - 1.96 * sqrt(vcov(model9)[2,2] + vcov(model9)[4,4] + 2*vcov(model9)[2,4]))
CI_UB_highbp_female <- exp(beta_highbp_female + 1.96 * sqrt(vcov(model9)[2,2] + vcov(model9)[4,4] + 2*vcov(model9)[2,4]))
rbind(OR_highbp_female, CI_LB_highbp_female, CI_UB_highbp_female)



# Estimate the OR for highbp among men
lincom highbp, or                

# Estimate the OR for highbp among women
lincom highbp + highbpwomen, or  













