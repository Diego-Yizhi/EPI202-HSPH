
# 1. Read the dataset into your preferred software package.
CVdeath <- read.csv("C:/Users/MittlemanMurrayA/Dropbox (Harvard University)/EPI202 Fall/EPI202 Fall 2022/Homework/Homework 1/Datasets/Case-control/CVdeath_Case_control.csv")

# 2. Prepare a 2x2 table and compute the odds ratio for the association between diabetes at baseline and case status (death from cardiovascular causes).
dm_table<-table(CVdeath$case, CVdeath$dm) 
dm_table
(dm_table[1,1]*dm_table[2,2]) / (dm_table[1,2]*dm_table[2,1])

#********************************************************************************************************
# 3.	Write out a logistic regression model with case status as the outcome, and diabetes at baseline as the only independent variable in the model.
# a.	What is the interpretation of beta_1?
#********************************************************************************************************/

# 4. Fit the model you described in question 3.
model1 <- glm(case ~ dm, data=CVdeath, family=binomial(link='logit'))
summary(model1)
exp(coefficients(model1))
exp(confint(model1))

# 5. Fit a model evaluating the association between diabetes at baseline and CVD death adjusting for age as a continuous variable.
model2 <- glm(case ~ dm + age, data=CVdeath, family=binomial(link='logit'))
summary(model2)
exp(coefficients(model2))
exp(confint(model2))

# 6. What is the odds ratio for the association between a 10-year increment in age and CVD death after adjusting for diabetes at baseline.
1.05863238 ^10
# Note that age is the third coefficient in model2
exp(model2$coefficients["age"]*10)

# 7. Extend the model you fit in question 5 to evaluate the association between diabetes at baseline after adjusting for age (continuous), hypertension at baseline, sex at birth, physical activity (3 categories), and educational attainment (3 categories).  Interpret the odds ratio and 95% confidence interval for the association hypertension at baseline and CVD death.
model3 <- glm(case ~ dm + age + htn + female + as.factor(phys_activity) + as.factor(educ), data=CVdeath, family=binomial(link='logit'))
summary(model3)
exp(coefficients(model3))
exp(confint(model3))

# 8. Extend the model that you fit in question 7 to evaluate whether there is evidence that the association between diabetes and CVD death is different for those with and without a history of hypertension.
CVdeath$dm_htn <- CVdeath$dm*CVdeath$htn
model4 <- glm(case ~ dm + age + htn + female + as.factor(phys_activity) + as.factor(educ) + dm_htn, data=CVdeath, family=binomial(link='logit'))
summary(model4)
exp(coefficients(model4))
exp(confint(model4))

# 9. Compute the point estimate and 95% confidence interval for the association between diabetes at baseline and CVD death among participants free of hypertension.

# Display the point estimate and 95% CI for diabetes among those without hypertension
# we can read this directly from the model output using the term for diabetes
# or we can calculate it using the variance covariance matrix.
# Note that in the output for Model4 dm is the second term
# we refer to elements in the variance covariance matrix using the syntax
# vcov(Modelname)[row, col] where Modelname is the model that you are working 
# with and [row, col] specifies which element of the variance covariance matrix 
# you are accessing

#retrieve the variance-covaraince matrix
vcov(model4)
beta_dm_no_htn <- model4$coefficients["dm"]
OR_dm_no_htn <- exp(beta_dm_no_htn)
CI_LB_dm_no_htn <- exp(beta_dm_no_htn - 1.96 * sqrt(vcov(model4)["dm","dm"]))
CI_UB_dm_no_htn <- exp(beta_dm_no_htn + 1.96 * sqrt(vcov(model4)["dm","dm"]))
rbind(OR_dm_no_htn, CI_LB_dm_no_htn, CI_UB_dm_no_htn)

# 10. Compute the point estimate and 95% confidence interval for the association between diabetes at baseline and CVD death among participants with hypertension at baseline.
# To display the point estimate and 95% CI for diabetes among those with hypertension
# we need to add the coefficients for dm and the interaction term (dm_htn)
# to obtain the point estimate.
# We need to calculate the variance of the sum of the coefficients as:
# var(dm) + var(dm_htn) + 2*covariance(dm, htn)

beta_dm_htn <- model4$coefficients["dm"] + model4$coefficients["dm_htn"]
OR_dm_htn <- exp(beta_dm_htn)
CI_LB_dm_htn <- exp(beta_dm_htn - 1.96 * sqrt(vcov(model4)["dm","dm"] + vcov(model4)["dm_htn","dm_htn"] + 2*vcov(model4)["dm","dm_htn"]))
CI_UB_dm_htn <- exp(beta_dm_htn + 1.96 * sqrt(vcov(model4)["dm","dm"] + vcov(model4)["dm_htn","dm_htn"] + 2*vcov(model4)["dm","dm_htn"]))
rbind(OR_dm_htn, CI_LB_dm_htn, CI_UB_dm_htn)

# Calculate the RERI to evaluate whether there is evidence supporting the hypothesis that the assocaition between diabetes at baseline is modified by hypertension on the additive scale.

# Remember that RERI = RR11 - RR10 - RR01 +1
# In this example RERI = OR(dm and htn) - OR(dm alone) - OR (htn alone) +1
exp(model4$coefficients["dm"] + model4$coefficients["htn"] + model4$coefficients["dm_htn"]) - exp(model4$coefficients["dm"]) - exp(model4$coefficients["htn"]) + 1


