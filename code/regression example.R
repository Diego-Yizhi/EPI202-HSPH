#EPI 202 
# Linear regression example using the HeightWeight dataset

setwd("C:/Users/Murray/Dropbox (Harvard University)/EPI202 Fall/EPI202 Fall 2021/Regression data sets/HeightWeight")
library(ggplot2)

# Open the csv file
HeightWeight <- read.csv("HeightWeight.csv")
summary(HeightWeight)

#Note the outlier in age - max value is 25 and should be dropped
HeightWeight = HeightWeight[HeightWeight$age <= 21,]

# plot height as a function of age
qplot(age, height, data = HeightWeight)

# linear regression predicting height as a function of age
Model1 <- lm(height ~ age, data=HeightWeight)
summary(Model1)

# plot height as a function of sex
qplot(as.factor(female), height, data = HeightWeight)

# linear regression predicting height as a function of sex
Model2 <- lm(height ~ as.factor(female), data=HeightWeight)
summary(Model2)

#plot height as a function of age and color code by sex
qplot(age, height, colour= female, data = HeightWeight)

# linear regression predicting height as a function of age and sex
Model3 <- lm(height ~ age + as.factor(female), data=HeightWeight)
summary(Model3)

#Now add the interaction term between age and sex
Model4 <- lm(height ~ age + as.factor(female) + femage, data=HeightWeight)
summary(Model4)

# A simpler way to code the interaction term without explictly creating it prior to running the model
Model5 <- lm(height ~ age * as.factor(female), data=HeightWeight)
summary(Model5)

#retrieve the variance-covaraince matrix
vcov(Model4)

# Now, display the point estimate and 95% CI for height among males (female=0)
# we can read this directly from the model output using the term for age
# or we can calculate it using the variance covariance matrix.
# Note that in the output for Model4 age is the second term
# we refer to elements in the variance covariance matrix using the syntax
# vcov(Modelname)[row, col] where Modelname is the model that you are working 
# with and [row, col] specifies which element of the variance covariance matrix 
# you are accessing

beta_age_male <- Model4$coefficients[2]
CI_LB_age_male <- beta_age_male - 1.96 * sqrt(vcov(Model4)[2,2])
CI_UB_age_male <- beta_age_male + 1.96 * sqrt(vcov(Model4)[2,2])
rbind(beta_age_male, CI_LB_age_male, CI_UB_age_male)

# Now display the point estimate and 95% CI for height among females (female=1)
# Note that we need to add the coefficients for age and the interaction term (femage)
# to obtain the point estimate.
# We need to calculate the variance of the sum of the coefficients as:
# var(age) + var(femage) + 2*covariance(age, femage)

beta_age_female <- Model4$coefficients[2] + Model4$coefficients[4]
CI_LB_age_female <- beta_age_female - 1.96 * sqrt(vcov(Model4)[2,2] + vcov(Model4)[4,4] + 2*vcov(Model4)[2,4])
CI_UB_age_female <- beta_age_female + 1.96 * sqrt(vcov(Model4)[2,2] + vcov(Model4)[4,4] + 2*vcov(Model4)[2,4])
rbind(beta_age_female, CI_LB_age_female, CI_UB_age_female)







