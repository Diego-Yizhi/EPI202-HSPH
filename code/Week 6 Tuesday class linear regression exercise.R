#EPI 202 
# Linear regression example using the HeightWeight dataset

library(ggplot2)
install.packages("table1")
require(table1)

# 1.	Open the HeightWeight dataset
setwd("C:/Users/MittlemanMurrayA/Dropbox (Harvard University)/EPI202 Fall/EPI202 Fall 2022/Regression data sets/HeightWeight")

# Open the csv file
HeightWeight <- read.csv("HeightWeight.csv")
summary(HeightWeight)

#Create a new female variable andd declare as factor
HeightWeight$female_cat <- HeightWeight$female 
HeightWeight$female_cat  <- factor(HeightWeight$female_cat, levels=c(0, 1), labels=c("Male", "Female"))

# 2.	Inspect the data by preparing a two-way scatterplot of height as predicted by age. Do you notice any outliers that are of concern?  
#     Note the outlier in age - max value is 25 and should be dropped
# qplot(age, height, data = HeightWeight)
ggplot(HeightWeight, aes(x=age, y=height)) + geom_point()

# 3.	Delete any observations outside of the intended age range of the study.
HeightWeight = HeightWeight[HeightWeight$age <= 21,]
ggplot(HeightWeight, aes(x=age, y=height)) + geom_point()

# 4. Prepare a Table 1.  
#    Stratify by sex in the columns and show the data for age (continuous), age in 3 categories (<15, 15-18, >=18). 

HeightWeight$age_cat <- as.factor(ifelse(HeightWeight$age < 15, 1,
                                         ifelse (HeightWeight$age < 18, 2,
                                                 ifelse (HeightWeight$age >= 18,3, NA))))
table(HeightWeight$age_cat)

table(HeightWeight$age_cat, HeightWeight$female)
chisq.test(HeightWeight$age_cat, HeightWeight$female, correct=FALSE)
t.test (age~female, data = HeightWeight)

# you can optionally use the next 2 lines of code to generate the row percents.
# for column percents, you would place a 2 after the comma in the prop.table command.
# age_table <- table(HeightWeight$age_cat, HeightWeight$female)
# prop.table(age_table,1)

# There is an R package that will generate a formatted Table 1
# To use it, you need to install the package
# you can check the documentation to learn how to suppress the "Overall column
# There is code online to enhance the table and have the Table 1 include p-values
# for example, see https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

#prepare function to add p-values to table 1
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

table1(~factor(age_cat) + age |female, data=HeightWeight) 
table1(~factor(age_cat) + age |female, data=HeightWeight,
       overall=F, extra.col=list(`P-value`=pvalue))

# 5.	Write out the linear regression model estimating the expected value of height as a function of age using standard notation and interpret each term in the model.

# a.	What is the interpretation of ??0?
# b.	What is the interpretation of ??1?
# c.	What is the expected distribution of the residuals?


# 6. Use software to fit the linear regression model specified in your answer to question 5.
Model1 <- lm(height ~ age, data=HeightWeight)
summary(Model1)

# 7.	Based on the fitted model, what is the expected height for a 16-year-old?
37.4290 +  1.4573 *16
Model1$coefficients[1] + Model1$coefficients[2]*16

# 8.	Inspect the data by preparing a two-way scatterplot of height as predicted by sexx
ggplot(HeightWeight, aes(x=female, y=height)) + geom_point()

# 9.	Use software to fit a linear regression model estimating the expected value of height as a function of sex (consider female as exposed).
# a.	What is the interpretation of ??0?
# b.	What is the interpretation of ??1?
# c.	State the null and alternative hypothesis (in terms of the model coefficients), testing the hypothesis that mean height does not vary according to sex.
# d.	Interpret the p-value for the test described in part 9c.

Model2 <- lm(height ~ as.factor(female), data=HeightWeight)
summary(Model2)

#plot height as a function of age and color code by sex
qplot(age, height, colour= female, data = HeightWeight)
ggplot(HeightWeight, aes(x=age, y=height)) + geom_point(aes(color=factor(female)))

# 10.	Use software to fit a linear regression model for the association between age and height, adjusting for sex.  
#     Interpret each term in the model.
Model3 <- lm(height ~ age + as.factor(female), data=HeightWeight)
summary(Model3)

# 11.	Using your expert subject matter knowledge, do you think the relationship between age and height might be different for adolescent males and females?  
#     Note that this asks if you hypothesize that there might be effect measure modification of the age-height relationship by sex.

# 12.	Write out the linear regression model estimating the expected value of height as a function of age, sex, and the interaction between age and sex 
#     using standard notation and interpret each term in the model.

# 13.	Use software to fit a linear regression model for the association between age and height, adjusting for female and allowing for the relationship 
#     between age and height to be different for males and females.   HINT: You will need to create a multiplicative interaction term between age and sex 
#     to complete this analysis.

# 14.	State the null and alternative hypotheses for the test of whether the association between age and height varies according to sex.  
#     What is the p-value for this test?

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

# 15.	Estimate the association (including the point estimate and 95% confidence interval) between age and height among males. 
beta_age_male <- Model4$coefficients[2]
CI_LB_age_male <- beta_age_male - 1.96 * sqrt(vcov(Model4)[2,2])
CI_UB_age_male <- beta_age_male + 1.96 * sqrt(vcov(Model4)[2,2])
rbind(beta_age_male, CI_LB_age_male, CI_UB_age_male)

#     What is the expected height for a 16-year-old male?
30.6470 + 1.918139 *16
Model4$coefficients[1] + Model4$coefficients[2]*16

# Now display the point estimate and 95% CI for height among females (female=1)
# Note that we need to add the coefficients for age and the interaction term (femage)
# to obtain the point estimate.
# We need to calculate the variance of the sum of the coefficients as:
# var(age) + var(femage) + 2*covariance(age, femage)

# 16.	Estimate the association (including the point estimate and 95% confidence interval) between age and height among females. 
beta_age_female <- Model4$coefficients[2] + Model4$coefficients[4]
CI_LB_age_female <- beta_age_female - 1.96 * sqrt(vcov(Model4)[2,2] + vcov(Model4)[4,4] + 2*vcov(Model4)[2,4])
CI_UB_age_female <- beta_age_female + 1.96 * sqrt(vcov(Model4)[2,2] + vcov(Model4)[4,4] + 2*vcov(Model4)[2,4])
rbind(beta_age_female, CI_LB_age_female, CI_UB_age_female)

#     What is the expected height for a 16-year-old female?
30.6470 + 13.3146 + 1.0075383 * 16
Model4$coefficients[1] + Model4$coefficients[3] + (Model4$coefficients[2] + Model4$coefficients[4])*16
