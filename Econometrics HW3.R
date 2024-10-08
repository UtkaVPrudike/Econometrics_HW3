library(tidyverse)
library(ggplot2)
library(lmtest)
library(sandwich)
library(Metrics)
data <- read_csv("cps99_ps1.csv")


# Task 5

lm_1 <- lm(log(ahe) ~ yrseduc + age + female + hsdipl, data)
summary(lm_1)
# the coefficient on yrseduc is about 0.084, which means one additional year of education increases earnings by 8.4%, other regressors held constant
# the coefficient on age is about 0.061, which means one additional year of age increases earnings by 6.1%, other regressors held constant
# the coefficient on female is about -0.224, which means women earn 22.4% less than men on average, other regressors held constant

# RMSE of this model
rmse(log(data$ahe), lm_1$fitted.values)

# construct 95% confidence interval for the gender gap
ct <- coeftest(lm_1, vcov. = vcovHC(lm_1, "HC1"))
confint(ct, 'female')

# The t-statistic for the null hypothesis that the marginal value of a highschool diploma is 0:
ct
# The t statistic is greater than 1.96 => reject null hypothesis at 5% significance

