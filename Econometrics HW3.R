library(tidyverse)
library(ggplot2)
library(lmtest)
library(sandwich)
library(Metrics)
library(car)
data <- read_csv("cps99_ps1.csv")


# Task 5

lm_1 <- lm(log(ahe) ~ yrseduc + age + female + hsdipl, data)
summary(lm_1)
# the coefficient on yrseduc is about 0.084, which means one additional year of education increases earnings by 8.4%, other regressors held constant
# the coefficient on age is about 0.061, which means one additional year of age increases earnings by 6.1%, other regressors held constant
# the coefficient on female is about -0.224, which means women earn 22.4% less than men on average, other regressors held constant

# RMSE of this model
rmse(log(data$ahe), lm_1$fitted.values)
# For any regresion, rmse is the standard deviation of the residuals in the mathematical sense. 
# In this particular case residuals are basically the percentage difference between true and predicted ahe.
# So rmse is the standard deviation of the percentage differences between true and predicted ahe

# construct 95% confidence interval for the gender gap
ct_1 <- coeftest(lm_1, vcov. = vcovHC(lm_1, "HC1"))
confint(ct_1, 'female')

# The t-statistic for the null hypothesis that the marginal value of a highschool diploma is 0:
ct_1
# The t statistic is greater than 1.96 => reject null hypothesis at 5% significance

# The marginal value of a high school diploma, years of education held constant, is the coefficient of hsdipl on ahe:
ct_1[5,]
# The t value of null hypothesis "marginal value of a high school diploma is zero" is 2.6. We reject the null hypothesis
# This value is quite small in a real-world sense - the diploma increases earnings only by 8% on average

# V is essentially the marginal value of two years of education plus a high school diploma. Thus, the estimate is
V_estimate <- ct_1[5,1] + 2 * ct_1[2,1]
V_estimate
# This value is substantial in the real world sense - two years of school and a diploma provide an increase in earnings of 25%
# To calculate confidence intervals, we need to know SE(V).
# From results of task 1, we know the formula SE(b1+2*b2) = sqrt(Var(b1) + 4*Cov(b1,b2) + 4*Var(b2)); b1 and b2 are coefficients on hsdipl and yrseduc
cov_mat <- vcovHC(lm_1, "HC1")
V_SE <- sqrt(cov_mat[5,5] + 4*cov_mat[5,2] + 4*cov_mat[2,2])
c(V_estimate - 1.96*V_SE, V_estimate + 1.96*V_SE)



# Task 6

data <- data %>% mutate(age2 = age^2)
lm_2 <- lm(log(ahe) ~ yrseduc + age + age2 + female + hsdipl, data)
summary(lm_2)
# The coefficient on age is positive; coefficient on age^2 is negative and much smaller than on age in absolute terms
# This means that when a worker is young, his earnings increase with his age
# A middle-aged worker's earnings are practically unaffected as his age increases
# A senior worker's earnings decrease when his age increases

# A simple t-test of the null hypothesis "coefficient on age^2 is zero" is enough in this case:
ct_2 <- coeftest(lm_2, vcov. = vcovHC(lm_2, "HC1"))
ct_2[4,]
# the t value is -14,8. We reject null hypothesis

# the gender gap estimate in lm_1 (without age^2):
ct_1["female",]
# the gender gap estimate in lm_2 (with age^2):
ct_2["female",]
# We can see the values are practically the same. This is because the correlation between age^2 and female is very small:
cor(data$female, data$age2)

data <- data %>% mutate(age3 = age^3)
lm_3 <- lm(log(ahe) ~ yrseduc + age + age2 + age3 + female + hsdipl, data)
ct_3 <- coeftest(lm_3, vcov. = vcovHC(lm_3, "HC1"))
ct_3
# The t test of Null hypothesis "coefficient of age3 is zero" gives value -0.56. DO NOT Reject null hypothesis at 5% significance
# Thus, the relation between ahe and age is quadratic

# construct null hypothesis:
myH0 <- c("age2", "age3")
linearHypothesis(lm_3, myH0, vcovHC(lm_3, "HC1"))
# p value is smaller than 0.05. Reject null hypothesis that the relation is linear.

# I would recommend using the specification with quadratic age, because adding quadratic to linear is significant; adding cubic to quadratic is statistically insignificant.



# Task 7

data.frame(specification = c('linear', 'quadratic', 'qubic'),
                 age_30_to_31 = c(ct_1["age", 1],
                                  ct_2["age", 1] * (31-30) + ct_2["age2", 1] * (31^2 - 30^2),
                                  ct_3["age", 1] * (31-30) + ct_3["age2", 1] * (31^2 - 30^2) + ct_3["age3", 1] * (31^3 - 30^3)),
                 age_45_to_461 = c(ct_1["age", 1],
                                   ct_2["age", 1] * (46-45) + ct_2["age2", 1] * (46^2 - 45^2),
                                   ct_3["age", 1] * (46-45) + ct_3["age2", 1] * (46^2 - 45^2) + ct_3["age3", 1] * (46^3 - 45^3)),
                 age_60_to_61 = c(ct_1["age", 1],
                                  ct_2["age", 1] * (61-60) + ct_2["age2", 1] * (61^2 - 60^2),
                                  ct_3["age", 1] * (61-60) + ct_3["age2", 1] * (61^2 - 60^2) + ct_3["age3", 1] * (61^3 - 60^3)))


# The entries differ significantly between linear and quadratic specifications. 
# At 30 to 31, the marginal value of one year is 12 percentage points higher in quadratic. At 60 to 61, it is 12 percentage points lower.
# The differences between quadratic and cubic specifications are negligible in the real world sense.

# The quadratic row makes the most sense to me. When you are young, you don't have a lot of experience, so your earnings rise quite fast.
# When you reach around 40-50 years, the career growth slows because you don't have as much energy. The earnings reach a plateau at this point.
# When you are around 60, some of your knowledge becomes obsolete, and it is hard to compete with young workers; thus your earnings decline.