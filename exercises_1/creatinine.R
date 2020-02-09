library(mosaic)
library(tidyverse)

# read in data
creatinine = read.csv('./creatinine.csv')

# simple scatter plot
plot(creatclear ~ age, data = creatinine, ylab="Creatinine Clearance Rate", xlab="Age", main="Creatinine Clearance Rate vs. Age")

# fit a linear model for price versus food score
lm1 = lm(creatclear ~ age, data = creatinine)

# look at the coefficients
coef(lm1)

# plot the regression line
abline(lm(creatclear ~ age, data = creatinine))

# make prediction on new data
new_data = data.frame(age = c(55))
predict(lm1, new_data)

# look at the residuals (actual - predicted)
resid(lm1)

# compute the residual model
creatinine_res = resid(lm1)

# who has the smallest residual? pipe to which.min
creatinine_res %>% which.absolute_min

# plot the residual plot
plot(creatinine$age, creatinine_res, ylab="Residuals", xlab="Age", main="Creatinine Clearance Rate")
abline(0, 0)

# make predictions on new data to compare patients
patient_data = data.frame(age = c(40, 60))
predict(lm1, patient_data)

# compute residuals for each patient
135 - 123.0203
112 - 110.6240

# 1. On average, for a 55-year-old, we should expect a creatinine clearance rate of 113.723.
# 2. The linear regression model shows that the creatinine clearance rate decreases by 0.6198 mL/minute for every increase of 1 year.
# 3. Since the 40-year-old with a rate of 135 has a residual of 11.9797 and the 60-year-old with a rate of 112 has a residual of 1.376, the 60-year-old has a healthier creatinine clearance rate.
