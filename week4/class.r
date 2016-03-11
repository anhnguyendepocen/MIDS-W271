
    
load("Wage1.rdata")

# fit the linear model
model1 = lm(wage ~ educ + exper, data = data)

# we could use the summary command, but the errors are not
# robust to heteroskedasticity.
summary(model1)


# Look at the residuals directly
hist(model1$residuals, breaks = 50)

qqnorm(model1$residuals)
qqline(model1$residuals)

library(car)
stdres = rstandard(model1)
stures = rstudent(model1)

plot(model1$fitted, model1$residuals)
plot(model1$fitted, stdres)
abline(2,0)
abline(-2,0)

#outlier detection
which(stdres > 2)

outlierTest(model1)

#heteroscedasticity - more robust methods for small sample sizes
ncvTest(model1)

library(lmtest)
coeftest(model1)

waldtest(model1)

#Question 3: not time ordered

#Q 4

plot(data$educ, model1$residuals)
plot(data$educ, stdres)

plot(data$exper, model1$residuals)
plot(data$exper, stdres)

plot(data$wage, model1$residuals)

#no patterns in resiuals vs (educ or exper)

#Question 5
mean(model1$residuals[data$female==1])
mean(model1$residuals[data$female==0])
mean(model1$residuals[data$married==0])
mean(model1$residuals[data$married==1])




