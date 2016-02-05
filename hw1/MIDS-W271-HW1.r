####################################################
#
# Applied Regression and Time Series Analysis
#
# W271-4 Homework 1: OLS Estimation
#
# Lei Yang, Suhashini R., Ron Cordell
#
# R Script used for Homework 1
# 
####################################################
#
# Load the data from the birthweight_w271.Rdata file
#
load("birthweight_w271.Rdata")

library(car)

ls()

desc

str(data)

summary(data)

summary(data$bwght)

quantile(data$bwght, probs=c(1,5,10,25,50,75,90,99, NA)/100)

hist(data$bwght, main='Birthweight bins=50', xlab='Birthweight', breaks=50)

hist(data$bwght[data$bwght<250 & data$bwght>0], main='Birthweight Between 1 and 250 ounces, bins=50',
                                                xlab='Birthweight', breaks=50)

hist(data$bwght[data$bwght<250 & data$bwght>0], main='Birthweight Between 1 and 250 ounces, bins=250',
                                                xlab='Birthweight', breaks=250)

boxplot(data$bwght, data=data, main = "Birthweight Boxplot", ylab=("ounces"))

summary(data$cigs)

quantile(data$cigs, probs=c(1,5,10,25,50,75,90,99, NA)/100)

hist(data$cigs, main='Cigarettes Consumed While Pregnant', xlab='Cigarettes/Day', breaks=25)

hist(data$cigs[data$cigs>0], main='Cigarettes per Day for Smokers', xlab='Cigarettes/Day', breaks=25)

hist(data$cigs[data$cigs>0], main='Cigarettes per Day for Smokers', xlab='Cigarettes/Day', breaks=50)

boxplot(data$cigs[data$cigs>0], data=data, main="Cigarettes Consumption By Smokers While Pregnant Boxplot",
        ylab="cigarettes per day")

scatterplot(data$cigs, data$bwght,
            main="Birthweight and Cigarette Consumption While Pregnant",
           xlab = "Cigarettes Per Day", ylab="Ounces")

scatterplot(data$cigs[data$cigs>0], data$bwght[data$cigs>0],
            main="Birthweight and Cigarette Consumption For Smokers While Pregnant",
           xlab = "Cigarettes Per Day", ylab="Ounces")

hist(data$bwght[data$cigs==0], main="Histogram of Birthweights for Non-Smokers While Pregnant",
                                xlab="Birthweight, ounces")

hist(data$bwght[data$cigs>0], main="Histogram of Birthweights for Smokers While Pregnant",
                                xlab="Birthweight, ounces")

d1 <- data[data$bwght>1 & data$bwght<250,]

m1 <- lm(bwght ~ cigs, data=d1)

plot(m1)

summary(m1)

m1 <- lm(bwght ~ cigs, data=subset(data, cigs>0))

summary(m1)

summary(data$faminc)

quantile(data$faminc, probs=c(1,5,10,25,50,75,90,99, NA)/100)

hist(data$faminc, breaks=30, main="Family Income Histogram", xlab="Family Income")

table(data$faminc)

hist(data$faminc[data$faminc < 55], breaks=50, main="Family Income Histogram", xlab="Family Income")

hist(log(data$faminc[data$faminc < 55]), breaks=50, main="Family Income Histogram", xlab="Family Income")

library(car)

scatterplotMatrix(~ bwght  + cigs + faminc, data=subset(data, faminc<55))

d2 <- data[data$faminc<55 & data$bwght>1 & data$bwght<250,]

m2 <- lm(bwght ~ cigs + faminc, data=d2)

summary(m2)


