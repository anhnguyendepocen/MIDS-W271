## @knitr part1
plot(c(1,2,3),c(1,2,3))

## @knitr part2
plot(c(1,2,3),c(1,2,3))

#########################################################################
#
# Lab 3, Setup
#
#########################################################################
library(ggplot2)
library(psych)
library(astsa)
library(zoo)
library(forecast)
library(stargazer)

#########################################################################
#
# Lab 3, Part 3 - Forecast the Web Search Activity for global Warming 
#
# Imagine that you group is part of a data science team in an appreal 
# company. One of its recent products is Global-Warming T-shirts. The 
# marketing director expects that the demand for the t-shirts tends to 
# increase when global warming issues are reported in the news. As such,
# the director asks your group to forecast the level of interest in 
# global warming in the news. The dataset given to your group captures
# the relative web search activity for the phrase, “global warming” over
# time. For the purpose of this exercise, ignore the units reported in
# the data as they are unimportant and irrelevant. Your task is to
# produce the weekly forecast for the next 3 months for the relative web
# search activity for global warming. For the purpose of this exercise, 
# treat it as a 12-step ahead forecast.
#
# The dataset for this exercise is provided in globalWarming.csv. Use
# only models and techniques covered in the course (up to lecture 13).
# Note that one of the modeling issues you may have to consider is
# whether or not to use the entire series provided in the data set.
# Your choice will have to be clearly explained and supported with 
# empirical evidence. As in other parts of the lab, the general
# instructions in the Instruction Section apply.
# 
#########################################################################
ts1.csv_load <- read.csv('lab3_data/globalWarming.csv')

str(ts1.csv_load)
tdshead(ts1.csv_load, 10)
tail(ts1.csv_load, 10)

# weekly time series. This has problems because the number of weeks
# per year is not the same every year, wither 52 or 53.
ts1 <- ts(ts1.csv_load$data.science, start = c(2004, 1), frequency=365.25/7)

plot.ts(ts1, main="Search Activity", xlab="Year", ylab="Value")

# the time series has no real change in it until about the beginning of 2013
# break the time series at 2013 and beyond
ts2 <- ts(ts1.csv_load[471:630,]$data.science, start=c(2013, 1), frequency = 52)

summary(ts2)

par(mfrow=c(2,2))
plot.ts(ts2, main="Search Activity from 2013", 
        ylab="Value", xlab="Year")
hist(ts1, main="Histogram of Search Activity from 2013",
     xlab="Value", breaks=50)
acf(ts1, main="Autocorrelation of Search Activity",
    xlab="Lag")
pacf(ts1, main="Partial Autocorrelation of Search Activity",
     xlab="Lag")

ts2.diff <- diff(log(ts2))
summary(ts2.diff)

par(mfrow=c(2,2))
plot.ts(ts2.diff, main="First Difference in Search Activity", 
        ylab="Value", xlab="Year")
hist(ts2.diff, main="Histogram of First Difference Search Activity",
     xlab="Value", breaks=50)
acf(ts2.diff, main="Autocorrelation of First Difference Search",
    xlab="Lag")
pacf(ts2.diff, main="Partial Autocorrelation of First Difference",
     xlab="Lag")

# we've removed the trend but there remains a seasonality component

ts2.fit1 <- Arima(ts2, c(1,1,1), 
                  seasonal=list(order=c(0,1,1), period=2))
summary(ts2.fit1)

plot.ts(ts2.fit1$residuals)
plot(ts2.fit1)
