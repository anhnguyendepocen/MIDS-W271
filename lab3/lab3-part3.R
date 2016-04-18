
#########################################################################
#
# Lab 3, Part 3
#
#########################################################################
## ---- part3_setup ----
library(ggplot2)
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
## ---- part3_loaddata ----
ts1.csv_load <- read.csv('lab3_data/globalWarming.csv')

str(ts1.csv_load)
head(ts1.csv_load, 10)
tail(ts1.csv_load, 10)

# weekly time series. This has problems because the number of weeks
# per year is not the same every year, wither 52 or 53.
ts1 <- ts(ts1.csv_load$data.science, start = c(2004, 1), frequency=365.25/7)

## ---- part3_summarize_data_text -----
fit.df <- data.frame(ts1)
class(df)
stargazer(fit.df, type="text", header=FALSE,
          title="Descriptive Statistics", digits=2,
          covariate.labels = "Search Series")

## ---- part3_summarize_data_latex -----
fit.df <- data.frame(ts1)
class(df)
stargazer(fit.df, type="latex", header=FALSE,
          title="Descriptive Statistics", digits=1,
          covariate.labels = "Search Series")


## ---- part3_ts_initial_plot ----

plot.ts(ts1, main="Search Activity", xlab="Year", ylab="Value")

## ---- part3_ts_extract ----
# the time series has no real change in it until about the beginning of 2013
# break the time series at 2013 and beyond
ts2 <- ts(ts1.csv_load[471:630,]$data.science, start=c(2013, 1), frequency = 52)

## ---- part3_summarize_ts2_text -----
fit.df <- data.frame(ts2)
class(df)
stargazer(fit.df, type="text", header=FALSE,
          title="Descriptive Statistics", digits=2,
          covariate.labels = "Search Series")

## ---- part3_summarize_ts2_latex -----
fit.df <- data.frame(ts2)
class(df)
stargazer(fit.df, type="latex", header=FALSE,
          title="Descriptive Statistics", digits=1,
          covariate.labels = "Search Series")

## ---- part3_plots_ts2 ----
par(mfrow=c(2,2))
plot.ts(ts2, main="Search Activity from 2013", 
        ylab="Value", xlab="Year")
hist(ts1, main="Histogram of Search Activity from 2013",
     xlab="Value", breaks=50)
acf(ts1, main="Autocorrelation of Search Activity",
    xlab="Lag")
pacf(ts1, main="Partial Autocorrelation of Search Activity",
     xlab="Lag")

# peristent trend, different periods of seasonality
# ACF is a slow roll-off indicating a possible ARMA() process
# Take the difference of the series to remove the trend

## ---- part3_ts2_diff_calc ----
ts2.diff <- diff(ts2)
summary(ts2.diff)

## ---- part3_plots_ts2_diff ----
par(mfrow=c(2,2))
plot.ts(ts2.diff, main="First Difference in Search Activity", 
        ylab="Value", xlab="Year")
hist(ts2.diff, main="Histogram of First Difference Search Activity",
     xlab="Value", breaks=50)
acf(ts2.diff, main="Autocorrelation of First Difference Search",
    xlab="Lag",lag.max = 100)
pacf(ts2.diff, main="Partial Autocorrelation of First Difference",
     xlab="Lag", lag.max = 100)

# we've removed the trend but there remains a seasonality component
# ACF and PACF indicate a possible MA(1) or MA(2) process, with
# a seasonality component at lag 12

# Take a look at the de-seasoned series
ts2.lag12diff <- diff(ts2, lag = 13)

## ---- part3_plots_ts2_diff_lag12 ----
par(mfrow=c(2,2))
plot.ts(ts2.lag12diff, main="First Difference in Search Activity", 
        ylab="Value", xlab="Year")
hist(ts2.lag12diff, main="Histogram of First Difference Search Activity",
     xlab="Value", breaks=50)
acf(ts2.lag12diff, main="Autocorrelation of First Difference Search",
    xlab="Lag",lag.max = 100)
pacf(ts2.lag12diff, main="Partial Autocorrelation of First Difference",
     xlab="Lag", lag.max = 100)

summary(ts2.lag12diff)

auto.arima(ts2)
ts2.fit1 <- Arima(ts2, c(1,1,1), 
                  seasonal=list(order=c(0,1,1), period=2))
summary(ts2.fit1)

plot.ts(ts2.fit1$residuals)
plot(ts2.fit1)
