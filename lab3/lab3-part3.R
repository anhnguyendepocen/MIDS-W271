
#########################################################################
#
# Lab 3, Part 3
#
#########################################################################
## ---- part3_setup ----
library(ggplot2)
library(astsa)
library(tseries)
library(zoo)
library(forecast)
library(stargazer)
library(texreg)
library(fGarch)

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
ts.csv_load <- read.csv('lab3_data/globalWarming.csv')

str(ts.csv_load)
head(ts.csv_load, 10)
tail(ts.csv_load, 10)

# weekly time series. This has problems because the number of weeks
# per year is not the same every year, wither 52 or 53.
ts.full <- ts(ts.csv_load$data.science, start = c(2004, 1), frequency=365.25/7)
frequency(ts.full)

## ---- part3_summarize_data_text -----
fit.df <- data.frame(ts.full)
class(df)
stargazer(fit.df, type="text", header=FALSE,
          title="Descriptive Statistics", digits=2,
          covariate.labels = "Search Series")

## ---- part3_summarize_data_latex -----
fit.df <- data.frame(ts.full)
class(df)
stargazer(fit.df, type="latex", header=FALSE,
          title="Descriptive Statistics", digits=2,
          covariate.labels = "Search Series")


## ---- part3_ts_initial_plot ----
par(mfrow=c(1,1))
plot.ts(ts.full, main="Search Activity", xlab="Year", ylab="Value",
        ylim=c(-1,4))

## ---- part3_ts_extract ----
# the time series has no real change in it until about the beginning of 2013
# break the time series at 2012 and beyond
# we can use the raw data here and an even number frequency because there are
# no extra weeks in the time interval

tail(ts.csv_load[1:365,])
head(ts.csv_load[366:630,])
tail(ts.csv_load[366:630,])
ts1 <- ts(ts.csv_load[1:417,]$data.science,   start=c(2004, 1), frequency = 52)
ts2 <- ts(ts.csv_load[418:630,]$data.science, start=c(2011, 1), frequency = 52)

Box.test(ts1, type="Ljung-Box")                                          
Box.test(ts2, type="Ljung-Box") 

## ---- part3_compare_ts_text -----
fit.df <- data.frame(cbind(ts1, ts2))
class(df)
stargazer(fit.df, type="text", header=FALSE,
         title="Comparative Statistics", digits=2,
         covariate.labels = c("2004-2010","2011-2016"))
                     
## ---- part3_compare_ts_latex -----
fit.df <- data.frame(cbind(ts1, ts2))
class(df)
stargazer(fit.df, type="latex", header=FALSE,
        title="Comparative Statistics", digits=2,
        covariate.labels = c("2004-2010","2011-2016"))

## ---- part3_plots_ts1 ----
par(mfrow=c(2,2))
plot.ts(ts1, main="Search Activity Through 2010", 
        ylab="Value", xlab="Year")
hist(ts1, main="Histogram of Search Activity Through 2010",
     xlab="Value", breaks=50)
acf(ts1, main="Autocorrelation of Search Activity",
    xlab="Lag")
pacf(ts1, main="Partial Autocorrelation of Search Activity",
     xlab="Lag")

## ---- part3_plots_ts2 ----
par(mfrow=c(2,2))
plot.ts(ts2, main="Search Activity from 2011", 
        ylab="Value", xlab="Year")
hist(ts2, main="Histogram of Search Activity from 2011",
     xlab="Value", breaks=50)
acf(ts2, main="Autocorrelation of Search Activity",
    xlab="Lag")
pacf(ts2, main="Partial Autocorrelation of Search Activity",
     xlab="Lag")

# peristent trend, different periods of seasonality
# ACF is a slow roll-off indicating a possible ARMA() process
# Take the difference of the series to remove the trend

## ---- part3_ts2_diff_calc ----
ts2.diff <- diff(ts2)
summary(ts2.diff)

## ---- part3_plots_ts2_diff ----
par(mfrow=c(2,2))
plot.ts(ts2.diff, main="Differenced Search Activity", 
        ylab="Value", xlab="Year")
hist(ts2.diff, main="Histogram of Differenced Search Activity",
     xlab="Value", breaks=50)
acf(ts2.diff, main="ACF of First Difference Search",
    xlab="Lag",lag.max = 100)
pacf(ts2.diff, main="PACF of First Difference",
     xlab="Lag", lag.max = 100)

# we've removed the trend but there remains a seasonality component
# ACF and PACF indicate a possible MA(1) or MA(2) process, with
# a seasonality MA(4) process


## ---- part3_seasonal_diff ----
# Take a look at the de-seasoned series
ts2.seasonal <- diff(ts2, lag = 4)
summary(ts2.seasonal)

## ---- part3_seasonal_plot ----
par(mfrow=c(2,2))
plot.ts(ts2.seasonal, main="Seasonality in Search Activity", 
        ylab="Value", xlab="Year")
hist(ts2.seasonal, main="Histogram of Seasonality",
     xlab="Value", breaks=50)
acf(ts2.seasonal, main="ACF ofSeasonality",
    xlab="Lag",lag.max = 100)
pacf(ts2.seasonal, main="PACF of Seasonality",
     xlab="Lag", lag.max = 100)

## ---- part3_model ----
ts2.fit <- auto.arima(ts2)
summary(ts2.fit)
t(confint(ts2.fit))
# best model is ARIMA(1,1,1)(0,1,1)[52]

## ---- part3_model_plot_ts ----
par(mfrow=c(1,1))
plot.ts(ts2, col='cyan', 
        main='Time Series vs. SARIMA(1,1,1)(0,1,1)[52] Model',
        ylab='Original and Estimated Values', xlab='Period',
        pch=1, lty=1)
par(new=T)
plot.ts(fitted(ts2.fit), col='navy', axes=F,
        xlab='',ylab='', lty=2)
par(new=T)
plot.ts(ts2.fit$residuals, axes=F, xlab='', ylab='', col='green',
        lty=2, pch=1, col.axis='green', ylim=c(-1,5))
axis(side=4, col='green')
leg.txt <- c("Original Series", "SARIMA(0,1,0)(0,1,1)[52]", "Residual")
legend("topleft", legend=leg.txt, lty=c(2,1), col=c("cyan","navy","green"),
       bty='n', cex=1)


## ---- part3_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts2.fit$residuals, main="SARIMA Residuals", 
        ylab="Price", xlab="Interval")
hist(ts2.fit$residuals, main="Histogram of SARIMA Residuals",
     xlab="log(Price)", breaks=50)
acf(ts2.fit$residuals, main="ACF of SARIMA Residuals",
    xlab="Lag")
acf(ts2.fit$residuals^2, main="ACF of SARIMA Residuals Squared",
    xlab="Lag")

## ---- part3_garch ----
# shows a time-varying residual so fit a GARCH model
# an ARCH(1)
ts2.garch <- garchFit(ts2.fit$residuals~garch(1,1), trace=FALSE)
summary(ts2.garch)

## ---- part3_garch_residual_plots ----
g <- garch(ts2.fit$residuals, order=c(1,1), trace=FALSE)

res <- ts2.garch@residuals
par(mfrow=c(2,2))
plot.ts(res, main="GARCH(1,1) Residuals", 
        ylab="Price", xlab="Interval")
hist(res, main="Histogram GARCH(1,1) Residuals",
     xlab="log(Price)", breaks=50)
acf(res, main="ACF GARCH(1,1) Residuals",
    xlab="Lag", na.action = na.omit)
acf(res, main="ACF GARCH(1,1) Residuals Squared",
    xlab="Lag", na.action = na.omit)

## ---- part3_forecast ----
steps <- 12
# standard error of the residuals from the ARIMA model
se.resid <- sqrt(ts2.fit$sigma2) # or stdev(x.arima.residuals)
# forecast with GARCH model
ts2.garch.fcast <- predict(ts2.garch, n.ahead = steps)
# forecast with ARIMA model
forecast <- forecast.Arima(ts2.fit, h=steps)
# standard errors from the ARIMA model (for prediction)
se.arima <- (forecast$upper[,2]-forecast$mean)/1.96
# forecasting conditional standard errors from GARCH
cse.garch <- ts2.garch.fcast$standardDeviation
# put the conditional SE back to ARIMA SE
se.combine <- se.arima / se.resid * cse.garch
forecast$mean <- forecast$mean + ts2.garch.fcast$meanForecast 
forecast$lower[,2] <- forecast$mean - 1.96 * se.combine
forecast$lower[,1] <- forecast$mean - 1.645 * se.combine
forecast$upper[,2] <- forecast$mean + 1.96 * se.combine
forecast$upper[,1] <- forecast$mean + 1.645 * se.combine

## ---- part3_forecast_plot ----
# chart overlay
par(mfrow=c(1,1))
plot(forecast,
     main="12-Step Ahead Forecast and Original & Estimated Series",
     xlab="Time Period", 
     ylab="Original and Forecasted Values",
     lty=2,lwd=1.5)
par(new=T)
#plot.ts(fitted(ts2.fit),axes=F, col="blue", 
#        lty=1, lwd=2, xlab="",ylab="",xlim=c(2011,2016))
leg.txt <- c("Original Series", "Forecast")
legend("topleft", legend=leg.txt, lty=c(2,2,1), lwd=c(1,2,2),
       col=c("black","blue"), bty='n', cex=1)