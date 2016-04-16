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
# Lab 3, Part 2 - Modeling and Forecasting a Real-World Macroeconomic / 
#                 Financial time series
#
# Build a time-series model for the series in lab3_series02.csv, which is
# extracted from a real-world macroe- conomic/financial time series, and
# use it to perform a 36-step ahead forecast. The periodicity of the series
# is purposely not provided. Possible models include AR, MA, ARMA, ARIMA, 
# Seasonal ARIMA, GARCH, ARIMA-GARCH, or Seasonal ARIMA-GARCH models.
# 
#########################################################################
ts1.csv_load <- read.csv('lab3_data/lab3_series02.csv')
ts1 <- ts(ts1.csv_load$DXCM.Close)

# Exploratory Data Analysis

# we don't know the frequency of the data
summary(ts1)
head(ts1, 5)
tail(ts1, 5)
str(ts1)

par(mfrow=c(2,2))
plot.ts(ts1, main="Time Series of DXCM Close", 
        ylab="Price", xlab="Interval")
hist(ts1, main="Histogram of Time Series DXCM Close",
     xlab="Price", breaks=50)
acf(ts1, main="Autocorrelation of DXCM Close Series",
    xlab="Lag")
pacf(ts1, main="Partial Autocorrelation of DXCM Close Series",
     xlab="Lag")

# The time series plot reveals that the DXCM time series is a persistently
# upward trending series and is not stationary. There are shocks at time
# period 500, 1200, 1800, and 2200 (approximately). The autocorrelation 
# shows a slight decay over more than 30 lags while the partial 
# autocorrelation shows bareley statistically significant results at lags
# 14 and 32 which could be spurious. There doesn't appear to be any
# seasonality in this time series.

# detrend by taking the first difference
ts1.diff <- diff(ts1)

summary(ts1.diff)

par(mfrow=c(2,2))
plot.ts(ts1.diff, main="First Difference of DXCM Series", 
        ylab="Price", xlab="Interval")
hist(ts1.diff, main="Histogram of First Difference DXCM Series",
     xlab="Price", breaks=50)
acf(ts1.diff, main="Autocorrelation of DXCM Series First Difference",
    xlab="Lag")
pacf(ts1.diff, main="Partial Autocorrelation of DXCM Series First Difference",
     xlab="Lag")

# The plot of the first differenced series indicates an increasing volatility
# beginning around 1600 with some large spikes. The ACF indicates no significant
# correlations until lag 13, 15, 16, 24, 31
# The PACF shows a cycling pattern with significance at lags 11, 13-16, 24,25,31.

# Examine the differenced log values

ts1.diff_log <- diff(log(ts1))

summary(ts1.diff_log)

par(mfrow=c(2,2))
plot.ts(ts1.diff_log, main="First Difference of Log DXCM Series", 
        ylab="Price", xlab="Interval")
hist(ts1.diff_log, main="Histogram of First Difference Log DXCM Series",
     xlab="Price", breaks=50)
acf(ts1.diff_log, main="Autocorrelation of First Difference og Log DXCM Series",
    xlab="Lag")
pacf(ts1.diff_log, main="Partial Autocorrelation of First Difference Log DXCM Series",
     xlab="Lag")

# Only the plot of the log(diff) is very different - the volatility is shifted up to
# around 500, with spikes occuring more frequently throughout. Need to understand 
# better what the PACF is telling me

ts1.fit1 <- Arima(ts1, order=c(1,1,1))
summary(ts1.fit1)

ts1.fit2 <- Arima(ts1, order=c(2,1,1))
summary(ts1.fit2)

ts1.fit3 <- Arima(ts1, order=c(2,1,2))
summary(ts1.fit3)

plot(ts1.fit1)
plot.ts(ts1.fit1$residuals)

plot.ts(fitted(ts1.fit1))
Box.test(ts1.fit1$residuals, type="Ljung-Box")  # indicates that we can reject nill

fit.df <- data.frame(cbind(ts1, fitted(ts1.fit1), ts1.fit1$residuals))
class(df)
stargazer(fit.df, type="text",title="Descriptive Statistics", digits=1)


par(mfrow=c(1,1))
plot.ts(ts1, col='blue', 
        main='Time Series vs. ARIMA(1,1,1) Model',
        ylab='Original and Estimated Values', xlab='Period',
        pch=1, lty=2)
par(new=T)
plot.ts(fitted(ts1.fit1), col='navy', axes=F,
        xlab='',ylab='', lty=1)
leg.txt <- 
  par(new=T)
plot.ts(ts1.fit1$residuals, axes=F, xlab='', ylab='', col='green',
        lty=2, pch=1, col.axis='green', ylim=c(-15,50))
axis(side=4, col='green')

# a really good fit but...
# do we need to add a GARCH model on the residuals of the ARIMA(1,1,1) model??
