#########################################################################
#
# Lab 3, Part 2
#
#########################################################################
## ---- part2_setup ----
library(ggplot2)
library(psych)
library(astsa)
library(fGarch)
library(tseries)
library(zoo)
library(forecast)
library(stargazer)
library(texreg)
library(xtable)
options(xtable.comment=FALSE)


#########################################################################
#
# Lab 3, Part 2 - Modeling and Forecasting a Real-World Macroeconomic / 
#                 Financial time series
#
# Build a time-series model for the series in lab3_series02.csv, which is
# extracted from a real-world macroeconomic/financial time series, and
# use it to perform a 36-step ahead forecast. The periodicity of the series
# is purposely not provided. Possible models include AR, MA, ARMA, ARIMA, 
# Seasonal ARIMA, GARCH, ARIMA-GARCH, or Seasonal ARIMA-GARCH models.
# 
#########################################################################
## ---- part2_loaddata ----
ts1.csv_load <- read.csv('lab3_data/lab3_series02.csv')
#ts1 <- ts(ts1.csv_load$DXCM.Close, frequency = 365.25/7) # weekly)
ts1 <- ts(ts1.csv_load$DXCM.Close)

# Exploratory Data Analysis

# we don't know the frequency of the data
summary(ts1)
head(ts1, 5)
tail(ts1, 5)
str(ts1)

## ---- part2_summarize_data_text -----
fit.df <- data.frame(ts1)
class(df)
stargazer(fit.df, type="text", header=FALSE,
          title="Descriptive Statistics", digits=1,
          covariate.labels = "DXCM Series")

## ---- part2_summarize_data_latex -----
fit.df <- data.frame(ts1)
class(df)
stargazer(fit.df, type="latex", header=FALSE,
          title="Descriptive Statistics", digits=1,
          covariate.labels = "DXCM Series")

## ---- part2_tsplots ----
par(mfrow=c(2,2))
plot.ts(ts1, main="DXCM Series", 
        ylab="Price", xlab="Interval")
hist(ts1, main="Histogram of DXCM Series",
     xlab="Price", breaks=50)
acf(ts1, main="Autocorrelation of DXCM",
    xlab="Lag")
pacf(ts1, main="Partial Autocorrelation of DXCM",
     xlab="Lag")

# The time series plot reveals that the DXCM time series is a persistently
# upward trending series and is not stationary. There are shocks at time
# period 500, 1200, 1800, and 2200 (approximately). The autocorrelation 
# shows a slight decay over more than 30 lags while the partial 
# autocorrelation shows bareley statistically significant results at lags
# 14 and 32 which could be spurious. There is seasonality in the time series
# so we need to recast the ts object with a frequency. Since this is a daily
# closing price and markets are open 5 days/week we will set the frequency
# as weekly even though there are not the same number of weeks every year.

## ---- part2_timeseries_frequency ----

ts1 <- ts(ts1.csv_load$DXCM.Close, frequency = 365.25/7)

## ---- part2_diff_calcs ----
# detrend by taking the first difference
ts1.diff <- diff(ts1)

summary(ts1.diff)

## ---- part2_diff_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.diff, main="First Difference", 
        ylab="Price", xlab="Interval")
hist(ts1.diff, main="Histogram of First Difference",
     xlab="Price", breaks=50)
acf(ts1.diff, main="Autocorrelation of First Difference",
    xlab="Lag")
pacf(ts1.diff, main="Partial Autocorrelation of First Difference",
     xlab="Lag")

# The plot of the first differenced series indicates an increasing volatility
# beginning around 1600 with some large spikes. The ACF indicates no significant
# correlations until lag 13, 15, 16, 24, 31
# The PACF shows a cycling pattern with significance at lags 11, 13-16, 24,25,31.

# Examine the differenced log values

## ---- part2_difflog_calcs ----
ts1.diff_log <- diff(log(ts1))
summary(ts1.diff_log)

## ---- part2_difflog_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.diff_log, main="First Differenced Log-Series", 
        ylab="Price", xlab="Interval")
hist(ts1.diff_log, main="Histogram of First Differenced Log",
     xlab="log(Price)", breaks=50)
acf(ts1.diff_log, main="ACF of First Differenced Log-Series",
    xlab="Lag", lag.max = 30)
pacf(ts1.diff_log, main="PACF of First Differenced Log-Series",
     xlab="Lag", lag.max = 30)


## ---- part2_diff2_calcs ----
ts1.diff2 <- diff(ts1.diff)

summary(ts1.diff2)

## ---- part2_diff2_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.diff2, main="Twice Differenced Series", 
        ylab="Price", xlab="Interval")
hist(ts1.diff2, main="Histogram of Twice Differenced",
     xlab="log(Price)", breaks=50)
acf(ts1.diff2, main="ACF of Twice Differenced Series",
    xlab="Lag")
pacf(ts1.diff2, main="PACF of Twice Differenced Series",
     xlab="Lag")

# Only the plot of the log(diff) is very different - the volatility is shifted up to
# around 500, with spikes occuring more frequently throughout. Need to understand 
# better what the PACF is telling me

## ---- part2_seasonal_calcs ----
# Our assumption is a weekly seasonality, so every 5 business days. Let's examine
# a first differenced seasonality to get some hints for model parameters
ts1.seasonal_diff <- diff(ts1, lag=5)

## ---- part2_seasonal_plots ----
# Compare the seasonal ACF and PACF differenced series plots to
# the ACF/PACF of the differenced series
par(mfrow=c(2,2))
acf(ts1.seasonal_diff, main="ACF Differenced Seasonal, Lag=5",
    xlab="Lag")
pacf(ts1.seasonal_diff, main="PACF of Differenced Seasonal, Lag=5",
     xlab="Lag")
acf(ts1.diff, main="ACF Differenced Series",
    xlab="Lag")
pacf(ts1.diff, main="PACF of Difference Series",
     xlab="Lag")

# the plots suggest an underlying AR process for seasonality

## ---- part2_models ----

ts1.fit1 <- Arima(ts1, order=c(2,1,2), seasonal = list(order=c(1,1,0), 365.25/7))
summary(ts1.fit1)
t(confint(ts1.fit1))

plot.ts(ts1.fit1$residuals)
acf(ts1.fit1$residuals^2, main="",
    xlab="Lag")

## ---- part2_arima_summary_text ----

fit.df <- data.frame(cbind(ts1, fitted(ts1.fit1), ts1.fit1$residuals))
class(df)
stargazer(fit.df, type="text",title="Descriptive Statistics", digits=1,
          covariate.labels = c("DXCS Series","Model", "Residuals"))

## ---- part2_arima_summary_latex ----

fit.df <- data.frame(cbind(ts1, fitted(ts1.fit1), ts1.fit1$residuals))
class(df)
stargazer(fit.df, type="latex",title="Descriptive Statistics", digits=1,
          covariate.labels = c("DXCS Series","Model", "Residuals"))

par(mfrow=c(1,1))
plot.ts(ts1, col='blue', 
        main='Time Series vs. ARIMA(1,1,1) Model',
        ylab='Original and Estimated Values', xlab='Period',
        pch=1, lty=2)
par(new=T)
plot.ts(fitted(ts1.fit1), col='navy', axes=F,
        xlab='',ylab='', lty=1)
par(new=T)
plot.ts(ts1.fit1$residuals, axes=F, xlab='', ylab='', col='green',
        lty=2, pch=1, col.axis='green', ylim=c(-15,50))
axis(side=4, col='green')
leg.txt <- c("Original Series", "ARIMA(1,1,1)")
legend("topleft", legend=leg.txt, lty=c(2,1), col=c("blue","green"),
       bty='n', cex=1)
# a really good fit but...
# do we need to add a GARCH model on the residuals of the ARIMA(1,1,1) model??

## ---- part2_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.fit1$residuals, main="ARIMA(1,1,1) Residuals", 
        ylab="Price", xlab="Interval")
hist(ts1.fit1$residuals, main="Histogram of ARIMA(1,1,1) Residuals",
     xlab="log(Price)", breaks=50)
acf(ts1.fit1$residuals, main="ACF of ARIMA(1,1,1) Residuals",
    xlab="Lag")
acf(ts1.fit1$residuals^2, main="ACF of ARIMA(1,1,1) Residuals Squared",
     xlab="Lag")

## ---- part2_garch_model ----
# use a GARCH model for the residuals
ts1.garch <- garch(ts1.fit1$residuals, order=c(0,8), trace=FALSE)
summary(ts1.garch)

## ---- part2_garch_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.garch$residuals, main="ARIMA(1,1,1)GARCH(1,1) Residuals", 
        ylab="Price", xlab="Interval")
hist(ts1.garch$residuals, main="Histogram of ARIMA(1,1,1)GARCH(1,1) Residuals",
     xlab="log(Price)", breaks=50)
acf(ts1.garch$residuals, main="ACF of ARIMA(1,1,1)GARCH(1,1) Residuals",
    xlab="Lag", na.action = na.omit)
acf(ts1.garch$residuals^2, main="ACF of ARIMA(1,1,1)GARCH(1,1) Residuals Squared",
    xlab="Lag", na.action = na.omit)

## ---- part2_auto_model ----

# first lets try an auto.arima fit and compare
ts1.auto <- auto.arima(log(ts1))
summary(ts1.auto)  # ARIMA(2,1,2)
# the residuals definitely show time variance

## ---- part2_auto_model_summary_text ----

fit.df <- data.frame(cbind(ts1, fitted(ts1.auto), ts1.auto$residuals))
class(df)
stargazer(fit.df, type="text",title="Descriptive Statistics", digits=1,
          covariate.labels = c("DXCS Series","Model", "Residuals"))

## ---- part2_auto_model_summary_latex ----

fit.df <- data.frame(cbind(ts1, fitted(ts1.auto), ts1.auto$residuals))
class(df)
stargazer(fit.df, type="text",title="Descriptive Statistics", digits=1,
          covariate.labels = c("DXCS Series","Model", "Residuals"))

## ---- part2_auto_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.auto$residuals, main="ARIMA(2,1,1) Residuals", 
        ylab="Price", xlab="Interval")
hist(ts1.auto$residuals, main="Histogram of ARIMA(2,1,1) Residuals",
     xlab="log(Price)", breaks=50)
acf(ts1.auto$residuals, main="ACF of ARIMA(2,1,1) Residuals",
    xlab="Lag")
acf(ts1.auto$residuals^2, main="ACF of ARIMA(2,1,1) Residuals Squared",
     xlab="Lag")

## ---- part2-auto_garch_model ----
# use a GARCH model for the residuals
ts1.garch <- garchFit(~ garch(1,1), data=ts1.auto$residuals, trace=FALSE)
summary(ts1.garch)

## ---- part2_auto_garch_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.garch@residuals, main="ARIMA(2,1,1)GARCH(1,1) Residuals", 
        ylab="Price", xlab="Interval")
hist(ts1.garch@residuals, main="Histogram of ARIMA(2,1,1)GARCH(1,1) Residuals",
     xlab="log(Price)", breaks=50)
acf(ts1.garch@residuals, main="ACF of ARIMA(2,1,1)GARCH(1,1) Residuals",
    xlab="Lag")
acf(ts1.garch@residuals^2, main="ACF of ARIMA(2,1,1)GARCH(1,1) Residuals Squared",
    xlab="Lag")


## ---- part2_difflog_auto_model ----

ts1.difflog.auto <- auto.arima(diff(log(ts1)), seasonal = TRUE)
summary(ts1.difflog.auto)  # ARIMA(0,1,0)
# the residuals definitely show time variance

## ---- part2_difflog_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.difflog.auto$residuals, main="ARIMA(2,1,1) Residuals", 
        ylab="Price", xlab="Interval")
hist(ts1.difflog.auto$residuals, main="Histogram of ARIMA(2,1,1) Residuals",
     xlab="log(Price)", breaks=50)
acf(ts1.difflog.auto$residuals, main="ACF of ARIMA(2,1,1) Residuals",
    xlab="Lag")
acf(ts1.difflog.auto$residuals^2, main="ACF of ARIMA(2,1,1) Residuals Squared",
    xlab="Lag")

## ---- part2-auto_garch_model ----
# use a GARCH model for the residuals
ts1.garch <- garchFit(~ garch(1,1), data=ts1.difflog.auto$residuals, trace=FALSE)
summary(ts1.garch)

## ---- part2_auto_garch_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.garch@residuals, main="ARIMA(2,1,1)GARCH(1,1) Residuals", 
        ylab="Price", xlab="Interval")
hist(ts1.garch@residuals, main="Histogram of ARIMA(2,1,1)GARCH(1,1) Residuals",
     xlab="log(Price)", breaks=50)
acf(ts1.garch@residuals, main="ACF of ARIMA(2,1,1)GARCH(1,1) Residuals",
    xlab="Lag")
acf(ts1.garch@residuals^2, main="ACF of ARIMA(2,1,1)GARCH(1,1) Residuals Squared",
    xlab="Lag")


## ---- part2_multiple_model_calcs ----
logts1 <- log(ts1)

# compute non-seasonal models on the differenced series. We'll actually be computing
# ARIMA models of order d=1, but we need to use fGarch to forecast so we need to use
# an ARMA model instead on the differenced series.
a1 <- Arima(ts1.diff_log, order=c(0,0,0))
a2 <- Arima(ts1.diff_log, order=c(1,0,0))
a3 <- Arima(ts1.diff_log, order=c(0,0,1))
a4 <- Arima(ts1.diff_log, order=c(1,0,1))
a5 <- Arima(ts1.diff_log, order=c(2,0,1))
a6 <- Arima(ts1.diff_log, order=c(1,0,2))
a7 <- Arima(ts1.diff_log, order=c(2,0,2))
a8 <- Arima(ts1.diff_log, order=c(2,0,3))
a9 <- Arima(ts1.diff_log, order=c(3,0,2))
a10 <- Arima(ts1.diff_log, order=c(3,0,3))

m1 <- Arima(logts1, order=c(0,1,1), seasonal=list(order=c(0,1,0), 365.25/7))
m2 <- Arima(logts1, order=c(0,1,1), seasonal=list(order=c(0,1,1), 365.25/7))
m3 <- Arima(logts1, order=c(0,1,1), seasonal=list(order=c(1,1,0), 365.25/7))
m4 <- Arima(logts1, order=c(0,1,1), seasonal=list(order=c(1,1,1), 365.25/7))
m5 <- Arima(logts1, order=c(1,1,0), seasonal=list(order=c(0,1,0), 365.25/7))
m6 <- Arima(logts1, order=c(1,1,0), seasonal=list(order=c(0,1,1), 365.25/7))
m7 <- Arima(logts1, order=c(1,1,0), seasonal=list(order=c(1,1,0), 365.25/7))
m8 <- Arima(logts1, order=c(1,1,0), seasonal=list(order=c(1,1,1), 365.25/7))
m9 <- Arima(logts1, order=c(1,1,1), seasonal=list(order=c(0,1,0), 365.25/7))
m10 <- Arima(logts1, order=c(1,1,1), seasonal=list(order=c(0,1,1), 365.25/7))
m11 <- Arima(logts1, order=c(1,1,1), seasonal=list(order=c(1,1,0), 365.25/7))
m12 <- Arima(logts1, order=c(1,1,1), seasonal=list(order=c(1,1,1), 365.25/7))
m13 <- Arima(logts1, order=c(0,1,2), seasonal=list(order=c(0,1,0), 365.25/7))
m14 <- Arima(logts1, order=c(0,1,2), seasonal=list(order=c(0,1,1), 365.25/7))
m15 <- Arima(logts1, order=c(0,1,2), seasonal=list(order=c(1,1,0), 365.25/7))
m16 <- Arima(logts1, order=c(0,1,2), seasonal=list(order=c(1,1,1), 365.25/7))
m17 <- Arima(logts1, order=c(1,1,2), seasonal=list(order=c(0,1,0), 365.25/7))
m18 <- Arima(logts1, order=c(1,1,2), seasonal=list(order=c(0,1,1), 365.25/7))
m19 <- Arima(logts1, order=c(1,1,2), seasonal=list(order=c(1,1,0), 365.25/7))
m20 <- Arima(logts1, order=c(1,1,2), seasonal=list(order=c(1,1,1), 365.25/7))
m21 <- Arima(logts1, order=c(2,1,2), seasonal=list(order=c(0,1,0), 365.25/7))
m22 <- Arima(logts1, order=c(2,1,2), seasonal=list(order=c(0,1,1), 365.25/7))
m23 <- Arima(logts1, order=c(2,1,2), seasonal=list(order=c(1,1,0), 365.25/7))
m24 <- Arima(logts1, order=c(2,1,2), seasonal=list(order=c(1,1,1), 365.25/7))

## ---- part2_multiple_model_table1_text ----
screenreg(list(m1,m2,m3,m4,m5,m6,m7,m8),
          custom.model.names = c('(0,1,1)|(0,1,0)','(0,1,1)|(0,1,1)',
                                 '(0,1,1)|(1,1,0)','(0,1,1)|(1,1,1)',
                                 '(1,1,0)|(0,1,0)','(1,1,0)|(1,1,1)',
                                 '(1,1,0)|(1,1,0)','(1,1,0)|(1,1,1)'),
          reorder.coef = c(4,1,3,2))

## ---- part2_multiple_model_table1_latex ----
texreg(list(m1,m2,m3,m4,m5,m6,m7,m8),
          custom.model.names = c('(0,1,1)|(0,1,0)','(0,1,1)|(0,1,1)',
                                 '(0,1,1)|(1,1,0)','(0,1,1)|(1,1,1)',
                                 '(1,1,0)|(0,1,0)','(1,1,0)|(1,1,1)',
                                 '(1,1,0)|(1,1,0)','(1,1,0)|(1,1,1)'),
          reorder.coef = c(4,1,3,2))

## ---- part2_multiple_model_table2_text ----
screenreg(list(m9,m10,m11,m12,m13,m14,m15,m16),
          custom.model.names = c('(1,1,1)|(0,1,0)','(1,1,1)|(0,1,1)',
                                 '(1,1,1)|(1,1,0)','(1,1,1)|(1,1,1)',
                                 '(0,1,2)|(0,1,0)','(0,1,2)|(1,1,1)',
                                 '(0,1,2)|(1,1,0)','(0,1,2)|(1,1,1)'),
          reorder.coef = c(1,2,5,4,3))

## ---- part2_multiple_model_table2_latex ----
texreg(list(m9,m10,m11,m12,m13,m14,m15,m16),
          custom.model.names = c('(1,1,1)|(0,1,0)','(1,1,1)|(0,1,1)',
                                 '(1,1,1)|(1,1,0)','(1,1,1)|(1,1,1)',
                                 '(0,1,2)|(0,1,0)','(0,1,2)|(1,1,1)',
                                 '(0,1,2)|(1,1,0)','(0,1,2)|(1,1,1)'),
          reorder.coef = c(1,2,5,4,3))

## ---- part2_multiple_model_table3_text ----
screenreg(list(m17,m18,m19,m20,m21,m22,m23,m24),
          custom.model.names = c('(1,1,2)|(0,1,0)','(1,1,2)|(0,1,1)',
                                 '(1,1,2)|(1,1,0)','(1,1,2)|(1,1,1)',
                                 '(2,1,2)|(0,1,0)','(2,1,2)|(1,1,1)',
                                 '(2,1,2)|(1,1,0)','(2,1,2)|(1,1,1)'),
          reorder.coef = c(1,6,2,3,5,4))

## ---- part2_multiple_model_table3_latex ----
texreg(list(m17,m18,m19,m20,m21,m22,m23,m24),
          custom.model.names = c('(1,1,2)|(0,1,0)','(1,1,2)|(0,1,1)',
                                 '(1,1,2)|(1,1,0)','(1,1,2)|(1,1,1)',
                                 '(2,1,2)|(0,1,0)','(2,1,2)|(1,1,1)',
                                 '(2,1,2)|(1,1,0)','(2,1,2)|(1,1,1)'),
          reorder.coef = c(1,6,2,3,5,4))

## ---- part2_nonseasonal_model_table1_text ----
screenreg(list(a2,a3,a4,a5,a6,a7,a8,a9,a10),
          custom.model.names = c('(1,1,0)','(0,1,1)','(1,1,1)',
                                 '(2,1,1)','(1,1,2)','(2,1,2)',
                                 '(2,1,3)','(3,1,2)','(3,1,3)'),
          reorder.coef = c(2,1,4,7,3,5,6))

## ---- part2_nonseasonal_model_table1_latex ----
texreg(list(m1,m2,m3,m4,m5,m6,m7,m8),
       custom.model.names = c('(1,1,0)',
                              '(0,1,1)','(1,1,1)',
                              '(2,1,1)','(1,1,2)',
                              '(2,1,2)','(2,1,3)',
                              '(3,1,2)','(3,1,3)'),
       reorder.coef = c(2,1,4,7,3,5,6))


## ---- part2_nonseasonal_model_residual_plot ----
ns <- a9  # a10 is our best pick
par(mfrow=c(2,2))
plot.ts(ns$residuals, main="ARIMA Residuals", 
        ylab="Residual", xlab="Interval")
hist(ns$residuals, main="Histogram of ARIMA Residuals",
     xlab="log(Price)", breaks=50)
acf(ns$residuals, main="ACF of ARIMA Residuals",
    xlab="Lag")
acf(ns$residuals^2, main="ACF of ARIMA Residuals Squared",
    xlab="Lag")

## ---- part2_nonseasonal_model_root_plot ----
plot(ns)

## ---- part2_garch_nonseasonal_model ----
# use a GARCH model for the residuals
ns.garch <- garchFit(~arma(1,1)+garch(1,1), ts1.diff_log, trace=FALSE)

## ---- part2_garch_nonseasonal_model_ci ----
screenreg(ns.garch, digits=4, custom.model.names = c('ARIMA(1,1,1)-GARCH(1,1)'))

## ---- part2_garch_nonseasonal_model_residual_plot ----
par(mfrow=c(2,2))
plot.ts(a8.garch@residuals, main="ARCH Residuals", 
        ylab="Price", xlab="Interval")
hist(a8.garch@residuals, main="Histogram of ARCH Residuals",
     xlab="log(Price)", breaks=50)
acf(a8.garch@residuals, main="ACF of ARCH Residuals",
    xlab="Lag", na.action = na.omit)
acf(a8.garch@residuals^2, main="ACF of ARCH Residuals Squared",
    xlab="Lag", na.action = na.omit)


## ---- part2_multi_model_residual_plot_12 ----
par(mfrow=c(2,2))
plot.ts(m12$residuals, main="SARIMA Residuals", 
        ylab="Residual", xlab="Interval")
hist(m12$residuals, main="Histogram of SARIMA Residuals",
     xlab="log(Price)", breaks=50)
acf(m12$residuals, main="ACF of SARIMA Residuals",
    xlab="Lag")
acf(m12$residuals^2, main="ACF of SARIMAResiduals Squared",
    xlab="Lag")

## ---- part2_multi_model_root_plot_12 ----
plot(m12)

## ---- part2_garch_model12 ----
# use a GARCH model for the residuals
m12.garch <- garch(m12$residuals, order=c(0,7), trace=FALSE)

## ---- part2_garch_model12_ci ----
xtable(t(confint(m12.garch)), caption='ARCH(7) Coefficient Confidence Intervals')

## ---- part2_garch_model_residual_plot12 ----
par(mfrow=c(2,2))
plot.ts(m12.garch$residuals, main="ARCH Residuals", 
        ylab="Price", xlab="Interval")
hist(m12.garch$residuals, main="Histogram of ARCH Residuals",
     xlab="log(Price)", breaks=50)
acf(m12.garch$residuals, main="ACF of ARCH Residuals",
    xlab="Lag", na.action = na.omit)
acf(m12.garch$residuals^2, main="ACF of ARCH Residuals Squared",
    xlab="Lag", na.action = na.omit)
