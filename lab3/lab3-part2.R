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
          title="DXCM Series Descriptive Statistics", digits=1,
          covariate.labels = "DXCM Series")

## ---- part2_summarize_data_latex -----
fit.df <- data.frame(ts1)
class(df)
stargazer(fit.df, type="latex", header=FALSE,
          title="DXCM Series Descriptive Statistics", digits=1,
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
# 14 and 32 which could be spurious. The mean of the series is 23.2.

## ---- part2_timeseries_frequency ----

ts1 <- ts(ts1.csv_load$DXCM.Close, frequency = 1)

## ---- part2_diff_calcs ----
# detrend by taking the first difference
ts1.diff <- diff(ts1)

## ---- part2_summarize_diff_data_text -----
fit.df <- data.frame(ts1.diff)
class(df)
stargazer(fit.df, type="text", header=FALSE,
          title="Differenced Series Descriptive Statistics", digits=1,
          covariate.labels = "DXCM Differenced Series")

## ---- part2_summarize_diff_data_latex -----
fit.df <- data.frame(ts1.diff)
class(df)
stargazer(fit.df, type="latex", header=FALSE,
          title="Differenced Series Descriptive Statistics", digits=1,
          covariate.labels = "DXCM Differenced Series")

# The mean of the differenced time series is 0.02 so much closer to 0.

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
# but for now we're going to stick with an ARIMA model

## ---- part2_models ----
# ARIMA model selection code from the book and async
# procedure to get best ARIMA order
get.best.arima <- function(x.ts, maxord = c(1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3]) 
  {
    fit <- arima(x.ts, order = c(p,d,q))
    fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
    if (fit.aic < best.aic) 
    {
      best.aic <- fit.aic
      best.fit <- fit
      best.model <- c(p,d,q) 
    }
  }
  list(best.aic, best.fit, best.model)
}
# model selection
order = get.best.arima(ts1, maxord = c(2,2,2))[[3]]

# best model is ARIMA(0,1,0)
ts1.fit1 <- Arima(ts1, order=order)
summary(ts1.fit1)
t(confint(ts1.fit1)) # there aren't any coefficients

## ---- part2_arima_summary_text ----

fit.df <- data.frame(cbind(ts1, fitted(ts1.fit1), ts1.fit1$residuals))
class(df)
stargazer(fit.df, type="text",title="Comparative Statistics", digits=1,
          header=FALSE,
          covariate.labels = c("DXCS Series","ARIMA(0,1,0) Model", "Residuals"))

## ---- part2_arima_summary_latex ----

fit.df <- data.frame(cbind(ts1, fitted(ts1.fit1), ts1.fit1$residuals))
class(df)
stargazer(fit.df, type="latex",title="Comparative Statistics", digits=1,
          header=FALSE,
          covariate.labels = c("DXCS Series","ARIMA(0,1,0) Model", "Residuals"))

## ---- part2_model_plot_ts ----
par(mfrow=c(1,1))
plot.ts(ts1, col='cyan', 
        main='Time Series vs. ARIMA(0,1,0) Model',
        ylab='Original and Estimated Values', xlab='Period',
        pch=1, lty=1)
par(new=T)
plot.ts(fitted(ts1.fit1), col='navy', axes=F,
        xlab='',ylab='', lty=2)
par(new=T)
plot.ts(ts1.fit1$residuals, axes=F, xlab='', ylab='', col='green',
        lty=2, pch=1, col.axis='green', ylim=c(-15,50))
axis(side=4, col='green')
leg.txt <- c("Original Series", "ARIMA(0,1,0)", "Residual")
legend("topleft", legend=leg.txt, lty=c(2,1), col=c("cyan","navy","green"),
       bty='n', cex=1)
# a really good fit but...

## ---- part2_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.fit1$residuals, main="ARIMA(0,1,0) Residuals", 
        ylab="Price", xlab="Interval")
hist(ts1.fit1$residuals, main="Histogram of ARIMA(0,1,0) Residuals",
     xlab="log(Price)", breaks=50)
acf(ts1.fit1$residuals, main="ACF of ARIMA(0,1,0) Residuals",
    xlab="Lag")
acf(ts1.fit1$residuals^2, main="ACF of ARIMA(0,1,0) Residuals Squared",
     xlab="Lag")

## ---- part2_garch_model ----
# use a GARCH model for the residuals
ts1.garch <- garchFit(ts1.fit1$residuals~garch(1,1), trace=FALSE)
summary(ts1.garch)

## ---- part2_garch_model_residual_plots ----
res <- ts1.garch@residuals
par(mfrow=c(2,2))
plot.ts(res, main="GARCH(1,1) Residuals", 
        ylab="Price", xlab="Interval")
hist(res, main="Histogram GARCH(1,1) Residuals",
     xlab="log(Price)", breaks=50)
acf(res, main="ACF GARCH(1,1) Residuals",
    xlab="Lag", na.action = na.omit)
acf(res, main="ACF GARCH(1,1) Residuals Squared",
    xlab="Lag", na.action = na.omit)

## ---- part2_garch_model_table_latex ----
texreg(ts1.garch, digits=3, 
       caption.above = 'GARCH(1,1) Model Parameters',
       custom.model.names = c('GARCH(1,1)'))

## ---- part2_garch_model_summary ---
summary(ts1.garch)

## ---- part2_forecast_model ----

# standard error of the residuals from the ARIMA model
se.resid <- sqrt(ts1.fit1$sigma2) 

# forecast with GARCH model
ts1.garch.fcast <- predict(ts1.garch, n.ahead = 36)
# forecast with ARIMA model
ts1.forecast <- forecast.Arima(ts1.fit1, h=36)
# standard errors from the ARIMA model (for prediction)
se.arima <- (ts1.forecast$upper[,2]-ts1.forecast$mean)/1.96
# forecasting conditional standard errors from GARCH
cse.garch <- ts1.garch.fcast$standardDeviation
# put the conditional SE back to ARIMA SE
se.combine <- se.arima / se.resid * cse.garch
ts1.forecast$mean <- ts1.forecast$mean + ts1.garch.fcast$meanForecast 
ts1.forecast$lower[,2] <- ts1.forecast$mean - 1.96 * se.combine
ts1.forecast$lower[,1] <- ts1.forecast$mean - 1.645 * se.combine
ts1.forecast$upper[,2] <- ts1.forecast$mean + 1.96 * se.combine
ts1.forecast$upper[,1] <- ts1.forecast$mean + 1.645 * se.combine

## ---- part2_forecast_plot ----
par(mfrow=c(1,1))
plot(ts1.forecast,
     main="36-Step Ahead Forecast and Original & Estimated Series",
     xlab="Time Period", 
     ylab="Original, Estimated, and Forecasted Values",
     xlim=c(2100,2368),ylim=c(50,100),lty=2,lwd=1.5)
par(new=T)
plot.ts(fitted(ts1.fit1),col="blue", 
        lty=2, lwd=2, xlab="",ylab="",xlim=c(2100,2368),ylim=c(50,100))
leg.txt <- c("Original", "Estimated", "Forecast")
legend("topright", legend=leg.txt, lty=c(2,2,1), lwd=c(1,2,2),
       col=c("black","blue","blue"), bty='n', cex=1)

## ---- part2_end ----

####################################################################
#
# This section is exploratory code and experimentation with garch
#
####################################################################


# Examine the results of an auto model algorithm such as auto.arima
## ---- part2_auto_model ----

# first lets try an auto.arima fit and compare
ts1.auto <- auto.arima(ts1)
summary(ts1.auto)  # ARIMA(2,1,2)
# the residuals definitely show time variance

## ---- part2_auto_model_summary_text ----

fit.df <- data.frame(cbind(ts1, fitted(ts1.auto), ts1.auto$residuals))
class(df)
stargazer(fit.df, type="text",title="Comparative Statistics", digits=1,
          header=FALSE,
          covariate.labels = c("DXCS Series","ARIMA(2,1,2) Model", "Residuals"))

## ---- part2_auto_model_summary_latex ----

fit.df <- data.frame(cbind(ts1, fitted(ts1.auto), ts1.auto$residuals))
class(df)
stargazer(fit.df, type="latex",title="Comparative Statistics", digits=1,
          header=FALSE,
          covariate.labels = c("DXCS Series","ARIMA(2,1,2) Model", "Residuals"))

## ---- part2_auto_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.auto$residuals, main="ARIMA(2,1,2) Residuals", 
        ylab="Price", xlab="Interval")
hist(ts1.auto$residuals, main="Histogram of ARIMA(2,1,2) Residuals",
     xlab="log(Price)", breaks=50)
acf(ts1.auto$residuals, main="ACF of ARIMA(2,1,2) Residuals",
    xlab="Lag")
acf(ts1.auto$residuals^2, main="ACF of ARIMA(2,1,2) Residuals Squared",
     xlab="Lag")

## ---- part2_auto_model_plot_ts ----
# plot the model and series together for a comparative visual
par(mfrow=c(1,1))
plot.ts(ts1, col="navy", 
        main="DXCM Time Series",
        ylab="Price",
        xlab="Interval",
        ylim=c(0,100), pch=1, lty=1)
par(new=T)
plot.ts(fitted(ts1.auto),col="blue",axes=F,
        xlab="",ylab="", lty=2) 
par(new=T)
plot.ts(ts1.auto$residuals, axes=F, xlab='', ylab='', col='green',
        lty=2, pch=1, col.axis='green', ylim=c(-15,50))
axis(side=4, col='green')
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend=leg.txt, lty=c(1,1,1), 
       col=c("navy","blue","green"), bty='n', cex=1)

# Examine the results of an auto algorithm on the log series
## ---- part2_auto_log_model ----

# first lets try an auto.arima fit and compare
# set for stationary models only
ts1.log <- log(ts1)
ts1.autolog <- auto.arima(ts1.log)
summary(ts1.autolog)  # ARIMA(0,1,0) !
# the residuals definitely show time variance

## ---- part2_auto_log_model_summary_text ----

fit.df <- data.frame(cbind(ts1, exp(fitted(ts1.autolog)), ts1.autolog$residuals))
class(df)
stargazer(fit.df, type="text",title="Comparative Statistics", digits=1,
          header=FALSE,
          covariate.labels = c("Log(Series)","ARIMA(0,1,0) Model", "Residuals"))

## ---- part2_auto_log_model_summary_latex ----

fit.df <- data.frame(cbind(ts1, fitted(ts1.autolog), ts1.autolog$residuals))
class(df)
stargazer(fit.df, type="latex",title="Comparative Statistics", digits=1,
          header=FALSE,
          covariate.labels = c("Log(Series)","ARIMA(0,1,0) Model", "Residuals"))

## ---- part2_auto_log_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.autolog$residuals, main="ARIMA(0,1,0) Residuals", 
        ylab="Price", xlab="Interval")
hist(ts1.autolog$residuals, main="Histogram ARIMA(0,1,0) Residuals",
     xlab="log(Price)", breaks=50)
acf(ts1.autolog$residuals, main="ACF of ARIMA(0,1,0) Residuals",
    xlab="Lag")
acf(ts1.autolog$residuals^2, main="ACF of ARIMA(0,1,0) Residuals Squared",
    xlab="Lag")

## ---- part2_auto_log_model_plot_ts ----
# plot the model and series together for a comparative visual
# make the residuals comparable by taking e^residual
automodelres <- exp(ts1.autolog$residuals)
par(mfrow=c(1,1))
plot.ts(ts1, col="navy", 
        main="DXCM Time Series",
        ylab="Price",
        xlab="Interval",
        ylim=c(0,100), pch=1, lty=1)
par(new=T)
plot.ts(exp(fitted(ts1.autolog)),col="blue",axes=F,
        xlab="",ylab="", lty=2) 
par(new=T)
plot.ts(ts1.autolog$residuals, axes=F, xlab='', ylab='', col='green',
        lty=2, pch=1, col.axis='green', ylim=c(-0.5,1))

axis(side=4, col='green')
leg.txt <- c("Original Series", "Estimated Log-Series", "Residuals")
legend("topleft", legend=leg.txt, lty=c(1,1,1), 
       col=c("navy","blue","green"), bty='n', cex=1)


## ---- part2_model_comparison_table_text ----
screenreg(l=list(ts1.auto, ts1.autolog), digits=3,
          custom.model.names = c('ARIMA(2,1,2)','ARIMA(0,1,0)'))

## ---- part2_model_comparison_table_latex ----
texreg(l=list(ts1.auto, ts1.autolog), digits=3, 
       caption.above = 'ARIMA(2,1,2) and ARIMA(0,1,0)',
       custom.model.names = c('ARIMA(2,1,2)','ARIMA(0,1,0)'))

## ---- part2_auto_garch_model ----
# use a GARCH model for the residuals
ts1.garch <- garchFit(ts1.autolog$residuals~garch(1,1), trace=FALSE)
#ts1.garch <- garch(ts1.autolog$residuals, trace=F)
summary(ts1.garch)
#ts1.res <- ts1.garch$res[-1]
ts1.res <- ts1.garch@residuals[-1]

# all parameters are highly significant and the Box-Llung test fails
# to reject the null hypothesis that the residuals are different 
# than 0.
## ---- part2_garch_table_text ----
screenreg(ts1.garch, digits=3,
          custom.model.names = c('GARCH(1,1'))

## ---- part2_garch_table_latex ----
texreg(ts1.garch, digits=3, 
       caption.above = 'GARCH(1,1) Model Parameters',
       custom.model.names = c('GARCH(1,1)'))

## ---- part2_garch_summary ---
summary(ts1.garch)

## ---- part2_auto_garch_model_residual_plots ----
par(mfrow=c(2,2))
plot.ts(ts1.res, main="GARCH(1,1) Residuals", 
        ylab="Residual", xlab="Interval")
hist(ts1.res, main="Histogram ofGARCH(1,1) Residuals",
     xlab="log(Price)", breaks=50)
acf(ts1.res, main="ACF of GARCH(1,1) Residuals",
    xlab="Lag")
acf(ts1.res, main="ACF of GARCH(1,1) Residuals Squared",
    xlab="Lag")
