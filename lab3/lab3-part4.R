
#########################################################################
#
# Lab 3, Part 4
#
#########################################################################
## ---- part4_setup ----
library(ggplot2)
library(astsa)
library(zoo)
library(fGarch)
library(tseries)
library(forecast)
library(stargazer)
library(xtable)

# A function to output nice summaries into xtable
summaryfunction= function (x){
  if( is.numeric(x)!=TRUE) {stop("Supplied X is not numeric")}
  mysummary = data.frame(
    "Min." =as.numeric( min(x)),
    "1st Qu." = quantile(x)[2],
    "Median" = median(x),
    "Mean" = mean(x),
    "3rd Qu." = quantile(x)[4],
    "Max." = max(x),
    row.names=""

  )
  names(mysummary) = c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
  return( mysummary )
}
#########################################################################
#
# Lab 3, Part 4 - Forecast Inflation Adjusted Gas Price
#
# During 2013 amid high gas prices, the Associated Press (AP) published
# an article about the U.S. inflation- adjusted price of gasoline and U.S.
# oil production. The article claims that there is “evidence of no
# statistical correlation” between oil production and gas prices. The
# data was not made publicly available, but comparable data was created
# using data from the Energy Information Administration. The workspace
# and data frame gasOil.Rdata contains the U.S. oil production (in
# millions of barrels of oil) and the inflation-adjusted average gas
# prices (in dollars) over the date range the article indicates.
#
# In support of their conclusion, the AP reported a single p-value.
# You have two tasks for this exericse, and both tasks need the use
# of the data set gasOil.Rdata.
#
# Your first task is to recreate the analysis that the AP likely used
# to reach their conclusion. Thoroughly discuss all of the errors the
# AP made in their analysis and conclusion.
#
# Your second task is to create a more statistically-sound model that
# can be used to predict/forecast inflation- adjusted gas prices. Use
# your model to forecast the inflation-adjusted gas prices from 2012
# to 2016.
#
#########################################################################
## ---- part4_loaddata ----
load('lab3_data/gasOil.Rdata')

str(gasOil)
head(gasOil)
tail(gasOil)

# gasOil is a dataframe with 410 rows and 3 columns: Date,
# Production, Price

## ---- part4_summarize_data_text -----
stargazer(gasOil, type="text", header=FALSE,
          title="gasOil Series Descriptive Statistics", digits=2)

## ---- part4_summarize_data_latex -----
stargazer(gasOil, type="latex", header=FALSE,
          title="gasOil Series Descriptive Statistics", digits=2)

## ---- part4_create_ts ----
ts_prod <- ts(gasOil$Production, start=c(1978,1),
              end=c(2012,2), frequency=12)
ts_price <- ts(gasOil$Price, start=c(1978,1),
               end=c(2012,2), frequency=12)

## ---- part4_plot_ts ----
# plot the two series together for a comparative visual

par(mfrow=c(1,1), xpd=NA)
plot.ts(ts_prod, col="navy",
        main="US Oil Production and Inflation-Adjusted Gas Price",
        ylab="Million BBL/day",
        xlab="Year",
        ylim=c(100,300), pch=1, lty=1)
par(new=T)
plot.ts(ts_price,col="blue",axes=F,xlab="",ylab="",
        ylim=c(0.00,6.00), pch=1, lty=1, col.axis="blue")
leg.txt <- c("US Oil Production", "Gas Price")
legend("topleft", legend=leg.txt, lty=c(1,1),
       col=c("navy","blue"), bty='n', cex=1)
axis(side=4, col="blue")
mtext("Inlflation-Adjusted Dollars", line=2, side=4, col="blue")

# I'm going to guess that the only thing we can do with the
# tool set we have is to do a regression of one variable
# on the other. So let's look at things that way.

## ---- part4_plot_qplot ----
qplot(Production, Price, data=gasOil,
      geom=c("point", "smooth"),
      main="Gas Price and Oil Production",
      xlab="Gas Price US Dollars",
      ylab="Oil Production, Millions of Barrels")

## ---- part4_diff_series ----

# Let's look at the changes with respect to each other
df_diffs <- data.frame(diff(gasOil$Production), diff(gasOil$Price))
colnames(df_diffs) <- c('prod_diff','price_diff')
head(df_diffs)

# normalize/scale the variables
scaled.diffs <- as.data.frame(scale(df_diffs))

# create a new time series
delta.prod <- ts(scaled.diffs$prod_diff, frequency=12)
delta.price <- ts(scaled.diffs$price_diff, frequency=12)

df_h <- cbind(df_diffs, scaled.diffs)

## ---- part4_summarize_diff_data_text -----

stargazer(df_h, type="text", header=FALSE,
          title="Descriptive Statistics of Differenced Series",
          digits=2,
          covariate.labels = c('Change in Production',
                               'Change in Price',
                               'Scaled Change in Production',
                               'Scaled Change in Price'))

## ---- part4_summarize_diff_data_latex -----
stargazer(df_h, type="latex", header=FALSE,
          title="Descriptive Statistics of Differenced Series",
          digits=2,
          covariate.labels = c('Change in Production',
                               'Change in Price',
                               'Scaled Change in Production',
                               'Scaled Change in Price'))

## ---- part4_differenced_plots ----

par(mfrow=c(1,1), xpd=NA)
plot.ts(delta.prod, col="navy",
        main="Change in Production and Change in Price",
        ylab="Change",
        xlab="Interval",
        pch=1, lty=1)
par(new=T)
plot.ts(delta.price,col="blue",axes=F,xlab="",ylab="",
        pch=1, lty=1,
        col.axis="blue")
leg.txt <- c("Change in Production", "Change in Price")
legend("topleft", legend=leg.txt, lty=c(1,1),
       col=c("navy","blue"), bty='n', cex=1)

## ---- part4_differenced_qplot ----

qplot(prod_diff, price_diff, data=scaled.diffs,
      geom=c("point", "smooth"),
      main="Normalized Change in Gas Price and Oil Production",
      xlab="Change in Gas Price",
      ylab="Change Oil Production")

## ---- part4_lm ----
model1 <- lm(prod_diff ~ price_diff, data=scaled.diffs)
summary(model1)

model2 <- lm(Production ~ Price, data=gasOil)
summary(model2)

model3 <- lm(price_diff ~ prod_diff, data=scaled.diffs)
summary(model3)

model4 <- lm(Price ~ Production, data=gasOil)
summary(model4)

## ---- part4_summarize_regression_models_text -----
stargazer(model1, model2, model3, model4, type="text", header=FALSE,
          title='Oil Production and Gas Price Model Summary',
          covariate.labels = c('Change in Price','Price',
                               'Change In Production','Production'),
          dep.var.labels = c('Change in Production', 'Production',
                             'Change in Price','Price'), digits=5)

## ---- part4_summarize_regression_models_latex -----
stargazer(model1, model2, model3, model4, type="latex", header=FALSE,
          title='Oil Production and Gas Price Model Summary',
          covariate.labels = c('Change in Price','Price',
                               'Change In Production','Production'),
          dep.var.labels = c('Change in Production', 'Production',
                             'Change in Price','Price'), digits=5)

# At this point I'm not sure what else to do. I have no information
# about what process the AP used to produce their analysis, so I have
# to assume they were comparing the means of the two variables
# through regression. There is no meaningful or statistically
# significant relationship between the two variables, either differenced
# or not.
#
# Question: does it make sense to try lagged variables?
#
###########################################################################
#
# Part 2: Forecast Gas Prices
#
###########################################################################
## ---- part4_plots_ts_price ----
par(mfrow=c(2,2))
plot.ts(ts_price, main="Gas Price Time Series",
        ylab="Value", xlab="Year")
hist(ts_price, main="Histogram of Gas Price Series",
     xlab="Value", breaks=50)
acf(ts_price, main="Autocorrelation of Gas Price",
    xlab="Lag",lag.max = 100)
pacf(ts_price, main="Partial Autocorrelation of Price",
     xlab="Lag", lag.max = 100)

# Observations:
# The Gas Price series doesn't seem to have persistent trends but is more
# like a random walk. The mean is not 0 so we will have to de-mean the series.
# The ACF indicates an AR() process

## ---- part4_pricets_summary ----
xtable(summaryfunction(ts_price), caption=c('Price Series Statistical Summary'),
       digits=3)

## ---- part4_price_diff_plots ----
par(mfrow=c(2,2))
plot.ts(delta.price, main="Price Change Time Series",
        ylab="Value", xlab="Year")
hist(delta.price, main="Histogram of Price Change Series",
     xlab="Value", breaks=50)
acf(delta.price, main="Acf of Price Change",
    xlab="Lag",lag.max = 100)
pacf(delta.price, main="PACF of Price Change",
     xlab="Lag", lag.max = 100)


## ---- part4_model_price ----
# we open the model parameters up a bit to consider higher
# order models, especially for seasonality.
price.arima <- auto.arima(ts_price)

stargazer(summary(price.arima))
xtable(confint(price.arima), caption='ARIMA Confidence Intervals')

## ---- part4_arima_residuals_plot ----
par(mfrow=c(2,2))
plot.ts(price.arima$residuals, main="ARIMA Residuals",
        ylab="Value", xlab="Year")
hist(price.arima$residuals, main="Histogram of Residuals",
     xlab="Value", breaks=50)
acf(price.arima$residuals, main="Acf of Residuals",
    xlab="Lag",lag.max = 100)
acf(price.arima$residuals^2, main="ACF of Squared Residuals",
     xlab="Lag", lag.max = 100)

## ---- part4_price_garch_model ----

price.garch <- garchFit(price.arima$residuals~garch(1,1), trace=F)
res.garch <- price.garch@residuals

summary(price.garch)

## ---- part4_garch_residuals_plot ----
par(mfrow=c(2,2))
plot.ts(res.garch, main="GARCH Residuals",
        ylab="Value", xlab="Year")
hist(res.garch, main="Histogram of GARCH Residuals",
     xlab="Value", breaks=50)
acf(res.garch, main="Acf of GARCH Residuals",
    xlab="Lag",lag.max = 100)
acf((res.garch)^2, main="ACF of GARCH Squared Residuals",
    xlab="Lag", lag.max = 100)


## ---- part4_forecast ----
steps <- 48
# standard error of the residuals from the ARIMA model
se.resid <- sqrt(price.arima$sigma2) # or stdev(x.arima.residuals)
# forecast with GARCH model
price.garch.fcast <- predict(price.garch, n.ahead = steps)
# forecast with ARIMA model
forecast <- forecast.Arima(price.arima, h=steps)
# standard errors from the ARIMA model (for prediction)
se.arima <- (forecast$upper[,2]-forecast$mean)/1.96
# forecasting conditional standard errors from GARCH
cse.garch <- price.garch.fcast$standardDeviation
# put the conditional SE back to ARIMA SE
se.combine <- se.arima / se.resid * cse.garch
forecast$mean <- forecast$mean + ts2.garch.fcast$meanForecast
forecast$lower[,2] <- forecast$mean - 1.96 * se.combine
forecast$lower[,1] <- forecast$mean - 1.645 * se.combine
forecast$upper[,2] <- forecast$mean + 1.96 * se.combine
forecast$upper[,1] <- forecast$mean + 1.645 * se.combine

## ---- part4_forecast_plot ----
# chart overlay
par(mfrow=c(1,1))
plot(forecast,
     main="48-Step Ahead Forecast and Original & Estimated Series",
     xlab="Time Period",
     ylab="Original and Forecasted Values",
     lty=2,lwd=1.5)
par(new=T)
#plot.ts(fitted(ts2.fit),axes=F, col="blue",
#        lty=1, lwd=2, xlab="",ylab="",xlim=c(2011,2016))
leg.txt <- c("Original Series", "Forecast")
legend("topleft", legend=leg.txt, lty=c(2,2,1), lwd=c(1,2,2),
       col=c("black","blue"), bty='n', cex=1)



## ---- part4_test ----

plot.ts(price.garch2$residuals, main="ARMA-GARCH Residuals",
        ylab="Value", xlab="Year")
acf(price.garch2$residuals^2, na.action = na.omit)

# package rugarch
par(mfrow=c(1,1))
spec <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1,1),
                                       submodel = NULL, external.regressors = NULL,
                                       variance.targeting = FALSE),
                   mean.model = list(armaOrder = c(3, 3)))
fit <- ugarchfit(spec=spec, data=ts_price)
show(fit)
plot(fit)

f <- ugarchforecast(fit, n.ahead=48)
plot(f)
