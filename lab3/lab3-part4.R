
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
qplot(Price, Production, data=gasOil,
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

qplot(price_diff, prod_diff, data=scaled.diffs,
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

summary(ts_price)

# Observations:
# The Gas Price series doesn't seem to have persistent trends but is more
# like a random walk. The mean is not 0 so we will have to de-mean the series.
# The ACF indicates an AR() process

## ---- part4_model_price ----
price.ar <- ar(ts_price, method='mle')
price.ar$order
price.arima <- auto.arima(ts_price, max.p=12, max.q=12,
                          max.d=2, max.P=12, max.Q=12,
                          max.D=1, seasonal = T)
summary(price.arima)

plot.ts(price.arima$residuals, main="ARIMA Residuals", 
        ylab="Value", xlab="Year")

price.garch <- garchFit(~arma(3,3)+garch(5,1), trace=F)
summary(price.garch)

price.garch2 <- garch(price.arima$residuals, order=c(0,3), trace=F)

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

