
#########################################################################
#
# Lab 3, Part 4
#
#########################################################################
## ---- part4_setup ----
library(ggplot2)
library(astsa)
library(zoo)
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
          title="Descriptive Statistics", digits=2)

## ---- part4_summarize_data_latex -----
stargazer(gasOil, type="latex", header=FALSE,
          title="Descriptive Statistics", digits=2)

## ---- part4_create_ts ----
ts_prod <- ts(gasOil$Production, start=c(1978,1), 
              end=c(2012,2), frequency=12)
ts_price <- ts(gasOil$Price, start=c(1978,1),
               end=c(2012,2), frequency=12)

## ---- part4_plot_ts ----
# plot the two series together for a comparative visual

par(mfrow=c(1,1))
plot.ts(ts_prod, col="navy", 
        main="US Oil Production",
        ylab="Million BBL/day",
        xlab="Year",
        ylim=c(100,300), pch=1, lty=1)
par(new=T)
plot.ts(ts_price,col="green",axes=F,xlab="",ylab="",
        ylim=c(0.00,6.00), pch=1, lty=1, col.axis="green") 
leg.txt <- c("US Oil Production", "Gas Price")
legend("topleft", legend=leg.txt, lty=c(1,1), 
       col=c("navy","green"), bty='n', cex=1)
axis(side=4, col="green")
mtext("Dollars", side=4, line=2,col="green")

