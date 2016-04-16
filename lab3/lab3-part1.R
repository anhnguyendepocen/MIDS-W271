
#########################################################################
#
# Lab 3, Setup
#
#########################################################################
## ---- part1setup ----
library(ggplot2)
library(lmtest)
library(car)
library(MASS)
library(stargazer)
library(sfsmisc)
library(psych)
library(e1071)

#########################################################################
#
# Lab 3, Part 1 - Modeling House Values
#
# In Part 1, you will use the data set **houseValue.csv** to build a linear
# regression model, which includes the possible use of the instrumental variable
# approach, to answer a set of questions interested by a philanthropist group.
# You will also need to test hypotheses using these questions.
#
# The philanthropist group hires a think tank to examine the relationship between 
# the house values and neighborhood characteristics. For instance, they are 
# interested in the extent to which houses in neighbhorhood with desirable 
# features command higher values. They are specifically interested in environmental 
# features, such as proximity to water body (i.e. lake, river, or ocean) or 
# air quality of a region.
#
# The think tank has collected information from tens of thousands of 
# neighborhoods throughout the United States. They hire your group as 
# contractors, and you are given a small sample and selected variables 
# of the original data set collected to conduct an initial, proof-of-concept
# analysis. Many variables, in their original form or transfomed forms, that 
# can explain the house values are included in the dataset. Analyze each of 
# these variables as well as different combinations of them very carefully 
# and use them (or a subset of them), in its original or transformed version, 
# to build a linear regression model and test hypotheses to address the 
# questions. Also address potential (statistical) issues that may be 
# casued by omitted variables.
#
# Data Set Field Descriptions
#
# crimeRate_pc: crime rate per capital, measured by number of crimes per
#               1000 residents in neighborhood
#
# nonRetailBusiness: the proportion of non-retail business acres per 
#                    neighborhood
#
# withWater: the neighborhood within 5 miles of a water body 
#           (lake, river, etc); 1 if true and 0 otherwise 
#
# ageHouse: proportion of house built before 1950
#
# distanceToCity: distances to the nearest city (measured in miles)
#
# pupilTeacherRatio: average pupil-teacher ratio in all the schools 
#                    in the neighborhood 
#
# pctLowIncome: percentage of low income household in the neighborhood
#
# homeValue: median price of single-family house in the neighborhood 
#            (measured in dollars)
#
# pollutionIndex: pollution index, scaled between 0 and 100, with 0 
#                 being the best and 100 being the worst (i.e. uninhabitable) 
#
# nBedRooms: average number of bed rooms in the single family houses 
#            in the neighborhood
#
#########################################################################
hv_df <- read.csv('lab3_data/houseValueData.csv')
str(hv_df)
summary(hv_df)

head(hv_df,10)

tail(hv_df,10)

## ---- summary_latex ----
stargazer(hv_df, type = "latex", header=FALSE)

## ---- summary_text ----
stargazer(hv_df, type = "text", header=FALSE)

# Notes about each of the variables from the summary
# crimerate_pc - from almost 0 to 90 per 1000 residents, mean of 3.76
# noRetailBusiness - from almost 0 to 0.27 business-acres/neighborhood
#                    These would be be manufacturing, light industrial, etc.
# withWater - binary 0/1 with median of 6.75% 5 miles or closer to water
# ageHouse - the majority of houses are built before 1950 with a mean of
#            68.93 percent.
# distanceToCity - the median distance to a city is 6.1 miles, but there
#            are no qualifications of what city means. Max is 54
#            Some of the data doesn't look as I would expect where there
#            is a distance to a city of 2.7 and a distance to highway of 24.
#            The max distance to a highway is 24 and I see that value in
#            several data rows where I suspect a coding error.
# distanceToHighway - the median distance is 5 miles with some much more
#            remote
# pupilTeacherRatio - mean of 22 pupils/teacher with a high of 25.
# pctLowIncome - ranges from 2-49% with an average of 16
# homeValue - range over a factor of 10 from 112500 - 11125000. Need
#           more examination; this is likely a categorical variable
# pollutionIndex - many are rather high on the pollution index, with a
#           mean of 40.61 and a max of 72.10.
# nBedRooms - mean of 4.2 (higher than I would've thought)

## ---- data_transformations ----
sum(hv_df$distanceToHighway==24)

# There are 104 out of 400 observations that are 24.

# Let's see what we can do about some of the worst variables for skew 
# and kurtosis
kurtosis(log(hv_df$crimeRate_pc))  # changes kurtosis from 33.9 -> -1.0
skew(log(hv_df$crimeRate_pc))      # changes skew from 4.96 -> 0.434

kurtosis(log(hv_df$distanceToCity)) # changes kurtosis from 2.86 -> -0.99
skew(log(hv_df$distanceToCity))    # changes skew from 1.63 -> 0.202

kurtosis(log(hv_df$pctLowIncome)) # changes kurtosis from 0.61 -> -0.464
skew(log(hv_df$pctLowIncome))     # changes skew from 0.97 -> -0.322

kurtosis(log(hv_df$ageHouse))
skew(log(hv_df$ageHouse))

# transform the crimeRate_pc and distanceToCity variables with log
hv_df$crimeRate_pc_log <- log(hv_df$crimeRate_pc)
hv_df$distanceToCity_log <- log(hv_df$distanceToCity)
hv_df$pctLowIncome_log <- log(hv_df$pctLowIncome)
hv_df$ageHouse_log <- log(hv_df$ageHouse)
hv_df$homeValue_log <- log(hv_df$homeValue)

# attempt to get some of the fine structure at the very beginning of the 
# distribution for crimeRate_pc. The distribution is highly right skewed
## ---- histogram_of_crimerate ----
par(mfrow=c(2,1))
histBxp(hv_df$crimeRate_pc, breaks=800, main="Histogram of Crime Rate per Capita",
     xlab="Crimes per 1000", xlim=c(0,5), width=0.01,
     boxcol='lightblue')
histBxp(hv_df$crimeRate_pc_log, breaks=50, main="Histogram of Log Crime Rate per Capita",
     xlab="Log(Crimes per 1000)", width=0.01,
     boxcol='lightblue')

## ---- histogram_of_nonbizretail_agehouse ----
# The distribution of non-retail business acres appears uniform with
# a very large spike at 0.18
par(mfrow=c(2,1))
histBxp(hv_df$nonRetailBusiness, breaks=50, 
     main="Frequency of Non-retail Business Acres",
     xlab="Business Acres", 
     width=0.01, boxcol="lightblue")

# The Proportion of Houses build before 1950 is extremely right-skewed
# with an almost uniform left tail
histBxp(hv_df$ageHouse, breaks=50,
     main="Histogram of Proportion of Houses Built Before 1950",
     xlab="Proportion of Houses Built Before 1950",
     width=0.01, boxcol="lightblue")

## ---- histogram_of_distcity ----
# Distance to City distribution is highly right skewed with a long
# tail to the higher distance-to-city values.
par(mfrow=c(2,1))
histBxp(hv_df$distanceToCity, breaks=50,
     main="Histogram of Distance to City",
     xlab="Distance To City",
     width=0.01, boxcol="lightblue")
histBxp(hv_df$distanceToCity_log, breaks=50,
     main="Histogram of Log(Distance to City)",
     xlab="Log(Distance To City)",
     width=0.01, boxcol="lightblue")

## ---- histogram_of_disthiway_pupils ----
# The distance to highway appears to have a coding error with many of
# the distances set to 24. Otherwise the distribution appears normal-like
par(mfrow=c(2,1))
histBxp(hv_df$distanceToHighway, breaks=50,
     main="Distance To Highway", 
     xlab="Distance to Highway", 
     width=0.01, boxcol="lightblue")

# The distribution is more or less uniform except for one large peak
# at just over 23 pupils per teacher.
histBxp(hv_df$pupilTeacherRatio, breaks=50,
     main="Frequency of Pupil to Teacher Ratio",
     xlab="Pupils per Teacher", 
     width=0.01, boxcol="lightblue")

## ---- histogram_of_lowincome ----
# Low income housing distribution appears as a skewed-right normal-like
# distribution with a long tail towards the higher percentage of low
# income houses.
par(mfrow=c(2,1))
histBxp(hv_df$pctLowIncome, breaks=50, 
     main="Frequency of Low Income Housing",
     xlab="Percentage of Low Income Houses", 
     width=0.01, boxcol="lightblue")
histBxp(hv_df$pctLowIncome_log, breaks=50, 
     main="Frequency of Log(% Low Income Housing)",
     xlab="Log(Percentage) of Low Income Houses", 
     width=0.01, boxcol="lightblue")

## ---- histogram_of_homevalue ----
# Home value distribution appears normal-like with a longer taper
# to the higher value homes
par(mfrow=c(2,1))
histBxp(hv_df$homeValue, breaks=50,
     main="Histogram of Home Values per Neighborhood",
     xlab="Home Value", width=0.01, boxcol="lightblue")
histBxp(hv_df$homeValue_log, breaks=50,
     main="Histogram of Log(Home Values) per Neighborhood",
     xlab="Log(Home Value)", 
     width=0.01, boxcol="lightblue")

## ---- histogram_of_pollution_nbeds ----
# The pollution index distribution seems almost random
par(mfrow=c(2,1))
histBxp(hv_df$pollutionIndex, breaks=50,
     main="Distribution of Pollution Index Across Neighborhoods",
     xlab="Pollution Index", 
     width=0.01, boxcol="lightblue")

## ---- matrixplot_variables ----
# The next two functions are used to calculate the correlation
# and histograms used in the pair graph further down
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot=FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

# Panel correlation graph function
panel.cor <- function(x, y, digits=2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Matrix of histogram, correlations and scatterplots for all the
# variables in the data set
pairs(homeValue ~ crimeRate_pc_log + nonRetailBusiness + withWater + 
      ageHouse + distanceToCity_log + distanceToHighway +
      pupilTeacherRatio + pctLowIncome_log + pollutionIndex + nBedRooms, 
      data=hv_df, upper.panel=panel.smooth, 
      lower.panel=panel.cor, diag.panel=panel.hist,
      main="Data Set Variable Scatterplot Matrix")

## ---- disthiway_detail ----
# The pairs grid shows strong correlations between pollution index
# and nonRetailBusiness, house age, distance to the city and moderate
# correlation with distanceToHighway and percent Low income housing.
#
# The percent low income shows a strong correlation with home values
# and with nonRetailBusiness and house age.
#
# Teacher to pupil ratio shows moderate correlation with home values
# and the distance to highway but I am suspicious of the distanceToHighway
# variable
#
# House age shows a moderately strong correlation with nonRetailBusiness
# and NonRetailBusiness shows a moderate correlation with home values.
#
# Crime rate shows a moderate correlation with home values
#
# Next I want to take a look at the DistanceToHighway variable and filter
# out the encoding issue. I'm going to look at 2 different approaches - 
# removing those entries that have the encoding issue which reduces the
# data set by 25%, and to recompute the 24 mile entries as the mean
# of the DistanceToHighway with the 24 mile values included.
#
# first we create a transformed column that replaces all values of
# 24 with the mean value of distanceToHighway
distanceToHighway.mean <- mean(hv_df$distanceToHighway)
hv_df$distanceToHighwayX <- ifelse(hv_df$distanceToHighway<24,
                                   hv_df$distanceToHighway,
                                   distanceToHighway.mean)

# Next we create a subset dataframe that filters out rows where
# distanceToCity = 24
hv_df_hiway_filtered <- subset(hv_df, distanceToHighway<24)

# compare the summary statistics of the filtered and raw data set
stargazer(hv_df_hiway_filtered, type="text")
stargazer(hv_df, type="text")

# compare the histograms on a single "page"
par(mfrow=c(2,1))
hist(hv_df_hiway_filtered$distanceToHighway,
     main="Histogram of Distance to Highway",
     xlab="Distance")
hist(hv_df$distanceToHighwayX,
     main="Histogram of Distance to Highway",
     xlab="Distance")
# It's easy to see that there is a big difference between the filtered
# data and the raw data and a huge reduction in the size of the data
# Comparing the histograms shows that replacing the distanceToHighway=24
# with the mean value still creates a large number of values at one end
# of the scale, it's just closer to the rest of the data than before
#
# What if we replace the values of 24 with the mean of the filtered set
distanceToHighwayFiltered.mean <- mean(hv_df_hiway_filtered$distanceToHighway)
hv_df$distanceToHighwayXX <- ifelse(hv_df$distanceToHighway<24,
                                   hv_df$distanceToHighway,
                                   distanceToHighwayFiltered.mean)
# compare the histograms on a single "page"
par(mfrow=c(2,1))
hist(hv_df_hiway_filtered$distanceToHighway,
     main="Histogram of Distance to Highway",
     xlab="Distance")
hist(hv_df$distanceToHighwayXX,
     main="Histogram of Distance to Highway",
     xlab="Distance")


# Let's see what this does to our pairs grid

# Matrix of histogram, correlations and scatterplots for all the
# variables in the data set
par(mfrow=c(1,1))
pairs(homeValue ~ crimeRate_pc_log + nonRetailBusiness + withWater + 
        ageHouse + distanceToCity_log + distanceToHighwayXX +
        pupilTeacherRatio + pctLowIncome_log + pollutionIndex + nBedRooms, 
      data=hv_df, upper.panel=panel.smooth, 
      lower.panel=panel.cor, diag.panel=panel.hist)

pairs(homeValue ~ crimeRate_pc + nonRetailBusiness + withWater + 
        ageHouse + distanceToCity + distanceToHighway +
        pupilTeacherRatio + pctLowIncome + pollutionIndex + nBedRooms, 
      data=hv_df_hiway_filtered, upper.panel=panel.smooth, 
      lower.panel=panel.cor, diag.panel=panel.hist)


qplot(distanceToHighwayX, homeValue, data=hv_df, geom=c("point", "smooth"))
qplot(distanceToHighway, homeValue, data=hv_df_hiway_filtered, geom=c("point", "smooth"))
qplot(distanceToHighwayXX, homeValue, data=hv_df, geom=c("point", "smooth"))

# End note - I don't think any of these transformations on distanceToHighway
# make much difference. There aren't really any strong correlations that
# appear as a result. The qPlots between home value and distance to highway indicate
# that there is a very weak relationship, if at all

# General Analysis of Overall Trends (from the pairs grid)

# There is a strong linear correlation between homeValue and nBedRooms
qplot(nBedRooms, homeValue, data=hv_df, 
      geom=c("point", "smooth"), main='Home Values and Crime Rate',
      xlab='Log(Crime Rate per Capita)', ylab='Home Value $')

# There doesn't appear to be any real correlation between pollution and
# home price, at least directly; perhaps through interaction
qplot(pollutionIndex, homeValue, data=hv_df, 
      geom=c("point", "smooth"), main='Home Values and Pollution Index',
      xlab='Pollution Index', ylab='Home Value $')

# There seems to be a linear relationship between the log(%) of
# low income housing and the change in home values
qplot(pctLowIncome_log, homeValue, data=hv_df,
      geom=c("point", "smooth"), main='Home Values and Low Income Housing',
      xlab='Log(% Low Income Housing)', ylab='Home Value $')
qplot(pctLowIncome, homeValue, data=hv_df,
      geom=c("point","smooth"), main='Home Values and Low Income Housing',
      xlab='% Low Income Housing', ylab='Home Value $')

# doesn't seem to be much here with pupil:teacher ratio
qplot(pupilTeacherRatio, homeValue, data=hv_df, 
      geom=c("point", "smooth"), main='Home Values and Pupil:Teacher Ratio',
      xlab='Pupil:Teacher Ratio', ylab='Home Value $')

# maybe a smallish correlation in log(crime rate)?
qplot(crimeRate_pc_log, homeValue, data=hv_df,
      geom=c("point", "smooth"), main='Home Values and Log(Crime Rate)',
      xlab='Log(Crime Rate per Capita)', ylab='Home Value $')
qplot(crimeRate_pc, homeValue, data=hv_df,
      geom=c("point", "smooth"), main='Home Values and Crime Rate',
      xlab='Crime Rate per Capita', ylab='Home Value $')

#### MODELING ####

m1 <- lm(homeValue ~ nBedRooms, data=hv_df)
coeftest(m1)

m2 <- lm(homeValue ~ nBedRooms + pctLowIncome_log, data=hv_df)
coeftest(m2)
vif(m2)

m3 <- lm(homeValue ~ nBedRooms + pctLowIncome_log + 
           nonRetailBusiness, data=hv_df)
coeftest(m3)
vif(m3)

m4 <- lm(homeValue ~ nBedRooms + pctLowIncome_log + 
           pollutionIndex, data=hv_df)
coeftest(m4)
vif(m4)

# Model 5 seems to have the optimal F-statistic, R-squared
# and the least number of parameters
m5 <- lm(homeValue ~ nBedRooms + pctLowIncome_log +
           pupilTeacherRatio, data=hv_df)
coeftest(m5)
vif(m5)

# the crimeRate_pc is slightly better than crimeRate_pc_log
m6 <- lm(homeValue ~ nBedRooms + pctLowIncome_log +
           pupilTeacherRatio + crimeRate_pc,
         data=hv_df)
coeftest(m6)
vif(m6)

m7 <- lm(homeValue ~ nBedRooms + pctLowIncome_log +
           pupilTeacherRatio + log(distanceToCity),
         data=hv_df)
coeftest(m7)
vif(m7)

plot(m5)
summary(m5)

# from what I see of the plots of these modesl, M6 so far provides
# the highest Adjusted R^2 value without the additional parameter
# of crime rate. The qqPlot for M6 shows a better normal quantile plot
# than for M7 as well.

# non-normality
qqPlot(m5)
sresid <- studres(m5) 
hist(sresid, freq=FALSE, breaks=50,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#non-constance error variance
ncvTest(m5)   # null hypothesis of constant error variance can't be rejected
spreadLevelPlot(m5)

# Non-independence of errors
durbinWatsonTest(m5)  # reject null hypotheses of autocorrelation

# models with environmental factors
m10 <- lm(homeValue ~ pollutionIndex, data=hv_df)
summary(m10)
# by itself, the pollution index shows a correlation with home values

# a very weak correlation with water 
m11 <- lm(homeValue ~ withWater, data=hv_df)
summary(m11)

m12 <- lm(homeValue ~ nBedRooms*ageHouse, data=hv_df)
summary(m12)

m13 <- lm(homeValue ~ distanceToCity*withWater, data=hv_df)
summary(m13)
