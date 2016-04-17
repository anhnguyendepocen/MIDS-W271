
#########################################################################
#
# Lab 3, Setup
#
#########################################################################
## ---- part1setup ----
library(ggplot2)
library(lmtest)
library(car)
library(sandwich)
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
stargazer(hv_df, type = "latex", header=FALSE, title = "Summary of Data")

## ---- summary_text ----
stargazer(hv_df, type = "text", header=FALSE, title="Summary of Data")

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
hv_df$pollutionIndex_log <- log(hv_df$pollutionIndex)

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

# The Proportion of Houses built before 1950 is extremely right-skewed
# with an almost uniform left tail but a transformation doesn't seem to help
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

## ---- histogram_of_pollution ----
# The pollution index distribution seems almost random
par(mfrow=c(2,1))
histBxp(hv_df$pollutionIndex, breaks=30,
     main="Distribution of Pollution Index Across Neighborhoods",
     xlab="Pollution Index", 
     width=0.01, boxcol="lightblue")
histBxp(hv_df$pollutionIndex_log, breaks=30,
        main="Distribution of Pollution Index Across Neighborhoods",
        xlab="Pollution Index", 
        width=0.01, boxcol="lightblue")

## ---- histogram_of_nbeds ----
# The nBedRooms variable looks mostly normal
histBxp(hv_df$nBedRooms, breaks=30,
        main="Distribution of Number of Bedrooms",
        xlab="Number of Bedrooms", 
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
pairs(homeValue_log ~ crimeRate_pc_log + nonRetailBusiness + withWater + 
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

# First we create a subset dataframe that filters out rows where
# distanceToCity = 24 and calculate the new mean
hv_df_hiway_filtered <- subset(hv_df, distanceToHighway<24)
distanceToHighway.mean <- mean(hv_df$distanceToHighway)

# Next we create the new variables based on the transformation
# of either replacing with the filtered mean or the distanceToCity
hv_df$distanceToHighway_modMean <- ifelse(hv_df$distanceToHighway<24,
                                   hv_df$distanceToHighway,
                                   distanceToHighway.mean)

hv_df$distanceToHighway_modCity <- ifelse(hv_df$distanceToHighway<24,
                                    hv_df$distanceToHighway,
                                    hv_df$distanceToCity)

## ---- disthiway_comparison_text ----
# compare the summary statistics of the filtered and raw data set
stargazer(hv_df_hiway_filtered, type="text")
stargazer(hv_df, type="text")

## ---- disthiway_comparison_latex ----
# compare the summary statistics of the filtered and raw data set
stargazer(hv_df_hiway_filtered, type="latex", header=FALSE, title='Filtered Dataset')
stargazer(hv_df, type="latex", header=FALSE, title='Full Dataset')

## ---- histogram_of_disthiway2 ----
# The distance to highway appears to have a coding error with many of
# the distances set to 24. Otherwise the distribution appears normal-like
par(mfrow=c(1,1))
histBxp(hv_df$distanceToHighway, breaks=50,
        main="Distance To Highway", 
        xlab="Distance to Highway", 
        width=0.01, boxcol="lightblue")

## ---- disthiway_histograms ----
# compare the histograms on a single "page"
par(mfrow=c(2,2))
hist(hv_df_hiway_filtered$distanceToHighway,
     breaks=20,
     main="Distance To Highway - Filtered",
     xlab="Distance")
hist(hv_df$distanceToHighway_modMean,
     breaks=20,
     main="Distance To Highway - Mean Xform",
     xlab="Distance")
hist(hv_df$distanceToHighway_modCity,
     breaks=20,
     main="Distance To Highway - City Xform",
     xlab="Distance")
hist(hv_df$distanceToCity,
     breaks=20,
     main="Histogram of Distance to City",
     xlab="Distance")

# It's easy to see that there is a big difference between the filtered
# data and the raw data and a huge reduction in the size of the data
# Comparing the histograms shows that replacing the distanceToHighway=24
# with the mean value still creates a large number of values at one end
# of the scale, it's just closer to the rest of the data than before
# However, replacing the values of 24 with the distanceToCity value
# produces a distribution that's very close to that of the filtered
# distribution and appears to be a good candiate transformation to deal
# with this coding issue

# Let's see what this does to our pairs grid

## ---- matrixplot_withvariablemods ----
# Matrix of histogram, correlations and scatterplots for all the
# variables in the data set
par(mfrow=c(1,1))
pairs(homeValue_log ~ crimeRate_pc_log + nonRetailBusiness +  
        ageHouse + distanceToCity_log + distanceToHighway_modCity +
        pupilTeacherRatio + pctLowIncome_log + pollutionIndex + nBedRooms, 
      data=hv_df, upper.panel=panel.smooth, 
      lower.panel=panel.cor, diag.panel=panel.hist,
      main="Scatterplot Matrix of Transformed Variables")

## ---- multivariate-relationships-env1 ----
# General Analysis of Overall Trends (from the pairs grid)
# Environment factors first
par(mfrow=c(2,2))
# There appears to be a relationships between pollution index and homeValue
qplot(pollutionIndex, homeValue_log, data=hv_df, 
      geom=c("point", "smooth"), main='',
      xlab='Pollution Index', ylab='log(Home Value)')

qplot(nonRetailBusiness, homeValue_log, data=hv_df,
      geom=c("point", "smooth"), main='',
      xlab='Non-Retail Business-Acres', ylab='log(Home Value)')

# There seems to be a linear relationship between the log(%) of
# low income housing and the change in home values
qplot(pctLowIncome_log, homeValue_log, data=hv_df,
      geom=c("point", "smooth"), main='',
      xlab='Log(% Low Income Housing)', ylab='log(Home Value)')

# maybe a  correlation in log(crime rate)?
qplot(crimeRate_pc_log, homeValue_log, data=hv_df,
      geom=c("point", "smooth"), main='',
      xlab='Log(Crime Rate per Capita)', ylab='log(Home Value)')

## ---- multivariate-relationships-env2 ----
par(mfrow=c(2,1))
# not sure what to make of the relationship of distanceToCity - starts out
# as a solid trend but then levels off. Possible explanation is that since
# this is a log trend it has a diminishing effect that levels off.
qplot(distanceToCity_log, homeValue_log, data=hv_df,
      geom=c("point", "smooth"), main='',
      xlab='Log(DistanceToCity)', ylab='log(Home Value)')

qplot(distanceToHighway_modCity, homeValue_log, data=hv_df,
      geom=c("point", "smooth"), main='',
      xlab='Log(DistanceToHighway)', ylab='log(Home Value)')

# There seems to be a linear relationship between pupilTeachRation and homeValue
qplot(pupilTeacherRatio, homeValue_log, data=hv_df, 
      geom=c("point", "smooth"), main='',
      xlab='Pupil:Teacher Ratio', ylab='log(Home Value)')

## ---- multivariate-relationships-attr1 ----
par(mfrow=c(2,1))
# Home Attribute Values
# There is a strong linear correlation between homeValue and nBedRooms
qplot(nBedRooms, homeValue_log, data=hv_df, 
      geom=c("point", "smooth"), main='',
      xlab='Number Bedrooms', ylab='log(Home Value)')

qplot(ageHouse, homeValue_log, data=hv_df,
       geom=c("point", "smooth"), main='',
       xlab='Log(% Homes Built Before 1950)', ylab='log(Home Value)')


#### MODELING ####

## ---- modeling ----
model1 <- lm(homeValue_log ~ pctLowIncome_log, data=hv_df)
coeftest(model1, vcov=vcovHC)
summary(model1)

model2 <- lm(homeValue_log ~ pctLowIncome_log + crimeRate_pc_log, data=hv_df)
coeftest(model2, vcov=vcovHC)
summary(model2)
vif(model2)

# low income housing is somewhat correlated with crime rate but
# the vif(model2) shows that we're 'in bounds'

model3 <- lm(homeValue_log ~ pctLowIncome_log + crimeRate_pc_log +
               pollutionIndex, data=hv_df)
coeftest(model3, vcov=vcovHC)
summary(model3)

# indicates that pollution index doesn't explain any variation in the model
# when lowIncomeHousing and crimeRate are controlled for.

model4 <- lm(homeValue_log ~ pctLowIncome_log + crimeRate_pc_log +
               nonRetailBusiness, data=hv_df)
coeftest(model4, vcov=vcovHC)
summary(model4)

# indicates that nonRetailBusiness doesn't explain any variation in the model
# when lowIncomeHousing and crimeRate are controlled for.

model5 <- lm(homeValue_log ~ pollutionIndex + nonRetailBusiness, data=hv_df)
coeftest(model5, vcov=vcovHC)
vif(model5)

# these coefficients don't pass the VIF test and add too much bias because of
# their collinearity. 

model6 <- lm(homeValue_log ~ pctLowIncome_log + crimeRate_pc_log +
               withWater, data=hv_df)
coeftest(model6, vcov=vcovHC)
vif(model6)
summary(model6)

model7 <- lm(homeValue_log ~ pctLowIncome_log + crimeRate_pc_log +
               distanceToCity_log, data=hv_df)
coeftest(model7, vcov=vcovHC)
vif(model7)

# crime rate and distanceToCity add too much collinearity

model8 <- lm(homeValue_log ~ pctLowIncome_log + distanceToCity_log,
             data=hv_df)
coeftest(model8, vcov=vcovHC)

# distanceToCity isn't a good replacement for crimeRate

model9 <- lm(homeValue_log ~ pctLowIncome_log + crimeRate_pc_log +
               distanceToHighway_modCity, data=hv_df)
coeftest(model9, vcov=vcovHC)
vif(model9)
summary(model9)

model10 <- lm(homeValue_log ~ pctLowIncome_log + crimeRate_pc_log +
                distanceToHighway_modCity + withWater, data=hv_df)
coeftest(model10, vcov=vcovHC)
vif(model10)
summary(model10)

# Use robust standard errors in the table output


## ---- model_comparison_text ----
stargazer(model1, model2, model3, model6, model10,
          type="text", header=FALSE, df=FALSE,
          title="Regression Model Comparison",
          dep.var.labels = c("log(Home Values)"),
          covariate.labels = c("log(Low Income Housing)",
                               "log(Crime Rate)",
                               "Pollution Index",
                               "Close To Water",
                               "Distance To Highway"))

## ---- model_comparison_latex ----
stargazer(model1, model2, model3, model6, model10,
          type="latex", header=FALSE, df=FALSE,
          title="Regression Model Comparison",
          dep.var.labels = c("log(Home Values)"),
          covariate.labels = c("log(Low Income Housing)",
                               "log(Crime Rate)",
                               "Pollution Index",
                               "Close To Water",
                               "Distance To Highway"))
