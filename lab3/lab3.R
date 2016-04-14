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
library(lmtest)
library(car)



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

sum(hv_df$distanceToHighway==24)

# There are 104 out of 400 observations that are 24.

# attempt to get some of the fine structure at the very beginning of the 
# distribution for crimeRate_pc. The distribution is highly right skewed
hist(hv_df$crimeRate_pc, breaks=800, main="Histogram of Crime Rate per Capita",
     xlab="Crimes per 1000", xlim=c(0,5))

# The distribution of non-retail business acres appears uniform with
# a very large spike at 0.18
hist(hv_df$nonRetailBusiness, breaks=50, 
     main="Frequency of Non-retail Business Acres",
     xlab="Business Acres")

# This is a binary variable so a histogram doesn't really help here.
hist(hv_df$withWater, breaks=50, 
     main="Histogram of Houses Within 5 Miles of a Body of Water",
     xlab="Proportion of Houses within 5 Miles of a Body of Water")

# The Proportion of Houses build before 1950 is extremely right-skewed
# with an almost uniform left tail
hist(hv_df$ageHouse, breaks=50,
     main="Histogram of Proportion of Houses Built Before 1950",
     xlab="Proportion of Houses Built Before 1950")

# Distance to City distribution is highly right skewed with a long
# tail to the higher distance-to-city values.
hist(hv_df$distanceToCity, breaks=50,
     main="Histogram of Distance to City", xlab="Distance To City")

# The distance to highway appears to have a coding error with many of
# the distances set to 24. Otherwise the distribution appears normal-like
hist(hv_df$distanceToHighway, breaks=50,
     main="Distance To Highway", xlab="Distance to Highway")

# The distribution is more or less uniform except for one large peak
# at just over 23 pupils per teacher.
hist(hv_df$pupilTeacherRatio, breaks=50,
     main="Frequency of Pupil to Teacher Ratio",
     xlab="Pupils per Teacher")

# Low income housing distribution appears as a skewed-right normal-like
# distribution with a long tail towards the higher percentage of low
# income houses.
hist(hv_df$pctLowIncome, breaks=50, 
     main="Frequency of Low Income Housing",
     xlab="Percentage of Low Income Houses")

# Home value distribution appears normal-like with a longer taper
# to the higher value homes
hist(hv_df$homeValue, breaks=50,
     main="Histogram of Home Values per Neighborhood",
     xlab="Home Value")

# The pollution index distribution seems almost random
hist(hv_df$pollutionIndex, breaks=50,
     main="Distribution of Pollution Index Across Neighborhoods",
     xlab="Pollution Index")

# The bedroom counts appear to be normal-like in distribution
# and mostly un-skewed in any direction
hist(hv_df$nBedRooms, breaks=50,
     main="Frequency of Bedroom Counts per Neighborhood",
     xlab="Bedroom Count")

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
pairs(homeValue ~ crimeRate_pc + nonRetailBusiness + withWater + 
      ageHouse + distanceToCity + distanceToHighway +
      pupilTeacherRatio + pctLowIncome + pollutionIndex + nBedRooms, 
      data=hv_df, upper.panel=panel.smooth, 
      lower.panel=panel.cor, diag.panel=panel.hist)

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

# There is a strong linear correlation between homeValue and nBedRooms
qplot(nBedRooms, homeValue, data=hv_df, geom=c("point", "smooth"))

# There doesn't appear to be any real correlation between pollution and
# home price, at least directly; perhaps through interaction
qplot(pollutionIndex, homeValue, data=hv_df, geom=c("point", "smooth"))

# There seems to be a linear relationship between the percentage of
# low income housing and the change in home values
qplot(pctLowIncome, homeValue, data=hv_df, geom=c("point", "smooth"))

# doesn't seem to be much here with pupil:teacher ratio
qplot(pupilTeacherRatio, homeValue, data=hv_df, geom=c("point", "smooth"))

# maybe a smallish correlation in crime rate?
# NOTE- log log looks interesting
qplot(crimeRate_pc, homeValue, data=hv_df, geom=c("point", "smooth"))


#### MODELING ####

m1 <- lm(homeValue ~ nBedRooms, data=hv_df)
coeftest(m1)

m2 <- lm(homeValue ~ nBedRooms + pctLowIncome, data=hv_df)
coeftest(m2)
vif(m2)

m3 <- lm(homeValue ~ nBedRooms + pctLowIncome + nonRetailBusiness, data=hv_df)
coeftest(m3)
vif(m3)

m4 <- lm(homeValue ~ nBedRooms + pctLowIncome + 
           nonRetailBusiness + pollutionIndex, data=hv_df)
coeftest(m4)
vif(m4)

m5 <- lm(homeValue ~ nBedRooms + pctLowIncome +
           nonRetailBusiness + pupilTeacherRatio, data=hv_df)
coeftest(m5)
vif(m5)

m6 <- lm(homeValue ~ nBedRooms + pctLowIncome +
           nonRetailBusiness + pupilTeacherRatio +
           log(distanceToCity), data=hv_df)
coeftest(m6)
vif(m6)

# It looks like dropping the nonRetailBusiness from the model at 
# this point makes for a slightly better model with one less
# parameter
m7 <- lm(homeValue ~ nBedRooms + pctLowIncome +
           pupilTeacherRatio + log(distanceToCity) +
           log(crimeRate_pc), data=hv_df)
coeftest(m7)
vif(m7)

plot(m7)

summary(m7)

# from what I see of the plots of these modesl, M6 so far provides
# the highest Adjusted R^2 value without the additional parameter
# of crime rate.
