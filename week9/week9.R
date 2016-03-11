# Load Bush.csv
data <- read.csv('Bush.csv')

str(data)
dim(data)
head(data)
summary(data)

# Coerce "Approval" to time series object
approval <- as.ts(data$Approval, start=c(2001,1), frequency=12)

plot(approval, type='b')

# acf(data)$acf

# ACF Plotting (Correlogram)
# we significant autocorrelation that doesn't die off for 7 months
# we don't see any autocorrelation that implies seasonality
acf(approval)$acf

# PACF plotting
# There are no significant lags beyond the first month
pacf(approval)

# Kernel Smoothing
plot(approval, main="Level of Approval - Kernel Smoothing", pch=4, lty=5, lwd=1, xlab="Year", ylab="Approval Rating")
lines(ksmooth(time(approval), approval, "normal", bandwidth=2),lty=1, lwd=1.5, col="blue")
lines(ksmooth(time(approval), approval, "normal", bandwidth=5),lty=1, lwd=1.5, col="red")

# Lowess Smoothing
plot(approval, main="Level of Approval - Lowess Smoothing", pch=4, lty=5, lwd=1, xlab="Year", ylab="Approval Rating")
lines(supsmu(time(approval), approval, span=.01),lty=1, lwd=1.5, col="blue")
lines(supsmu(time(approval), approval, span=.5),lty=1, lwd=1.5, col="red")

# Smoothing Splines
plot(approval, main="Level of Approval - Smoothing Splines", pch=4, lty=5, lwd=1, xlab="Year", ylab="Approval Rating")
lines(smooth.spline(time(approval), approval, spar=0.05),lty=1, lwd=1.5, col="blue")          
lines(smooth.spline(time(approval), approval, spar=0.9),lty=1, lwd=1.5, col="red") 


##################################################################################
# Problem 8 (Samuel)
time = as.numeric(time(dataTs) - mean(time(dataTs)));
cs = cos(2*pi*time);
sn = sin(2*pi*time);
data = data.frame(data, time, cs, sn);
data$Month = as.factor(data$Month);

m1 = lm(Approval~poly(time, 3) + Month, data=data);
m2 = lm(Approval~poly(time, 3) + cs + sn, data=data);

plot(dataTs, type='b', col=4, pch=20, xlab='Year', ylab='Approval (Percentage)', main='Regression');
lines(ts(fitted(m1), start=c(2001, 1), end=c(2005, 2), frequency=12), col=2);
lines(ts(fitted(m2), start=c(2001, 1), end=c(2005, 2), frequency=12), col=3);



> summary(m1)

Call:
  lm(formula = Approval ~ poly(time, 3) + Month, data = data)

Residuals:
  Min      1Q  Median      3Q     Max 
-9.6534 -3.1991 -0.5581  4.0045 11.8268 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     60.3601     2.6266  22.980  < 2e-16 ***
  poly(time, 3)1 -42.2299     5.9127  -7.142 2.51e-08 ***
  poly(time, 3)2 -33.2872     5.9564  -5.588 2.69e-06 ***
  poly(time, 3)3  39.2346     5.9398   6.605 1.24e-07 ***
  Month2          -0.1919     3.6815  -0.052    0.959    
Month3           0.5828     3.9331   0.148    0.883    
Month4           1.6528     3.9303   0.421    0.677    
Month5          -1.0362     3.9294  -0.264    0.794    
Month6          -2.2476     3.9297  -0.572    0.571    
Month7          -4.0614     3.9308  -1.033    0.309    
Month8          -4.9272     3.9326  -1.253    0.219    
Month9           0.6262     3.9351   0.159    0.874    
Month10          1.5081     3.9386   0.383    0.704    
Month11          2.7456     3.9436   0.696    0.491    
Month12          2.1365     3.9507   0.541    0.592    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.814 on 35 degrees of freedom
Multiple R-squared:  0.7972,	Adjusted R-squared:  0.7161 
F-statistic:  9.83 on 14 and 35 DF,  p-value: 2.429e-08

> summary(m2)

> summary(m2)

Call:
  lm(formula = Approval ~ poly(time, 3) + cs + sn, data = data)

Residuals:
  Min       1Q   Median       3Q      Max 
-12.5048  -3.2235   0.5645   2.6632  13.1782 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     60.0220     0.7728  77.667  < 2e-16 ***
  poly(time, 3)1 -42.2529     5.5295  -7.641 1.33e-09 ***
  poly(time, 3)2 -34.4416     5.5324  -6.225 1.57e-07 ***
  poly(time, 3)3  39.2224     5.5379   7.082 8.66e-09 ***
  cs               2.0013     1.0894   1.837    0.073 .  
sn              -1.0755     1.1419  -0.942    0.351    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.457 on 44 degrees of freedom
Multiple R-squared:  0.7755,	Adjusted R-squared:  0.7499 
F-statistic: 30.39 on 5 and 44 DF,  p-value: 3.14e-13
