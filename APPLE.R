# Stock Benchmark: S&P600: Plot the chart for S&P600 from 2016-04-24 to 2023-04-24\
# attache the package to convert dataframes into an xts (time-series) object for simplicity in cal.

require (xts) 
require(zoo)
library(tidyquant)
start_date = "2018-04-30"
end_date = "2023-04-30"

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)


# Get ticket of AAPL
getSymbols("AAPL", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
chart_Series(AAPL) 
aapl_data.xts = AAPL$AAPL.Close

# Log return calculation
aapl_return = diff (log(aapl_data.xts)) 

# removing the first empty observation, received after return calculation
aapl_return = aapl_return [-1] 
summary (aapl_return)

# plot the graph
boxplot(aapl_return,  horizontal=TRUE, main = "Apple return Boxplot", ylab = "Return")
plot(return, main = "Apple weekly returns", xlab = "year", type = "l", ylab = "log return") 

## Location
# calculate mean of the return
mean (aapl_return) 

# calculate the median value of the return
median(aapl_return) 

# Mean absolute deviation
install.packages("DescTools")
library (DescTools) 
MeanAD(aapl_return)

# Variance
var(aapl_return)

# Standard deviation
sd(aapl_return)

# Median Absolute Deviation -> compare with Std Dev
mad (aapl_return)

#skewness
library(e1071) 
skewness (aapl_return$AAPL.Close)
kurtosis(aapl_return$AAPL.Close)

# IQRx = Qx(0,75) - Qx(0.25) (interquartile range)
IQR (aapl_return) 

# kernel density
aapl_return.density = density(aapl_return) 
plot(aapl_return.density) 

#QQ
qqnorm(aapl_return)
qqline(aapl_return)

## Shapiro-Wilk test 
aapl_return2 = fortify.zoo(aapl_return)

# Shapiro-Wilk test, use a vector as the input, not a dataframe
shapiro.test(aapl_return2$AAPL.Close) 

# D'Agostino skewness test
library (moments)
agostino.test(aapl_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (aapl_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (aapl_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
install.packages("fBasics")
library (fBasics)
ks.test(aapl_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(aapl_return2$AAPL.Close)

#Anderson-Darling goodness of fit test
install.packages("ADGofTest")
library (ADGofTest)
ad.test (aapl_return2$AAPL.Close, plnorm)

##################################################################
#COKE

# Get ticket of COKE
getSymbols("COKE", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
chart_Series(COKE) 
coke_data.xts = COKE$COKE.Close

# Log return calculation
coke_return = diff (log(coke_data.xts)) 

# removing the first empty observation, received after return calculation
coke_return = coke_return [-1] 
summary (coke_return)


# plot the graph
boxplot(coke_return,  horizontal=TRUE, main = "Coke return Boxplot", ylab = "Return")

# Mean absolute deviation
library (DescTools) 
MeanAD(coke_return)

# Variance
var(coke_return)

# Standard deviation
sd(coke_return)

# Median Absolute Deviation -> compare with Std Dev
mad (coke_return)

#skewness
library(e1071) 
skewness (coke_return$COKE.Close)
kurtosis(coke_return$COKE.Close)

# IQRx = Qx(0,75) - Qx(0.25) (interquartile range)
IQR (coke_return) 

# kernel density
coke_return.density = density(coke_return) 
plot(coke_return.density) 

#QQ
qqnorm(coke_return)
qqline(coke_return)

## Shapiro-Wilk test 
coke_return2 = fortify.zoo(coke_return)

# Shapiro-Wilk test, use a vector as the input, not a dataframe
shapiro.test(coke_return2$COKE.Close) 

# D'Agostino skewness test
library (moments)
agostino.test(coke_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (coke_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (coke_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(coke_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(coke_return2$COKE.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (coke_return2$COKE.Close, plnorm)

####################################################################
#AMAZON

# Get ticket of Amazon
getSymbols("AMZN", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
chart_Series(AMZN) 
amzn_data.xts = AMZN$AMZN.Close

# Log return calculation
amzn_return = diff (log(amzn_data.xts)) 

# removing the first empty observation, received after return calculation
amzn_return = amzn_return [-1] 
summary (amzn_return)

# plot the graph
boxplot(amzn_return,  horizontal=TRUE, main = "Amazon return Boxplot", ylab = "Return")

# Mean absolute deviation
library (DescTools) 
MeanAD(amzn_return)

# Variance
var(amzn_return)

# Standard deviation
sd(amzn_return)

# Median Absolute Deviation -> compare with Std Dev
mad (amzn_return)

#skewness
library(e1071) 
skewness (amzn_return$AMZN.Close)
kurtosis(amzn_return$AMZN.Close)

# IQRx = Qx(0,75) - Qx(0.25) (interquartile range)
IQR (amzn_return) 

# kernel density
amzn_return.density = density(amzn_return) 
plot(amzn_return.density) 

#QQ
qqnorm(amzn_return)
qqline(amzn_return)

## Shapiro-Wilk test 
amzn_return2 = fortify.zoo(amzn_return)

# Shapiro-Wilk test, use a vector as the input, not a dataframe
shapiro.test(amzn_return2$AMZN.Close) 

# D'Agostino skewness test
library (moments)
agostino.test(amzn_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (amzn_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (amzn_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(amzn_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(amzn_return2$AMZN.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (amzn_return2$AMZN.Close, plnorm)


####################################################################
#NETFLİX

# Get ticket of netflix
getSymbols("NFLX", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
chart_Series(NFLX) 
net_data.xts = NFLX$NFLX.Close

# Log return calculation
net_return = diff (log(net_data.xts)) 

# removing the first empty observation, received after return calculation
net_return = net_return [-1] 
summary (net_return)

# plot the graph
boxplot(net_return,  horizontal=TRUE, main = "Netflix return Boxplot", ylab = "Return")

# Mean absolute deviation
library (DescTools) 
MeanAD(net_return)

# Variance
var(net_return)

# Standard deviation
sd(net_return)

# Median Absolute Deviation -> compare with Std Dev
mad (net_return)

#skewness
library(e1071) 
skewness (net_return$NFLX.Close)
kurtosis(net_return$NFLX.Close)

# IQRx = Qx(0,75) - Qx(0.25) (interquartile range)
IQR (net_return) 

# kernel density
net_return.density = density(net_return) 
plot(net_return.density) 

#QQ
qqnorm(net_return)
qqline(net_return)

## Shapiro-Wilk test 
net_return2 = fortify.zoo(net_return)

# Shapiro-Wilk test, use a vector as the input, not a dataframe
shapiro.test(net_return2$NFLX.Close) 

# D'Agostino skewness test
library (moments)
agostino.test(net_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (net_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (net_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(net_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(net_return2$NFLX.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (net_return2$NFLX.Close, plnorm)


####################################################################
#META

# Get ticket of meta
getSymbols("META", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
chart_Series(META) 
meta_data.xts = META$META.Close

# Log return calculation
meta_return = diff (log(meta_data.xts)) 

# removing the first empty observation, received after return calculation
meta_return = meta_return [-1] 
summary (meta_return)

# plot the graph
boxplot(meta_return,  horizontal=TRUE, main = "META return Boxplot", ylab = "Return")

# Mean absolute deviation
library (DescTools) 
MeanAD(meta_return)

# Variance
var(meta_return)

# Standard deviation
sd(meta_return)

# Median Absolute Deviation -> compare with Std Dev
mad (meta_return)

#skewness
library(e1071) 
skewness (meta_return$META.Close)
kurtosis(meta_return$META.Close)

# IQRx = Qx(0,75) - Qx(0.25) (interquartile range)
IQR (meta_return) 

# kernel density
meta_return.density = density(meta_return) 
plot(meta_return.density) 

#QQ
qqnorm(meta_return)
qqline(meta_return)

## Shapiro-Wilk test 
meta_return2 = fortify.zoo(meta_return)

# Shapiro-Wilk test, use a vector as the input, not a dataframe
shapiro.test(meta_return2$META.Close) 

# D'Agostino skewness test
library (moments)
agostino.test(meta_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (meta_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (meta_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(meta_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(meta_return2$META.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (meta_return2$META.Close, plnorm)

#################################################################################
##################################################################################

#META

# Get ticket of meta
getSymbols("META", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
chart_Series(META) 
meta_data.xts = META$META.Close

# Log return calculation
meta_return = diff (log(meta_data.xts)) 

# removing the first empty observation, received after return calculation
meta_return = meta_return [-1] 
summary (meta_return)

# plot the graph
boxplot(meta_return,  horizontal=TRUE, main = "META return Boxplot", ylab = "Return")

# Mean absolute deviation
library (DescTools) 
MeanAD(meta_return)

# Variance
var(meta_return)

# Standard deviation
sd(meta_return)

# Median Absolute Deviation -> compare with Std Dev
mad (meta_return)

#skewness
library(e1071) 
skewness (meta_return$META.Close)
kurtosis(meta_return$META.Close)

# IQRx = Qx(0,75) - Qx(0.25) (interquartile range)
IQR (meta_return) 

# kernel density
meta_return.density = density(meta_return) 
plot(meta_return.density) 

#QQ
qqnorm(meta_return)
qqline(meta_return)

## Shapiro-Wilk test 
meta_return2 = fortify.zoo(meta_return)

# Shapiro-Wilk test, use a vector as the input, not a dataframe
shapiro.test(meta_return2$META.Close) 

# D'Agostino skewness test
library (moments)
agostino.test(meta_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (meta_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (meta_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(meta_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(meta_return2$META.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (meta_return2$META.Close, plnorm)

##############################################################################
##############################################################################

#SP600

ticker = "^SP600"

# Get ticket of netflix
getSymbols(ticker, from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
chart_Series(SP600) 
sp_data.xts = SP600$SP600.Close

# Log return calculation
sp_return = diff (log(sp_data.xts)) 

# removing the first empty observation, received after return calculation
sp_return = sp_return [-1] 
summary (sp_return)

# plot the graph
boxplot(sp_return,  horizontal=TRUE, main = "SP600 return Boxplot", ylab = "Return")

# Mean absolute deviation
library (DescTools) 
MeanAD(sp_return)

# Variance
var(sp_return)

# Standard deviation
sd(sp_return)

# Median Absolute Deviation -> compare with Std Dev
mad (sp_return)

#skewness
library(e1071) 
skewness (sp_return$SP600.Close)
kurtosis(sp_return$SP600.Close)

# IQRx = Qx(0,75) - Qx(0.25) (interquartile range)
IQR (sp_return) 

# kernel density
sp_return.density = density(sp_return) 
plot(sp_return.density) 

#QQ
qqnorm(sp_return)
qqline(sp_return)

## Shapiro-Wilk test 
sp_return2 = fortify.zoo(sp_return)

# Shapiro-Wilk test, use a vector as the input, not a dataframe
shapiro.test(sp_return2$SP600.Close) 

# D'Agostino skewness test
library (moments)
agostino.test(sp_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (sp_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (sp_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(sp_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(sp_return2$SP600.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (sp_return2$SP600.Close, plnorm)

########################################################################################
########################################################################################
## Multivariate analysis

## Benchmark vs AAPL#
# merging 2 dataframes
together =  merge(SP600$SP600.Close,AAPL$AAPL.Close, all=FALSE)

# Calculate log return
require (PerformanceAnalytics) 
return.together = Return.calculate( together , method = "log")
plot (return.together)

#convert into dataframes for further usage
require (ggplot2) 

# convert the  xts object into a dataframe
return.together.df = fortify(return.together) 
aapl_return.together.df = return.together.df [-1,]
head(aapl_return.together.df)

# for further simplicity let's assign returns to X and Y
X= aapl_return.together.df$SP600.Close
Y= aapl_return.together.df$AAPL.Close

# Correlation 
require (fBasics) 

#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)

#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")
#The Spearman rank correlation between x and y is .1. 

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")


## Benchmark vs COKE#
# merging 2 dataframes
together =  merge(SP600$SP600.Close,COKE$COKE.Close, all=FALSE)

# Calculate log return
require (PerformanceAnalytics) 
return.together = Return.calculate( together , method = "log")
plot (return.together)

#convert into dataframes for further usage
require (ggplot2) 

# convert the  xts object into a dataframe
return.together.df = fortify(return.together) 
coke_return.together.df = return.together.df [-1,]
head(coke_return.together.df)

# for further simplicity let's assign returns to X and Y
X= coke_return.together.df$SP600.Close
Y= coke_return.together.df$COKE.Close

# Correlation 
require (fBasics) 

#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)

#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")
#The Spearman rank correlation between x and y is .1. 

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")



## Benchmark vs AMZN#
# merging 2 dataframes
together =  merge(SP600$SP600.Close,AMZN$AMZN.Close, all=FALSE)

# Calculate log return
require (PerformanceAnalytics) 
return.together = Return.calculate( together , method = "log")
plot (return.together)

#convert into dataframes for further usage
require (ggplot2) 

# convert the  xts object into a dataframe
return.together.df = fortify(return.together) 
amzn_return.together.df = return.together.df [-1,]
head(amzn_return.together.df)

# for further simplicity let's assign returns to X and Y
X= amzn_return.together.df$SP600.Close
Y= amzn_return.together.df$AMZN.Close

# Correlation 
require (fBasics) 

#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)

#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")
#The Spearman rank correlation between x and y is .1. 

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")




## Benchmark vs NFLX#
# merging 2 dataframes
together =  merge(SP600$SP600.Close,NFLX$NFLX.Close, all=FALSE)

# Calculate log return
require (PerformanceAnalytics) 
return.together = Return.calculate( together , method = "log")
plot (return.together)

#convert into dataframes for further usage
require (ggplot2) 

# convert the  xts object into a dataframe
return.together.df = fortify(return.together) 
nflx_return.together.df = return.together.df [-1,]
head(nflx_return.together.df)

# for further simplicity let's assign returns to X and Y
X= nflx_return.together.df$SP600.Close
Y= nflx_return.together.df$NFLX.Close

# Correlation 
require (fBasics) 

#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)

#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")
#The Spearman rank correlation between x and y is .1. 

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")



## Benchmark vs META#
# merging 2 dataframes
together =  merge(SP600$SP600.Close,META$META.Close, all=FALSE)

# Calculate log return
require (PerformanceAnalytics) 
return.together = Return.calculate( together , method = "log")
plot (return.together)

#convert into dataframes for further usage
require (ggplot2) 

# convert the  meta object into a dataframe
return.together.df = fortify(return.together) 
meta_return.together.df = return.together.df [-1,]
head(meta_return.together.df)

# for further simplicity let's assign returns to X and Y
X= meta_return.together.df$SP600.Close
Y= meta_return.together.df$META.Close

# Correlation 
require (fBasics) 

#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)

#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")
#The Spearman rank correlation between x and y is .1. 

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")

##############################################################################################
##############################################################################################################
#causality

library(yfR)
#  options 
my_ticker = c("^SP600","AAPL","COKE","AMZN","NFLX","META") 

# get  data
df_yf = yf_get(tickers = my_ticker, 
               first_date = start_date,
               last_date = end_date)
head(df_yf)

unique.tikers = unique(df_yf$ticker)

SP600= df_yf[df_yf$ticker==unique.tikers[1], ] 
AAPL= df_yf[df_yf$ticker==unique.tikers[2], ] 
COKE= df_yf[df_yf$ticker==unique.tikers[3], ] 
AMZN= df_yf[df_yf$ticker==unique.tikers[4], ] 
NFLX= df_yf[df_yf$ticker==unique.tikers[5], ] 
META= df_yf[df_yf$ticker==unique.tikers[6], ] 

library (lmtest)
grangertest(SP600$price_close ~ AAPL$price_close, order = 1)
grangertest(SP600$price_close ~ COKE$price_close, order = 1)
grangertest(SP600$price_close ~ AMZN$price_close, order = 1)
grangertest(SP600$price_close ~ NFLX$price_close, order = 1)
grangertest(SP600$price_close ~ META$price_close, order = 1)


#############################################################################################################
## Volatility
## GARCH model

X = sp_return
Y= aapl_return

library (tseries)
aapl.garch.1 <- garch(Y, order =c(1,1))
summary (aapl.garch.1)

library (rugarch)
aapl.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
aapl.garch.2 = ugarchfit(aapl.garch.spec, Y)
summary(aapl.garch.2)
plot (aapl.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))


X = sp_return
Y= coke_return

library (tseries)
coke.garch.1 <- garch(Y, order =c(1,1))
summary (coke.garch.1)

library (rugarch)
coke.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
coke.garch.2 = ugarchfit(coke.garch.spec, Y)
summary(coke.garch.2)
plot (coke.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

X = sp_return
Y= amzn_return

library (tseries)
amzn.garch.1 <- garch(Y, order =c(1,1))
summary (amzn.garch.1)

library (rugarch)
amzn.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
amzn.garch.2 = ugarchfit(amzn.garch.spec, Y)
summary(amzn.garch.2)
plot (amzn.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))


X = sp_return
Y= net_return

library (tseries)
net.garch.1 <- garch(Y, order =c(1,1))
summary (net.garch.1)

library (rugarch)
net.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
net.garch.2 = ugarchfit(net.garch.spec, Y)
summary(net.garch.2)
plot (net.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))


X = sp_return
Y= meta_return

library (tseries)
meta.garch.1 <- garch(Y, order =c(1,1))
summary (meta.garch.1)

library (rugarch)
meta.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
meta.garch.2 = ugarchfit(meta.garch.spec, Y)
summary(meta.garch.2)
plot (meta.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

###############################################################################################################
##Simulasyon

mu <- mean(aapl_return)
sigma <- sd(aapl_return)
start_price <- tail(aapl_data.xts, n=1)
simulated_prices <- numeric(length=100)

# Monte Carlo 
for(i in 1:100) {
  random_return <- rnorm(1, mean=mu, sd=sigma)
  future_price <- start_price * exp(random_return)
  simulated_prices[i] <- future_price
}

plot(simulated_prices)
lines(simulated_prices, col="red")

#coke
mu <- mean(coke_return)
sigma <- sd(coke_return)
start_price <- tail(coke_data.xts, n=1)
simulated_prices <- numeric(length=100)

# Monte Carlo 
for(i in 1:100) {
  random_return <- rnorm(1, mean=mu, sd=sigma)
  future_price <- start_price * exp(random_return)
  simulated_prices[i] <- future_price
}

plot(simulated_prices)
lines(simulated_prices, col="red")

#nflx
mu <- mean(net_return)
sigma <- sd(net_return)
start_price <- tail(net_data.xts, n=1)
simulated_prices <- numeric(length=100)

# Monte Carlo 
for(i in 1:100) {
  random_return <- rnorm(1, mean=mu, sd=sigma)
  future_price <- start_price * exp(random_return)
  simulated_prices[i] <- future_price
}

plot(simulated_prices)
lines(simulated_prices, col="red")

#amzn
mu <- mean(amzn_return)
sigma <- sd(amzn_return)
start_price <- tail(amzn_data.xts, n=1)
simulated_prices <- numeric(length=100)

# Monte Carlo 
for(i in 1:100) {
  random_return <- rnorm(1, mean=mu, sd=sigma)
  future_price <- start_price * exp(random_return)
  simulated_prices[i] <- future_price
}

plot(simulated_prices)
lines(simulated_prices, col="red")

#amzn
mu <- mean(meta_return)
sigma <- sd(meta_return)
start_price <- tail(meta_data.xts, n=1)
simulated_prices <- numeric(length=100)

# Monte Carlo 
for(i in 1:100) {
  random_return <- rnorm(1, mean=mu, sd=sigma)
  future_price <- start_price * exp(random_return)
  simulated_prices[i] <- future_price
}

plot(simulated_prices)
lines(simulated_prices, col="red")





