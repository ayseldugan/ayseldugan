require (xts) 
require(zoo)
library(tidyquant)
start_date = "2018-04-30"
end_date = "2023-04-30"

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
ticker = "^SP600"

# Get ticket of SP600
getSymbols(ticker, from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
chart_Series(SP600) 
SP600_data.xts = SP600$SP600.Close

# Log return calculation
sp_return = diff (log(SP600_data.xts)) 

# removing the first empty observation, received after return calculation
sp_return = sp_return [-1] 
summary (sp_return)

# kernel density
sp_return.density = density(sp_return) 
plot(sp_return.density) 

## Shapiro-Wilk test 
sp_return2 = fortify.zoo(sp_return)

##################################################################################################
# SP600 vs Bitcoin # import a time series of different frequency
BTC = read.csv("BTC-USD.csv")

# converting a data frame into xts onject (time-series object)
BTC$Date = as.Date(BTC$Date,format="%Y-%m-%d", tz = "")
BTC.xts = xts(BTC['Close'], order.by=BTC$Date)  
head(BTC.xts$Close)
plot(BTC$Close, main = "Bitcoin weekly Close Price", type = "l", ylab = "Price")

# Log return calculation
btc_return = diff (log(BTC.xts)) 
                   
# removing the first empty observation, received after return calculation
btc_return = btc_return [-1] 
summary (btc_return)
                   
boxplot(btc_return,  horizontal=TRUE, main = "Bitcoin return Boxplot", ylab = "Return")
plot(btc_return, main = "Bitcoin weekly returns", xlab = "year", type = "l", ylab = "log return") 
                   
# boxplot# return as a vector
btc_return2 = diff (log(BTC$Close)) 
boxplot(btc_return2, horizontal=TRUE, main="Bitcoin Return")
hist(btc_return) 

## Location
mean (btc_return) 
median(btc_return) 
                   
# Variability
library (DescTools) 
MeanAD(btc_return)
var(btc_return)
sd(btc_return)
mad (btc_return)
IQR (btc_return)
                   
#skewness
library(e1071) 
skewness (btc_return$Close)
kurtosis(btc_return$Close)
                   
# kernel density
btc_return.density = density(btc_return) 
plot (btc_return.density) 
                   
#QQ
qqnorm(btc_return)
qqline(btc_return)
                   
## Shapiro-Wilk test 
btc_return2 = fortify.zoo(btc_return)
head(btc_return)
head(btc_return2)
shapiro.test(btc_return2$Close)
                   
# D'Agostino skewness test
library (moments)
agostino.test(btc_return, alternative = "two.sided") 
                   
# Anscombe-Glynn test of kurtosis
anscombe.test (btc_return, alternative = "two.sided" ) 
                   
#Bonett-seier test of kurtosis
bonett.test (btc_return, alternative = "two.sided" )
                   
# Kolmogorov-Smirnov
library (fBasics)
ks.test(btc_return, "pnorm")
                   
# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(btc_return2$Close)
                   
#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (btc_return2$Close, plnorm)

# merging 2 dataframes
btc_together =  merge(SP600$SP600.Close,BTC.xts$Close, all=FALSE) 
head(btc_together)
                   
# Calculate log return
require (PerformanceAnalytics) 
btc_return.together = Return.calculate( btc_together , method = "log")
plot (btc_return.together)
                   
#convert into dataframes for further usage
require (ggplot2) 
btc_return.together.df = fortify(btc_return.together) 
btc_return.together.df = btc_return.together.df [-1,]
                   
# for further simplicity let's assign returns to X and Y
X= btc_return.together.df$SP600.Close
Y= btc_return.together.df$Close
                   
# Correlation 
require (fBasics) 
                   
#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)
                   
#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")
                   
###################################################################################################################
#etherium
# SP600 vs ETH # import a time series of different frequency
ETH = read.csv("ETH-USD.csv")

# converting a data frame into xts onject (time-series object)
ETH$Date = as.Date(ETH$Date,format="%Y-%m-%d", tz = "")
ETH.xts = xts(ETH['Close'], order.by=ETH$Date)  
head(ETH.xts$Close)
plot(ETH$Close, main = "Etherium weekly Close Price", type = "l", ylab = "Price")

# Log return calculation
eth_return = diff (log(ETH.xts)) 

# removing the first empty observation, received after return calculation
eth_return = eth_return [-1] 
summary (eth_return)

boxplot(eth_return,  horizontal=TRUE, main = "Etherium return Boxplot", ylab = "Return")
plot(eth_return, main = "Bitcoin weekly returns", xlab = "year", type = "l", ylab = "log return") 

# boxplot# return as a vector
eth_return2 = diff (log(ETH$Close)) 
boxplot(eth_return2, horizontal=TRUE, main="Bitcoin Return")
hist(eth_return) 

## Location
mean (eth_return) 
median(eth_return) 

# Variability
library (DescTools) 
MeanAD(eth_return)
var(eth_return)
sd(eth_return)
mad (eth_return)
IQR (eth_return)

#skewness
library(e1071) 
skewness (eth_return$Close)
kurtosis(eth_return$Close)

# kernel density
eth_return.density = density(eth_return) 
plot (eth_return.density) 

#QQ
qqnorm(eth_return)
qqline(eth_return)

## Shapiro-Wilk test 
eth_return2 = fortify.zoo(eth_return)
shapiro.test(eth_return2$Close)

# D'Agostino skewness test
library (moments)
agostino.test(eth_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (eth_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (eth_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(eth_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(eth_return2$Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (eth_return2$Close, plnorm)

# merging 2 dataframes
eth_together =  merge(SP600$SP600.Close,ETH.xts$Close, all=FALSE) 
head(eth_together)

# Calculate log return
require (PerformanceAnalytics) 
eth_return.together = Return.calculate( eth_together , method = "log")
plot (eth_return.together)

#convert into dataframes for further usage
require (ggplot2) 
eth_return.together.df = fortify(eth_return.together) 
eth_return.together.df = eth_return.together.df [-1,]

# for further simplicity let's assign returns to X and Y
X= eth_return.together.df$SP600.Close
Y= eth_return.together.df$Close

# Correlation 
require (fBasics) 

#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)

#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")


###############################################################################################
#Litecoin
# SP600 vs LTC # import a time series of different frequency
LTC = read.csv("LTC-USD.csv")

# converting a data frame into xts onject (time-series object)
LTC$Date = as.Date(LTC$Date,format="%Y-%m-%d", tz = "")
LTC.xts = xts(LTC['Close'], order.by=LTC$Date)  
head(LTC.xts$Close)
plot(LTC$Close, main = "Litcoin weekly Close Price", type = "l", ylab = "Price")

# Log return calculation
ltc_return = diff (log(LTC.xts)) 

# removing the first empty observation, received after return calculation
ltc_return = ltc_return [-1] 
summary (ltc_return)

boxplot(ltc_return,  horizontal=TRUE, main = "Litecoin return Boxplot", ylab = "Return")
plot(ltc_return, main = "Litecoin weekly returns", xlab = "year", type = "l", ylab = "log return") 

# boxplot# return as a vector
ltc_return2 = diff (log(LTC$Close)) 
boxplot(ltc_return2, horizontal=TRUE, main="Bitcoin Return")
hist(ltc_return) 

## Location
mean (ltc_return) 
median(ltc_return) 

# Variability
library (DescTools) 
MeanAD(ltc_return)
var(ltc_return)
sd(ltc_return)
mad (ltc_return)
IQR (ltc_return)

#skewness
library(e1071) 
skewness (ltc_return$Close)
kurtosis(ltc_return$Close)

# kernel density
ltc_return.density = density(ltc_return) 
plot (ltc_return.density) 

#QQ
qqnorm(ltc_return)
qqline(ltc_return)

## Shapiro-Wilk test 
ltc_return2 = fortify.zoo(ltc_return)
shapiro.test(ltc_return2$Close)

# D'Agostino skewness test
library (moments)
agostino.test(ltc_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (ltc_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (ltc_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(ltc_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(ltc_return2$Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (ltc_return2$Close, plnorm)

# merging 2 dataframes
ltc_together =  merge(SP600$SP600.Close,LTC.xts$Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) 
ltc_return.together = Return.calculate( ltc_together , method = "log")
plot (ltc_return.together)

#convert into dataframes for further usage
require (ggplot2) 
ltc_return.together.df = fortify(ltc_return.together) 
ltc_return.together.df = ltc_return.together.df [-1,]

# for further simplicity let's assign returns to X and Y
X= ltc_return.together.df$SP600.Close
Y= ltc_return.together.df$Close

# Correlation 
require (fBasics) 

#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)

#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")

########################################################################################################

#Tron
# SP600 vs XRP # import a time series of different frequency
TRX = read.csv("TRX-USD.csv")

# converting a data frame into xts onject (time-series object)
TRX$Date = as.Date(TRX$Date,format="%Y-%m-%d", tz = "")
TRX.xts = xts(TRX['Close'], order.by=TRX$Date)  
head(TRX.xts$Close)
plot(TRX$Close, main = "TRON weekly Close Price", type = "l", ylab = "Price")

# Log return calculation
TRX_return = diff (log(TRX.xts)) 

# removing the first empty observation, received after return calculation
TRX_return = TRX_return [-1] 
summary (TRX_return)

boxplot(TRX_return,  horizontal=TRUE, main = "TRON return Boxplot", ylab = "Return")
plot(TRX_return, main = "TRON weekly returns", xlab = "year", type = "l", ylab = "log return") 

# boxplot# return as a vector
TRX_return2 = diff (log(TRX$Close)) 
boxplot(TRX_return2, horizontal=TRUE, main="TRX Return")
hist(TRX_return) 

## Location
mean (TRX_return) 
median(TRX_return) 

# Variability
library (DescTools) 
MeanAD(TRX_return)
var(TRX_return)
sd(TRX_return)
mad (TRX_return)
IQR (TRX_return)

#skewness
library(e1071) 
skewness (TRX_return$Close)
kurtosis(TRX_return$Close)

# kernel density
TRX_return.density = density(TRX_return) 
plot (TRX_return.density) 

#QQ
qqnorm(TRX_return)
qqline(TRX_return)

## Shapiro-Wilk test 
TRX_return2 = fortify.zoo(TRX_return)
shapiro.test(TRX_return2$Close)

# D'Agostino skewness test
library (moments)
agostino.test(TRX_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (TRX_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (TRX_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(TRX_return, "pnorm")

# JARQUE-BERA TEST
jarqueberaTest(TRX_return2$Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (TRX_return2$Close, plnorm)

# merging 2 dataframes
TRX_together =  merge(SP600$SP600.Close,TRX.xts$Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) 
TRX_return.together = Return.calculate( TRX_together , method = "log")
plot (TRX_return.together)

#convert into dataframes for further usage
require (ggplot2) 
TRX_return.together.df = fortify(TRX_return.together) 
TRX_return.together.df = TRX_return.together.df [-1,]

# for further simplicity let's assign returns to X and Y
X= TRX_return.together.df$SP600.Close
Y= TRX_return.together.df$Close

# Correlation 
require (fBasics) 

#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)

#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")

####################################################################################3
#DOGE

# SP600 vs ADA # import a time series of different frequency
DOGE = read.csv("DOGE-USD.csv")

# converting a data frame into xts onject (time-series object)
DOGE$Date = as.Date(DOGE$Date,format="%Y-%m-%d", tz = "")
DOGE.xts = xts(DOGE['Close'], order.by=DOGE$Date)  
head(DOGE.xts$Close)
plot(DOGE$Close, main = "DOGE weekly Close Price", type = "l", ylab = "Price")

# Log return calculation
DOGE_return = diff (log(DOGE.xts)) 

# removing the first empty observation, received after return calculation
DOGE_return = DOGE_return [-1] 
summary (DOGE_return)

boxplot(DOGE_return,  horizontal=TRUE, main = "DOGE return Boxplot", ylab = "Return")
plot(DOGE_return, main = "DOGE weekly returns", xlab = "year", type = "l", ylab = "log return") 

## Location
mean (DOGE_return) 
median(DOGE_return) 

# Variability
library (DescTools) 
MeanAD(DOGE_return)
var(DOGE_return)
sd(DOGE_return)
mad (DOGE_return)
IQR (DOGE_return)

#skewness
library(e1071) 
skewness (DOGE_return$Close)
kurtosis(DOGE_return$Close)

# kernel density
DOGE_return.density = density(DOGE_return) 
plot (DOGE_return.density) 

#QQ
qqnorm(DOGE_return)
qqline(DOGE_return)

## Shapiro-Wilk test 
DOGE_return2 = fortify.zoo(DOGE_return)
shapiro.test(DOGE_return2$Close)

# D'Agostino skewness test
library (moments)
agostino.test(DOGE_return, alternative = "two.sided") 

# Anscombe-Glynn test of kurtosis
anscombe.test (DOGE_return, alternative = "two.sided" ) 

#Bonett-seier test of kurtosis
bonett.test (DOGE_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(DOGE_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(DOGE_return2$Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (DOGE_return2$Close, plnorm)

# merging 2 dataframes
DOGE_together =  merge(SP600$SP600.Close,DOGE.xts$Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) 
DOGE_return.together = Return.calculate( DOGE_together , method = "log")
plot (DOGE_return.together)

#convert into dataframes for further usage
require (ggplot2) 
DOGE_return.together.df = fortify(DOGE_return.together) 
DOGE_return.together.df = DOGE_return.together.df [-1,]

# for further simplicity let's assign returns to X and Y
X= DOGE_return.together.df$SP600.Close
Y= DOGE_return.together.df$Close

# Correlation 
require (fBasics) 

#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)

#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")


#################################################################################################################
##Causality based on VAR
##SP600 vs BTC
library(vars)
head(btc_return.together.df)
data = cbind(btc_return.together.df$SP600.Close, btc_return.together.df$Close) # Combine the time series into a matrix
# Set the maximum number of lags to consider
max_lags = 10

# Calculate selection criteria for different lag orders
lag_selection = VARselect(data, lag.max = max_lags, type = "both")

# View the selection criteria
print(lag_selection)

# Extract the optimal lag order based on the AIC or BIC criterion
optimal_lag_aic = lag_selection$selection["AIC(n)"]
optimal_lag_bic = lag_selection$selection["SC(n)"]

# Estimate the VAR model with the optimal lag order
model_aic = VAR(data, p = optimal_lag_aic)
model_bic = VAR(data, p = optimal_lag_bic)

#To test for causality within a VAR model, you can use the causality() function:
causal_test = causality(model_aic, cause = "y1")
causal_test

##SP600 vs LTC
library(vars)
head(ltc_return.together.df)
data = cbind(ltc_return.together.df$SP600.Close, ltc_return.together.df$Close) # Combine the time series into a matrix
# Set the maximum number of lags to consider
max_lags = 10

# Calculate selection criteria for different lag orders
lag_selection = VARselect(data, lag.max = max_lags, type = "both")

# View the selection criteria
print(lag_selection)

# Extract the optimal lag order based on the AIC or BIC criterion
optimal_lag_aic = lag_selection$selection["AIC(n)"]
optimal_lag_bic = lag_selection$selection["SC(n)"]

# Estimate the VAR model with the optimal lag order
model_aic = VAR(data, p = optimal_lag_aic)
model_bic = VAR(data, p = optimal_lag_bic)

#To test for causality within a VAR model, you can use the causality() function:
causal_test = causality(model_aic, cause = "y1")
causal_test

##SP600 vs ETH
library(vars)
head(eth_return.together.df)
data = cbind(eth_return.together.df$SP600.Close, eth_return.together.df$Close) # Combine the time series into a matrix
# Set the maximum number of lags to consider
max_lags = 10

# Calculate selection criteria for different lag orders
lag_selection = VARselect(data, lag.max = max_lags, type = "both")

# View the selection criteria
print(lag_selection)

# Extract the optimal lag order based on the AIC or BIC criterion
optimal_lag_aic = lag_selection$selection["AIC(n)"]
optimal_lag_bic = lag_selection$selection["SC(n)"]

# Estimate the VAR model with the optimal lag order
model_aic = VAR(data, p = optimal_lag_aic)
model_bic = VAR(data, p = optimal_lag_bic)

#To test for causality within a VAR model, you can use the causality() function:
causal_test = causality(model_aic, cause = "y1")
causal_test

##SP600 vs TRX
library(vars)
head(TRX_return.together.df)
data = cbind(TRX_return.together.df$SP600.Close, TRX_return.together.df$Close) # Combine the time series into a matrix
# Set the maximum number of lags to consider
max_lags = 10

# Calculate selection criteria for different lag orders
lag_selection = VARselect(data, lag.max = max_lags, type = "both")

# View the selection criteria
print(lag_selection)

# Extract the optimal lag order based on the AIC or BIC criterion
optimal_lag_aic = lag_selection$selection["AIC(n)"]
optimal_lag_bic = lag_selection$selection["SC(n)"]

# Estimate the VAR model with the optimal lag order
model_aic = VAR(data, p = optimal_lag_aic)
model_bic = VAR(data, p = optimal_lag_bic)

#To test for causality within a VAR model, you can use the causality() function:
causal_test = causality(model_aic, cause = "y1")
causal_test


##SP600 vs TRX
library(vars)
head(DOGE_return.together.df)
data = cbind(DOGE_return.together.df$SP600.Close, DOGE_return.together.df$Close) # Combine the time series into a matrix
# Set the maximum number of lags to consider
max_lags = 10

# Calculate selection criteria for different lag orders
lag_selection = VARselect(data, lag.max = max_lags, type = "both")

# View the selection criteria
print(lag_selection)

# Extract the optimal lag order based on the AIC or BIC criterion
optimal_lag_aic = lag_selection$selection["AIC(n)"]
optimal_lag_bic = lag_selection$selection["SC(n)"]

# Estimate the VAR model with the optimal lag order
model_aic = VAR(data, p = optimal_lag_aic)
model_bic = VAR(data, p = optimal_lag_bic)

#To test for causality within a VAR model, you can use the causality() function:
causal_test = causality(model_aic, cause = "y1")
causal_test


#######################################################################################################
#volatility

## Volatility
## GARCH model

X = sp_return
Y= btc_return

library (tseries)
btc.garch.1 <- garch(Y, order =c(1,1))
summary (btc.garch.1)

library (rugarch)
btc.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
btc.garch.2 = ugarchfit(btc.garch.spec, Y)
summary(btc.garch.2)
plot (btc.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))


X = sp_return
Y= ltc_return

library (tseries)
ltc.garch.1 <- garch(Y, order =c(1,1))
summary (ltc.garch.1)

library (rugarch)
btc.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
btc.garch.2 = ugarchfit(btc.garch.spec, Y)
summary(btc.garch.2)
plot (btc.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))


X = sp_return
Y= TRX_return

library (tseries)
TRX.garch.1 <- garch(Y, order =c(1,1))
summary (TRX.garch.1)

library (rugarch)
TRX.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
TRX.garch.2 = ugarchfit(TRX.garch.spec, Y)
summary(TRX.garch.2)
plot (TRX.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))


X = sp_return
Y= DOGE_return

library (tseries)
DOGE.garch.1 <- garch(Y, order =c(1,1))
summary (DOGE.garch.1)

library (rugarch)
TRX.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
TRX.garch.2 = ugarchfit(TRX.garch.spec, Y)
summary(TRX.garch.2)
plot (TRX.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

############################################################################################################

#btc
mu <- mean(btc_return)
sigma <- sd(meta_return)
start_price <- tail(btc_data.xts, n=1)
simulated_prices <- numeric(length=100)

# Monte Carlo 
for(i in 1:100) {
  random_return <- rnorm(1, mean=mu, sd=sigma)
  future_price <- start_price * exp(random_return)
  simulated_prices[i] <- future_price
}

plot(simulated_prices)
lines(simulated_prices, col="red")

#eth
mu <- mean(eth_return)
sigma <- sd(eth_return)
start_price <- tail(ETH.xts, n=1)
simulated_prices <- numeric(length=100)

# Monte Carlo 
for(i in 1:100) {
  random_return <- rnorm(1, mean=mu, sd=sigma)
  future_price <- start_price * exp(random_return)
  simulated_prices[i] <- future_price
}

plot(simulated_prices)
lines(simulated_prices, col="red")

#ltc
mu <- mean(ltc_return)
sigma <- sd(ltc_return)
start_price <- tail(LTC.xts, n=1)
simulated_prices <- numeric(length=100)

# Monte Carlo 
for(i in 1:100) {
  random_return <- rnorm(1, mean=mu, sd=sigma)
  future_price <- start_price * exp(random_return)
  simulated_prices[i] <- future_price
}

plot(simulated_prices)
lines(simulated_prices, col="red")


#trx
mu <- mean(TRX_return)
sigma <- sd(TRX_return)
start_price <- tail(TRX.xts, n=1)
simulated_prices <- numeric(length=100)

# Monte Carlo 
for(i in 1:100) {
  random_return <- rnorm(1, mean=mu, sd=sigma)
  future_price <- start_price * exp(random_return)
  simulated_prices[i] <- future_price
}

plot(simulated_prices)
lines(simulated_prices, col="red")


#trx
mu <- mean(DOGE_return)
sigma <- sd(DOGE_return)
start_price <- tail(DOGE.xts, n=1)
simulated_prices <- numeric(length=100)

# Monte Carlo 
for(i in 1:100) {
  random_return <- rnorm(1, mean=mu, sd=sigma)
  future_price <- start_price * exp(random_return)
  simulated_prices[i] <- future_price
}

plot(simulated_prices)
lines(simulated_prices, col="red")


