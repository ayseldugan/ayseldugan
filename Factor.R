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

aapl_data.xts = AAPL$AAPL.Close

# Log return calculation
aapl_return = diff (log(aapl_data.xts)) 

# removing the first empty observation, received after return calculation
aapl_return = aapl_return [-1] 
View(aapl_return)

# Get ticket of COKE
getSymbols("COKE", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
coke_data.xts = COKE$COKE.Close

# Log return calculation
coke_return = diff (log(coke_data.xts)) 

# removing the first empty observation, received after return calculation
coke_return = coke_return [-1] 

# Get ticket of Amazon
getSymbols("AMZN", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
amzn_data.xts = AMZN$AMZN.Close

# Log return calculation
amzn_return = diff (log(amzn_data.xts)) 

# removing the first empty observation, received after return calculation
amzn_return = amzn_return [-1] 

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

BTC = read.csv("BTC-USD.csv")

# converting a data frame into xts onject (time-series object)
BTC$Date = as.Date(BTC$Date,format="%Y-%m-%d", tz = "")
BTC.xts = xts(BTC['Close'], order.by=BTC$Date)  

# Log return calculation
btc_return = diff (log(BTC.xts)) 

# removing the first empty observation, received after return calculation
btc_return = btc_return [-1] 

ETH = read.csv("ETH-USD.csv")

# converting a data frame into xts onject (time-series object)
ETH$Date = as.Date(ETH$Date,format="%Y-%m-%d", tz = "")
ETH.xts = xts(ETH['Close'], order.by=ETH$Date)  

# Log return calculation
eth_return = diff (log(ETH.xts)) 

# removing the first empty observation, received after return calculation
eth_return = eth_return [-1] 

LTC = read.csv("LTC-USD.csv")

# converting a data frame into xts onject (time-series object)
LTC$Date = as.Date(LTC$Date,format="%Y-%m-%d", tz = "")
LTC.xts = xts(LTC['Close'], order.by=LTC$Date)  

# Log return calculation
ltc_return = diff (log(LTC.xts)) 

# removing the first empty observation, received after return calculation
ltc_return = ltc_return [-1] 

TRX = read.csv("TRX-USD.csv")

# converting a data frame into xts onject (time-series object)
TRX$Date = as.Date(TRX$Date,format="%Y-%m-%d", tz = "")
TRX.xts = xts(TRX['Close'], order.by=TRX$Date)  

# Log return calculation
trx_return = diff (log(TRX.xts)) 

# removing the first empty observation, received after return calculation
trx_return = trx_return [-1] 
length(trx_return)

DOGE = read.csv("DOGE-USD.csv")

# converting a data frame into xts onject (time-series object)
DOGE$Date = as.Date(DOGE$Date,format="%Y-%m-%d", tz = "")
DOGE.xts = xts(DOGE['Close'], order.by=DOGE$Date)  

# Log return calculation
doge_return = diff (log(DOGE.xts)) 

# removing the first empty observation, received after return calculation
doge_return = doge_return [-1] 

assets_return<-cbind.data.frame(aapl_return,coke_return,amzn_return,net_return,meta_return,btc_return,eth_return,ltc_return,trx_return,doge_return)

names(assets_return)[1]="AAPL"
names(assets_return)[2]="COKE"
names(assets_return)[3]="AMZN"
names(assets_return)[4]="NFLX"
names(assets_return)[5]="META"
names(assets_return)[6]="BTC"
names(assets_return)[7]="ETH"
names(assets_return)[8]="LTC"
names(assets_return)[9]="TRX"
names(assets_return)[10]="DOGE"

View(assets_return)

#################################################################################################################################


# Example : Statistical Factor analyses

install.packages("psych")
install.packages("GPArotation")
library (psych) # the library for factor analysis
library (GPArotation) # to estimate oblimin rotation
assets = assets_return[,1:10]
describe(assets) # general description of the data 

##Assessing the Factorability of the Data
#Bartlett's Test of Sphericity
cortest.bartlett(assets)

#KMO
KMO(assets)


##Determining the Number of Factors to Extract
# scree plot
par(mfrow=c(1,1))
scree(assets)

#Parallel Analysis
fa.parallel (assets) 


# estimation factor model
factor.model <- fa(assets, nfactors = 2, fm="ols", max.iter = 100, rotate = "oblimin")
# we estimate a factor model with 3 factors, estimated by means of OLS, 100 is the number of iterations or attempts to use when identifying the "best"" solution
# rotate - we apply oblimin rotation, allowing factors to correlate.

par(mfrow=c(1,1))
# make it visual
fa.diagram(factor.model) # 
# we can see that factor 1 is the common factor for GOld and mining equities
# Factor 2 affects energy, Latam and water equities
# and Factor 3 affects India and China
# it means that the respective groups of the equities have something in common. 
summary(factor.model)

# Communality 
factor.model$communality
factor.model$uniquenesses

#Eeigenvalues
factor.model$e.values

#Percentage of Variance Accounted For
100*factor.model$e.values/length(factor.model$e.values)

print(factor.model$loadings, cutoff=0, digits=3)
print(factor.model$Structure, cutoff=0, digits=3)


############################################################################################################
## FACTOR Analysis- Macro

DJ = read.csv("DowJones.csv")
DJ$Date = as.Date(DJ$Date,format="%Y-%m-%d", tz = "")
DJ.xts = xts(DJ$Close, order.by=DJ$Date)
DJ2 = as.matrix(DJ.xts) 
DJ3 = as.data.frame(diff(log(DJ2)))  # log difference

TYX = read.csv("TYX.csv")
TYX$Date = as.Date(TYX$Date,format="%Y-%m-%d", tz = "")
TYX.xts = xts(TYX$Close, order.by=TYX$Date)
TYX2 = as.matrix(TYX.xts) 
TYX3 = as.data.frame(diff(log(TYX2)))  # log difference
length(TYX3)

names(DJ3)[1]="DJ" #we name the column of the CPI dataframe
names(TYX3)[1] = "TYX"
TYX_DJ = cbind(TYX3,DJ3)

# autoregressive model - by default selecting the complexity by AIC. Here it is AR(5)
arFit = ar(cbind(TYX3,DJ3)) 
summary(arFit)

# residuals of the AR (4) modle (the interpretations of the residuals is that they are unexpected shocks)
res = arFit$resid[5:260,]

#fit a regression Y is a set of the stock returns and X1 and X2 - residuals from the AR modles
assets2 = as.matrix(assets)
lmfit = lm(assets2[5:260,]~res[,1]+res[,2]) 
slmfit = summary(lmfit) #summary of the regression

rsq = rep(0,9) #create a variable rsq with nine 0 values

for (i in 1:9){rsq[i]= slmfit[[i]][[8]]}
beta_TYX3 = lmfit$coef[2,] 
beta_DJ3 = lmfit$coef[3,] 

par(mfrow=c(1,3)) 
barplot(rsq,horiz = T,main="R squared") 
barplot(beta_DJ3,hori=T,main="beta DJ3") 
barplot(beta_TYX3,hori=T,main="beta TYX3") 












