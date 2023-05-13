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

# Get ticket of COKE
getSymbols("COKE", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
coke_data.xts = COKE$COKE.Close

# Get ticket of Amazon
getSymbols("AMZN", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

#plotting the series
amzn_data.xts = AMZN$AMZN.Close

# Get ticket of netflix
getSymbols("NFLX", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

net_data.xts = NFLX$NFLX.Close

# Get ticket of meta
getSymbols("META", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) 

meta_data.xts = META$META.Close

assets_xts<-cbind.data.frame(aapl_data.xts,coke_data.xts,amzn_data.xts,net_data.xts,meta_data.xts)

View(assets_xts)

###################################################################################################################3

library (xts)
library (zoo)
library (PerformanceAnalytics)
require (sandwich)
require(lmtest)


## load the data

close.prices.zoo  <- as.zoo(assets_xts) # convert the time series object into zoo object

############################################################################################################
## the model
############################################################################################################

## calculate the return

return = Return.calculate( assets_xts , method = "log") # automatically calculate return
library(pastecs)
descriptive.stat.return = stat.desc(return) # descriptive statistics

# a function to create CSAD and Rm
exchange.herd = function(return) 
{
  n=ncol(return)
  Rm = rowMeans(return)
  temp_dif =abs(return-Rm)
  temp_sum = rowSums(temp_dif)
  CSAD = temp_sum / ncol(return)
  CSAD = cbind (CSAD, Rm)
  return (CSAD)
}


f = exchange.herd(return) # calling the function "exchange.herd" that calculates CSAD and Rm
head (f) # show the first 6 rows
View(CSAD.df)
CSAD.df = fortify.zoo(f) # converting f into a dataframe (to simplify further calculations)
CSAD.df$Rm2 = CSAD.df$Rm^2 # calculating Rm^2
CSAD.df = CSAD.df[-c(1),] # removing the first row with NAs
head (CSAD.df) # show the first 6 rows
tail (CSAD.df) # show the last 6 rows


# reassign my columns as Y and Xs to look better in the regression model
y = CSAD.df$CSAD  # reassign my columns as Y and Xs to look better in the regression model
x1 = abs (CSAD.df$Rm)
x2 = CSAD.df$Rm2


#Linear model
linearMod <- lm(y~x1+x2)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

## For curious students advanced estimatimation 
#Newey-West Heteroscedasticity and Autocorrelation consistent (HAC) estimators
coeftest(linearMod,vcov=NeweyWest(linearMod,verbose=T))


## For curious students - if you wish you can take a look at the Time varying regression models 
## the package tvReg

# Bayesian models
install.packages("brms")
library (brms)
hourly = cbind(y, x1, x2)
model = brm(formula = y ~ x1+x2, 
            data    = weekly,
            seed    = 123)
summary(model)


# Markow regime-switching model
install.packages(MSwM)
library (MSwM)

nstates <- 2 # a number of states
msEuro = msmFit(linearMod, k = nstates, sw = rep(TRUE, 4)) # estimation; linearMod is an object from a linear estimation
summary(msEuro) #show the 
plotProb(msEuro ,which=1)


#Quantile regression
library (quantreg)
taus<-seq(from = .1, to = .9, by = .1) 
coef0 <- rq( y ~ x1+x2, tau=taus)
summary (coef0)



























