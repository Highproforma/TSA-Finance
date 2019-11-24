## ---------------------- Setup and Configuration
nodename = Sys.info()['nodename'] #Get OS name for dynamic working directory setting

if (grepl('SKYLLA', nodename)){
  Sys.setlocale("LC_TIME", "C") #LOCALE ISSUES WITH DATETIME ON WINDOWS
  setwd("G:/Dev/DataScience/TSA-Finance/data") #Pascal Desktop
} else if (grepl('ARES', nodename)) {
  Sys.setlocale("LC_TIME", "C") #LOCALE ISSUES WITH DATETIME ON WINDOWS
  setwd("C:/Users/Pascal/Documents/Repository/DataScience/TSA-Finance/data") #Pascal Laptop

} else {
  setwd("~/TSA in Finance/Project/git/data") #Nic
}

library(fBasics)
library(collections) # install.packages("collections")
library(forecast) # install.packages('forecast')
library(TTR) # install.packages('TTR')
library(moments) # install.packages('moments')
library(ggplot2)
library(timeSeries)
library(tseries)


# Read In of serialised objects
top50 <- readRDS(file = "tso.decomposed.top50.rds") #Top 50 decomposed time-series objects
top50.ts <- readRDS(file = "tso.top50.rds") #Top50 original time-series objects

# Selection of available keys = crypto assets
top50$keys()
top50.ts$keys()

## ---------------------- Descriptive Statistics ==> additive / multiplicative??
# ETH
plot(top50.ts$get('ETH'))         # Plot
mean(top50.ts$get('ETH'))         # Central Moment: Mean
var(top50.ts$get('ETH'))          # Central Moment: Variance
skewness(top50.ts$get('ETH'))     # Central Moment: Skewness
kurtosis(top50.ts$get('ETH'))     # Central Moment: Kurtosis

# BTC
plot(top50.ts$get('BTC'))         # Plot
mean(top50.ts$get('BTC'))         # Central Moment: Mean
var(top50.ts$get('BTC'))          # Central Moment: Variance
skewness(top50.ts$get('BTC'))     # Central Moment: Skewness
kurtosis(top50.ts$get('BTC'))     # Central Moment: Kurtosis


## ---------------------- Testing for Stationarity using (augmented) Dickey-Fuller
# 2 examples
adf.test(top50.ts$get('ETH')) # p-Value = 0.02107 < 0.05 therefor H0 is rejected and alterantive accepted (stationary)
adf.test(top50.ts$get('BTC')) # p-Value = 0.01 < 0.05 therefor H0 is rejected and alterantive accepted (stationary)

# Calculation p-Value of ADF for each top 50 currency using alpha = 95% (confidence interval)
adf.results <- list()
for (crypto in top50.ts$keys()){

  print(crypto)
  adf.results[[crypto]] <- adf.test(top50.ts$get(crypto))$p.value

}
# Non-stationary TS
top50.ts.nonst <- adf.results[adf.results >= 0.05]  # 40 crypto asset time-series are non-stationary
# Stationary TS
top50.ts.st <- adf.results[adf.results < 0.05]  # 10 crypto asset time-series are stationary

length(top50.ts.nonst)
length(top50.ts.st)

start(top50$get('ETH'))
end(top50$get('ETH'))

#Box-Cox Transformation --> advanced transformation to take care of heteroskedacity
l <- BoxCox.lambda(ts(top50$get('ETH')$x))
autoplot(BoxCox(top50$get('ETH')$x, lambda = l))
autoplot(ma(top50$get('ETH')$x, order = 5))
plot(SMA(top50$get('ETH')$x, n = 5))


#forecast of seasonality using arima
plot(stlf(log(top50$get('ETH')$x), method='arima'))
