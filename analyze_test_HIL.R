
nodename = Sys.info()['nodename'] #Get OS name for working directory setting
Sys.info()
if (grepl('SKYLLA', nodename)){
  Sys.setlocale("LC_TIME", "C") #FREAKING LOCALE ISSUES WITH DATETIME
  setwd("G:/Dev/DataScience/TSA-Finance (GitHub Desktop)/data") #Pascal Desktop
} else if (grepl('ARES', nodename)) {
  Sys.setlocale("LC_TIME", "C") #FREAKING LOCALE ISSUES WITH DATETIME
  setwd("C:/Users/Pascal/Documents/Repository/DataScience/TSA-Finance/data") #Pascal Laptop

} else {
  setwd("~/TSA in Finance/Project/git/data") #Nic
}

library(fBasics)
library(collections) # install.packages("collections")
library(forecast) # install.packages('forecast')
library(TTR) # install.packages('TTR')
library(moments) # install.packages('moments')


top50 <- readRDS(file = "tso.decomposed.top50.rds")


top50$keys()

#deskriptive statistik
mean(top50$get('ETH')$x)
var(top50$get('ETH')$x)
skewness(top50$get('ETH')$x)
kurtosis(top50$get('ETH')$x)


plot(top50$get('ETH'))
start(top50$get('ETH'))
end(top50$get('ETH'))
autoplot(top50$get('ETH')$x)
#Box-Cox Transformation --> advanced transformation to take care of heteroskedacity
l <- BoxCox.lambda(ts(top50$get('ETH')$x))
autoplot(BoxCox(top50$get('ETH')$x, lambda = l))
autoplot(ma(top50$get('ETH')$x, order = 5))
plot(SMA(top50$get('ETH')$x, n = 5))


#forecast of seasonality using arima
plot(stlf(log(top50$get('ETH')$x), method='arima'))
?subset
?cbind