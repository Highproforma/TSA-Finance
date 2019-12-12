## ---------------------- Setup and Configuration
nodename = Sys.info()['nodename'] #Get OS name for dynamic working directory setting

if (grepl('SKYLLA', nodename)){
  Sys.setlocale("LC_TIME", "C") #LOCALE ISSUES WITH DATETIME ON WINDOWS
  setwd("G:/Dev/DataScience/TSA-Finance/data") #Pascal Desktop
} else if (grepl('ARES', nodename)) {
  Sys.setlocale("LC_TIME", "C") #LOCALE ISSUES WITH DATETIME ON WINDOWS
  setwd("C:/Users/Pascal/Documents/Repository/DataScience/TSA-Finance/data") #Pascal Laptop

} else {
  setwd("~/Code/TSA-Finance/data") #Nic
}

library(fBasics)
library(collections) # install.packages("collections")

tso <- Dict()

skip_count <- 0


## ---------------------- Load and Transformation to TSO
for (file in list.files(path='.', pattern='*_5y.csv')) {
  
  # READ CSV
  input <- read.csv2(file, sep=';', header = TRUE, stringsAsFactors = FALSE)
  if(dim(input) == 0){
    cat('Skipping ', file)
    skip_count <- skip_count + 1
    next()
  }
  
  # PARSE DATE
  input$timestamp <- as.Date(as.POSIXct(as.double(input$timestamp)/1000, origin="1970-01-01")) # / 1000 because we have unix timestamp in ms

  # CREATE TS
  ts.obj <- ts(
    input[,-1],
    start=c(as.numeric(strftime(min(input$timestamp), format = "%Y")), as.numeric(strftime(min(input$timestamp), format = "%j"))),
    end=c(as.numeric(strftime(max(input$timestamp), format = "%Y")), as.numeric(strftime(max(input$timestamp), format = "%j"))),
    frequency=365 # days per year 365.25
  )
  # Append to TSOs
  tso$set(substr(file, 0, regexpr('_', file)-1), ts.obj)
}
cat('Skipped ',skip_count)
# ----------------------------------------------

# USING XTS instead of TS 
# Experiment
library(xts)

dat <- read.csv2('BTC_1576167363_5y.csv', sep=';', header = TRUE, stringsAsFactors = FALSE)
dat$price <- as.numeric(dat$price)
dat$timestamp <- as.Date(as.POSIXct(as.double(dat$timestamp)/1000, origin="1970-01-01")) # / 1000 because we have unix timestamp in ms

# Convert dat into xts
btc.xts <- xts(dat[,-1], order.by = dat$timestamp)
plot.xts(btc.xts)



# ----------------------------------------------

## ---------------------- TSO Decomposition and Serialization
# Decompose all
tso.decomposed <- Dict()
for (key in tso$keys()){
  decomposed.ts <- decompose((tso$get(key))) 
  tso.decomposed$set(key, decomposed.ts)
}
# Serialization - Store all decomposed results
saveRDS(tso.decomposed, file = "tso.decomposed.rds")

# Decompose Top 50
tso.decomposed.top50 <- Dict()
coins <- read.csv2('coins.csv', sep=';', header = TRUE)
for (key in coins[0:50,3]){
  decomposed.ts <- decompose((tso$get(key))) 
  tso.decomposed.top50$set(key, decomposed.ts)
}
# Serialization - Store top 50 decomposed results
saveRDS(tso.decomposed.top50, file = "tso.decomposed.top50.rds")



# TSO Top 50
tso.top50 <- Dict()
coins <- read.csv2('coins.csv', sep=';', header = TRUE)
for (key in coins[0:50,3]){
  tsobj <- tso$get(key)
  tso.top50$set(key, tsobj)
}
# Serialization - Store original time series 
saveRDS(tso.top50, file = "tso.top50.rds")



