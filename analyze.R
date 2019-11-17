setwd('~/TSA in Finance/Project/git/data')
# https://www.kaggle.com/sudalairajkumar/cryptocurrencypricehistory
library(fBasics)
library(collections) # install.packages("collections")

tso <- Dict()

# Load
for (file in list.files(path='.', pattern='*_price.csv')) {
  # READ CSV
  input <- read.csv(file, sep=',', header = TRUE)
  
  # PARSE DATE
  input$Date <- as.Date(input$Date,format="%b %d, %Y")
  
  # CREATE TS
  ts.obj <- ts(
    input[,-1], 
    start=c(as.numeric(strftime(min(input$Date), format = "%Y")), as.numeric(strftime(min(input$Date), format = "%j"))), 
    end=c(as.numeric(strftime(max(input$Date), format = "%Y")), as.numeric(strftime(max(input$Date), format = "%j"))), 
    frequency=365 # days per year 365.25
  )
  
  # Append to TSOs
  tso$set(gsub('_price.csv', '', file), ts.obj)
}

# Decompose
decomposed.ts.btc <- decompose((tso$get('bitcoin'))[,2]) # [,2] -> High
plot(decomposed.ts.btc)

