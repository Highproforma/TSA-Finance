
osname = Sys.info()['sysname'] #Get OS name for working directory setting

if (grepl('Windows', osname)){
  Sys.setlocale("LC_TIME", "C") #FREAKING LOCALE ISSUES WITH DATETIME
  setwd("G:/Dev/DataScience/TSA-Finance/data") #Pascal
} else {
  setwd("~/TSA in Finance/Project/git/data") #Nic
}

library(fBasics)
library(collections) # install.packages("collections")

tso <- Dict()

skip_count <- 0
# Load
for (file in list.files(path='.', pattern='*_5y.csv')) {
  
  # READ CSV
  input <- read.csv2(file, sep=';', header = TRUE)
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

# Decompose all
tso.decomposed <- Dict()
for (key in tso$keys()){
  decomposed.ts <- decompose((tso$get(key))) 
  tso.decomposed$set(key, decomposed.ts)
}
# Store results
saveRDS(tso.decomposed, file = "tso.decomposed.rds")

# Decompose Top 50
tso.decomposed.top50 <- Dict()
coins <- read.csv2('coins.csv', sep=';', header = TRUE)
for (key in coins[0:50,3]){
  decomposed.ts <- decompose((tso$get(key))) 
  tso.decomposed.top50$set(key, decomposed.ts)
}
# Store results
saveRDS(tso.decomposed.top50, file = "tso.decomposed.top50.rds")



# TSO Top 50
tso.top50 <- Dict()
coins <- read.csv2('coins.csv', sep=';', header = TRUE)
for (key in coins[0:50,3]){
  tsobj <- tso$get(key)
  tso.top50$set(key, tsobj)
}
# Store results
saveRDS(tso.top50, file = "tso.top50.rds")



