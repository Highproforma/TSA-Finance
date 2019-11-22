
osname = Sys.info()['sysname'] #Get OS name for working directory setting

if (grepl('Windows', osname)){
  Sys.setlocale("LC_TIME", "C") #FREAKING LOCALE ISSUES WITH DATETIME
  setwd("G:/Dev/DataScience/TSA-Finance (GitHub Desktop)/data") #Pascal
} else {
  setwd("~/TSA in Finance/Project/git/data") #Nic
}

library(fBasics)
library(collections) # install.packages("collections")

top50 <- readRDS(file = "tso.decomposed.top50.rds")


top50$keys()

plot(top50$get('XRP'))
