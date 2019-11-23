
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
library(ggfortify)

top50 <- readRDS(file = "tso.decomposed.top50.rds")

rows = function(tab) lapply(
  seq_len(nrow(tab)),
  function(i) unclass(tab[i,,drop=F])
)

## - - - - - - - - - - - - - - - - - - - FIND CORRELATING CURRENCIY TRENDS

# cartesian product
cart.prod <- expand.grid(top50$keys(),top50$keys())
cor_limit <- 0.8

for (row in rows(cart.prod)){
  print('')
  Var1 <- paste(row$Var1)
  Var2 <- paste(row$Var2)
  if (Var1 == Var2){
    next()
  }else{
    s1 <- as.numeric((top50$get(Var1))$seasonal)
    s2 <- as.numeric((top50$get(Var2))$seasonal)
    dim_min_len <- min(length(s1), length(s2))
    correlation_coefficient <- cor(s1[0:dim_min_len],s2[0:dim_min_len], method = c("pearson", "kendall", "spearman"))
    if (abs(correlation_coefficient) >= cor_limit){
      cat('Looking at combination of ',Var1, Var2)
      df <- data.frame(s1[0:dim_min_len],s2[0:dim_min_len])
      plot(df$s1, type='l', main=paste(Var1, Var2, correlation_coefficient, sep=' - '), xlab='', ylab='Value', col='blue')
      lines(df$s2, col='red')
    }else{
      cat('Skipped combination of ',Var1, Var2)
    }
  }
}
