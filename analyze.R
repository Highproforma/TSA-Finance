
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
library(TTR)
library(dplyr)
library(matrixStats)

top50 <- readRDS(file = "tso.decomposed.top50.rds")
tso.top50 <- readRDS(file = "tso.top50.rds")

rows = function(tab) lapply(
  seq_len(nrow(tab)),
  function(i) unclass(tab[i,,drop=F])
)

## - - - - - - - - - - - - - - - - - - - FIND CORRELATING CURRENCIY TRENDS

# cartesian product
cart.prod <- expand.grid(top50$keys(),top50$keys())
cor_limit <- 0.7 # Faustregel

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

## - - - - - - - - - - - - - - - - - - - FIND SIMILAR BEHAVIOR ACCROSS CURRENCIES
get_bigger_date_vector <- function(date1, date2) {
  if(date1[1] > date2[1]){
    return(date1)
  }else if(date1[1] < date2[1]){
    return(date2)
  }else{
    if(date1[2] > date2[2]){
      return(date1)
    }else if(date1[2] < date2[2]){
      return(date2)
    }else{
      return(date1)
    }
  }
}

get_smaller_date_vector <- function(date1, date2){
  result <- get_bigger_date_vector(date1, date2)
  if (result == date1){
    return(date2)
  }else{
    return(date1)
  }
}

max_start_date <- c(1970, 1)
min_end_date <- c(2999, 300)
# finding latest start date and earliest finish date
for (currency in tso.top50$keys()) {
  currency.ts <- tso.top50$get(currency)
  max_start_date <- get_bigger_date_vector(start(currency.ts), max_start_date)
  min_end_date <- get_smaller_date_vector(end(currency.ts), min_end_date)
}

currency.tsos <- Dict()
length.tso <- 0
# windowing the timeseries -> all should have the same dimensions
for (currency in tso.top50$keys()) {
  currency.ts <- tso.top50$get(currency)
  sub.ts <- window(currency.ts, start=max_start_date, end=min_end_date)
  length.tso <- length(sub.ts)
  currency.tsos$set(currency, sub.ts)
}

currency.df <- data.frame(matrix(ncol = length(currency.tsos$keys()), nrow = length.tso))
colnames(currency.df) <- currency.tsos$keys()
# Build seasonality DF
for (currency in currency.tsos$keys()){
  decomposed <- decompose(currency.tsos$get(currency))
  currency.df[currency] <- as.numeric(decomposed$seasonal)
}

row.names(currency.df) <- seq(from = as.Date(toString(max_start_date), '%Y, %j'), by = "day", length.out = length.tso)


# calculate row stats
row.mean <- rowMeans(currency.df)
row.sd <- rowSds(currency.df)
currency.df <- mutate(currency.df, row.mean = row.mean, row.sd = row.sd)

# generate window
window.size <- 20
min.sd <- min(currency.df$row.sd)
currency.df <- mutate(currency.df, in.window = seq(from=FALSE, by=FALSE,length.out =  length.tso))

currency.df$in.window <- apply(currency.df,1, function(row) { row[['row.sd']] <= min.sd + window.size } )

# plot
ccy <- 'BTC'
rn <- row.names(currency.df)
plot.data.out.window <- data.frame(matrix(ncol = 1, nrow = length.tso))
plot.data.in.window <- data.frame(matrix(ncol = 1, nrow = length.tso))
row.names(plot.data.out.window) <- rn
row.names(plot.data.in.window) <- rn
colnames(plot.data.out.window) <- c(ccy)
colnames(plot.data.in.window) <- c(ccy)

currency.df.bkp <- currency.df
currency.df[currency.df$in.window == TRUE,][[ccy]] <- NA 
plot.data.out.window[[ccy]] <- currency.df[[ccy]]
currency.df <- currency.df.bkp
currency.df[currency.df$in.window == FALSE,][[ccy]] <- NA 
plot.data.in.window[[ccy]] <- currency.df[[ccy]]
currency.df <- currency.df.bkp


plot(plot.data.out.window[[ccy]],
     type='l',
     main=ccy,
     col='orange',
     ylab = 'Seasonality',
     xaxt = 'n', xlab='Day'
     )
axis(1, at=seq(from = 0, to = length.tso, by = 180), labels=rn[seq(1,length(rn), 180)])
abline(v=seq(from = 0, to = length.tso, by = 180))
lines(plot.data.in.window[[ccy]], col='purple')


