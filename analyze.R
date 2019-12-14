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
library(ggfortify)
library(TTR)
library(dplyr)
library(matrixStats)

## ---------------------- Read-In of serialized objects
top50 <- readRDS(file = "tso.decomposed.top50.rds")   # decomposed time-series of top 50 currencies
tso.top50 <- readRDS(file = "tso.top50.rds")          # original time-series of top 50 currencies

rows = function(tab) lapply(
  seq_len(nrow(tab)),
  function(i) unclass(tab[i,,drop=F])
)

## - - - - - - - - - - - - - - - - - - - FIND CORRELATING CURRENCIY TRENDS

# cartesian product
cart.prod <- expand.grid(top50$keys(),top50$keys())
cor_limit <- 0.7 # Faustregel for statistically significant correlation

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
  decomposed <- stl(currency.tsos$get(currency), s.window='periodic', na.action = na.omit)
  seasonal.part <- as.numeric(decomposed$time.series[,'seasonal'])
  seasonal.part.max <- max(seasonal.part)
  seasonal.part.min <- min(seasonal.part)
  # normalize data to -1 and 1
  #currency.df[currency] <- 2 * ((seasonal.part - seasonal.part.min)/(seasonal.part.max - seasonal.part.min)) - 1
  # normalize data to 0 and 1
  currency.df[currency] <- (((seasonal.part - seasonal.part.min)/(seasonal.part.max - seasonal.part.min)))
}

row.names(currency.df) <- seq(from = as.Date(toString(max_start_date), '%Y, %j'), by = "day", length.out = length.tso)

# calculate row stats
currency.df <- transform(currency.df, row.sd=apply(currency.df, 1, sd, na.rm=TRUE))
currency.df <- transform(currency.df, row.mean=apply(currency.df, 1, mean, na.rm=TRUE))

plot(currency.df$row.sd, type='l')
?hist
# generate window
window.size <- .23
min.sd <- 0 #min(currency.df$row.sd)
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


par(mar=c(7,4,4,2))
day.interval <- 90
plot(plot.data.out.window[[ccy]],
     type='l',
     main=ccy,
     col='orange',
     ylab = 'Seasonality',
     xaxt = 'n', xlab=''
     )
axis(1, at=seq(from = 0, to = length.tso, by = day.interval), labels=rn[seq(1,length(rn), day.interval)], las=2)
abline(v=seq(from = 0, to = length.tso, by = day.interval))
lines(plot.data.in.window[[ccy]], col='purple')
legend('bottomright', legend=c('Common', 'Unique'), col=c('purple', 'orange'), lty=1)


# get common dates
row.names(currency.df) <- seq(from = as.Date(toString(max_start_date), '%Y, %j'), by = "day", length.out = length.tso)

common.dates <- row.names(currency.df[currency.df$in.window == TRUE,])

common.dates <- as.Date(common.dates, format="%Y-%m-%d")

# 
# hist(as.integer(format(common.dates, format = '%j')), 
#      breaks = 365,
#      main='Frequency of common days in a year',
#      xlim=c(1,365),
#      xlab='Day of the Year', xaxt='n')
# axis(side=1, at=seq(0,365, 5), labels=seq(0,365,5), las=2)


hist(as.integer(format(common.dates, format = '%j')), 
     breaks = 73,
     main='Frequency of common days in a year',
     xlim=c(1,365),
     xlab='Day of the Year', xaxt='n',
     col='gray')
axis(side=1, at=seq(0,365, 5), labels=seq(0,365,5), las=2)



# EXPERIMENTS
library(TSA)
p <- periodogram(as.numeric(tso.top50$get('BTC')))
dd <- data.frame(freq=p$freq, spec=p$spec)
order <- dd[order(-dd$spec),]
top2 <- head(order, 2)
top2
time = 1/top2$f
time
#[1] 937.5 625.0

s <- stl(tso.top50$get('BTC'), s.window='periodic', na.action = na.omit)
m <- mstl(tso.top50$get('BTC'))
d <- decompose(tso.top50$get('BTC'))

plot(m[,'Seasonal365'], type='l', ylab='Seasonality', col='darkgreen')
lines(d$seasonal, col='lightblue')
lines(s$time.series[,'seasonal'], col='darkblue')
legend('bottomright', legend=c('stl', 'mstl', 'decompose'), col=c('darkblue', 'darkgreen', 'lightblue'), lty=1)

# SEASONPLOT

library(forecast)
ggseasonplot(tso.top50$get('BTC') ,year.labels=TRUE, continuous=TRUE, main='BTC Seasonplot')
