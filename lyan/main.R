library(ggplot2)
library(forecast)

setwd('/home/lyan/Documents/kasp/kasp/')

auto_arima_predictor <- function(series){
  s_len <- length(series)
  start <- 0.1 * s_len
  result <- 1:(s_len-1-start)
  
  opt_model <- auto.arima(series[1:s_len])
  mdl_order <- arimaorder(opt_model)
  
  for(i in (start:s_len-1)){
    mdl <- arima(x=series[1:i])
    f <- forecast(mdl, h=1)
    result[i] <- abs(f$mean[1] - series[i+1])
  }
  return(result)
}

train <- read.csv('train.csv')

train <- train[sample(nrow(train), 10000), ]

ggplot(train, aes(x=Time, y=tag00)) + geom_line()
ggplot(train, aes(x=Time, y=tag01)) + geom_line()
ggplot(train, aes(x=Time, y=tag05)) + geom_line()
ggplot(train, aes(x=Time, y=tag07)) + geom_line()
ggplot(train, aes(x=Time, y=tag12)) + geom_line()
ggplot(train, aes(x=Time, y=tag13)) + geom_line() # almost line
ggplot(train, aes(x=Time, y=tag02)) + geom_line()

ggplot(train[0:500,], aes(x=Time, y=(tag16))) + geom_line() # wtf

ggplot(train, aes(x=Time, y=tag17)) + geom_line()

ggplot(train, aes(x=Time, y=tag18)) + geom_line()

# some douplicates?

# getting auto arima params
