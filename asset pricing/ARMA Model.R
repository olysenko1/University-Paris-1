library(tseries)
library(urca)
library(dplyr)
library(FitARMA)
# Function to do lag (lag command won't work)
shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}


# Retrieve data
mydata<-read.csv("C:/Users/oleksandr/Desktop/fintech/S1/financial_econometrics/lesson 1/nasdaq.csv")
# Rename columns
names(mydata)<- c('date','nasdaq')
# Date format
mydata$date = as.Date(as.character(mydata$date), 
                              order = '%Y-%m-%d')
# Calculate returns from prices 
mydata$nasdaq<- as.numeric(as.character(mydata$nasdaq))
mydata$lagnasdaq= shift(mydata$nasdaq, shift_by = 1)
mydata$rnasdaq= (mydata$nasdaq - mydata$lagnasdaq)*100/mydata$lagnasdaq
# Calculate returns from log prices
mydata$lognasdaq= log(mydata$nasdaq)
mydata$loglagnasdaq= log(mydata$lagnasdaq)
mydata$rlognasdaq= (mydata$lognasdaq - mydata$loglagnasdaq)*100/mydata$loglagnasdaq
# the return from log prices is smaller (in absolute value) than that from prices. Applying the log reduces volatility. 
# Plotting the returns
subdata <- mydata[c(5478:8784),c(1:7)]
plot(subdata$date, subdata$rlognasdaq)
# Mean of return
mydata[complete.cases(mydata[ , 1:7]),]
returnmean <- mean(mydata$rlognasdaq, na.rm= TRUE)
# On average, the daily return of the nasdaq is -0.0068054
# The volatility of the return
returnvariance <- var(mydata$rlognasdaq, na.rm= TRUE)
returnvolatility <- sqrt(returnvariance)
returnvariancesub <- var(subdata$rlognasdaq, na.rm=TRUE)
# Volatility of the return=0.235549
# If we consider the return to be a weak white noise with a drift, then  the volatility is constant.
# autocorrelogram 
mydata <-mydata[complete.cases(mydata), ]
acf(mydata$rlognasdaq, lag.max = 5, type='correlation', plot=TRUE)
# An autocorrelation plot (autocorrelogram) is designed to show whether the elements of a time series are positively correlated, negatively correlated, or independent of each other. The blue dotted lines are the confidence interval. If the vertical line is outside the confidence interval, the time series are correlated for that lag. Conclusion: Returns of the nasdaq are not serially correlated.
# Fit an ARMA
fit= arma(mydata$rlognasdaq, order=c(1,1))
summary(fit, which="all")
# The ar1 and ma1 processes are significant. 