rm(list = ls())
library(rugarch)
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forecast)
library(urca)
library(xtable)
library(readxl)

#load the CAC 40 data downloaded from Yahoo
CAC <- read_excel("C:/Users/oleksandr/Desktop/fintech/S1/financial_econometrics/lesson 1/CAC.xlsx")
View(CAC)
#we will work with adjusted closed value
CACts <- zoo(CAC$Adj.Close, as.Date(as.character(CAC$Date), format = c("%Y-%m-
%d")))
basicStats(CACts)
plot(CACts, type='l', ylab = " adj close price", main="Plot of 2006-2019 weekly CAC 40
prices", col = 'blue')
#Graph says us that it's not stationary
acf(coredata(CACts), main="ACF plot of the 2006-2019 weekly CAC40 prices")
#super slow decay to 0 from acf plot -
pacf(coredata(CACts), main="ACF plot of the 2006-2019 weekly CAC40 prices")
#Calculated rate of return (we use log, since the ts isn't stationary)
CAC_rets <- log(CACts/lag(CACts,-1))
#strip-off the data, turn into numerical object
CAC_ret_num <- coredata(CAC_rets)
basicStats(CAC_rets) #mean centered around 0
#Check for stationarity
adf.CAC_rets_num <- ur.df(CAC_rets, type = "trend", selectlags = "AIC")
summary(adf.CAC_rets_num)
#We reject the null
#series follow normal distibution:
hist(CAC_rets, xlab="Weekly return of stock prices", prob=TRUE, main="Histogram for
weekly return of stock prices")
xfit<-seq(min(CAC_rets),max(CAC_rets),length=40)
yfit<-dnorm(xfit,mean=mean(CAC_rets),sd=sd(CAC_rets))
lines(xfit, yfit, col="blue", lwd=2)
plot(CAC_rets, type='l', ylab = "stock price return", main="Plot of 2006-2019 weekly CAC40
stock price return")
#several huge spikes during 2008, 2012 crises
plot(CAC_rets^2, type='l', ylab = "stock price return", main="Plot of 2006-2019 weekly
CAC40 stock price return")
par(mfrow=c(2,1))
acf(CAC_ret_num)
acf(CAC_ret_num^2)
#Conclusions: mean is constant and nearly 0. From the ACF plot squared price return values have high correlation, and log price returns are also correlatted - we probably have weak linear and strong non-linear dependence.
#We perform the Ljung Boxâ??? Ts test to test the independence of the stock return prices
Box.test(CAC_ret_num, lag=2, type="Ljung")
Box.test(CAC_ret_num, lag=4, type="Ljung")
Box.test(CAC_ret_num, lag=6, type="Ljung")
#From all the above Ljung Box Tests, we observe that the log returns are not correlated as
the p-values>>0.05 and hence we fail to reject the null
#Determine the order of the model
pacf(CAC_ret_num, lag=10, main="PACF plot of the log return of the stock prices")
pacf(CAC_ret_num^2, lag=10, main="PACF plot of the squared log return of the stock
prices")



#Model 1: AR(0)-GARCH(1,1) with normally distributed errors
garch11.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(0,0)))
garch11.fit=ugarchfit(spec=garch11.spec, data=CAC_rets)
garch11.fit
#Fitted model: r_t = 0.0011 + a_t; a_t= sigma_te_t; sigma2_t=0.00+0.11a2_(t1)+0.855sigma^2_(t-1) AIC value = -4.3995, BIC value = -4.3738. Residuals have p-value
more than 0,05% - we fail to reject the null of no serial-correlation. Hence we conclude that
residuals behave as a white noise
#Test for ARCH behaviour - the p-values>0.05 and we fail to reject the null hypothesis hence
there is no evidence of serial correlation in squared residuals.
#Looking at the Goodness-of-fit test, we observe that for group 20 and 30, the p-value<0.01;
for group 50 the p-value<0.05 and for group 40 the p-value<0.1
#Hence we conclude that our model is not adequate for this process
coef(garch11.fit) #GARCH coefficients
uncmean(garch11.fit) #unconditional mean
uncvariance(garch11.fit)
persistence(garch11.fit)
plot(garch11.fit, which = "all")
#conditional volatility plot
plot.ts(sigma(garch11.fit), ylab="sigma(t)", col="red")



#Model 2 ARMA(0,0)-GARCH(1,1) model with skewed t-distribution
garch11.skt.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)),
                            mean.model=list(armaOrder=c(0,0)), distribution.model = "sstd")
garch11.skt.fit=ugarchfit(spec=garch11.skt.spec, data=CAC_rets)
garch11.skt.fit
#Fitted model: rt=0.0007 + at, at=stet s2t = 0.000002 + 0.095a2t-1 + 0.8675s2t-1


#Looking at the output for the goodness of fit test, since the p-values>0.05, the null
hypothesis canâ??? Tt be rejected and hence this model is a good fit.
#no evidence of serial correlation in squared residuals or autocorrelation in the residuals
coef(garch11.fit) #ARCH coefficients
plot(garch11.skt.fit, which = "all")
plot.ts(sigma(garch11.fit), ylab="sigma(t)", col="pink")


#Forecast
f=ugarchforecast(garch11.skt.fit, n.ahead=20)
f


library(tseriesChaos)
library(tsDyn)
mod.ar <- linear(CAC_ret_num, m=2)
mod.ar
mod.setar <- setar(CAC_ret_num, m=2, mL=2, mH=2, thDelay=1)
mod.setar
mod <- list()
mod[["linear"]] <- linear(CAC_ret_num,, m=2)
mod[["setar"]] <- setar(CAC_ret_num,, m=2, thDelay=1)
mod[["lstar"]] <- lstar(CAC_ret_num,, m=2, thDelay=1)
#Now the mod object contains a labelled list of fitted nlar models.
sapply(mod, AIC)
sapply(mod, MAPE)
#So from this comparison LSTR model seems to be the best
summary(mod[["lstar"]])
plot(mod[["lstar"]])