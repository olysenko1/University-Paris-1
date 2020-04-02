library(forecast)
library(vars)
library(urca)
library(xtable)
library(tsDyn)
library(FitARMA)
library(lubridate)
library(ggplot2)
library(zoo)


#------------------------------------------------------------------------------------------------------------------------------
#BEGIN DATA MUNGING
#------------------------------------------------------------------------------------------------------------------------------

ir <-  read.csv("C:/Users/oleksandr/Desktop/fintech/S1/financial_econometrics/lesson 3/FEDFUNDS.csv", col.names = c('Date', 'Rate'))
sp500 <- read.csv("C:/Users/oleksandr/Desktop/fintech/S1/financial_econometrics/lesson 3/^GSPC.csv")
sp500 <- sp500[, c('Date', 'Close')]
div <- read.csv("C:/Users/oleksandr/Desktop/fintech/S1/financial_econometrics/lesson 3/sp500 div.csv", header=T, col.names = c('Date', 'Dividend'))


#The common datapoints between the 3 sets start in 1954. The dividends are reported annually, so the variables need to be converted
#into annual returns, using the forumua r = r[0]/r[-1]. GREP does a regex for all dates containing the year of interest and returns
#a vector of dates. We take out the first and the last for each year.
begin <- c()
end <- c()
for(i in seq(1954, 2019, 1)){
  x <- grep((as.character(i)), sp500[, 'Date'])
  begin <- append(begin, x[1])
  end <- append(end, x[length(x)])
}

#implements the formula r[yearly] = r[0]/r[-1]. Zoo is a time-series like object (ordered object)
yearly_sp500 <- zoo(log(sp500[begin, 2] / sp500[end, 2]))


#Same process for the interest rate. We also calculate an annual mean en passant, to compare a level result. Also because I have
#no idea what i'm doing here so I'll just try everything.

begin <- c()
end <- c()
mean_ir <- c()
for(i in seq(1954, 2019, 1)){
  x <- grep((as.character(i)), ir[, 'Date'])
  begin <- append(begin, x[1])
  end <- append(end, x[length(x)])
  mean_ir <- append(mean_ir, mean(ir[x, 'Rate']))
}

yearly_ir <- zoo(log(ir[begin, 2] / ir[end, 2]))
mean_ir <- zoo(mean_ir)


#The dividend table I downloaded has two problems. First, each entry is a string, not a number. Second, that string has a % sign
#on it. We can't directly convert '1.34%' to 1.34 with as.numeric because the non-number symbol % makes the computer shit a brick.
#So we write this little function that 1) takes a string and replaces any % with nothing, then 2) gives back the number as a number
remove_pct <- function(x){ 
  p <- gsub(pattern = '%',replacement = '', x)
  return(as.numeric(trimws(p)))
}


#To broadcast a function over a whole matrix, use "apply". It will take each value from each cell, do the function to it, then
#put it back in its place.

div ['Dividend'] <- apply(X = div['Dividend'], MARGIN = 2, FUN = remove_pct)
div <- zoo(div[1:66,2])


#------------------------------------------------------------------------------------------------------------------------------
#QUESTION 1: CHECK FOR INTEGRATION 
#------------------------------------------------------------------------------------------------------------------------------

#We now check for integration. The autocorrelogram for returns shows no autocorrelaton, so the series is I(0).
acf(yearly_sp500)

#We can also test for the present of constant or drift using an ADF procedure. 
sp500_test_const_drift <- ur.df(yearly_sp500, type='trend', selectlags = 'AIC')
summary(sp500_test_const_drift)

#Trend and constant are insignificant, so the constant-only model is implemented. Intercept is found to be significant, so there is 
#a constant term. ADF results support the interpertation of the autocorrelogram, I(0) process.
sp500_test_const <- ur.df(yearly_sp500, type='drift', selectlags = 'AIC')
summary(sp500_test_const)

#Same procedure on the interest rate. Lag4 is outside the CI, but this is likely noise.
acf(yearly_ir)

#Trend and constant ADF specification find trend not significant, proceed to interpect only
ir_test_const_drift <- ur.df(yearly_ir, type='trend', selectlags = 'AIC')
summary(ir_test_const_drift)

#Intercept found not significant, proceed to final specification.
ir_test_const <- ur.df(yearly_ir, type='drift', selectlags = 'AIC')
summary(ir_test_const)

#Test shows I(0) process, confirming interpertation of the autocorrelogram.
ir_test <- ur.df(yearly_ir, type='none', selectlags = 'AIC')
summary(ir_test)

#Recall that the series yearly_ir is basically a difference (i took the log of firstday/lastday), so we check the level mean as 
#well and find an AR process. Diffing once removes the autocorrelation, so we conclude it's AR(1)
acf(mean_ir)
acf(diff(mean_ir, 1))

dmean_ir <- diff(mean_ir, 1)

#The 3-step ADF process on dmean_ir reveals no trend or constant term in the specification.
dmean_ir_test_const_trend <- ur.df(mean, type='trend', selectlags = 'AIC')
dmean_ir_test_const <- ur.df(dmean_ir, type='drift', selectlags = 'AIC')
dmean_ir_test_none <- ur.df(dmean_ir, type='none', selectlags = 'AIC')
summary(dmean_ir_test_const_trend)
summary(dmean_ir_test_const)
summary(dmean_ir_test_none)


#Same procedure on the dividend. Strong and persistant autocorrelation on the dividend suggests an AR process.
acf(div)

#First difference shows less autocorrelation (lags 2 and 3 outside CI, but probably noise).
acf(diff(div, 1))
ddiv <- diff(div,1)

#aDF test procedure shows no drift or trend in the model specification. 
ddiv_test_const_trend <- ur.df(ddiv, type='trend', selectlags = 'AIC')
ddiv_test_const <-  ur.df(ddiv, type='drift', selectlags = 'AIC')
ddiv_test_none <- ur.df(ddiv, type='none', selectlags = 'AIC')
summary(ddiv_test_const_trend)
summary(ddiv_test_const)
summary(ddiv_test_none)

#Summary of results:
#Annualized Rate of Return: I(0) with Constant
#Annualized Rate of Change in Interest Rate: I(0) 
#Mean Annual Interest Rate: I(1)
#Annual Dividends: I(1)

#------------------------------------------------------------------------------------------------------------------------------
#QUESTION 2: CHECK FOR COINGTEGRATION
#------------------------------------------------------------------------------------------------------------------------------

df <- cbind.zoo(yearly_sp500, div, mean_ir)

cotest <- ca.jo(df, type='eigen', ecdet='const', K=2)
summary(cotest)

#Johansen test is positive for one cointegrating vector. 

#------------------------------------------------------------------------------------------------------------------------------
#QUESTION 3: VAR OR VECM?
#------------------------------------------------------------------------------------------------------------------------------
#A VECM model is chosen in this case, because 1) a cointegrating factor is found, and 2) it allows us to keep variables in more
#easily interperted level values.

vecm_model <- VECM(df, 2, include='none', estim='ML')
summary(vecm_model)

#Results are all garbage. Data quality issue???? Compare to VAR results (all variables must be I(0), so differences are used)

df2 <- cbind(yearly_sp500, dmean_ir, ddiv)
df2 <- df[-1,]
var_model <- VAR(df2, p=1)
summary(var_model)

#------------------------------------------------------------------------------------------------------------------------------
#QUESTION 4: CHECK THE MEAN-REVERTING BEHAVIOR
#------------------------------------------------------------------------------------------------------------------------------
#The following graph plots the residuals of the VECM estimation over time. We see that the residuals vascillate randomly around 0,
#indicating a reversion to the mean.

resid <- vecm_model$residuals

ggplot(data = as.data.frame(resid), aes(seq(1,63, 1), yearly_sp500))+
  geom_line(color='red') +
  geom_segment(aes(x=0, y=mean(yearly_sp500) - sd(yearly_sp500), xend=63, yend=mean(yearly_sp500) - sd(yearly_sp500)), color='black',
               alpha=0.5, linetype='dashed') +
  geom_segment(aes(x=0, y=mean(yearly_sp500 + sd(yearly_sp500)), xend=63, yend=mean(yearly_sp500 + sd(yearly_sp500))), color='black',
               alpha=0.5, linetype='dashed')


#------------------------------------------------------------------------------------------------------------------------------
#QUESTION 5: PREDICT TWO PERIODS AHEAD
#------------------------------------------------------------------------------------------------------------------------------

vecm_predict <- predict(vecm_model, n.ahead = 2)
print(vecm_predict)


