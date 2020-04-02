rm(list = ls())
graphics.off()

library(devtools)
library(urca)
library(vars)
library(urca)
library(tseries)
library(xtable)
library(tsDyn)
library(vars)
library(readxl)
library(dplyr)

DATA<- read_excel("C:/Users/oleksandr/Desktop/fintech/S1/financial_econometrics/lesson 1/data.xlsx")
GDP<- ts(DATA$GDP, start = c(1980, 1), freq = 4)
inv<- ts(DATA$Investment, start = c(1980, 1), freq = 4)
cons<- ts(DATA$Consumtion, start = c(1980, 1), freq = 4)

plot(cbind(GDP,inv, cons))

GDP.acf <- acf(GDP, main = "output")
GDP.acf <- acf(GDP,lag.max = NULL, type = "partial")
adf.GDP <- ur.df(GDP, type = "drift", selectlags = "AIC")
summary(adf.GDP)

inv.acf <- acf(inv, main = "output2")
inv.acf <- acf(inv,lag.max = NULL, type = "partial")
adf.inv <- ur.df(inv, type = "drift", selectlags = "AIC")
summary(adf.inv)

cons.acf <- acf(cons, main = "output3")
cons.acf <- acf(cons,lag.max = NULL, type = "partial")
adf.cons <- ur.df(cons, type = "trend", selectlags = "AIC")
summary(adf.cons)

#It can be concluded that all time series are integrated of order one
#So we'll take FD
diff(GDP, differences = 1)
D.GDP<- GDP - diff(GDP, differences = 1)
D.inv<- inv - diff(inv, differences = 1)
D.cons<- cons - diff(cons, differences = 1)
model.bv <- cbind(D.GDP, D.inv, D.cons)
colnames(model.bv) <- c("D.GDP", "D.inv", "D.cons")
info.bv <- VARselect(model.bv, lag.max = 12, type = "const")
info.bv$selection
#So we use 2 lags
bv.est <- VAR(model.bv, p = 2, type = "const", season = NULL,
              exog = NULL)
summary(bv.est)
VARselect(model.bv, lag.max = 8, type = "both")
p1ct <- VAR(model.bv, p = 2, type = "both")
p1ct
summary(p1ct, equation = "D.GDP")
bv.serial <- serial.test(bv.est, lags.pt = 12, type = "PT.asymptotic")
bv.serial
plot(bv.serial, names = "D.GDP")