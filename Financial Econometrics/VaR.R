library(readxl)
library(tidyverse)
library(PerformanceAnalytics)
library(zoo)
#Import the data keep only adj. close value, merge
df <- read_excel("C:/Users/oleksandr/Desktop/fintech/S1/financial_econometrics/lesson 1/data.xlsx")
drops <- c("Close", "Open", "High", "Low", 'Volume')
df = df[ , !(names(df) %in% drops)]
weights <- c(0.35, 0.2, 0.25, 0.3)

#Now I want to create different dataframes for years 2009-2014, 2009-2015, 2009-2016, 2009-2017, 2009-2018,2009-2019
#In order to calculate VaR and CVaR for each of them

#VaR and CVar for period 2009-2013
df2014 = df %>% filter(Date <= "2014-01-01")
drops1 <- c("Date")
df2014 = df2014[ , !(names(df2014) %in% drops1)]
df2014=data.frame(lapply(df2014,function(x)as.numeric(as.character(x))))
df2014 = diff(as.matrix(df2014),differences = 1)
values = seq(from = as.Date("2009-12-01"), to = as.Date("2013-12-01"), by = 'month')
df2014 <- zoo(df2014, as.Date(as.character(values), format = c("%Y-%m-%d")))
VaR(df2014, p=0.99, weights = weights, portfolio_method="component", method="modified")
CVaR(df2014, p=0.99, weights = weights, portfolio_method="component", method="modified")

#For period from 2009 to 2014 Netflix stock is the most dangerous, Apple - 2nd place, British
American Tobacco - 3rd Nvidia - the least dangerous
#--------------------------------------------------------------------------------------------------
#VaR and CVar for period 2009-2014
df2015 = df %>% filter(Date <= "2015-01-01")
df2015 = df2015[ , !(names(df2015) %in% drops1)]
df2015=data.frame(lapply(df2015,function(x)as.numeric(as.character(x))))
df2015 = diff(as.matrix(df2015),differences = 1)
values = seq(from = as.Date("2009-12-01"), to = as.Date("2014-12-01"), by = 'month')
df2015 <- zoo(df2015, as.Date(as.character(values), format = c("%Y-%m-%d")))
VaR(df2015, p=0.99, weights = weights, portfolio_method="component", method="modified")
CVaR(df2015, p=0.99, weights = weights, portfolio_method="component", method="modified")
#No changes for period 2009-2015
#--------------------------------------------------------------------------------------------------
#VaR and CVar for period 2009-2015
df2016 = df %>% filter(Date <= "2016-01-01")
df2016 = df2016[ , !(names(df2016) %in% drops1)]
df2016=data.frame(lapply(df2016,function(x)as.numeric(as.character(x))))
df2016 = diff(as.matrix(df2016),differences = 1)
values = seq(from = as.Date("2009-12-01"), to = as.Date("2015-12-01"), by = 'month')
df2016 <- zoo(df2016, as.Date(as.character(values), format = c("%Y-%m-%d")))
VaR(df2016, p=0.99, weights = weights, portfolio_method="component", method="modified")
CVaR(df2016, p=0.99, weights = weights, portfolio_method="component", method="modified")
#Same situation in volatility
#--------------------------------------------------------------------------------------------------
#VaR and CVar for period 2009-2016
df2017 = df %>% filter(Date <= "2017-01-01")
df2017 = df2017[ , !(names(df2017) %in% drops1)]
df2017=data.frame(lapply(df2017,function(x)as.numeric(as.character(x))))
df2017 = diff(as.matrix(df2017),differences = 1)
values = seq(from = as.Date("2009-12-01"), to = as.Date("2016-12-01"), by = 'month')
df2017 <- zoo(df2017, as.Date(as.character(values), format = c("%Y-%m-%d")))
VaR(df2017, p=0.99, weights = weights, portfolio_method="component", method="modified")
CVaR(df2017, p=0.99, weights = weights, portfolio_method="component", method="modified")
#Netflix continues to be the riskiest, apple is the 2nd, though now British American Tobacco is the least risky
#--------------------------------------------------------------------------------------------------
#VaR and CVar for the whole period
df = df[ , !(names(df) %in% drops1)]
df=data.frame(lapply(df,function(x)as.numeric(as.character(x))))
df = diff(as.matrix(df),differences = 1)
values = seq(from = as.Date("2009-12-01"), to = as.Date("2019-10-01"), by = 'month')
df<- zoo(df, as.Date(as.character(values), format = c("%Y-%m-%d")))
VaR(df, p=0.99, weights = weights, portfolio_method="component", method="modified")
CVaR(df2017, p=0.99, weights = weights, portfolio_method="component", method="modified")
#For the whole period: Netflix stocks are the riskiest, Nvidia is 2nd (volatility has significantly increased in last 2 years) might be linked with the mining obsession, Apple is on the 3rd place and British American Tobacco - are the least risky
