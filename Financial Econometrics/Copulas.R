rm(list = ls())
library(BatchGetSymbols)
library(tidyquant)
library(data.table)
library(fGarch)
library(xts)
library(timetk)
library(copula)
library(VineCopula)
my_seed <- 42
#For Facebook and Google stocks 
tickers <- c("FB", "GOOG")
first.date <- "2012-06-01"
last.date <- "2019-09-30"
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         type.return = "log",
                         freq.data = "daily",
                         bench.ticker = "^GSPC",
                         do.cache = FALSE)
raw_data <- data.table(l.out$df.tickers)
mydata <- reshape.wide(raw_data)
mydata <- mydata$ret.adjusted.prices
mydata <- na.omit(mydata, cols = tickers)
mydata <- mydata[FB != 0 & GOOG != 0]                  
mydata[, facebook_100 := 100 * FB ]
mydata[, google_100 := 100 * GOOG]


pseudo_observations <- pobs(as.matrix(mydata[, c("facebook_100", "google_100")]))
copula <- BiCopSelect(pseudo_observations[, 1], pseudo_observations[, 2],
                      familyset = NA, selectioncrit = "AIC")
copula
# Best fit: t copula with rho = 0.58, df = 4.04
rho <- copula$par
df <- copula$par2
rho
df

# Compare to simulated t copula along the diagonal:
set.seed(my_seed)
n <- 10000 
diag_low <- -4
diag_high <- 4
step_size <- 0.1

t_mvd <- mvdc(
  copula = ellipCopula(family = "t", dim = 2, dispstr = "ex", param = rho, df = df),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)
copula_sim <- rMvdc(mvdc = t_mvd, n)

#For Bitcoin and USD pair, merge with previous, estimate copulas 
getSymbols('BTCUSD=X', auto.assign=T, warning=F, src='yahoo')
newdata <- 
  `BTCUSD=X` %>%
  tk_tbl(preserve_index = T, rename_index = 'Date') %>%
  select(Date, Price = contains("Adjusted")) %>%
  mutate(Returns = log(Price/lag.xts(Price))) %>%
  drop_na()
newdata <- newdata[(newdata['Date'] > '2012-06-01') & (newdata['Date'] < '2019-09-30'), ]
full <- merge(newdata, mydata, by.x="Date", by.y="ref.date")
pseudo_observations <- pobs(as.matrix(full[, c("facebook_100", "google_100", "Returns")]))
copula1 <- BiCopSelect(pseudo_observations[, 1], pseudo_observations[, 3],
                      familyset = NA, selectioncrit = "AIC")
copula2 <- BiCopSelect(pseudo_observations[, 2], pseudo_observations[, 3],
                     familyset = NA, selectioncrit = "AIC")
copula1
copula2

#Add S&P500
getSymbols('^GSPC', auto.assign=T, warning=F, src='yahoo')

newdata2 <- 
  GSPC %>%
  tk_tbl(preserve_index = T, rename_index = 'Date') %>%
  select(Date, Price = contains("Adjusted")) %>%
  mutate(Returns = log(Price/lag.xts(Price))) %>%
  drop_na()

newdata2 <- newdata2[(newdata2['Date'] > '2012-06-01') & (newdata2['Date'] < '2019-09-30'), ]
full2 <- merge(newdata2, full, by.x="Date", by.y="Date")
colnames(full2)
names(full2)[3] <- "SP500"
names(full2)[5] <- "BTCUSD"

pseudo_observations <- pobs(as.matrix(full2[, c("facebook_100", "google_100", "SP500", "BTCUSD")]))
copula3 <- BiCopSelect(pseudo_observations[, 3], pseudo_observations[, 4],
                       familyset = NA, selectioncrit = "AIC")
copula3
