rm(list = ls())
library(BatchGetSymbols)
library(tidyquant)
library(data.table)
library(xts)
library(tidyverse)
library(tibbletime)
my_seed <- 36
tickers <- c("FB", "GOOG", "NFLX", "AMZN", "AAPL")
first.date <- "2012-06-01"
last.date <- "2019-07-31"
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
mydata <- 
  mydata%>% column_to_rownames(., var = "ref.date")

mydata <- na.omit(mydata, cols = tickers)

returns = diff(as.matrix(mydata), 
               differences = 1)
monthly <- 
  returns %>%
  to.monthly(indexAt = 'lastof', OHLC=F) %>%
  tk_tbl(preserve_index = TRUE, rename_index = 'date') %>%
  gather(tickers, returns, -date) 

weights <- c(0.20, 0.25, 0.10, 0.20, 0.25)

portfolio_returns<- 
  monthly %>%
  tq_portfolio(assets_col = tickers,
               returns_col = returns,
               weights = weights,
               col_rename = "returns",
               rebalance_on = "months")

temp <- tempfile()
download.file('http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_3_Factors_CSV.zip',temp)

Global_3_Factors <- read_csv(unz(temp, "Global_3_Factors.csv"),skip = 6) %>%
  rename(date=X1) %>%
  mutate_at(vars(-date), as.numeric) %>%
  mutate(date=ymd(parse_date_time(date, "%Y%m") + months(1)))%>%
  mutate(date = lubridate::rollback(date + months(1)))%>%
  filter(date >= first(portfolio_returns$date) & date <= last(portfolio_returns$date))


Global_3_Factors <-  Global_3_Factors %>% column_to_rownames(., var = "date")


FF <- 
  portfolio_returns %>% 
  left_join(Global_3_Factors, by = "date") %>% 
  mutate(MKT_RF = Global_3_Factors$`Mkt-RF`/100,
         SMB = Global_3_Factors$SMB/100,
         HML = Global_3_Factors$HML/100,
         RF = Global_3_Factors$RF/100,
         R_excess = round(returns - RF, 4))

rolling_lm <- 
  rollify(.f = function(R_excess, MKT_RF, SMB, HML) {
    lm(R_excess ~ MKT_RF + SMB + HML)
  }, window = my_seed, unlist = FALSE)

rolling_ff_betas <-
  FF %>% 
  mutate(rolling_ff = 
           rolling_lm(R_excess, 
                      MKT_RF, 
                      SMB, 
                      HML)) %>%  
  slice(-1:-35) %>% 
  select(date, rolling_ff)

head(rolling_ff_betas, 3)     

rolling_ff_betas <-
 FF %>% 
  mutate(rolling_ff = 
           rolling_lm(R_excess, 
                      MKT_RF, 
                      SMB, 
                      HML)) %>% 
  mutate(tidied = map(rolling_ff, 
                      tidy, 
                      conf.int = T)) %>% 
  unnest(tidied) %>% 
  slice(-1:-35) %>% 
  select(date, term, estimate, conf.low, conf.high) %>% 
  filter(term != "(Intercept)") %>% 
  rename(beta = estimate, factor = term) %>% 
  group_by(factor)

head(rolling_ff_betas, 3)

rolling_ff_betas %>% 
  ggplot(aes(x = date, 
             y = beta, 
             color = factor)) + 
  geom_line() +
  labs(title= "36-Month Rolling FF Factor Betas",
       x = "rolling betas") +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

