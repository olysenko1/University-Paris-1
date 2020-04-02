rm(list = ls())

library(fOptions)
library(tidyquant)
library(tidyverse)
library(tidyselect)
library(xts)
library(dplyr)

make_data <- function(ticker, interest_rate_series, date_of_interest){
  #This is a helper function to take stock ticker data retreived via the getSymbol function in 
  #tidyquant. It requires you pass:
  
  #ticker = an xts object of stock price data
  #interest_rate_serise = an xts object of interest rates
  #date_of_interest = a single date you wish to study. 
  
  #NOTE: DATE OF INTEREST MUST BE THE FINAL DAY OF A MONTH.
  #NOTE: Stock series should be passed in monthly or daily. Daily data will be converted to monthly.
  
  #The function returns a 4 element list, containing:
  #1 - the stock volitality (standard deviation)
  #2 - the closing stock price on the date of interest
  #3 - the interest rate on the date of interest
  #4 - the date of interest
  
  #This list of 4 elements should be passed to the make_graph function to make a nice graph.
  
  df <- 
    to.monthly(ticker, indexAt = 'lastof', OHLC=F)%>%
    tk_tbl(preserve_index = TRUE, rename_index = 'date') %>%
    select(date, close = contains('Close')) %>%
    mutate(returns = log(close) - log(dplyr::lag(close))) %>%
    na.omit()
  
  sigma <- sd(df$returns)
  
  S <- df %>%
    dplyr::filter(date == date_of_interest) %>%
    select(close) %>%
    as.numeric()
  
  r <- interest_rate_series%>%
    tk_tbl(preserve_index = T, rename_index = 'date') %>%
    mutate(date = lubridate::rollback(ymd(date) + month(1))) %>%
    dplyr::filter(date == date_of_interest) %>%
    select(FEDFUNDS) %>%
    as.numeric()
  
  return(list(sigma, S, r, date_of_interest))
}

make_graph <- function(stock, params, type='call', target_variable='X', n=100){
  #This function creates a graph of fair option prices, as computed by the CRR formula, 
  #for a given stock on a given day, and allows the user to see the effects of varying the parameters
  #of the CRR formula. The function requires the following parameters be passed:
  
  #stock = a STRING of the stock-ticker being studied (for writing to the plot title)
  #params = a LIST of parameters (RETURNED FROM THE make_data FUNCTION)
  #type = the TYPE of option call of interest as a STRING, either 'put' or 'call'
  #target_variable = the PARAMETER OF INTEREST in the CRR function. Currently only the strike (X) and
  #                  the cost of carry (b) are supported.
  #n = the number of iterations used for caluculation (default 100)
  
  sigma <- as.numeric(params[1])
  S <- as.numeric(params[2])
  r <- as.numeric(params[3])
  date_of_interest <- as.character(params[4])
  subtitle <- paste('Stock: ', stock, ', Price: ', round(S, digits = 2), ', Observed: ', date_of_interest)  

  if(target_variable == 'X' & type=='call'){
    prices_a <- c()
    prices_e <- c()
    x <- S/seq(1:n)
    xlabel <- 'Strike'
    title <- 'Call Option Fair Price Sensitivity to Strike Price'
    for (i in x) {
      result_american <- CRRBinomialTreeOption('ca', S=S, X=i, n=10, r=r, b=.1, sigma=sigma, Time=0.5)
      result_european <- CRRBinomialTreeOption('ce', S=S, X=i, n=10, r=r, b=.1, sigma=sigma, Time=0.5)
      prices_a <- append(prices_a, result_american@price)
      prices_e <- append(prices_e, result_european@price)
    }
  }
    else if(target_variable == 'X' & type=='put'){
      prices_a <- c()
      prices_e <- c()
      title <- 'Put Option Fair Price Sensitivity to Strike Price'
      xlabel <- 'Strike'
      x <- S*(1+1/seq(1:n))
      for (i in x) {
        result_american <- CRRBinomialTreeOption('pa', S=S, X=i, n=10, r=r, b=.1, sigma=sigma, Time=0.5)
        result_european <- CRRBinomialTreeOption('pe', S=S, X=i, n=10, r=r, b=.1, sigma=sigma, Time=0.5)
        prices_a <- append(prices_a, result_american@price)
        prices_e <- append(prices_e, result_european@price)
      }
    }
  else if(target_variable == 'b' & type=='call'){
    prices_a <- c()
    prices_e <- c()
    title <- 'Call Option Fair Price Sensitivity to Cost-of-Carry'
    xlabel <- 'Cost-of-Carry'
    x <- seq(1:n)/100
    for (i in x) {
      result_american <- CRRBinomialTreeOption('ca', S=S, X=S-S*sigma, n=10, r=r, b=i, sigma=sigma, Time=0.5)
      result_european <- CRRBinomialTreeOption('ce', S=S, X=S-S*sigma, n=10, r=r, b=i, sigma=sigma, Time=0.5)
      prices_a <- append(prices_a, result_american@price)
      prices_e <- append(prices_e, result_european@price)
    }
  }
  
  else if(target_variable == 'b' & type=='put'){
    prices_a <- c()
    prices_e <- c()
    title <- 'Call Option Fair Price Sensitivity to Cost-of-Carry'
    xlabel <- 'Cost-of-Carry'
    x <- seq(1:n)/100
    for (i in x) {
      result_american <- CRRBinomialTreeOption('pa', S=S, X=S+S*sigma, n=10, r=r, b=i, sigma=sigma, Time=0.5)
      result_european <- CRRBinomialTreeOption('pe', S=S, X=S+S*sigma, n=10, r=r, b=i, sigma=sigma, Time=0.5)
      prices_a <- append(prices_a, result_american@price)
      prices_e <- append(prices_e, result_european@price)
    }
  }

  ggplot(data=NULL, aes(x=x)) + 
    geom_line(aes(y=prices_a, color='American Call')) + 
    geom_line(aes(y=prices_e, color='European Call')) + 
    scale_color_manual("Price Functions",
                       values=c('red', 'blue')) + 
    ggtitle(paste(title, subtitle, sep='\n')) + 
    xlab(xlabel) +
    ylab('Fair Price') +
    theme(plot.title= element_text(hjust=0.5, face = 'bold'))
}

getSymbols('BABA', src='yahoo', auto.assign = T, warnings = F)
getSymbols('FEDFUNDS', src='FRED', auto.assign =T, warnings=F)
  
params <- make_data(BABA, FEDFUNDS, '2017-10-31')
make_graph(stock='BABA', type = 'put', target_variable = 'X', n = 100, params = params)
