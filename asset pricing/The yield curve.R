rm(list=ls())

library(tidyquant)
library(tidyverse)
library(YieldCurve)
library(ggplot2)
library(xts)

bonds <- c('DGS1MO', 'DGS3MO', 'DGS6MO', 'DGS1', 'DGS2', 'DGS5', 'DGS7', 'DGS10', 'DGS20', 'DGS30')
col_names <- c(1/12, 3/12, 6/12, 1, 2, 5, 7, 10, 20, 30)
bond_df <- getSymbols(bonds, src='FRED', auto.assign = TRUE, warnings = F) %>%
  map(~get(.)) %>%
  reduce(merge) %>%
  `colnames<-`(col_names) %>%
  to.monthly(indexAt = 'lastof', OHLC=FALSE) %>%
  tk_tbl(preserve_index = TRUE, rename_index = 'date') %>%
  filter(date >= ymd('2006-01-01')) %>%
  column_to_rownames(var="date") %>%
  xts(order.by = ymd(row.names(.)))

y <- Nelson.Siegel(rate = bond_df, maturity = col_names) %>%
  NSrates(col_names) %>%
  `colnames<-`(col_names) %>%
  tk_tbl(preserve_index = TRUE, rename_index = 'date') %>%
  gather(maturity, rate, -date) %>%
  mutate(maturity = as.numeric(maturity))

df_plot <-
  bond_df %>%
  tk_tbl(preserve_index = T, rename_index = 'date') %>%
  gather(maturity, rate, -date) %>%
  mutate(maturity = as.numeric(maturity))

df <- merge(df_plot, y, by=c('date', 'maturity')) %>%
  `colnames<-`(c('date', 'maturity', 'observed', 'estimated'))
  
ggplot(data=filter(df, date == ymd('2019-08-31')), aes(x=maturity), log="x") +
  geom_line(aes(y=observed, color='observed')) +
  geom_line(aes(y=estimated, color='estimated')) +
  scale_color_manual("Yield Curve",
                     values=c('red', 'blue')) +
  ggtitle('Observed and Estimated Yield Curves, August 2019') +
  theme(plot.title = element_text(hjust=0.5, face='bold'))
  
  
  