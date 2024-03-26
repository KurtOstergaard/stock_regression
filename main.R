# NASDAQ data from Sharadar
library(NasdaqDataLink)
library(Quandl)
library(tidyquant)
library(tidyverse)
library(curl)
library(httr2)
library(rvest)

source(key.R)
Quandl.api_key(key)
NasdaqDataLink.api_key(key)

# From SF1, Fundamentals data, all tickers in S&P500 history, trimmed to current
SP500 <- Quandl.datatable('SHARADAR/SP500', paginate=TRUE) %>%
  filter(action == "current") %>%
  select(date, ticker, name) %>%
  arrange(ticker)

table(SP500$date)
tickers <- c(SP500$ticker)

# Get prices
test <- Quandl.datatable('SHARADAR/SEP', date.gte='2024-01-01', paginate=TRUE)

# curl_download(url=SEP_url, "big_price_file.csv", mode = "w", handle = new_handle())
# req <- request(SEP_url)
# req %>% req_dry_run()
# resp <- req_perform(req)

px <- read_csv("SHARADAR_SEP.csv", col_names = TRUE)
px3 <- px %>% 
  filter(lastupdated > "2023-12-31")
