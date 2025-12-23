# utils.R
library(quantmod)
library(TTR)
library(data.table)
library(rvest)
library(zoo)

#-----------------------------
# Fetch S&P 500 symbols
#-----------------------------
get_sp500_symbols <- function() {
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  page <- read_html(url)
  sp500_table <- page %>% html_node(xpath = '//*[@id="constituents"]') %>% html_table()
  sp500_stocks <- gsub("\\.", "-", sp500_table$Symbol)
  return(sp500_stocks)
}

#-----------------------------
# Fetch historical stock data
#-----------------------------
get_stock_data <- function(symbol, start_date, end_date = Sys.Date()) {
  data <- tryCatch({
    getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
  }, error = function(e) {
    warning(paste("Failed for", symbol, ":", e$message))
    return(NULL)
  })
  if (!is.null(data)) {
    dt <- data.table(Date = index(data), coredata(data))
    colnames(dt) <- gsub(paste0(symbol, "\\."), "", colnames(dt))
    return(dt)
  }
  return(NULL)
}

#-----------------------------
# MACD calculation
#-----------------------------
calc_macd <- function(dt, fast=12, slow=26, signal=9) {
  macd_vals <- MACD(dt$Close, nFast=fast, nSlow=slow, nSig=signal, maType=EMA)
  last_macd <- tail(macd_vals[, "macd"], 1)
  last_signal <- tail(macd_vals[, "signal"], 1)
  signal_type <- if(last_macd > last_signal) "Buy" else if(last_macd < last_signal) "Sell" else "Hold"
  base <- if(last_macd > 0) "Above" else "Below"
  return(list(MACD = last_macd, Signal = last_signal, MACD_Signal = signal_type, Base = base))
}

#-----------------------------
# RSI calculation
#-----------------------------
calc_rsi <- function(dt, n=14) {
  dt[, RSI := RSI(Close, n=n)]
  return(tail(dt$RSI, 1))
}

#-----------------------------
# Bollinger Bands %B
#-----------------------------
calc_bollinger <- function(dt, n=20, k=2) {
  dt[, SMA := rollmean(Close, k=n, fill=NA, align="right")]
  dt[, SD := rollapply(Close, width=n, FUN=sd, fill=NA, align="right")]
  dt[, Upper := SMA + k*SD]
  dt[, Lower := SMA - k*SD]
  dt[, Percent_B := (Close - Lower)/(Upper-Lower) * 100]
  return(tail(dt$Percent_B, 1))
}
