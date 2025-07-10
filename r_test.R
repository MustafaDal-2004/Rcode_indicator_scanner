rm(list = ls())
cat("\014")
# Load libraries
library(quantmod)
library(TTR)
library(data.table)
library(rvest)
library(dplyr)

#get symbols
data_name_getter<-function(){
  # URL of the Wikipedia page
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  
  # Read the HTML content
  page <- read_html(url)
  
  # Extract the table containing the S&P 500 list
  sp500_table <- page %>%
    html_node(xpath = '//*[@id="constituents"]') %>%
    html_table()
  
  sp500_stocks <- sp500_table$Symbol
  
  
  sp500_stocks <- gsub("\\.", "-",sp500_stocks)
  return(sp500_stocks)
}
sp500_stocks <- data_name_getter()

#the date 
date <- "2024-01-01"

#data getter and checker
data_collecter <- function(i) {
  # Get the stock symbol dynamically
  symbol <- sp500_stocks[i]
  
  # Fetch stock data
  stock_data <- getSymbols(symbol, from = date, auto.assign = FALSE)
  
  # Convert to data.table
  stock_data <- data.table(Date = index(stock_data), coredata(stock_data))
  
  # Define expected column suffixes
  column_suffixes <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  # Generate full column names based on the symbol
  original_columns <- paste0(symbol, ".", column_suffixes)
  
  # Check which columns exist in the fetched data
  existing_columns <- original_columns[original_columns %in% colnames(stock_data)]
  
  # Rename existing columns to remove the symbol prefix
  setnames(stock_data, existing_columns, column_suffixes[original_columns %in% existing_columns])
  
  return(stock_data)
}

macd <- function(stock_data, macd_fast = 12, macd_slow = 26, macd_signal = 9, maType = EMA) {
  # Compute MACD using the Close column
  macd_values <- MACD(stock_data$Close, 
                      nFast = macd_fast, 
                      nSlow = macd_slow, 
                      nSig = macd_signal, 
                      maType = EMA)
  
  # Add MACD, Signal, and Histogram to stock_data
  stock_data[, MACD := macd_values[, "macd"]]
  stock_data[, Signal := macd_values[, "signal"]]
  stock_data[, MACD_Histogram := MACD - Signal]
  
  # Get the last row's values
  last_macd <- tail(stock_data$MACD, 1)
  last_signal <- tail(stock_data$Signal, 1)
  
  # Determine the signal: Buy if MACD crosses above Signal, Sell if below, Hold otherwise
  macd_signal <- if (last_macd > last_signal) {
    "Buy"
  } else if (last_macd < last_signal) {
    "Sell"
  } else {
    "Hold"
  }
  
  # Determine the base: Above or below the zero line
  macd_base <- if (last_macd > 0) {
    "Above"
  } else {
    "Below"
  }
  
  # Return the MACD, Signal, Signal (Buy/Sell/Hold), and Base (Above/Below zero line)
  return(data.table(MACD = last_macd, Signal = last_signal, MACD_Signal = macd_signal, Base = macd_base))
}
rsi <- function(stock_data, rsi_period = 14) {
  # Compute RSI using the Close column
  stock_data[, RSI := RSI(Close, n = rsi_period)]
  
  # Return Date and RSI values
  return(tail(stock_data[, .(RSI)],1))
}
bollinger_bands <- function(stock_data, n = 20, k = 2) {
  
  # Compute Bollinger Bands
  stock_data[, SMA := zoo::rollmean(Close, k = n, fill = NA, align = "right")]
  stock_data[, SD := zoo::rollapply(Close, width = n, FUN = sd, fill = NA, align = "right")]
  stock_data[, Upper_Band := SMA + (k * SD)]
  stock_data[, Lower_Band := SMA - (k * SD)]
  
  stock_data[, Percent_B := ((Close - Lower_Band) / (Upper_Band - Lower_Band)) * 100]
  
  # Return Date, Close Price, SMA, Upper Band, Lower Band
  return(tail(stock_data[, .(Percent_B)], 1))
  
  
}

# Initialize an empty dataframe to store results
results <- data.frame(Symbol = character(),
                      RSI = numeric(),
                      Bollinger_Upper = numeric(),
                      Bollinger_Lower = numeric(),
                      stringsAsFactors = FALSE)


#check all the stocks
i=1
while (i != 503) {
  data <- data_collecter(i)
  macd_vaules <- macd(data)
  percent_diff <- abs(macd_vaules$MACD - macd_vaules$Signal) / abs(macd_vaules$Signal) * 100
  if((macd_vaules$MACD_Signal == "Buy") & (macd_vaules$Base =="Below") & (macd_vaules$MACD >= macd_vaules$Signal) & (percent_diff <= 2)){
    results <- rbind(results, data.frame(Symbol = sp500_stocks[i],
                                         RSI = rsi(data),
                                         Bollinger = bollinger_bands(data)))
  }
  i <- i + 1
}

View(results)











