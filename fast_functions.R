#package needed for project to work
install.packages("quantmod")
install.packages("data.table")
install.packages("TTR")
install.packages("rvest")
install.packages("dplyr")

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
date <- "24-12-13"

#data getter and checker
data_collecter <- function(i) {
  # Ensure valid index
  if (i > length(sp500_stocks) || i < 1) {
    stop("Error: Index out of bounds.")
  }
  
  # Get the stock symbol dynamically
  symbol <- sp500_stocks[i]
  
  # Fetch stock data
  stock_data <- tryCatch({
    getSymbols(symbol, from = date, auto.assign = FALSE)
  }, error = function(e) {
    stop(paste("Error fetching data for", symbol, ":", e$message))
  })
  
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


#atr function use to set stop loss 
get_atr <- function(stock_data, atr_n = 14) {
  
  # Compute ATR
  atr_values <- ATR(stock_data[, .(High, Low, Close)], n = atr_n)
  
  # Add ATR to stock_data
  stock_data[, ATR := atr_values[, "atr"]]
  
  # Return last ATR value
  return(tail(stock_data[, .(ATR)], 1))
}

#cmf function if over 0 buy if bellow sell
get_cmf <- function(stock_data, cmf_n = 20) {
  # Ensure stock_data is a data.table
  setDT(stock_data)
  
  # Ensure required columns exist
  if (!all(c("High", "Low", "Close", "Volume") %in% colnames(stock_data))) {
    stop("Error: 'High', 'Low', 'Close', or 'Volume' column not found in stock data")
  }
  
  # Compute CMF using the correct data structure
  cmf_values <- CMF(HLC = stock_data[, .(High, Low, Close)], volume = stock_data$Volume, n = cmf_n)
  
  # Add CMF to stock_data
  stock_data[, CMF := cmf_values]
  
  # Return last CMF value
  return(tail(stock_data[, .(CMF)], 1))
}

#williams funtion if less than -80 its a buy signal
get_williams_r <- function(stock_data, wr_n = 14) {
  
  # Compute Williams %R
  wr_values <- WPR(stock_data[, .(High, Low, Close)], n = wr_n)
  
  # Add Williams %R to stock_data
  stock_data[, Williams_R := wr_values]
  
  # Return last Williams %R value
  return((tail(stock_data[, .(Williams_R)], 1))*-100)
}

#RSI
get_rsi_fast <- function(stock_data, rsi_period = 14) {
  # Compute RSI using the Close column
  stock_data[, RSI := RSI(Close, n = rsi_period)]
  
  # Return Date and RSI values
  return(tail(stock_data[, .(RSI)],1))
}

#macd
get_macd_fast <- function(stock_data, macd_fast = 12, macd_slow = 26, macd_signal = 9) {
  # Ensure stock_data is not empty
  if (nrow(stock_data) == 0) {
    stop("Error: Stock data is empty")
  }
  
  # Ensure Close column exists
  if (!("Close" %in% colnames(stock_data))) {
    stop("Error: 'Close' column not found in stock data")
  }
  
  # Compute MACD using the Close column
  macd_values <- MACD(stock_data$Close, 
                      nFast = macd_fast, 
                      nSlow = macd_slow, 
                      nSig = macd_signal, 
                      maType = EMA)
  
  # Add MACD, Signal, and Histogram to the data.table
  stock_data[, MACD := macd_values[, "macd"]]
  stock_data[, Signal := macd_values[, "signal"]]
  stock_data[, MACD_Histogram := MACD - Signal]
  
  # Return Date, MACD, Signal, and Histogram
  return(tail(stock_data[, .(MACD)],1))
}

#stochastic
get_stochastic_fast <- function(stock_data, period = 14, stoch_n = 14, stoch_smooth = 3) {
  
  # Compute Stochastic Oscillator
  stock_data[, `:=`(
    best_low = rollapply(Low, width = period, FUN = min, align = "right", fill = NA),
    best_high = rollapply(High, width = period, FUN = max, align = "right", fill = NA)
  )]
  
  stock_data[, Fast_K := 100 * ((Close - best_low) / (best_high - best_low))]
  stock_data[, Fast_D := round(rollmean(Fast_K, k = stoch_smooth, fill = NA, align = "right"), 2)]
  stock_data[, Slow_K := Fast_D]
  stock_data[, Slow_D := round(rollmean(Slow_K, k = stoch_smooth, fill = NA, align = "right"), 2)]
  
  # Return Date, Fast %K, Fast %D, and Slow %D
  return(tail(stock_data[, .(Slow_D)],1)) # return(tail(stochastic_data$Slow_D, 1))
}

#bollinger bands
get_bollinger_bands_fast <- function(stock_data, n = 20, k = 2) {
  
  # Compute Bollinger Bands
  stock_data[, SMA := zoo::rollmean(Close, k = n, fill = NA, align = "right")]
  stock_data[, SD := zoo::rollapply(Close, width = n, FUN = sd, fill = NA, align = "right")]
  stock_data[, Upper_Band := SMA + (k * SD)]
  stock_data[, Lower_Band := SMA - (k * SD)]
  
  # Return Date, Close Price, SMA, Upper Band, Lower Band
  return(tail(stock_data[, .(SMA, Upper_Band, Lower_Band)],1))
}

assess_buy_signal_fast <- function(i) {
  buy_count <- 0
  data <- data_collecter(i)
  # Evaluate RSI condition
  if (get_rsi_fast(data) < 30) {
    buy_count <- buy_count + 1
  }
  
  # Evaluate MACD condition
  #if (get_macd_fast(data) > 0) {
    #buy_count <- buy_count + 1
  #}
  
  # Evaluate Stochastic condition
  if (get_stochastic_fast(data) < 20) {
    buy_count <- buy_count + 1
  }
  
  # Evaluate Bollinger Bands condition
  bollinger <- get_bollinger_bands_fast(data)
  if (!is.na(bollinger$Lower_Band) && get_rsi_fast(data) < bollinger$Lower_Band) {
    buy_count <- buy_count + 1
  }
  
  #williams_r function
  if (get_williams_r(data) < -80) {
    buy_count <- buy_count + 1
  }
  
  #cmf function
  if (get_cmf(data) > 0) {
    buy_count <- buy_count + 1
  }
  
  
  return(buy_count)
}
k=327
while(k != 503 ){
  if(assess_buy_signal_fast(k) == 4){
    print(sp500_stocks[k])
    print(assess_buy_signal_fast(k))
  }
  k=k+1
  print(k)
}



