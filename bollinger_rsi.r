
# Load libraries
library(quantmod)
library(TTR)
library(data.table)
library(rvest)


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
date2 <- "2025-05-03"
#data getter and checker
data_collecter <- function(i) {
  # Get the stock symbol dynamically
  symbol <- sp500_stocks[i]
  
  # Fetch stock data
  stock_data <- getSymbols(symbol, from = date, to = date2, auto.assign = FALSE)
  
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

results <- data.frame(Symbol = character(),
                      RSI = numeric(),
                      Bollinger = numeric(),
                      i = numeric(),
                      stringsAsFactors = FALSE)

i=1
pb <- txtProgressBar(min = 0, max = 503, style = 3)
while (i != 503) {
  data <- data_collecter(i)
  rsi_vaule <- rsi(data)
  bollinger <- bollinger_bands(data)
  if((rsi_vaule <= 30) & (bollinger <= 30)){
    print(sp500_stocks[i])
    results <- rbind(results, data.frame(Symbol = sp500_stocks[i],
                                         RSI = rsi(data),
                                         Bollinger = bollinger_bands(data),
                                         i = i))
  }
  setTxtProgressBar(pb, i)
  i=i+1
}

print(":finished")
#idea we can get a big list from this alone we need to get these macd checked as well

























