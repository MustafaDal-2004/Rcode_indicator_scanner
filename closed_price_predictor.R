# Load libraries
library(quantmod)
library(TTR)
library(data.table)
library(rvest)
library(dplyr)

# --- Get S&P 500 symbols ---
get_sp500_symbols <- function(){
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  page <- read_html(url)
  table <- page %>%
    html_node(xpath = '//*[@id="constituents"]') %>%
    html_table()
  symbols <- gsub("\\.", "-", table$Symbol)
  return(symbols)
}

sp500_stocks <- get_sp500_symbols()

# --- Data fetcher ---
fetch_stock_data <- function(symbol, from_date="2024-01-01", to_date=Sys.Date()){
  stock_data <- getSymbols(symbol, from = from_date, to = to_date, auto.assign = FALSE)
  dt <- data.table(Date = index(stock_data), coredata(stock_data))
  cols <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  setnames(dt, paste0(symbol, ".", cols)[paste0(symbol, ".", cols) %in% colnames(dt)], 
           cols[paste0(symbol, ".", cols) %in% colnames(dt)])
  return(dt)
}

# --- Indicators ---
compute_rsi <- function(dt, n = 14) tail(RSI(dt$Close, n), 1)
compute_macd <- function(dt) {
  macd_vals <- MACD(dt$Close)
  tail(macd_vals[, "macd"], 1)
}
compute_bollinger <- function(dt, n = 20, k = 2) {
  sma <- zoo::rollmean(dt$Close, k = n, fill = NA, align = "right")
  sd <- zoo::rollapply(dt$Close, width = n, FUN = sd, fill = NA, align = "right")
  upper <- sma + k * sd
  lower <- sma - k * sd
  tail(cbind(upper, lower), 1)
}

# --- Predictive model for Close ---
predict_close <- function(dt){
  if(nrow(dt) < 5) return(NA)  # need minimum rows
  df <- data.frame(Open = dt$Open, RSI = RSI(dt$Close))
  df$Close <- dt$Close
  model <- lm(Close ~ Open + RSI, data = df)
  # Predict next Close using last Open and RSI
  new_row <- data.frame(Open = tail(dt$Open, 1), RSI = tail(RSI(dt$Close), 1))
  return(predict(model, new_row))
}

# --- Screener ---
results <- data.frame(Symbol = character(),
                      RSI = numeric(),
                      MACD = numeric(),
                      Predicted_Close = numeric(),
                      stringsAsFactors = FALSE)

for(i in 1:length(sp500_stocks)){
  dt <- fetch_stock_data(sp500_stocks[i])
  rsi_val <- compute_rsi(dt)
  macd_val <- compute_macd(dt)
  pred_close <- predict_close(dt)
  
  # Example filter: RSI below 30 and MACD positive
  if(!is.na(rsi_val) & rsi_val < 30 & macd_val > 0){
    results <- rbind(results, data.frame(Symbol = sp500_stocks[i],
                                         RSI = rsi_val,
                                         MACD = macd_val,
                                         Predicted_Close = round(pred_close, 2)))
  }
}

print(results)


