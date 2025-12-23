# screener_advanced.R
source("utils.R")

sp500 <- get_sp500_symbols()
results <- list()

for(i in seq_along(sp500)) {
  dt <- get_stock_data(sp500[i], start_date="2024-01-01")
  if(is.null(dt)) next

  buy_score <- 0
  if(calc_rsi(dt) < 30) buy_score <- buy_score + 1
  if(calc_bollinger(dt) < 20) buy_score <- buy_score + 1
  macd_vals <- calc_macd(dt)
  if(macd_vals$MACD_Signal=="Buy") buy_score <- buy_score + 1

  if(buy_score >= 2) {
    results[[length(results)+1]] <- list(Symbol=sp500[i], BuyScore=buy_score)
  }
}

results_df <- rbindlist(results)
print(results_df)
