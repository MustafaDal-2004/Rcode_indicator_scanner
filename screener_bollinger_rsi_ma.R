# screener_bollinger_rsi_ma.R
source("utils.R")

sp500 <- get_sp500_symbols()
results <- list()
pb <- txtProgressBar(min=1, max=length(sp500), style=3)

for(i in seq_along(sp500)) {
  dt <- get_stock_data(sp500[i], start_date="2024-01-01")
  if(is.null(dt)) next

  rsi_val <- calc_rsi(dt)
  boll_val <- calc_bollinger(dt)
  ma_50 <- tail(SMA(dt$Close, n=50), 1)

  if(rsi_val <= 30 && boll_val <= 40 && tail(dt$Close,1) > ma_50) {
    results[[length(results)+1]] <- list(Symbol=sp500[i], RSI=rsi_val, Bollinger=boll_val, MA50=ma_50)
  }

  setTxtProgressBar(pb, i)
}
close(pb)

results_df <- rbindlist(results)
print(results_df)
