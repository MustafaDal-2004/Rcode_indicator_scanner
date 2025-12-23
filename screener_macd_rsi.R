# screener_macd_rsi.R
source("utils.R")

sp500 <- get_sp500_symbols()
results <- list()
pb <- txtProgressBar(min=1, max=length(sp500), style=3)

for(i in seq_along(sp500)) {
  dt <- get_stock_data(sp500[i], start_date="2024-01-01")
  if(is.null(dt)) next

  macd_vals <- calc_macd(dt)
  rsi_val <- calc_rsi(dt)

  # Buy signal example
  if(macd_vals$MACD_Signal=="Buy" && macd_vals$Base=="Below" && rsi_val >= 45) {
    results[[length(results)+1]] <- list(Symbol=sp500[i], MACD=macd_vals$MACD, RSI=rsi_val)
    # optional: browseURL(paste0("https://uk.finance.yahoo.com/chart/", sp500[i]))
  }

  setTxtProgressBar(pb, i)
}
close(pb)

results_df <- rbindlist(results)
print(results_df)
