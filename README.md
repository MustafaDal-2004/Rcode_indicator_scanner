S&P 500 Stock Screener with Technical Indicators in R

This project is an R-based S&P 500 stock screener designed to automate the process of identifying potential buy signals using widely-used technical indicators. The script retrieves the latest S&P 500 company list from Wikipedia, fetches historical stock data, calculates several technical indicators, and highlights stocks that meet predefined buy criteria. It is aimed at traders, analysts, and enthusiasts who want to quickly screen for opportunities in the market.

The script uses live retrieval of S&P 500 tickers to ensure the data is always up-to-date. Once the ticker list is obtained, the script loops through each symbol, fetching historical data using quantmod. The historical data is then analyzed with technical indicators such as moving averages (SMA and EMA), RSI (Relative Strength Index), MACD (Moving Average Convergence Divergence), and Bollinger Bands. These indicators help assess the stock’s momentum, trend, and potential oversold or overbought conditions.

Buy signals are determined based on simple rules applied to these indicators. For example, a stock may be flagged if its RSI is below 30, indicating oversold conditions, if its price crosses above the 50-day SMA, suggesting upward momentum, or if the MACD line crosses above the signal line, signaling a possible trend reversal. Users can customize these thresholds to fit their preferred trading strategy. The script can also integrate additional indicators such as ATR, CMF, Williams %R, or Stochastic Oscillators to refine screening logic.

The output of the script is a table of tickers that currently meet the buy criteria, along with the values of the calculated technical indicators. For users interested in predictive modeling, the script can be extended to include linear regression for forecasting the next closing price based on features like Open, Close, and RSI. While the script is built for analysis and educational purposes, it allows for fast, automated screening of a large number of stocks without manual intervention.

This project requires a few R packages to work effectively, including quantmod for stock data retrieval, TTR for technical indicators, rvest for scraping the S&P 500 list, data.table and dplyr for data manipulation, and optionally tidyverse for easier data handling and visualization. Overall, this script provides a versatile foundation for technical analysis-based screening of the S&P 500, offering a starting point for further customization, strategy testing, and trading research.

Please ensure these packages are installed into the R code terminal 

install.packages(c(

  "rvest",      # for scraping Wikipedia
  
  "quantmod",   # for financial data and indicators
  
  "TTR",        # technical analysis indicators
  
  "dplyr",      # data manipulation
  
  "tidyverse",  # optional, recommended
  
  "data.table", # optional, for faster data handling
  
  "zoo"         # for rolling calculations (SMA, Bollinger)
  
))

