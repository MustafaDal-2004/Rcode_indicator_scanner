S&P 500 Stock Screener with Technical Indicators in R

This R script automates the process of retrieving the current S&P 500 stock list from Wikipedia, fetching stock data, applying common technical indicators, and returning a list of stocks with potential buy signals based on those indicators.

Features

Live retrieval of the latest S&P 500 companies from Wikipedia
Uses popular technical indicators such as:
Moving Averages (SMA, EMA)
RSI (Relative Strength Index)
MACD (Moving Average Convergence Divergence)
Bollinger Bands
Basic buy signal logic based on indicator thresholds
Returns a list of tickers that currently meet buy criteria
Requirements

Make sure the following R packages are installed:

install.packages(c(
  "rvest",        # for scraping Wikipedia
  "quantmod",     # for financial data and indicators
  "TTR",          # technical analysis indicators
  "dplyr",        # data manipulation
  "tidyverse"     # optional but recommended
))
How It Works

Scrape Wikipedia for the current list of S&P 500 tickers.
Loop through each ticker:
Fetch historical stock data via quantmod::getSymbols.
Compute technical indicators (RSI, SMA, EMA, MACD, etc.).
Apply simple rules to detect buy signals.
Output a list of stocks that meet the buy criteria.
Sample Buy Criteria Logic

You can customize the logic, but a basic example includes:

RSI < 30 (oversold)
Price crosses above 50-day SMA
MACD line crosses above signal line
