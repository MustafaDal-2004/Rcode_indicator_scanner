# Load libraries
library(tidyverse)

# Example dataframe (replace with your actual data)
data <- data.frame(Open = c(100, 102, 105, 107, 110),
                   RSI = c(45, 55, 60, 50, 48),
                   Close = c(101, 104, 108, 106, 111))

# Fit a linear model
model <- lm(Close ~ Open + RSI, data = data)

# Summary of the model
summary(model)

# Predict close prices (for new data)
new_data <- data.frame(Open = c(108, 111), RSI = c(52, 47))
predict(model, new_data)

import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression

# Sample data
data = pd.DataFrame({
  'Open': [100, 102, 105, 107, 110],
  'RSI': [45, 55, 60, 50, 48],
  'Close': [101, 104, 108, 106, 111]
})

# Features and target
X = data[['Open', 'RSI']]
y = data['Close']

# Train model
model = LinearRegression()
model.fit(X, y)

# Predictions
new_data = pd.DataFrame({'Open': [108, 111], 'RSI': [52, 47]})
predictions = model.predict(new_data)
print(predictions)
