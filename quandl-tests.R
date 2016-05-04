library(Quandl)

# The quantmod package is ready for use:
library(quantmod)

# Load the Facebook data with the help of Quandl
Facebook <- Quandl("GOOG/NASDAQ_FB", type = "xts")

# Plot the chart with the help of candleChart()
candleChart(Facebook)