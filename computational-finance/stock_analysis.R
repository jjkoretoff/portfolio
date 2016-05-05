#import the package
library(Quandl)
library(quantmod)

#assign quandl extension to a variable
a = "GOOG/NYSE_PXD"
b = "GOOG/NASDAQ_AAPL"
c = "GOOG/NASDAQ_GOOGL"
d = "GOOG/NASDAQ_TSLA"

#assign dates to a variable
last_date <- "2016-05-03"
first_date <- "2015-05-01"

#assign data frame to a variable
#adjust for weekly, monthly, quarterly, yearly outputs here
stock <- Quandl(c, api_key="xPN8SdVKiNG7LAACxgfA", order = "asc")

#observe the data
str(stock)
summary(stock)
head(stock)
tail(stock)
class(stock)

#extract the closing prices in the data frame
stock_close <- stock[, "Close", drop = FALSE] 

#format row names as dates
rownames(stock_close) <- stock$Date

#subset prices by date
price_x_minus1 <- stock_close[first_date,]
price_x_minus1

price_x <- stock_close[last_date,]
price_x

#plot the data
candleChart(stock_close)

#calculate simple returns - no longer a dataframe
n <- nrow(stock_close)
stock_simple_return <- ((stock_close[2:n, 1] - stock_close[1:(n-1), 1])
                        / stock_close[1:(n-1), 1])

#name the previous variable outputs
names(stock_simple_return) <- stock$Date[2:n]
head(stock_simple_return)
tail(stock_simple_return)

#compute continuously compounded daily returns
stock_cc_return <- log(stock_close[2:n, 1]) - log(stock_close[1:(n-1), 1])

#name the previous variable outputs
names(stock_cc_return) <- stock$Date[2:n]
tail(stock_cc_return)

#compare simple and continuous returns
tail(cbind(stock_simple_return, stock_cc_return))

#compare graphs of simple and continuous
plot(stock_simple_return, type = 'l', col = 'blue', lwd = 2, ylab = 'Return', 
     main = 'Daily Returns')
abline(h = 0)
legend(x = 'bottomright', legend = c('Simple', 'CC'), lty = 1, lwd = 2, 
       col = c('blue', 'red'))
lines(stock_cc_return, col = 'red', lwd = 2)

#growth of one dollar in stock
stock_gross_returns <- 1 + stock_simple_return

stock_future_value <- cumprod(stock_gross_returns)

#plot the outcome of the afforementioned 
plot(stock_future_value, type = 'l', col = 'blue', lwd = 2, ylab = 'Dollars', 
     main = 'Future Value of $1 in stock')



