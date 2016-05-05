#import necessary packages
library(PerformanceAnalytics)
library(zoo)
library(tseries)

#get historic data

aapl_prices <- get.hist.quote(instrument = 'aapl', start = '2015-05-01', 
                              end = '2016-05-03', quote = 'AdjClose', 
                              provider = 'yahoo', origin = '1970-01-01', 
                              compression = 'm', retclass = 'zoo', 
                              quiet = TRUE)

goog_prices <- get.hist.quote(instrument = 'goog', start = '2015-05-01', 
                              end = '2016-05-03', quote = 'AdjClose', 
                              provider = 'yahoo', origin = '1970-01-01', 
                              compression = 'm', retclass = 'zoo', 
                              quiet = TRUE)

pxd_prices <- get.hist.quote(instrument = 'pxd', start = '2015-05-01', 
                             end = '2016-05-03', quote = 'AdjClose', 
                             provider = 'yahoo', origin = '1970-01-01', 
                             compression = 'm', retclass = 'zoo', 
                             quiet = TRUE)

tsla_prices <- get.hist.quote(instrument = 'tsla', start = '2015-05-01', 
                              end = '2016-05-03', quote = 'AdjClose', 
                              provider = 'yahoo', origin = '1970-01-01', 
                              compression = 'm', retclass = 'zoo', 
                              quiet = TRUE)

#adjusted index for monthly data
index(aapl_prices) <- as.yearmon(index(aapl_prices))

index(goog_prices) <- as.yearmon(index(goog_prices))

index(pxd_prices) <- as.yearmon(index(pxd_prices))

index(tsla_prices) <- as.yearmon(index(tsla_prices))

#inspect
start(aapl_prices)
end(aapl_prices)

start(goog_prices)
end(goog_prices)

start(pxd_prices)
end(pxd_prices)

start(tsla_prices)
end(tsla_prices)

#merge the data and rename columns
all_prices <- merge(aapl_prices, goog_prices, pxd_prices, tsla_prices)

colnames(all_prices) <- c('AAPL', 'GOOG', 'PXD', 'TSLA')

#calculate the returns
all_returns <- diff(log(all_prices))

#analyze
start(all_returns)
end(all_returns)
colnames(all_returns)
head(all_returns)

#plot the data
chart.TimeSeries(all_returns, legend.loc = 'bottom', main = '')

chart.Bar(all_returns, legend.loc = 'bottom', main = '')

simple_returns <- diff(all_prices) / lag(all_prices, k = 1)

chart.CumReturns(simple_returns, legend.loc = 'topleft', wealth.index = TRUE,  
                 main = 'Future Value of $1 invested')

#graphical summary AAPL
return_matrix <- coredata(all_returns)

par(mfrow = c(2, 2))

hist(return_matrix[, 'AAPL'], main = 'AAPL monthly returns', xlab = 
           'AAPL', probability = T, col = 'slateblue1')

boxplot(return_matrix[, 'AAPL'], outchar = T, main = 'Boxplot', 
        col = 'slateblue1')

plot(density(return_matrix[, 'AAPL']), type = 'l', main = 'Smoothed density',  
     xlab = 'monthly return', ylab = 'density estimate', col = 'slateblue1')

qqnorm(return_matrix[, 'AAPL'], col = 'slateblue1')

qqline(return_matrix[, 'AAPL'])

par(mfrow = c(1, 1))

#graphical summary GOOG
return_matrix <- coredata(all_returns)

par(mfrow = c(2, 2))

hist(return_matrix[, 'GOOG'], main = 'GOOG monthly returns', xlab = 
           'GOOG', probability = T, col = 'slateblue1')

boxplot(return_matrix[, 'GOOG'], outchar = T, main = 'Boxplot', 
        col = 'slateblue1')

plot(density(return_matrix[, 'GOOG']), type = 'l', main = 'Smoothed density',  
     xlab = 'monthly return', ylab = 'density estimate', col = 'slateblue1')

qqnorm(return_matrix[, 'GOOG'], col = 'slateblue1')

qqline(return_matrix[, 'GOOG'])

par(mfrow = c(1, 1))

#graphical summary PXD
return_matrix <- coredata(all_returns)

par(mfrow = c(2, 2))

hist(return_matrix[, 'PXD'], main = 'PXD monthly returns', xlab = 
           'PXD', probability = T, col = 'slateblue1')

boxplot(return_matrix[, 'PXD'], outchar = T, main = 'Boxplot', 
        col = 'slateblue1')

plot(density(return_matrix[, 'PXD']), type = 'l', main = 'Smoothed density',  
     xlab = 'monthly return', ylab = 'density estimate', col = 'slateblue1')

qqnorm(return_matrix[, 'PXD'], col = 'slateblue1')

qqline(return_matrix[, 'PXD'])

par(mfrow = c(1, 1))

#graphical summary TSLA
return_matrix <- coredata(all_returns)

par(mfrow = c(2, 2))

hist(return_matrix[, 'TSLA'], main = 'TSLA monthly returns', xlab = 
           'TSLA', probability = T, col = 'slateblue1')

boxplot(return_matrix[, 'TSLA'], outchar = T, main = 'Boxplot', 
        col = 'slateblue1')

plot(density(return_matrix[, 'TSLA']), type = 'l', main = 'Smoothed density',  
     xlab = 'monthly return', ylab = 'density estimate', col = 'slateblue1')

qqnorm(return_matrix[, 'TSLA'], col = 'slateblue1')

qqline(return_matrix[, 'TSLA'])

par(mfrow = c(1, 1))

#return comparison
boxplot(return_matrix[, 'AAPL'], return_matrix[, 'GOOG'], return_matrix[, 'PXD'], 
        return_matrix[, 'TSLA'], col = 'slateblue1')

chart.Boxplot(all_returns)

#compute univariate descriptive statistics 
summary(return_matrix)

args(apply)
apply(return_matrix, 2, mean)
apply(return_matrix, 2, var)
apply(return_matrix, 2, sd)
apply(return_matrix, 2, skewness)
apply(return_matrix, 2, kurtosis)

table.Stats(all_returns)

#annualized continuously compounded
12 * apply(return_matrix, 2, mean)

#annualized simple
exp(12 * apply(return_matrix, 2, mean)) - 1

#annualized sd
sqrt(12) * apply(return_matrix, 2, sd)

#bivariate graphical anaylsis
pairs(return_matrix, col = 'blue')

#compute a 4 by 4 covariance and correlation matrices
var(return_matrix)
cor(return_matrix)