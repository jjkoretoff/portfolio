#import necessary packages
library(PerformanceAnalytics)
library(zoo)
library(tseries)
library(boot)

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

#merge all price data
all_prices <- merge(aapl_prices, goog_prices, pxd_prices, tsla_prices)

#rename the columns
colnames(all_prices) <- c('AAPL', 'GOOG', 'PXD', 'TSLA')

#calculate cc returns as difference in log prices
all_returns <- diff(log(all_prices))

#create matrix
return_matrix <- coredata(all_returns)

#compute the standard error of the variances
#observations
n_obs <- dim(return_matrix)[1]

#estimtes sigma-squared
sigma2hat_vals <- apply(return_matrix, 2, var)

#standard error of sigma-squared
se_sigma2hat <- sigma2hat_vals / sqrt(n_obs / 2)
se_sigma2hat

#compute the standard error of correlation parameter
#correlation matrix
cor_matrix <- cor(return_matrix)

#lower triangle
rhohat_vals <- cor_matrix[lower.tri(cor_matrix)]

#set the names
names(rhohat_vals) <- c('AAPL, GOOG', 'AAPL, PXD', 'AAPL, TSLA', 'GOOG, PXD', 
                        'GOOG, TSLA', 'PXD, TSLA')

#compute the estimated standard error of correlation
se_rhohat <- (1 - rhohat_vals ^ 2) / sqrt(dim(return_matrix)[1])
se_rhohat

#hypothesis test for the mean
t.test(all_returns[, 'AAPL'])
t.test(all_returns[, 'GOOG'])
t.test(all_returns[, 'PXD'])
t.test(all_returns[, 'TSLA'])

#hypothesis test for correlation
cor.test(x = all_returns[,'AAPL'], y = all_returns[,'GOOG'])
cor.test(x = all_returns[,'AAPL'], y = all_returns[,'PXD'])
cor.test(x = all_returns[,'AAPL'], y = all_returns[,'TSLA'])
cor.test(x = all_returns[,'GOOG'], y = all_returns[,'PXD'])
cor.test(x = all_returns[,'GOOG'], y = all_returns[,'TSLA'])
cor.test(x = all_returns[,'PXD'], y = all_returns[,'TSLA'])

#normality of returns
jarque.bera.test(all_returns[,'AAPL'])
jarque.bera.test(all_returns[,'GOOG'])
jarque.bera.test(all_returns[,'PXD'])
jarque.bera.test(all_returns[,'TSLA'])

#bootstapping
mean_boot <- function(x, idx) {
      ans <- mean(x[idx])
      ans
}

#boot for different stocks
AAPL_mean_boot <- boot(return_matrix[,'AAPL'], statistic = 
                             mean_boot, R = 999)
AAPL_mean_boot
plot(AAPL_mean_boot)

GOOG_mean_boot <- boot(return_matrix[,'GOOG'], statistic = 
                             mean_boot, R = 999)
GOOG_mean_boot
plot(GOOG_mean_boot)

PXD_mean_boot <- boot(return_matrix[,'PXD'], statistic = 
                             mean_boot, R = 999)
PXD_mean_boot
plot(PXD_mean_boot)

TSLA_mean_boot <- boot(return_matrix[,'TSLA'], statistic = 
                             mean_boot, R = 999)
TSLA_mean_boot
plot(TSLA_mean_boot)