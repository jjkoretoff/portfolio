#load packages
library(PerformanceAnalytics)
library(zoo)

#get data 
aapl_prices <- get.hist.quote(instrument = 'aapl', start = '2000-05-01', 
                              end = '2016-05-03', quote = 'AdjClose', 
                              provider = 'yahoo', origin = '1970-01-01', 
                              compression = 'm', retclass = 'zoo', 
                              quiet = TRUE)

pxd_prices <- get.hist.quote(instrument = 'pxd', start = '2000-05-01', 
                              end = '2016-05-03', quote = 'AdjClose', 
                              provider = 'yahoo', origin = '1970-01-01', 
                              compression = 'm', retclass = 'zoo', 
                              quiet = TRUE)

#index      
index(aapl_prices) <- as.yearmon(index(aapl_prices))

index(pxd_prices) <- as.yearmon(index(pxd_prices))

#merge data
both_prices <- merge(aapl_prices, pxd_prices)
colnames(both_prices) <- c('AAPL', 'PXD')

#returns 
both_returns <- diff(log(both_prices))

#returns into a matrix
return_matrix <- coredata(both_returns)

#CER model
#estimate multivariate parameters
mu_hat_annual <- apply(return_matrix, 2, mean) * 12
sigma2_annual <- apply(return_matrix, 2, var) * 12
sigma_annual <- sqrt(sigma2_annual)
cov_mat_annual <- cov(return_matrix) * 12
cov_hat_annual <- cov(return_matrix)[1,2] * 12
rho_hat_annual <- cor(return_matrix)[1,2]

#annual estimates of CER for AAPL and PXD
mu_aapl <- mu_hat_annual['AAPL']
mu_pxd <- mu_hat_annual['PXD']
sigma2_aapl <- sigma2_annual['AAPL']
sigma2_pxd <- sigma2_annual['PXD']
sigma_aapl <- sigma_annual['AAPL']
sigma_pxd <- sigma_annual['PXD']
sigma_aapl_pxd <- cov_hat_annual
rho_aapl_pxd <- rho_hat_annual

#portfolio ratio
aapl_weights <- seq(from = -1, to = 2, by = 0.1)
pxd_weights <- 1 - aapl_weights

#portfolio parameters
mu_portfolio <- aapl_weights * mu_aapl + pxd_weights * mu_pxd

sigma2_portfolio <- aapl_weights ^ 2 * sigma2_aapl + pxd_weights ^ 2 * sigma2_pxd +  
      2 * aapl_weights * pxd_weights * sigma_aapl_pxd

sigma_portfolio <- sqrt(sigma2_portfolio)

#plotting the results
plot(sigma_portfolio, mu_portfolio, type = 'b', pch = 16, ylim = c(0, max(mu_portfolio)), 
     xlim = c(0.3, max(sigma_portfolio)), xlab = expression(sigma[p]), ylab = 
           expression(mu[p]), col = c(rep('green', 12), rep('green', 12)))

text(x = sigma_aapl, y = mu_aapl, labels = 'AAPL', pos = 4)
text(x = sigma_pxd, y = mu_pxd, labels = 'PXD', pos = 1)

#define global min portfolio
globalMin.portfolio <-
      function(er, cov.mat)
      {
            # Compute global minimum variance portfolio
            #
            # inputs:
            # er				N x 1 vector of expected returns
            # cov.mat		N x N return covariance matrix
            #
            # output is portfolio object with the following elements
            # call			original function call
            # er				portfolio expected return
            # sd				portfolio standard deviation
            # weights		N x 1 vector of portfolio weights
            call <- match.call()
            
            #
            # check for valid inputs
            #
            asset.names <- names(er)
            er <- as.vector(er)					# assign names if none exist
            cov.mat <- as.matrix(cov.mat)
            if(length(er) != nrow(cov.mat))
                  stop("invalid inputs")
            if(any(diag(chol(cov.mat)) <= 0))
                  stop("Covariance matrix not positive definite")
            # remark: could use generalized inverse if cov.mat is positive semi-definite
            
            #
            # compute global minimum portfolio
            #
            cov.mat.inv <- solve(cov.mat)
            one.vec <- rep(1,length(er))
            #  w.gmin <- cov.mat.inv %*% one.vec/as.vector(one.vec %*% cov.mat.inv %*% one.vec)
            w.gmin <- rowSums(cov.mat.inv) / sum(cov.mat.inv)
            w.gmin <- as.vector(w.gmin)
            names(w.gmin) <- asset.names
            er.gmin <- crossprod(w.gmin,er)
            sd.gmin <- sqrt(t(w.gmin) %*% cov.mat %*% w.gmin)
            gmin.port <- list("call" = call,
                              "er" = as.vector(er.gmin),
                              "sd" = as.vector(sd.gmin),
                              "weights" = w.gmin)
            class(gmin.port) <- "portfolio"
            gmin.port
      }

#add a 52 week t-bill
t_bill_rate <- .01

#appl weights
aapl_weights <- seq(from = -1, to = 2, by = 0.1)

# Portfolio parameters
mu_portfolio_aapl_bill <- t_bill_rate + appl_weights * (mu_aapl - t_bill_rate)
sigma_portfolio_aapl_bill <- aapl_weights * sigma_aapl

# Plot previous exercise
plot(sigma_portfolio, mu_portfolio, type="b", pch=16, ylim=c(0, 
      max(mu_portfolio)), xlim=c(0.3, max(sigma_portfolio)), 
      xlab=expression(sigma[p]), ylab=expression(mu[p]),col=c(rep("green", 18), 
      rep("red", 13)))
text(x=sigma_aapl, y=mu_aapl, labels="AAPL", pos=4)
text(x=sigma_pxd, y=mu_pxd, labels="PXD", pos=4)

# Portfolio Combination Boeing and T-bills
points(sigma_portfolio_aapl_bill, mu_portfolio_aapl_bill, type="b", col="blue")

#sharpe
sharp_ratio_aapl <- (mu_aapl - t_bill_rate)/sigma_aapl

# The global minimum variance portfolio
global_min_var_portfolio <- globalMin.portfolio(mu_hat_annual,cov_mat_annual)
global_min_var_portfolio

# Summary of global_min_var_portfolio that takes into account the annual risk-free rate of 3% per year
summary(global_min_var_portfolio, risk.free=0.01)

# Portfolio weights AAPL and PXD
plot(global_min_var_portfolio)

# Plot previous exercises
plot(sigma_portfolio, mu_portfolio, type="b", pch=16, ylim=c(0, max(mu_portfolio)), xlim=c(0, max(sigma_portfolio)), xlab=expression(sigma[p]), ylab=expression(mu[p]),col=c(rep("green", 18), rep("red", 13)))
text(x=sigma_aapl, y=mu_aapl, labels="AAPL", pos=4)
text(x=sigma_pxd, y=mu_pxd, labels="PXD", pos=4)

# Plot the position of the global minimum variance portfolio
text(x=global_min_var_portfolio$sd, y=global_min_var_portfolio$er, labels="Global min", pos=2)