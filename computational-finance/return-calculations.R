#import the dataset
pxd <- Quandl("GOOG/NYSE_PXD", api_key="xPN8SdVKiNG7LAACxgfA")

#checkout the dataset
str(pxd)
head(pxd)
tail(pxd)
class(pxd)

#format for chart
pxd_chart_format <- Quandl("GOOG/NYSE_PXD", api_key="xPN8SdVKiNG7LAACxgfA",
                           type = "xts", start_date = "2015-08-27", 
                           end_date = "2016-05-03")

#chart dataset
pxd_chart <- candleChart(pxd_chart_format)

#pull the desired pricing data
pxd_close <- Quandl("GOOG/NYSE_PXD", api_key="xPN8SdVKiNG7LAACxgfA", 
                    column_index = 4)

#inspect head
head(pxd_close)

#calculate simple returns
pxd_daily_returns <- Quandl("GOOG/NYSE_PXD", api_key="xPN8SdVKiNG7LAACxgfA", 
                          column_index = 4, transform = c("rdiff"), 
                          collapse = ("daily"))
head(pxd_daily_returns)

pxd_weekly_returns <- Quandl("GOOG/NYSE_PXD", api_key="xPN8SdVKiNG7LAACxgfA",
                           column_index = 4, transform = c("rdiff"), 
                           collapse = ("weekly"))
head(pxd_weekly_returns)

pxd_monthly_returns <- Quandl("GOOG/NYSE_PXD", api_key="xPN8SdVKiNG7LAACxgfA",
                              column_index = 4,transform = c("rdiff"), 
                              collapse = ("monthly"))
head(pxd_monthly_returns)

pxd_quarterly_returns <- Quandl("GOOG/NYSE_PXD", api_key="xPN8SdVKiNG7LAACxgfA",
                                column_index = 4, transform = c("rdiff"), 
                                collapse = ("quarterly"))
head(pxd_quarterly_returns)



