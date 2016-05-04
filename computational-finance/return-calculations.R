#import the dataset
pxd <- Quandl("GOOG/NYSE_PXD", api_key="xPN8SdVKiNG7LAACxgfA")

#checkout the dataset
str(pxd)
head(pxd)
tail(pxd)
class(pxd)

#pull the desired pricing data
pxd_close <- pxd[, "Close", drop = FALSE]

#inspect head
head(pxd_close)

#create a new dataframe with dates and closing prices
rownames(pxd_close) <- pxd$Date

#inspect head
head(pxd_close)

#subset the data
price_c <- pxd_close["2016-05-02",]
price_c      
price_h <- pxd_close["2016-04-25",]
price_h

#plot data
plot(pxd$Close, type = "l", col = "blue", lwd = 2, ylab = "Close", 
     main = "Daily Closing Price of PXD", xlim = c(4000, 1))

