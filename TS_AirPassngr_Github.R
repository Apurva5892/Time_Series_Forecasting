ap <- AirPassengers

ap

str(ap)

plot(ap)

start(ap)

end(ap)

ap

reg <- lm(ap~time(ap))

abline(coef(reg),lwd = 2)

cycle(ap)

plot(aggregate(ap,FUN = sum))

plot(aggregate(ap,FUN =mean))

# THIS PACKAGE IS USED FOR TESTING DICKEY FULLER TEST & PROVIDES LOT OF ADITIONAL FEATURES

install.packages("urca")
library(urca)

library(tseries)
library(forecast)

class(ap)

ap
length(ap)


adf.test(x =ap)
?nlags

# adf.test automatically DETRENDS THE TIME SERIES

adf.test(ap)

# HERE LAG IS ONLY TAKING TILL 69  DONT UDSTND WHY

# AS YOU INCREASE THE NO OF LAGS IT WILL BE TOUGH MAKING SERIES STATIONARY

# ALWAYS USE K-VALUE AS ZERO

ap5 <- adf.test(x = ap ,alternative = "stationary",k = 0)
?adf.test()
?ur.df

# ur.df IS USED FOR UNIT ROOT TESTING

# AS THE RESULT COMES IT IS NOT STATIONARY MAKING IT STATIONARY


# ap3 <- adf.test(diff(log(ap)),alternative = 'stationary',k = 0)
# ap3



# ap4 <- adf.test(diff(log(ap)),alternative = 'stationary',k = trunc((length(ap)-1)^(1/3)))
# ap4
# plot(ap4)


# ANOTHER WAY TO DETREND THE TIME SERIES

m <- lm(coredata(ap) ~ index(ap))
m
plot(m)
?coredata
coredata(ap) # GIVES YOU THE ACTUAL DATA
l<-length(coredata(ap))


?index
index(ap)
ap

detr <- zoo(resid(m),index(ap))

plot(detr)

adf.test(x = detr,alternative = "stationary", k = 5)

acf(ap)

acf(log(ap))

# NOW THE ABOVE PLOTS OF ACF SHOWS THAT TS IS STILL NOT STATIONARY
# WHEN TS IS STATIONARY      ==  THE ACF PLOT SHOWS SUDDEN CUT OFF
# WHEN TS IS NOT STATIONARY  ==  THE ACF PLOT SHOWS SLOW DECRESE IN THE GRAPH 

# THAT IS WHY WE NEED TO DIFFERENCE THE LOG'S OF TIME SERIES 
acf(diff(log(ap)))

par(mfrow=c(1,3))

pacf(ap)
pacf(log(ap))


par(mfrow=c(1,2))

acf(ap)

pacf(ap)

fit <- auto.arima(log(ap))
fit

pred <- predict(fit,n.ahead = 12)

# PREDICTION GIVES YOU THE FORECAST & STANDARD ERRORS

pred

# NOW THE ACTUAL PREDICTED VALUES ARE DERIVED BY TAKING ANTI-LOG

pred1 <- NA
pred1$pred <- 2.718^pred$pred

pred1$se <- 2.718^pred$se
pred1$se

# 2.718 IS TAKEN FOR ANTI LOG

ts.plot(ap,2.718^pred$pred, log = "y", lty = c(1,3))

