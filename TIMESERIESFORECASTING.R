 data("AirPassengers")
 AP <- AirPassengers
 plot(AP, ylab = "Passengers (1000s)", type = "o", pch = 20)

 View(AirPassengers)
 #This tells you that the data series in a time series format
 class(AirPassengers)
 #This is start of the time series
 start(AirPassengers)
 #This is end of the series
 end(AirPassengers)
 frequency(AirPassengers) 
 #The cycle of  this time series is 12 months in a year
 summary(AirPassengers) 
 #The number of passengers are distributed aross the spectrum
 plot(AirPassengers) 
 abline(reg=lm(AirPassengers~time(AirPassengers))) 
 #This will print cycle across the years
 cycle(AirPassengers) 
 #This will aggregate the cycles and display a year on year trend
 plot(aggregate(AirPassengers,FUN=mean))
 
 #This will give sense on seasonal effect
 boxplot(AirPassengers~cycle(AirPassengers))
 library(tseries)
 decomposeAP <- decompose(AP,"multiplicative")
 decomposeAP
 plot(decomposeAP)
 adf.test(AP)
 adf.test(diff(log(AirPassengers)), alternative="stationary", k=0) 
 plot(acf(AP,plot=F))+ labs(title="Correlogram of AP from 1949 to 1961")
  
 #ACF plots
 acf(log(AirPassengers))
 acf(diff(log(AirPassengers))) 
 pacf(diff(log(AirPassengers))) 
 #Model fitting
 library(forecast)
 (fit <- arima(log(AirPassengers), c(2, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12)))
 APforecast <- predict(fit,n.ahead = 10*12)
 APforecast
 #Prediction
 pred <- predict(fit, n.ahead = 10*12)
 pred
 ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))
 
 
 
 
 