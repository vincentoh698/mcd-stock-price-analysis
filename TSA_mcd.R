library(forecast)
library(tseries)

data <- read.csv("C:/Users/USER/Desktop/Mcd/Mcd_stockprice2017-2023.csv", header = TRUE, sep = ",")
data = (data[1:1742,])
data$Volume = as.numeric(data$Volume)
data$Date = as.Date(data$Date, format = '%m/%d/%Y')
data.ts = ts(rev(data$Volume), frequency = 5)
ts.plot(data.ts)
adf.test(data.ts) #p-value = 0.01 -> stationary

par(mfrow = c(1, 2))
acf(data.ts)
pacf(data.ts)

auto.arima(data.ts, stationary = TRUE)
parameters = array(c(c(5,0,0), c(2,0,1), c(1,0,0)), dim = c(3,3))
for (i in 1:3){
  fit = arima(data.ts, order = c(parameters[1,i], parameters[2,i], parameters[3,i])) 
  summary(fit)
  checkresiduals(fit)
  shapiro.test(fit$residuals)
  Box.test(fit$residuals)
}
