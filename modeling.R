#africa NAB
head(Africa_NAB,n=5)
plot(Africa_NAB$total,main = "Time Series Africa NAB",xlab = "Week ID",ylab = "Total pieces per week",type = "o",col="Blue")
acf(Africa_NAB$total,lag.max = length(Africa_NAB$Week.ID)-1)
#definate spike at 2, seasonality may be present
pacf(Africa_NAB$total,lag.max = length(Africa_NAB$Week.ID)-1)
#no spikes, no decay

#install.packages("forecast")
library(forecast)
#create a training set
model_set1 <- Africa_NAB[1:95,]
#fit arima
arima.model1= auto.arima(model_set1$total)
summary(arima.model1) #returns arima (1,0,0)
forecast1 <- forecast(arima.model1,h=10)
plot(forecast1)
fitted_forecast1 <- forecast1$mean
error1 = Africa_NAB$total[96:105] - fitted_forecast1
error1

##trying a neural network fit
neural.network1 = nnetar(model_set1$total)
forecast1_NN <- forecast(neural.network1)
plot(forecast1_NN)
fitted_forecast1_NN <- forecast1_NN$mean
error1_NN <- Africa_NAB$total[96:105] - fitted_forecast1_NN
plot(error1_NN,col="Red")
points(error1,col="Green") #Neural network does not do a good job

#Africa Spirits
plot(Africa_Spirits$total,main = "Time Series Africa Spirit",xlab = "Week ID",ylab = "Total pieces per week",type = "o",col="Blue")
acf(Africa_Spirits$total,lag.max = length(Africa_Spirits$Week.ID)-1)
pacf(Africa_Spirits$total,lag.max = length(Africa_Spirits$Week.ID)-1) #both suggest random shock
arima.model2 <- auto.arima(Africa_Spirits$total)
summary(arima.model2) #gives us arima(1,0,0)
forecast2 <- forecast(arima.model2,h=12)
plot(forecast2)

#LA drug Chem
plot(LA_Drug_Chemical$average,main = "Time Series LA Drug chemical",xlab = "Week ID",ylab = "Total pieces per week",type = "o",col="Blue")
acf(LA_Drug_Chemical$average) #white noise
arima.model3 <- auto.arima(LA_Drug_Chemical$average)
summary(arima.model3) #gives us arima(1,0,0)
forecast3 <- forecast(arima.model3,h=4)
plot(forecast3) ##No meaningful forecasts

#Africa wine
plot(Africa_Wine,main = "Time Series LA Drug chemical",xlab = "Week ID",ylab = "Total pieces per week",type = "o",col="Blue")
arima.model4 <- auto.arima(Africa_Wine$total)
summary(arima.model4) #gives us arima(1,0,0)
forecast4 <- forecast(arima.model4,h=10)
plot(forecast4) ##No meaningful forecasts



x <- ses(Africa_NAB$total[1:95], h=10, alpha=0.1, initial="simple")
plot(x)
x$model
x$residuals

