# Airpassengers dataset
library(forecast)
library(timeSeries)

data("AirPassengers")
View(AirPassengers)
plot(AirPassengers)

class(AirPassengers)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)

plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
cycle(AirPassengers)
plot(aggregate(AirPassengers,FUN=mean))

boxplot(AirPassengers~cycle(AirPassengers))

acf(log(AirPassengers))
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

fit <- arima(log(AirPassengers), c(0, 1, 1),
             seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))

h1 <- HoltWinters(AirPassengers, alpha="0.5", beta=0.3, gamma=0.2, seasonal = "multiplicative")
plot(h1)
h2 <- HoltWinters(AirPassengers, alpha="0.5", beta=0.3, gamma=0.2, seasonal = "additive")
plot(h2)
h3 <- HoltWinters(AirPassengers, alpha="0.5", beta=0.3, gamma=NULL, seasonal = "multiplicative")
plot(h3)
h3 <- HoltWinters(AirPassengers, alpha="0.5", beta=0.3, gamma=NULL, seasonal = "additive")
plot(h3)
h4 <- HoltWinters(AirPassengers, alpha="0.5", beta=NULL, gamma=NULL, seasonal = "additive")
plot(h4)

predict(h4)
predict(h4, n.ahead = 12)
predict(h1, n.ahead = 12)
predict(h1, n.ahead = 120)

p1 <- predict(h1, n.ahead = 12)
plot(p1)

h5 <- HoltWinters(AirPassengers, alpha=0.5, beta=0.5, gamma=0.5, seasonal = "multiplicative")
plot(h5)
h5 <- HoltWinters(AirPassengers, alpha=0.75, beta=0.2, gamma=0.1, seasonal = "multiplicative")
plot(h5)
h6 <- HoltWinters(AirPassengers, alpha=0.1, beta=0.5, gamma=0.8, seasonal = "multiplicative")
plot(h6)

m1 <- auto.arima(AirPassengers)
confint(m1)
tsdiag(m1)
data("CO2")
m <- stl(co2, s.window = 12)
plot(m)
m2 <- auto.arima(co2)
confint(m2)
tsdiag(m2)
# comparing two models, HoltWinters and ARIMA 
ph <- predict(h6, n.ahead = 12)
pa <- predict(m1, n.ahead = 12)
plot(pa$pred)
lines(ph, lty="dashed")

########################################################################
# time series of the number of births per month in New York city is seasonal 
#with a peak every summer and trough every winter

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
View(births)

# To make time series dataset
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
start(birthstimeseries)
end(birthstimeseries)
frequency(birthstimeseries)
class(birthstimeseries)
cycle(birthstimeseries)
plot(birthstimeseries)
# decompose() function
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal
plot(birthstimeseriescomponents)
# stl function
ts_beer = ts(birthstimeseries, frequency = 12)
stl_beer = stl(ts_beer, "periodic")
plot(stl_beer)
# regression line linear model
plot(birthstimeseries)
abline(reg = lm(birthstimeseries~time(birthstimeseries)))
plot(aggregate(birthstimeseries, FUN = mean))
# monthly boxplot
boxplot(birthstimeseries~cycle(birthstimeseries))
# auto corelation function, partial corelation function
acf(log(birthstimeseries))
acf(diff(log(birthstimeseries)))
pacf(diff(log(birthstimeseries)))
#ARIMA auto regression and moving avarage
fit <- arima(log(birthstimeseries), c(0, 1, 1),
             seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(birthstimeseries,2.718^pred$pred, log = "y", lty = c(1,3))

birthstimeseriesforecasts <- HoltWinters(birthstimeseries)
birthstimeseriesforecasts
birthstimeseriesforecasts$fitted
plot(birthstimeseriesforecasts)

h1 <- HoltWinters(birthstimeseries, alpha="0.5", beta=0.3, gamma=0.2, seasonal = "multiplicative")
plot(h1)
h2 <- HoltWinters(birthstimeseries, alpha="0.5", beta=0.3, gamma=0.2, seasonal = "additive")
plot(h2)
h3 <- HoltWinters(birthstimeseries, alpha="0.5", beta=0.3, gamma=NULL, seasonal = "multiplicative")
plot(h3)
h3 <- HoltWinters(birthstimeseries, alpha="0.5", beta=0.3, gamma=NULL, seasonal = "additive")
plot(h3)
h1 <- HoltWinters(birthstimeseries, alpha="0.5", beta=NULL, gamma=NULL, seasonal = "additive")
plot(h1)

predict(h4)
predict(h4, n.ahead = 12)
predict(h1, n.ahead = 12)
predict(h1, n.ahead = 120)

p1 <- predict(h1, n.ahead = 12)
plot(p1)

h5 <- HoltWinters(birthstimeseries, alpha=0.5, beta=0.5, gamma=0.5, seasonal = "multiplicative")
plot(h5)
h5 <- HoltWinters(birthstimeseries, alpha=0.75, beta=0.2, gamma=0.1, seasonal = "multiplicative")
plot(h5)
h6 <- HoltWinters(birthstimeseries, alpha=0.1, beta=0.5, gamma=0.8, seasonal = "multiplicative")
plot(h6)

m1 <- auto.arima(birthstimeseries)
confint(m1)
tsdiag(m1)
# comparing two models, HoltWinters and ARIMA 
ph <- predict(h6, n.ahead = 12)
pa <- predict(m1, n.ahead = 12)
plot(pa$pred)
lines(ph, lty="dashed")

####################################################################
library(zoo)
birthdata_zoo <- as.zoo(birthstimeseries)
zoo_75 <- birthdata_zoo[1:126]
zoo_25 <- birthdata_zoo[126:length(birthdata_zoo)]

df_75 <- as.ts(zoo_75)
df_25 <- as.ts(zoo_25)
df_25

birthdata_Predict <- HoltWinters(df_75, alpha = 0.485, beta = 0.001, gamma = 0.158)
p <- predict(birthdata_Predict,  n.ahead = 12)
plot(birthdata_zoo,p)


rollmean(birthstimeseries,2)
########################################################################



