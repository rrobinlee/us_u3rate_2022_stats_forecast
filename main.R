##################################
#  Preamble
#  Robin Lee, Ellen Wei, Xuxin Zhang
#  2/27/2023
##################################
library(Quandl)
Quandl.api_key("")

#### I. Introduction

unemployment_rate = Quandl(code="FRED/UNRATENSA",
                           type="ts",  
                           collapse="monthly", 
                           order="asc", 
                           start_date="1980-01-01",
                           end_date="2019-01-01",
                           meta=TRUE)

housing_supply_ratio = Quandl(code="FRED/MSACSRNSA", 
                              type="ts", 
                              collapse="monthly",
                              order="asc", 
                              start_date="1980-01-01",
                              end_date="2019-01-01",
                              meta=TRUE)

federal_funds_effective_rate = Quandl(code='FRED/FEDFUNDS', 
                                      type="ts",  
                                      collapse="monthly", 
                                      order="asc",
                                      start_date="1980-01-01",
                                      end_date="2019-01-01", 
                                      meta=TRUE)

library(tidyverse)
library(dygraphs)
### Visualize the three ts() in the dygraph
dygraph(cbind(unemployment_rate,
              housing_supply_ratio, 
              federal_funds_effective_rate),
        ylab="Rate/ Ratio", 
        xlab= "Time",
        main="How does unemployment rate, housing supply ratio,\n and federal funds effective rate vary together") %>% dyRangeSelector

#### I.3
#### Split the unemployment_rate into training and testing data, leaving 2018-02 to 2019-1 as the testing data
unemployment_rate_train = window(unemployment_rate, 
                                 start = c(1980, 1),
                                 end = c(2018, 1))

unemployment_rate_test = window(unemployment_rate,
                                start = c(2018, 2), 
                                end = c(2019, 1))

housing_supply_ratio_train = window(housing_supply_ratio, 
                       start = c(1980, 1),
                       end = c(2018, 1))

housing_supply_ratio_test = window(housing_supply_ratio,
                      start = c(2018, 2), 
                      end = c(2019, 1))


federal_funds_effective_rate_train = window(federal_funds_effective_rate,
                        start = c(1980, 1),
                        end = c(2018, 1))

federal_funds_effective_rate_test = window(federal_funds_effective_rate,
                       start = c(2018, 2), 
                       end = c(2019, 1))
#### II.(1)
#### Additive decomposition

unemployment_decompose = decompose(unemployment_rate_train,
                                   type = "additive")
plot(unemployment_decompose)
title("Additive decomposition of the raw unemployment data")

#### Presenting the additive decomposition of the unemployment_rate_train
unemployment_decompose = decompose(unemployment_rate_train, type = "additive")
plot(unemployment_decompose)
title("Additive decomposition of the unemployment data")

#### II.(2)
#### Draw the seasonal boxplot for unemployment_rate_train
boxplot(unemployment_rate_train ~ cycle(unemployment_rate_train),
        xlab = "Month",
        ylab = "Unemployment Rate")
title("Seaonal boxplot for unemployment data")

#### III
#### Draw the ACF & PACF of the random term
random.term = decompose(unemployment_rate_train, type="additive")$random
random.term = random.term[!is.na(random.term)]
par(mfrow=c(1,2))
acf(random.term, main="")
title("ACF of the unemployment data")
pacf(random.term, main="")
title("PACF of the unemployment data")
dev.off()

#### IV
#### Use raw training dependent variable 
#### fit to exponential smoothing model and forecast
unemployment_train_exp = HoltWinters(unemployment_rate_train, seasonal="add")
predict(unemployment_train_exp,n.ahead=12)
unemployment_train_exp$SSE
forecasts=predict(unemployment_train_exp,n.ahead=12)
#### compare the test set of the raw data with the forecast
forecasts
unemployment_rate_test

### forecast as many periods ahead as observations in the test set
forecasts2 = predict(unemployment_train_exp,
                     n.ahead= length(unemployment_rate_test))
### final fitted model
ts.plot(unemployment_rate_train,predict(unemployment_train_exp,n.ahead=12),
        col=c(1:2), 
        main="Seasonal Exponential Smoothing \nfor Unemployment Rate", 
        lty=1:2, ylab="Percent")

lines(fitted(unemployment_train_exp)[,1], type="l",col="red")

legend("topleft", legend=c("Unemployment","Forecast","Fitted"),
       lty=c(1,2,1), col=c("black", "red", "red"))

#### V
y = unemployment_rate_train
n = length(y)
time = seq(1:length(y))
That=lm(y~time)
summary(That)
That2 = lm(y~time+I(time^2))
That3 = lm(y~time+I(time^2)+I(time^3))
That4 = lm(y~time+I(time^2)+I(time^3)+I(time^4))
That5 = lm(y~time+I(time^2)+I(time^3)+I(time^4)+I(time^5))
summary(That4)
plot(time, y, type="l")
lines(time, That4$fitted.values, col="red") #4 seems to be the best
yhat = fitted(That4)
## determine goodness of fit: residuals
plot(ts(scale(That$residuals)))

### forecast the trend start = c(2018, 2), end = c(2019, 1)
# seasonal components
seasonal = unemployment_decompose_add$seasonal
forecast = predict(That4, newdata = data.frame(time =c(458:469)))
forecast_sc = forecast + seasonal[2:13]
forecast_sc
plot(c(time, 458:469), c(y, forecast_sc), lty=1, col=c("black"), type="l",
     xlab="Time", ylab="Percent", 
     main= "Polynomial Regression Smoother and Forecast")
lines(time, yhat, col="red", lty=1, lwd=2)
abline(v=n, lty=2, col ="blue")

##time plot of residuals

ts.plot(scale(That2$residuals))
ts.plot(scale(That3$residuals))
ts.plot(scale(That5$residuals))

ts.plot(scale(That4$residuals), 
        main = "Scaled Residuals of 4th Order Polynomial Model",
        ylab="Scaled Residuals")


#### VI
exponential_rmse = sqrt(mean((unemployment_rate_test -forecasts2)^2))

polynomial_rmse = sqrt(mean((unemployment_rate_test -forecast_sc)^2))
exponential_rmse 
polynomial_rmse

average_forecasts = (forecasts2 + forecast_sc)/2
average_rmse = sqrt(mean((unemployment_rate_test -average_forecasts)^2))
average_rmse 

#### VII
y1 = diff(unemployment_rate_train, lag = 1, differences = 1)
y2 = diff(unemployment_rate_train, lag = 12, differences = 1)
y3 = diff(y1, lag = 12, differences = 1)

par(mfrow=c(3,2))
acf(y1, lag = 50, main = "ACF of (1-B)yt")
pacf(y1, lag = 50, main = "PACF of (1-B)yt")
acf(y2, lag = 50, main = "ACF of (1-B^12)yt")
pacf(y2, lag = 50, main = "PACF of (1-B^12)yt")
acf(y3, lag = 50, main = "ACF of (1-B)(1-B^12)yt")
pacf(y3, lag = 50, main = "PACF of (1-B)(1-B^12)yt")


arima.model.1 = arima(unemployment_rate_train,
                      order = c(3,1,0), 
                      seasonal = list(order = c(1,1,0), 12))
acf(arima.model.1$residuals, lag = 100, main = "ACF of residuals of model 1")

arima.model.1$aic


arima.model.2 = arima(unemployment_rate_train,
                      order = c(5,1,0), 
                      seasonal = list(order = c(1,1,0), 12))
acf(arima.model.2$residuals, lag = 100, main = "ACF of residuals of model 2")
arima.model.2$aic

arima.model.3 = arima(unemployment_rate_train,
                      order = c(3,1,1), 
                      seasonal = list(order = c(1,1,1), 12))

acf(arima.model.3$residuals, lag = 100, main = "ACF of residuals of model 3")
arima.model.3$aic
length(arima.model.3$residuals)
Box.test(arima.model.3$residuals, lag =456, type="Ljung")
hist(arima.model.3$residuals, main = "Histogram of ARIMA(3,1,1)(1,1,1)_12 Residuals", xlab = "Residuals")

forecastmodel1 <- arima.model.3
x.test <- unemployment_rate_test
x <- unemployment_rate
forecast=predict(forecastmodel1,12) 
forecast.value=ts((forecast$pred), start=start(x.test), freq=12)

ci.low= ts((forecast$pred-1.96*forecast$se), start=start(x.test),freq=12)
ci.high=ts((forecast$pred+1.96*forecast$se),start=start(x.test),freq=12)

data.frame(x.test, ci.low, forecast.value, ci.high, forecast$se)

## Plot forecast 
par(mfrow=c(2,1))
ts.plot(cbind(x, forecast.value, ci.low, ci.high),lty=c(1,3,2,3), 
        col=c("black","red","blue","blue"), main="Predicted monthly values from best
model",ylab="unemployment")
legend(x=1980, y=1, lty=c(1,3,2,2), text.col=c("black","red","blue","blue"), 
       legend=c("unemployment", "forecast", "CIlow", "CIhigh"),text.font=1, cex=0.5)
abline(v=2018.1)
### Plot only the forecast and the observed data of last 12 months

ts.plot(cbind(x.test, forecast.value,ci.low, ci.high),lty=c(1,3,2,2),
        col=c("black","red","blue","blue"),
        main="Forecast and observed data 2019",ylab="unemployment")
legend(x=2018.7,y=4.5, lty=c(1,3,2,2), text.col=c("black","red","blue","blue"), 
       legend=c("unemployment", "forecast", "CIlow", "CIhigh"),text.font=1, cex=0.5)
dev.off()
#### Compute the root mean square error of the forecast.

last12data=x[(length(x)-12+1):length(x)]
last12data
forecast.value

MSE = sqrt((sum(last12data-forecast.value)^2)/12)
MSE     # view the MSE 
# Histogram for ARIMA Residuals
hist(arima.model.3$residuals, main = "Histogram of ARIMA(3,1,1)(1,1,1)_12 Residuals", xlab = "Residuals")

### VIII
library(nlme)
housing_supply_ratio_train = window(housing_supply_ratio, 
                                    start = c(1980, 1),
                                    end = c(2018, 1))
federal_funds_effective_rate_train = window(federal_funds_effective_rate, 
                                            start = c(1980, 1),
                                            end = c(2018, 1))
# multiple linear regression
model <- lm(unemployment_rate_train ~ housing_supply_ratio_train + federal_funds_effective_rate_train)
ts.plot(model$fitted.values, unemployment_rate_train,
        col = c("red", "black"),
        main = "Raw and fitted values")

ts.plot(model$residuals, 
        main = "Residuals of Multiple Linear Regression",
        ylab = "Residuals")
abline(h = 0, lty =3)
par(mfrow=c(1,2))
acf(model$residuals, lag=100, 
    main = "ACF of Residuals")
pacf(model$residuals, lag =100,
     main = "PACF of Residuals")

#residuals have a stationary pattern
# identify model for residuals of regression model
for (i in 1:20){
  resmodel = arima(model$residuals, order=c(i,0,0), 
                   seas=list(order=c(0,0,0), 12), include.mean=F)
  acf(resmodel$residuals, main= i, lag=100)
  cat(i, "aic: ", resmodel$aic, "\n")
}

## selected AR(16)
resmodel16= arima(model$residuals, order=c(16,0,0), 
                  seas=list(order=c(0,0,0), 12), include.mean=F)
resmodel16
par(mfrow=c(1,2))
plot(resmodel16$residuals,
     main = "Residuals of ARIMA model AR(16) of Residuals",
     ylab = "Residuals")
abline(h = 0, lty =3, col = "grey")
acf(resmodel19$residuals, 
    main = "ACF of ARIMA model AR(16) of Residuals",
    lag=50)

# GLS
resmodel16$coef #invalid for gls()
modelgls16 = gls(unemployment_rate_train ~ housing_supply_ratio_train + federal_funds_effective_rate_train,
                 correlation = corARMA(p=16))
summary(modelgls16)
modelgls16$modelStruct$corStruct #corARMA coefficients

plot(y=residuals(modelgls16, type="normalized"),
     x=as.vector(time(unemployment_rate_train)),
     type="l",
     main = "Residuals of GLS",
     ylab="Uncorrelated Residuals",
     xlab="Time")
abline(h=0, lty =3)
acf(ts(residuals(modelgls16, type="normalized")),
    main = "ACF of Residuals of GLS")
modelgls16$coefficients

ts.plot(modelgls16$fitted, unemployment_rate_train,
        col = c("red", "black"),
        main = "Raw and fitted values")

## forecast
iv = ts.union(housing_supply_ratio_test, federal_funds_effective_rate_test)

forecast_gls = predict(modelgls16, newdata = data.frame(iv))
rmse_gls = sqrt(mean((unemployment_rate_test -forecast_gls)^2))
rmse_gls 

## plot forecast with data
gls_forecast_ts = ts(forecast_gls, start=c(2018,2), frequency=12)
gls_fitted_ts = ts(modelgls16$fitted, start=c(1980,1), frequency=12)

ts.plot(unemployment_rate, gls_forecast_ts, 
        col =c(1:2), lty=1:2, main="GLS model of Unemployment rate on \nHousing Supply Ratio and Federal Funds Effective Rate")
lines(gls_fitted_ts, type="l",col="red")
abline(v=c(2018,1), col="blue")
legend("topright", legend=c("Unemployment","Forecast","Fitted"),
       lty=c(1,2,1), col=c("black", "red", "red"), 
       cex=0.5)

### IX
## housing_supply_ratio
ts.plot(housing_supply_ratio_train,main = "Time plot for housing supply ratio training data")
par(mfrow=c(1,2))
acf(housing_supply_ratio_train, lag=100)
pacf(housing_supply_ratio_train, lag=100)

## Regular Difference
temp1 = diff(housing_supply_ratio_train, lag=1, diff=1)
## Seasonal Difference
temp2 = diff(housing_supply_ratio_train, lag=12, diff=1)
## Seasonal Difference of the regular difference
temp3 = diff(temp1, lag = 12, diff = 1)

par(mfrow=c(3,2))
acf(temp1, lag = 50, main = "")
title("ACF of (1-B)x_1t")
pacf(temp1, lag = 50, main = "")
title("PACF of (1-B)x_1t")
acf(temp2, lag = 50, main = "")
title("ACF of (1-B^12)x_1t")
pacf(temp2, lag = 50, main = "")
title("PACF of (1-B^12)x_1t")
acf(temp3, lag = 50, main = "")
title("ACF of (1-B^12)(1-B)x_1t")
pacf(temp3, lag = 50, main = "")
title("PACF of (1-B^12)(1-B)x_1t")


# federal_funds_rate
ts.plot(federal_funds_effective_rate_train)
par(mfrow=c(1,2))
acf(federal_funds_effective_rate_train, lag=100)
pacf(federal_funds_effective_rate_train, lag = 100)

## Regular Difference
temp1 = diff(federal_funds_effective_rate_train, lag=1, diff=1)
## Seasonal Difference
temp2 = diff(federal_funds_effective_rate_train, lag=12, diff=1)
## Seasonal Difference of the regular difference
temp3 = diff(temp1, lag=12, diff=1)

par(mfrow=c(3,2))
acf(temp1, lag = 50, main = "")
title("ACF of (1-B)x_2t")
pacf(temp1, lag = 50, main = "")
title("PACF of (1-B)x_2t")
acf(temp2, lag = 50, main = "")
title("ACF of (1-B^12)x_2t")
pacf(temp2, lag = 50, main = "")
title("PACF of (1-B^12)x_2t")
acf(temp3, lag = 50, main = "")
title("ACF of (1-B^12)(1-B)x_2t")
pacf(temp3, lag = 50, main = "")
title("PACF of (1-B^12)(1-B)x_2t")

### 2 put stationary time series into multiple time series object
#unemployment_rate: r+s
temp = diff(unemployment_rate, lag=1, diff=1)
unemployment_rate_diff = diff(temp, lag=12, diff=1)
#housing_supply_ratio: r+s
temp = diff(housing_supply_ratio, lag=1, diff=1)
housing_diff = diff(temp, lag=12, diff=1)

#federal_funds_rate: r
fed_rate_diff = diff(federal_funds_effective_rate, lag=1, diff=1)

#cbind, ccf
mts = cbind(unemployment_rate_diff, 
            housing_diff, 
            fed_rate_diff)

## We select the time window from Feb 1981 to Jan 2018 to remove the NAs
mts = window(mts, start= c(1981, 2), end = c(2018,1))
colnames(mts) = c("Unemployment Rate",
                  "Housing Supply Raio",
                  "Fed Effective Rate")

## CCF between the differenced unemployment_train and housing_supply_ratio
acf(mts[,c(1,2)],lwd=1.5,cex=1.5,lag=50)  ## all correlation information in one image

## CCF between the differenced unemployment_train and fed_reserver_rate
acf(mts[,c(1,3)],lwd=1.5,cex=1.5,lag=50)  ## all correlation information in one image



## VAR

library(vars)

### Fit VAR(6) to unemployment and housing_supply_ratio
VAR.unemployment.house <- VAR(mts[,c(1, 2)], p=6,type=)
coef(VAR.unemployment.house)
summary(VAR.unemployment.house)
# Residual plots of VAR model fitted should be multivariate white noise.
acf(resid(VAR.unemployment.house))


### Fit VAR(2) to unemployment and housing_supply_ratio
VAR.unemployment.fed <- VAR(mts[,c(1, 3)], p=2,type=)
coef(VAR.unemployment.fed)
summary(VAR.unemployment.fed)
# Residual plots of VAR model fitted should be multivariate white noise.
acf(resid(VAR.unemployment.fed))


##########
# Forecast 
##########

# Predict 12 months out of sample .

VAR.house.pred <- predict(VAR.unemployment.house, n.ahead=12)
VAR.house.pred
house.pred <- ts(VAR.house.pred$fcst$Unemployment.Rate[,1],
                 st=c(2018,2),fr=12) 
house.pred.low <-ts(VAR.house.pred$fcst$Unemployment.Rate[,2],
                    st=c(2018,2),fr=12) 
house.pred.upper<- ts(VAR.house.pred$fcst$Unemployment.Rate[,3],
                      st=c(2018,2),fr=12) 

VAR.fed.pred <- predict(VAR.unemployment.fed, n.ahead=12)
VAR.fed.pred
fed.pred <- ts(VAR.fed.pred$fcst$Unemployment.Rate[,1],
               st=c(2018,2),fr=12) 
fed.pred.low <-ts(VAR.fed.pred$fcst$Unemployment.Rate[,2],
                  st=c(2018,2),fr=12) 
fed.pred.upper<- ts(VAR.fed.pred$fcst$Unemployment.Rate[,3],
                    st=c(2018,2),fr=12)


# Put predicted values in plot
## Get the differenced test data
unemployment_rate_diff_test = window(unemployment_rate_diff,start= c(2018, 2), end = c(2019,1))

ts.plot(cbind(mts[,1], house.pred, house.pred.low, house.pred.upper,
              unemployment_rate_diff_test),
        lty=c(1,2,3,3,4), col=c("black","red", "blue","blue","black"), lwd=c(2,2,2,2,2),
        main = "Forecast of change in Unemployment with VAR(6) of Housing Supply Ratio",ylab="Unemployment",ylim=c(-2,3))

ts.plot(cbind(mts[,1], fed.pred, fed.pred.low, fed.pred.upper,
              unemployment_rate_diff_test),
        lty=c(1,2,3,3,4), col=c("black","red", "blue","blue","black"), lwd=c(2,2,2,2,2),
        main = "Forecast of change in Unemployment with VAR(2) of Federal Reserver Effective Rate",ylab="Unemployment",ylim=c(-2,3))


### RMSE
sqrt(mean((house.pred - unemployment_rate_diff_test)^2))
sqrt(mean((fed.pred - unemployment_rate_diff_test)^2))

##### Impulse response functions 
irf1=irf(VAR.unemployment.house, impulse = "Unemployment.Rate", response = c("Unemployment.Rate","Housing.Supply.Raio"), boot =
           FALSE,n.ahead=40,lwd=2)
irf2=irf(VAR.unemployment.house, impulse = "Housing.Supply.Raio", response = c("Unemployment.Rate","Housing.Supply.Raio"), boot =
           FALSE,n.ahead=40,lwd=2)
irf3=irf(VAR.unemployment.fed, impulse = "Unemployment.Rate", response = c("Unemployment.Rate","Fed.Effective.Rate"), boot =
           FALSE,n.ahead=40,lwd=2)
irf4=irf(VAR.unemployment.fed, impulse = "Fed.Effective.Rate", response = c("Unemployment.Rate","Fed.Effective.Rate"), boot =
           FALSE,n.ahead=40,lwd=2)

plot(irf1)
plot(irf2)
plot(irf3)
plot(irf4)
dev.off()

### X
# ARIMA
model1 <- arima.model.3
x.test <- unemployment_rate_test
x <- unemployment_rate
forecast=predict(model1,12) 
forecast.value = ts((forecast$pred), start=start(x.test), freq=12)


# Polynomial
polynomial <- ts(c(3.939979,3.589400,2.977189,2.851970,3.148521,3.003566,2.635782,
2.325321,2.078094,1.999822,1.871225,2.602641), start = c(2018,2), end = c(2019,1), frequency = 12)

# Holt Winters
unemployment_train_exp <- HoltWinters(unemployment_rate_train, seasonal = "add")
forecasts_hw <- predict(unemployment_train_exp, n.ahead = length(unemployment_rate_test))

# Average
avg <- ts(c(4.441674,4.249191,3.728389,3.779068,4.163007,4.137926,3.781335,3.320676, 
            3.155865,3.111732,3.230751,3.963826), start = c(2018,2), end = c(2019,1), frequency = 12)

# Plot
ts.plot(cbind(x.test, forecast.value, polynomial, gls_forecast_ts, forecasts_hw, avg),lty=c(1,3,2,3), 
        col=c("black","red", "blue","green", "orange", "purple"),
        main="Plot of Actual and Forecasted Values",ylab="unemployment")
legend(x=2018.7,y=4.5, lty=c(1,3,2,2,3,2), text.col=c("black","red","blue","green", "orange", "purple"), 
       legend=c("Actual", "ARIMA", "Polynomial Regression","Multiple Regression", "Holt-Winters", "Average"),
       text.font=1, cex=0.5)
abline(v=2018.1)
