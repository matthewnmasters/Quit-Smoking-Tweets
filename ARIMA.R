#ARIMA Workbook
# https://otexts.com/fpp2/holt.html
library(dplyr)
library(forecast)
library(ggplot2)
temps<- fpp2::maxtemp

tweetData <- read.csv("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets/ARIMAtest.csv") %>% 
  group_by(tweetYear) %>% count(tweetMonth, wt=n) %>%
  mutate(newMonth = case_when(
    tweetYear == 2018 ~ tweetMonth,
    tweetYear == 2019 ~ tweetMonth +12L,
    tweetYear == 2020 ~ tweetMonth +24L
  ))

timeSeries <- ts(tweetData$n, start=1)
timeSeries2 <- ts(tweetData$n, start=1, frequency=12 ) #frequency=12 means each repeating time period is 12 (months in this case) long

autoplot(timeSeries)
autoplot(timeSeries2) #all the same

smoothed <- ses(timeSeries, h=5)
autoplot(smoothed) + autolayer(fitted(smoothed), series="Fitted")

tweetHolt <- holt(timeSeries, h=5)
autoplot(tweetHolt) + autolayer(fitted(tweetHolt), series="Fitted")
tweetHolt$fitted #fitted but not forecast observations
tweetHolt$x #original observations
tweetHolt$mean #forecasted observations (h)
tweetHolt$upper #gives upper bounds
tweetHolt$lower #gives lower bounds

tsCV(timeSeries, holt, h=1) #gets errors

tweetHWadd <- hw(timeSeries2, h=12, seasonal="additive") #had to alter the frequency in the time series from 1 to 12
autoplot(tweetHWadd) +autolayer(fitted(tweetHWadd), series="Fitted")

tweetHWmult <- hw(timeSeries2, h=12, seasonal="multiplicative") #had to alter the frequency in the time series from 1 to 12
autoplot(tweetHWmult) +autolayer(fitted(tweetHWmult), series="Fitted")

forecast(tweetHWadd) #gives us point forecasts and confidence intervals

tweetA_ARIMA <- auto.arima(timeSeries)
forecast(tweetA_ARIMA, h=20) #lets try with daily since this gives us identical responses

tweetData <- read.csv("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets/dailydataARIMA.csv") %>% 
  select(-newDate) %>% rename(point = X)

daily <- ts(tweetData$n, start=1, frequency=7)

try1 <- auto.arima(daily) # gives us 
try1
autoplot(forecast(try1, h=30))

try1monthly <- auto.arima(timeSeries2) #flat...
autoplot(forecast(try1monthly))

try2 <- Arima(daily, order=c(1,0,0), seasonal=c(1,1,0))
autoplot(forecast(try2, h=90))

try2monthly<- Arima(timeSeries2, order=c(2,0,0), seasonal=c(0,1,1))
forecast(try2monthly, h=12)
autoplot(forecast(try2monthly, h=12)) #better
Box.test(try2monthly$residuals, lag=12, type="Ljung-Box")
autoplot(try2monthly$fitted)
# Let's try decomposing per https://www.simplilearn.com/tutorials/data-science-tutorial/time-series-forecasting-in-r
plot(decompose(timeSeries2, "additive"))
plot(decompose(daily, "additive"))


#these are fine but I feel like im just kind of guessing and eyeballing the ARIMA order and seasonal components.




autoplot(temps)
x<-holt(temps, h=5, exponential = TRUE)
autoplot(x)

y<- fpp2::arrivals[,"Japan"]
autoplot(y)  

japarrival<- auto.arima(y)  
autoplot(forecast(japarrival))

z<- fpp2::arrivals[,"NZ"]
autoplot(z)
autoplot(decompose(z))
zmod <- auto.arima(z)
autoplot(forecast(z))
