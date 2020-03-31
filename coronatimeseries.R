cases=read.csv("C:\\Users\\Amine Boudlal\\Desktop\\Mathématiques\\daily-cases-covid-19.csv",sep=",")
# the dates are not in the proper format so we use the library anytime that helps us handle it 
install.packages("anytime")
cases$Date = anytime(as.factor(cases$Date))
coronaday=subset(cases,Entity!="World")
coronadaily=aggregate(coronaday["Daily.confirmed.cases..cases."], by=coronaday["Date"],sum)
plot(x=coronadaily$Date,y=coronadaily$Daily.confirmed.cases..cases.,main="COVID-19 cases by day",xlab="Days",ylab="Number of Cases",type='o',col="orange")
#creating a time series 
install.packages("forecast")
## Create a daily Date object
days=seq(as.Date("2019-12-31"), as.Date("2020-03-30"), by = "day")
covidts=ts(coronadaily$Daily.confirmed.cases..cases.,start=c(2019, as.numeric(format(days[1], "%j"))),frequency = 365)
#This time series contains the daily confirmed cases up to 30 MAR 2020 
# We're Gonna Start With Some Basic Time Series Analysis 
"Plotting The Time Series "
plot.ts(covidts,col="red",main="covid-19 cases-Daily")
#It is safe toassume that neither the multiplicative nor the additive model can fit right on sight 
"Seasonal Data Decomposition"
covidcomponents=decompose(covidts)
#We obtain an error message showing there are less than 2 periods 
"Non-Seasonal Data Decomposition"
library("TTR")
install.packages("smooth")
library("smooth")
#We are going to use the following function to determine the appropriate order"
#say we want predictions for the following 7 days counting today "
predictr=sma(covidts,h=14)
"so based on the criteria the fitting model is SMA(5) "
covidMA1=SMA(covidts,n=1) 
plot.ts(covidMA1,type='l',col="red",main="Predicted Model")
#Using The Exponetial Smoothing" 
covidexpsmo=HoltWinters(covidts,gamma=FALSE)
plot(covidexpsmo)
#Predictions"
library("forecast")
covidexpfor=forecast(covidexpsmo,h=14)
# 14 Days forecast 
plot(covidexpfor)
"Dickey-Fuller Tests"
library("tseries")
adf.test(covidts,alternative=c("stationary","explosive"),k=trunc((length(covidts)-1)^(1/3)))
#Dickey-Fuller Gives a p-value superior to 0.05 so we accept the null hypothesis that the series is not stationary
#KPSS Test 
kpss.test(covidts,null=c("Level", "Trend"))
pp.test(covidts,alternative=c("stationary","explosive"))
#KPSS gives us a pvalue superior to 0.05 so we refuse the null hypothesis 
# this time series is not stationnary so it is justified to use ARIMA(p,d,q) Models let's first determine the difference order 
forADF=ndiffs(covidts, test = "adf")
forKPSS =ndiffs(covidts, test = "kpss")
forpp = ndiffs(covidts, test = "pp")
#Each of the tests gave us a difference order 
#KPSS Test gave us a difference order of 2 while ADF and Phillips-Perron both gave d = 1
arifor1=auto.arima(covidts,d=2)
arima024= forecast(covidts,model=arifor1,h=13)
plot(arima024)
library("stats")
res=resid(arifor1)
Box.test(res,type="Ljung-Box")
#We accept the model propsed 
# let's compare the model to the actual values 
plot(x=covidts,type='l',col='red')
fitcovid=Arima(covidts,order=c(0,2,4))
lines(fitted(fitcovid),col='blue')
# this shows that the following model holds in all cases 
#I truly hope we are mistaken because this indicates that the tally would only go higher 