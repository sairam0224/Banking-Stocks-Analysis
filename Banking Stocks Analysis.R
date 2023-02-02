#Analyzing the Banking stocks

#DATE FORMAT - "YYYY-MM-DD"
#Orange candle - Loss(CLOSEPRICE<OPENPRICE)
#GREEN CANDLE - PROFIT(CLOSEPRICE> OPENPRICE)
#TRENDS - UP , DOWN , NEUTRAL OR HORIZONTAL

#Companies are "HDFC", "ICICI", "KOTAK MAHINDRA", "AXIS", INDUSIND"

#PERFORMANCE ANALYSIS OF HDFC BANK

library(quantmod)
library(tidyverse)
library(timetk)
library(tidyquant)
library(caret)
library(PerformanceAnalytics)

getSymbols("^BSESN") #GETTING SYMBOLS FOR BSE. It will represent the BSE- .BO
getSymbols("^NSEI") #GETTING SYMBOLS FOR NSE. It will represent the NSE- .NS

#1) Analyzing the HDFC BANK.NS Stock Perfomance 
dt="2012-01-01"
HDFC=getSymbols.yahoo("HDFCBANK.NS",from = dt, auto.assign=F)
view(HDFC)
head(HDFC)#First 6 rows of data
tail(HDFC)#last 6 rows of data
is.na(HDFC)#Finding the null values
sum(is.na(HDFC))
HDFC1=na.omit(dailyReturn(HDFC,type="log")) # Deleting na values
sum(is.na(HDFC1))

#Analyzing the HDFC data using visualization
#Line Plot- Close price
plot(HDFC$HDFCBANK.NS.Close, type="l")
hist(HDFC$HDFCBANK.NS.Adjusted)

#Understanding the chart of the stock performance after financial events
chartSeries(HDFC)
zoomChart("2016-11-09::")#The data of demonetisation 
zoomChart("2017-07-01::")#The date of GST has implememted
zoomChart("2020-03-25::")#Covid-19 lockdown in india

#TREND INDICATORS
#1. SIMPLE MOVING AVERAGE 
# IT IS A LAGGING INDICATOR
#SMA- 14, 50, 100, 200 DAYS MOVING AVERAGE
#SHORT TERM MOVING AVERAGE - 50 DAYS
# MEDIUM TERM MOVING AVERAGE - 100 DAYS
# LONG TERM MOVING AVERAGE - 200 DAYS
chartSeries(HDFC)
addSMA(60,col="red")
addSMA(120,col="yellow")
addSMA(230,col="green")

#Moving Averages act as Support or Resistance level
#It depends on time frame
#Cross Overs- Price Crosses above or below Moving average
#Average prices crosses below moving average- sell immediately
#Prices crosses above moving average- BUY

#Second startegy based on 2 moving averages
#When short term moving avg (60) crosses above long term moving avg(230)
#It is a buy signal-Golden cross
#when short term moving avg(60) crosses below long term moving avg(230)
#It is a sell signal-Death cross

#2. Volatility indicators - Standard deviation
sd(HDFC$HDFCBANK.NS.Close,na.rm=T)#HDFC SD

#3. BOLLINGER BANDS - IT IS A VOLATILITY INDICATOR
#ITS GIVE SUPPORT AND RESISTANCE LEVEL
chartSeries(HDFC)
addBBands() #ADD bollinger bands to charts
#UPPERBAND-RESISTANCE
#LOWER BAND-SUPPORT


#4. MOMENTUM INDICATORS
addRSI() #Relative Strength Index

addRSI(n=25)

#5. Volume Indicators- stock volumes
#it is leading indicator- chaikin oscillotor-mointor
#money flows IN and Out of market/stock
addChAD() #CHAIKIN Oscillator- it moves between -1 to 1

#6. MOVING AVAERAGE CONVERDENCE DIVERGENCE (MACD)
#IT IS LAGGING INDICATOR
addMACD()
#WHITE LINE- MACD LINE
#RED DOTTED LINE - SIGNAL 
#WHEN MACD FALLS BELOW SIGNAL LINE, IT IS A BEARSIH SIGNAL MEANS TIME TO SELL
#WHEN MACD RISES ABOVE SIGNAL LINE, IT IS A BULLISH SIGNAL MEANS TIME TO BUY

#2) Analyzing ICICIBANK STOCK PERFORMANCE 
ICICI=getSymbols.yahoo("ICICIBANK.NS",from = dt, auto.assign=F)
view(ICICI)
head(ICICI)#First 6 rows of data
tail(ICICI)#last 6 rows of data
is.na(ICICI)#Finding the null values
sum(is.na(HDFC))
ICICIBK=na.omit(dailyReturn(ICICI,type="log")) # Deleting na values
sum(is.na(ICICIBK))

#Analyzing the ICICI data using visualization
#Line Plot- Close price
plot(ICICI$ICICIBANK.NS.Close,type="l")#CLOSING STOCK 
hist(ICICI$ICICIBANK.NS.Adjusted)#ADJUSTED STOCK PRICE

chartSeries(ICICI)

#Understanding the chart of the stock performance after financial events
zoomChart("2016-11-09::")#The data of demonetisation 
zoomChart("2017-07-01::")#The date of GST has implememted
zoomChart("2020-03-25::")#Covid-19 lockdown in india

#1. SIMPLE MOVING AVERAGE
chartSeries(ICICI)
addSMA(60,col="red")
addSMA(120,col="yellow")
addSMA(230,col="green")

#2. Volatility indicators - Standard deviation
sd(ICICI$ICICIBANK.NS.Close,na.rm=T)#ICICI SD

#3. BOLLINGER BANDS
addBBands()

#4. MOMENTUM INDICATORS
addRSI() #Relative Strength Index
addRSI(n=25)

#5. Volume Indicators- stock volumeS
addChAD()

#6. MOVING AVAERAGE CONVERDENCE DIVERGENCE (MACD) 
addMACD()

#3) ANALYZING KOTAK MAHINDRA STOCK PERFORMANCE
KOTAK=getSymbols.yahoo("KOTAKBANK.NS",from = dt, auto.assign=F)
view(KOTAK)
head(KOTAK)#First 6 rows of data
tail(KOTAK)#last 6 rows of data
is.na(KOTAK)#Finding the null values
sum(is.na(KOTAK))
KOTAKBK=na.omit(dailyReturn(KOTAK,type="log")) # Deleting na values
sum(is.na(KOTAKBK))

#Analyzing the KOTAK data using visualization
#Line Plot- Close price
plot(KOTAK$KOTAKBANK.NS.Close, type="l")#CLOSING STOCK PRICE
hist(KOTAK$KOTAKBANK.NS.Adjusted)#ADJUSTED STOCK PRICE

chartSeries(KOTAK)

#Understanding the chart of the stock performance after financial events
zoomChart("2016-11-09::")#The data of demonetisation 
zoomChart("2017-07-01::")#The date of GST has implememted
zoomChart("2020-03-25::")#Covid-19 lockdown in india

# SIMPLE MOVING AVERAGE
chartSeries(KOTAK)
addSMA(60,col="red")
addSMA(120,col="yellow")
addSMA(230,col="green")

#2. Volatility indicators - Standard deviation
sd(KOTAK$KOTAKBANK.NS.Close,na.rm=T)#KOTAK SD

#3. BOLLINGER BANDS
addBBands()

#4. MOMENTUM INDICATORS
addRSI() #Relative Strength Index
addRSI(n=25)

#5. Volume Indicators- stock volumeS
addChAD()

#6. MOVING AVAERAGE CONVERDENCE DIVERGENCE (MACD) 
addMACD()

#4) ANALYZING AXIS BANK STOCK PERFORMANCE
AXIS=getSymbols.yahoo("AXISBANK.NS",from = dt, auto.assign=F)
view(AXIS)
head(AXIS)#First 6 rows of data
tail(AXIS)#last 6 rows of data
is.na(AXIS)#Finding the null values
sum(is.na(AXIS))
AXISBK=na.omit(dailyReturn(AXIS,type="log")) # Deleting na values
sum(is.na(AXISBK))

#Analyzing the AXIS data using visualization
#Line Plot- Close price
plot(AXIS$AXISBANK.NS.Close, type="l")#CLOSING STOCK PRICE
hist(AXIS$AXISBANK.NS.Adjusted)#ADJUSTED STOCK PRICE

chartSeries(AXIS)

#Understanding the chart of the stock performance after financial events 
zoomChart("2016-11-09::")#The data of demonetisation 
zoomChart("2017-07-01::")#The date of GST has implememted
zoomChart("2020-03-25::")#Covid-19 lockdown in india

# SIMPLE MOVING AVERAGE
chartSeries(AXIS)
addSMA(60,col="red")
addSMA(120,col="yellow")
addSMA(230,col="green")

#2. Volatility indicators - Standard deviation
sd(AXIS$AXISBANK.NS.Close,na.rm=T)#AXIS SD

#3. BOLLINGER BANDS
addBBands()

#4. MOMENTUM INDICATORS
addRSI() #Relative Strength Index
addRSI(n=25)

#5. Volume Indicators- stock volumeS
addChAD()

#6. MOVING AVAERAGE CONVERDENCE DIVERGENCE (MACD) 
addMACD()

#5) ANALYZING INDUSIND BANK STOCK PERFORMANCE
INDUS=getSymbols.yahoo("INDUSINDBK.NS",from = dt, auto.assign=F)
view(INDUS)
head(INDUS)#First 6 rows of data
tail(INDUS)#last 6 rows of data
is.na(INDUS)#Finding the null values
sum(is.na(INDUS))
INDUSBK=na.omit(dailyReturn(INDUS,type="log")) # Deleting na values
sum(is.na(INDUSBK))

#Analyzing the INDUSIND data using visualization
#Line Plot- Close price
plot(INDUS$INDUSINDBK.NS.Close, type="l")#CLOSING STOCK PRICE
hist(INDUS$INDUSINDBK.NS.Adjusted)#ADJUSTED STOCK PRICE

chartSeries(INDUS)

#Understanding the chart of the stock performance after financial events 
zoomChart("2016-11-09::")#The data of demonetisation 
zoomChart("2017-07-01::")#The date of GST has implememted
zoomChart("2020-03-25::")#Covid-19 lockdown in india

# SIMPLE MOVING AVERAGE
chartSeries(INDUS)
addSMA(60,col="red")
addSMA(120,col="yellow")
addSMA(230,col="green")

#2. Volatility indicators - Standard deviation
sd(INDUS$INDUSINDBK.NS.Close,na.rm=T)#INDUSIND SD

#3. BOLLINGER BANDS
addBBands()

#4. MOMENTUM INDICATORS
addRSI() #Relative Strength Index
addRSI(n=25)

#5. Volume Indicators- stock volumeS
addChAD()

#6. MOVING AVAERAGE CONVERDENCE DIVERGENCE (MACD) 
addMACD()


#PORTFOLIO ANALYTICS
library(PortfolioAnalytics)
symbols=c("HDFCBANK.NS","ICICIBANK.NS","KOTAKBANK.NS","AXISBANK.NS","INDUSINDBK.NS")
portfolio=lapply(symbols,function(x){
  dailyReturn(na.omit(getSymbols(x,
                                 from=as.Date("2012-01-01"),
                                 auto.assign = F)))
})
portfolio= do.call(merge.xts,portfolio)
colnames(portfolio)=c("HDFC","ICICI","KOTAK","AXIS","INDUSIND")
head(portfolio)#dailyreturns of the companies
tail(portfolio)

#Assigning the equal weights of each companies
eq_wts=c(0.20,0.20,0.20,0.20,0.20)
portfolioreturn= Return.portfolio(portfolio,
                                  weights=eq_wts,
                                  rebalance_on = "months",
                                  verbose = T)
portfolioreturn

portfolioannualret=Return.annualized(portfolioreturn$returns)
portfolioannualret
StdDev.annualized(portfolioreturn$returns)
table.AnnualizedReturns(portfolioreturn$returns,Rf=0.065/12)#negative sharpe indicates that the risk rate is higher than the portfolio return

table.AnnualizedReturns(portfolio)
table.CalendarReturns(portfolio)

#portfolio charts
charts.PerformanceSummary(portfolio)
#Drawdown- Time taken by the asset to fall from peak to down before it recovers back to peak.
#Drawdown is measure of downside volatility
#IT will tell you how long will the asset will recover the losses.

charts.RollingPerformance(portfolioreturn$returns)
chart.Histogram(portfolioreturn$returns,
                methods=c("add.density",
                          "add.normal"))

chart.Boxplot(portfolioreturn$returns)
skewness(portfolioreturn$returns)
kurtosis(portfolioreturn$returns)
chart.Correlation(portfolio)
cor(portfolio)

#PORTFOLIO RATIO
SortinoRatio(portfolioreturn$returns,MAR=0.20)
#Min expected return-20%
#Negative sortino ratio indicates risk and rewards are not equivalent

#Benchmark-Sensex or Nifty
getSymbols("^NSEI",from=as.Date("2012-01-01"))
niftyreturn=Return.calculate(NSEI$NSEI.Close)
niftyreturn

InformationRatio(portfolioreturn$returns,niftyreturn)
TrackingError(portfolioreturn$returns,niftyreturn)
BetaCoVariance(portfolioreturn$returns,niftyreturn)
CAPM.beta(portfolioreturn$returns,niftyreturn,Rf=0.065)

#Downside Risk Measure
VaR(portfolioreturn$returns)#Down trend happen 
#it gives the scenrio of there is a 80% of probability the portfolio 
#might have fall to -0.0225
ES(portfolioreturn$returns)#excepted shortfall giving -0.033
#this methods is considered as what is happen if market goes into worst scenorio

#Volatality Bucket of individual stocks
#so which is stock is highly volatality which stock is low volatality(volatality means risk)
volatilitybucket=StdDev(na.omit(portfolio),
                        portfolio_method = "component",
                        weights=eq_wts) #finding the individual stocks volatality
cbind(eq_wts,
      volatility=volatilitybucket$pct_contrib_StdDev)
#It is calculating the sd of return it gives table 
#Based on the standard deviation INDUSIND has high volatality followed by AXIS, ICICI and
#lowest Volatality is KOTAK followed by HDFC

#PORTFOLIO OPTIMIZATION-1
#IT IS CHANGE THE WEIGHT THAT GIVES THE HIGHER RETURN OF STOCK
library(tseries)
optimumportfolio=portfolio.optim(na.omit(portfolio))
pf_wts=optimumportfolio$pw
names(pf_wts)=colnames(portfolio)
round(pf_wts,digits=2)
barplot(pf_wts)

#Portfolio weights - Its target return 10% more than avgRet
optimumportfolio2=portfolio.optim(na.omit(portfolio),
                                  pm=1*mean(na.omit(
                                    portfolio)))
pf_wts2=optimumportfolio2$pw
names(pf_wts)=colnames(portfolio)
round(pf_wts,digit=2)
barplot(pf_wts)#It is suggesting same results as first we done
#we can invest stocks by this results to get better returns

#PORTFOLIO OPTIMIZATION-2:- WITH Constraints
#we can give mutliple comstraints of in this methodology
port_spec=portfolio.spec(colnames(portfolio))
port_spec
#Constraint 1:
port_spec=add.constraint(portfolio=port_spec,
                         type="full_investment")
#Constraint 2:
port_spec=add.constraint(portfolio = port_spec,
                         type="long_only")
#Objective1 - Maximizing portfolio return
port_spec=add.objective(portfolio = port_spec,
                        type="return",
                        name="mean")
#Objective 2- minimizing Risk of Portfolio
port_spec=add.objective(portfolio=port_spec,
                        type="risk",
                        name="StdDev")
print(port_spec)

optimumfinal=optimize.portfolio(na.omit(portfolio),
                                portfolio=port_spec,
                                optimize_method="random",
                                trace=T)
optimumfinal#It suggesting that how much investment to invest on different stocks

chart.RiskReward(optimumfinal,
                 risk.col="StdDev",
                 return.col="mean",
                 chart.assets = T)

chart.Weights(optimumfinal)#HDFC AND KOTAK STOCKS HAS PERFORM EXCELLENT. IF WE INVEST ON THIS TWO STOCK WE GET GOOD PROFIT 
#Remaining stocks are not performing well better to don't invest on this stocks. Because this stocks are not able to give optimal profit(20%).
