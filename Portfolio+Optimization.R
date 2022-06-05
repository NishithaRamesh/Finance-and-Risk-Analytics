

library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)
library(Quandl)



#Pull Data till date
maxDate <- "2018-01-01"
HDFCBank.prices <- Ad(getSymbols("HDFCBANK.BO",auto.assign = F, from=maxDate))
barChart(HDFCBank.prices)

Reliance.prices <- Ad(getSymbols("RELIANCE.BO",auto.assign = F, from=maxDate))
barChart(Reliance.prices)

HDFC.prices <- Ad(getSymbols("HDFC.BO",auto.assign = F, from=maxDate))
barChart(HDFC.prices)

Infy.prices <- Ad(getSymbols("INFY.BO",auto.assign = F, from=maxDate))
barChart(Infy.prices)

ICICI.prices <- Ad(getSymbols("ICICIBANK.BO",auto.assign = F, from=maxDate))
barChart(ICICI.prices)

TCS.prices <- Ad(getSymbols("TCS.BO",auto.assign = F, from=maxDate))
barChart(TCS.prices)

Hindunilvr.prices <- Ad(getSymbols("HINDUNILVR.BO",auto.assign = F, from=maxDate))
barChart(Hindunilvr.prices)

Kotak.prices <- Ad(getSymbols("KOTAKBANK.BO",auto.assign = F, from=maxDate))
barChart(Kotak.prices)

ITC.prices <- Ad(getSymbols("ITC.BO",auto.assign = F, from=maxDate))
barChart(ITC.prices)

LT.prices <- Ad(getSymbols("LT.BO",auto.assign = F, from=maxDate))
barChart(LT.prices)


#Delete missing values

HDFCBank.prices = na.omit(HDFCBank.prices)
Reliance.prices = na.omit(Reliance.prices)
HDFC.prices = na.omit(HDFC.prices)
Infy.prices = na.omit(Infy.prices)
ICICI.prices = na.omit(ICICI.prices)
TCS.prices = na.omit(TCS.prices)
Hindunilvr.prices = na.omit(Hindunilvr.prices)
Kotak.prices = na.omit(Kotak.prices)
ITC.prices = na.omit(ITC.prices)
LT.prices = na.omit(LT.prices)


#Calculate Daily Returns
HDFCBank.rets <- dailyReturn(HDFCBank.prices)
Reliance.rets <- dailyReturn(Reliance.prices)
HDFC.rets <- dailyReturn(HDFC.prices)
Infy.rets <- dailyReturn(Infy.prices)
ICICI.rets <- dailyReturn(ICICI.prices)
TCS.rets <- dailyReturn(TCS.prices)
Hindunilvr.rets <- dailyReturn(Hindunilvr.prices)
Kotak.rets <- dailyReturn(Kotak.prices)
ITC.rets <- dailyReturn(ITC.prices)
LT.rets <- dailyReturn(LT.prices)


barChart(HDFCBank.rets)
barChart(Reliance.rets)
barChart(HDFC.rets)
barChart(Infy.rets)
barChart(ICICI.rets)
barChart(TCS.rets)
barChart(Hindunilvr.rets)
barChart(Kotak.rets)
barChart(ITC.rets)
barChart(LT.rets)

#Calculate VaR and CVaR
VaR(HDFCBank.rets, p=0.95,method = "historical")
CVaR(HDFCBank.rets, p=0.95,method = "historical")

VaR(Reliance.rets, p=0.95,method = "historical")
CVaR(Reliance.rets, p=0.95,method = "historical")

VaR(HDFC.rets, p=0.95,method = "historical")
CVaR(HDFC.rets, p=0.95,method = "historical")

VaR(Infy.rets, p=0.95,method = "historical")
CVaR(Infy.rets, p=0.95,method = "historical")

VaR(ICICI.rets, p=0.95,method = "historical")
CVaR(ICICI.rets, p=0.95,method = "historical")

VaR(TCS.rets, p=0.95,method = "historical")
CVaR(TCS.rets, p=0.95,method = "historical")

VaR(Hindunilvr.rets, p=0.95,method = "historical")
CVaR(Hindunilvr.rets, p=0.95,method = "historical")

VaR(Kotak.rets, p=0.95,method = "historical")
CVaR(Kotak.rets, p=0.95,method = "historical")

VaR(ITC.rets, p=0.95,method = "historical")
CVaR(ITC.rets, p=0.95,method = "historical")

VaR(LT.rets, p=0.95,method = "historical")
CVaR(LT.rets, p=0.95,method = "historical")




#Create your Portfolio
tickers <- c("HDFCBANK.BO","RELIANCE.BO","HDFC.BO","INFY.BO","ICICIBANK.BO","TCS.BO",
             "HINDUNILVR.BO","KOTAKBANK.BO","ITC.BO","LT.BO")
weights <- c(0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10)
getSymbols(tickers, from=maxDate)

Port.prices <- na.omit(merge(Ad(HDFCBANK.BO),Ad(RELIANCE.BO),Ad(HDFC.BO),Ad(INFY.BO),
                             Ad(ICICIBANK.BO),Ad(TCS.BO),Ad(HINDUNILVR.BO),Ad(KOTAKBANK.BO),
                             Ad(ITC.BO),Ad(LT.BO)))
Port.returns <- ROC(Port.prices, type = "discrete")[-1,]

#Give column names to your portfolio
colnames(Port.returns) <- tickers

#Calculate Portfolio VaR and CVaR of your Portfolio
VaR(Port.returns,p=0.99,weights=weights,portfolio_method = "component", method = "historical")
CVaR(Port.returns,p=0.99,weights=weights,portfolio_method = "component", method = "historical")

VaR(Port.returns,p=0.99,weights=weights,portfolio_method = "component", method = "gaussian")
CVaR(Port.returns,p=0.99,weights=weights,portfolio_method = "component", method = "gaussian")

VaR(Port.returns,p=0.99,weights=weights,portfolio_method = "component", method = "modified")
CVaR(Port.returns,p=0.99,weights=weights,portfolio_method = "component", method = "modified")

#Calculate Individual stocks VaR by different methods
VaR.Hist <- VaR(Port.returns, p=0.95, weights = NULL, portfolio_method = "single",method = "historical")
VaR.Gaus <- VaR(Port.returns, p=0.95, weights = NULL, portfolio_method = "single",method = "gaussian")
VaR.Mod <- VaR(Port.returns, p=0.95, weights = NULL, portfolio_method = "single",method = "modified")

All.VaR <- abs(data.frame(rbind(VaR.Hist,VaR.Gaus,VaR.Mod)))
rownames(All.VaR) <- c("Hist","Gaus","Mod")

#Calculate Portfolio VaR by different methods
PortVaR.Hist <- as.data.frame(VaR(Port.returns,p=0.95,weights=weights, portfolio_method = "component", method = "historical"))[1,1]
PortVaR.Gaus <- VaR(Port.returns,p=0.95,weights=weights, portfolio_method = "component", method = "gaussian")$VaR
PortVaR.Mod <- VaR(Port.returns,p=0.95,weights=weights, portfolio_method = "component", method = "modified")$MVaR


#Combine Individual Stocks and Portfolio VaR
All.VaR$Portfolio <- 0
All.VaR$Portfolio <- c(PortVaR.Hist,PortVaR.Gaus,PortVaR.Mod)
All.VaR$Type <- c("Hist","Gaus","Mod")

#Visualization of VaR calculated for individual stocks and Portfolio


plotVaR <- melt(All.VaR, variable.name = "Ticker", value.name = "VaR")
ggplot(plotVaR,aes(x=Type, y=VaR, fill=Ticker)) + geom_bar(stat = "identity", position = "dodge")


#######################################################################################################


maxDate <- "2018-01-01"

#-----------Benchmark-----------#
ticker = '^BSESN'
#Get Data
stock <- getSymbols.yahoo(ticker, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Calculate Returns for Assets: Daily RoC
bench_ret <- na.omit(ROC(stock, type="discrete"))
#Rename Columns
colnames(bench_ret) <- "SP500"


#-----------Portfolio-----------#
#Create Vector of Tickers
tickers <- c("HDFCBANK.BO", "RELIANCE.BO",  "HDFC.BO", "INFY.BO", "ICICIBANK.BO","TCS.BO", 
             "HINDUNILVR.BO", "KOTAKBANK.BO", "ITC.BO", "LT.BO")


#Get Prices (can be monthly or weekly)
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4])

#Delete all dates with no prices
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(portfolioPrices) <- tickers

#Calculate Returns for Assets: Daily RoC
portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))

#Calculate Aggregate Returns for Portfolio
weights <-c(".1",".1", ".1", ".1", ".1", ".1", ".1", ".1", ".1", ".1")
port_rets <- Return.portfolio(portfolioReturns, weights = NULL, geometric = TRUE)
names(port_rets) <- "Portfolio"

#-----------Asset-----------#
# 
asset = 'HDFCBANK.BO'
#Get Data
stock <- getSymbols.yahoo(asset, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#---------------Calculate Beta-----------#
Port.Beta <- CAPM.beta(port_rets, bench_ret,)
Asset.Beta <- CAPM.beta(asset_ret, bench_ret)

print(Port.Beta)
print(Asset.Beta)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets) #----------Timing Ratio-------------# #Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

print(beta_pos)
print(beta_neg)
print(time_ratio)

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
                 excess.returns = FALSE, fit = c("loess", "linear"),
                 legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))



asset = 'RELIANCE.BO'
#Get Data
stock <- getSymbols.yahoo(asset, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#---------------Calculate Beta-----------#
Port.Beta <- CAPM.beta(port_rets, bench_ret,)
Asset.Beta <- CAPM.beta(asset_ret, bench_ret)

print(Port.Beta)
print(Asset.Beta)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets) #----------Timing Ratio-------------# #Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

print(beta_pos)
print(beta_neg)
print(time_ratio)

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
                 excess.returns = FALSE, fit = c("loess", "linear"),
                 legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))



asset = 'HDFC.BO'
#Get Data
stock <- getSymbols.yahoo(asset, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#---------------Calculate Beta-----------#
Port.Beta <- CAPM.beta(port_rets, bench_ret,)
Asset.Beta <- CAPM.beta(asset_ret, bench_ret)

print(Port.Beta)
print(Asset.Beta)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets) #----------Timing Ratio-------------# #Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

print(beta_pos)
print(beta_neg)
print(time_ratio)

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
                 excess.returns = FALSE, fit = c("loess", "linear"),
                 legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))



asset = 'INFY.BO'
#Get Data
stock <- getSymbols.yahoo(asset, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#---------------Calculate Beta-----------#
Port.Beta <- CAPM.beta(port_rets, bench_ret,)
Asset.Beta <- CAPM.beta(asset_ret, bench_ret)

print(Port.Beta)
print(Asset.Beta)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets) #----------Timing Ratio-------------# #Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

print(beta_pos)
print(beta_neg)
print(time_ratio)

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
                 excess.returns = FALSE, fit = c("loess", "linear"),
                 legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))


asset = 'ICICIBANK.BO'
#Get Data
stock <- getSymbols.yahoo(asset, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#---------------Calculate Beta-----------#
Port.Beta <- CAPM.beta(port_rets, bench_ret,)
Asset.Beta <- CAPM.beta(asset_ret, bench_ret)

print(Port.Beta)
print(Asset.Beta)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets) #----------Timing Ratio-------------# #Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

print(beta_pos)
print(beta_neg)
print(time_ratio)

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
                 excess.returns = FALSE, fit = c("loess", "linear"),
                 legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))


asset = 'TCS.BO'
#Get Data
stock <- getSymbols.yahoo(asset, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#---------------Calculate Beta-----------#
Port.Beta <- CAPM.beta(port_rets, bench_ret,)
Asset.Beta <- CAPM.beta(asset_ret, bench_ret)

print(Port.Beta)
print(Asset.Beta)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets) #----------Timing Ratio-------------# #Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

print(beta_pos)
print(beta_neg)
print(time_ratio)

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
                 excess.returns = FALSE, fit = c("loess", "linear"),
                 legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))


asset = 'HINDUNILVR.BO'
#Get Data
stock <- getSymbols.yahoo(asset, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#---------------Calculate Beta-----------#
Port.Beta <- CAPM.beta(port_rets, bench_ret,)
Asset.Beta <- CAPM.beta(asset_ret, bench_ret)

print(Port.Beta)
print(Asset.Beta)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets) #----------Timing Ratio-------------# #Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

print(beta_pos)
print(beta_neg)
print(time_ratio)

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
                 excess.returns = FALSE, fit = c("loess", "linear"),
                 legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))


asset = 'KOTAKBANK.BO'
#Get Data
stock <- getSymbols.yahoo(asset, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#---------------Calculate Beta-----------#
Port.Beta <- CAPM.beta(port_rets, bench_ret,)
Asset.Beta <- CAPM.beta(asset_ret, bench_ret)

print(Port.Beta)
print(Asset.Beta)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets) #----------Timing Ratio-------------# #Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

print(beta_pos)
print(beta_neg)
print(time_ratio)

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
                 excess.returns = FALSE, fit = c("loess", "linear"),
                 legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))



asset = 'ITC.BO'
#Get Data
stock <- getSymbols.yahoo(asset, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#---------------Calculate Beta-----------#
Port.Beta <- CAPM.beta(port_rets, bench_ret,)
Asset.Beta <- CAPM.beta(asset_ret, bench_ret)

print(Port.Beta)
print(Asset.Beta)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets) #----------Timing Ratio-------------# #Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

print(beta_pos)
print(beta_neg)
print(time_ratio)

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
                 excess.returns = FALSE, fit = c("loess", "linear"),
                 legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))



asset = 'LT.BO'
#Get Data
stock <- getSymbols.yahoo(asset, from=maxDate, periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#---------------Calculate Beta-----------#
Port.Beta <- CAPM.beta(port_rets, bench_ret,)
Asset.Beta <- CAPM.beta(asset_ret, bench_ret)

print(Port.Beta)
print(Asset.Beta)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets) #----------Timing Ratio-------------# #Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

print(beta_pos)
print(beta_neg)
print(time_ratio)

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
                 excess.returns = FALSE, fit = c("loess", "linear"),
                 legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))




#Portfolio Optimization
######################STEP ONE: Create Returns Time Series#########################################

#Create Vector of Tickers
tickers <- c("HINDUNILVR.BO","RELIANCE.BO","HDFCBANK.BO","LT.BO","TCS.BO")

#Calculate Returns: Daily
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2018-01-01", auto.assign=FALSE)[,4])

#Delete all dates with no prices
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(portfolioPrices) <- tickers

#Calculate Returns: Daily RoC
portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
portfolioReturns <- as.timeSeries(portfolioReturns)

#Calculate Monthly or Weekly Returns
Stock_Data <- tickers %>% lapply(function(x) getSymbols.yahoo(x, from="2018-01-01", auto.assign=FALSE)[,4]) %>%
  lapply(function(x) monthlyReturn(x))

portfolioReturns <- do.call(merge, Stock_Data)
# keep only the dates that have closing prices for all tickers
portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
colnames(portfolioReturns) <- tickers
portfolioReturns <- as.timeSeries(portfolioReturns)

#################STEP TWO: Calculate and Plot Frontier and Efficient Portfolios##############

# calculate the efficient frontier
effFrontier <- portfolioFrontier(portfolioReturns, constraints = "LongOnly")

# plot frontier
#'Options
#'1: Plot Efficient Frontier
#'2: Plot Minimum Variance Portfolio
#'3: Plot Tangency Portfolio
#'4: Plot Risk Returns of Each Asset
#'5: Plot Equal Weights Portfolio
#'6: Plot Two Asset Frontiers (Long)
#'7: Plot Monte Carlo Portfolios
#'8: Plot Sharpe Ratio

plot(effFrontier,c(1,2,3,4,5))

#Plot Frontier Weights (Can Adjust Number of Points)
frontierWeights <- getWeights(effFrontier) # get allocations for each instrument for each point on the efficient frontier
colnames(frontierWeights) <- tickers
risk_return <- frontierPoints(effFrontier)
write.csv(risk_return, "risk_return_portfolio.csv")

#Output Correlation
cor_matrix <- cor(portfolioReturns)
cov_matrix <- cov(portfolioReturns)
write.csv(cov_matrix, "covmatrix_portfolio.csv")

cov_matrix
#Annualize Data
riskReturnPoints <- frontierPoints(effFrontier) # get risk and return values for points on the efficient frontier
annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
                               targetReturn=riskReturnPoints[,"targetReturn"] * 252)
plot(annualizedPoints)

# plot Sharpe ratios for each point on the efficient frontier
riskFreeRate <- 0
plot((annualizedPoints[,"targetReturn"]-riskFreeRate) / annualizedPoints[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio")

#Plot Frontier Weights (Need to transpose matrix first)
barplot(t(frontierWeights), main="Frontier Weights", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(frontierWeights))


#Get Minimum Variance Port, Tangency Port, etc.
mvp <- minvariancePortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
mvp
tangencyPort <- tangencyPortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
tangencyPort

mvpweights <- getWeights(mvp)
tangencyweights <- getWeights(tangencyPort)

#Extract value at risk
covRisk(portfolioReturns, mvpweights)
varRisk(portfolioReturns, mvpweights, alpha = 0.05)
cvarRisk(portfolioReturns, mvpweights, alpha = 0.05)

#Plot MVP Weights: Basic Graphs
barplot(mvpweights, main="Minimum Variance Portfolio Weights", xlab="Assset", ylab="Weight In Portfolio (%)", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(weights))
pie(mvpweights, col=cm.colors(ncol(frontierWeights)+2))

#ggplot MVP Weights
df <- data.frame(mvpweights)
assets <- colnames(frontierWeights)
ggplot(data=df, aes(x=assets, y=mvpweights, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",mvpweights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")

dft <- data.frame(tangencyweights)
assets <- colnames(frontierWeights)
ggplot(data=dft, aes(x=assets, y=tangencyweights, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",tangencyweights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Tangency Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")

#ggplot Pie
bar <- ggplot(df, aes(x = "", y = mvpweights, fill=assets)) + geom_bar(width= 1, stat="identity") + ggtitle("Minimum Variance Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) 
pie <- bar + coord_polar("y", start=0)
pie + scale_fill_brewer(palette="Blues")+
  theme_minimal()

bar <- ggplot(dft, aes(x = "", y = tangencyweights, fill=assets)) + geom_bar(width= 1, stat="identity") + ggtitle("Tangency Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) 
pie <- bar + coord_polar("y", start=0)
pie + scale_fill_brewer(palette="Blues")+
  theme_minimal()

###########################Examine Constraints and Stats#####################################
#Example Constraints:
#"minW[asset]=percentage" for box constraints resp.
#"maxsumW[assets]=percentage" for sector constraints.
#eqsumWConstraints(data, spec=portfolioSpec(), constraints="LongOnly")

#Set Specs
Spec = portfolioSpec()
setSolver(Spec) = "solveRshortExact"
setTargetRisk(Spec) = .12
constraints <- c("minW[1:length(tickers)]=-1","maxW[1:length(tickers)]=.60", "Short")

effFrontierShort <- portfolioFrontier(portfolioReturns, Spec, constraints = constraints)
weights <- getWeights(effFrontierShort)
write.csv(weights, "weightsShort.csv")
colnames(weights) <- tickers

plot(effFrontierShort, c(1, 2, 3))

#Plot Frontier Weights (Need to transpose matrix first)
barplot(t(weights), main="Frontier Weights", col=cm.colors(ncol(weights)+2), legend=colnames(weights))

effPortShort <- minvariancePortfolio(portfolioReturns, Spec, constraints=constraints)
optWeights <- getWeights(effPortShort)
tanPortShort <- tangencyPortfolio(portfolioReturns, Spec, constraints=constraints)
tanWeights <- getWeights(tanPortShort)
maxR <- maxreturnPortfolio(portfolioReturns , Spec, constraints=constraints)
maxWeights <- getWeights(maxR)

#ggplot MVP Weights
df <- data.frame(tanWeights)
assets <- colnames(frontierWeights)
ggplot(data=df, aes(x=assets, y=tanWeights, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",tanWeights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Tangency Portfolio With Shorts Allowed")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")



###############################################################################################
# Allocation of weights

#Get Minimum Variance Port, Tangency Port, etc.
mvp1 <- minvariancePortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
mvp1
tangencyPort1 <- tangencyPortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
tangencyPort1

mvpweights1 <- c(0.3, 0.15, 0.30, 0.05, 0.20)
tangencyweights1 <- c(0.3, 0.15, 0.20, 0.05, 0.30)

#Extract value at risk
covRisk(portfolioReturns, mvpweights1)
varRisk(portfolioReturns, mvpweights1, alpha = 0.05)
cvarRisk(portfolioReturns, mvpweights1, alpha = 0.05)

#Plot MVP Weights: Basic Graphs
barplot(mvpweights1, main="Minimum Variance Portfolio Weights", xlab="Assset", ylab="Weight In Portfolio (%)", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(weights))
pie(mvpweights1, col=cm.colors(ncol(frontierWeights)+2))

#ggplot MVP Weights
df <- data.frame(mvpweights1)
assets <- colnames(frontierWeights)
ggplot(data=df, aes(x=assets, y=mvpweights1, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",mvpweights1*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")

dft <- data.frame(tangencyweights1)
assets <- colnames(frontierWeights)
ggplot(data=dft, aes(x=assets, y=tangencyweights1, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",tangencyweights1*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Tangency Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")
