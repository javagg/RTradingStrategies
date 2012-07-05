this.dir <- dirname(parent.frame(2)$ofile)

require(quantstrat)
require(qmao)
require(PerformanceAnalytics)
require(FactorAnalytics)

# clear out evironment
# rm(list=ls())
# try(rm(list=ls(pos=.blotter), pos=.blotter), silent=TRUE)
# try(rm(list=ls(pos=.strategy), pos=.strategy), silent=TRUE)
# try(rm(list=ls(pos=.instrument), pos=.instrument), silent=TRUE)


load(file.path(this.dir, "sample.500ms.rda"))

mkt <- sample.500ms
mkt <- mkt["T09:15/T15:14"]
mkt <- to.period(mkt, "seconds", k = 1)
mkt <- to.minutes(mkt)

strategy.name <- "ma_adx"
suppressWarnings(rm(list=paste("order_book", strategy.name, sep="."), pos=.strategy))
suppressWarnings(rm(list=paste(c("account", "portfolio"), strategy.name, sep="."), pos=.blotter))

# print(ls(.blotter))
# print(ls(.strategy))

contract <- 'IF2016'
portfolio.name <- strategy.name
account.name <- strategy.name

fastMA <- 2 
slowMA <- 12

currency('RMB')
print(ls_currencies())
stock(contract, currency='RMB', multiplier=1)
print(ls_stocks())

start.time <- '2011-10-10 10:11:20'
initial.equity <- 1000000

IF2016 <- mkt
  
initPortf(portfolio.name, symbols=contract, initDate=start.time)
initAcct(account.name, portfolios=portfolio.name, initDate=start.time)
initOrders(portfolio=portfolio.name, initDate=start.time)

strat <- strategy(strategy.name)

# indicators
strat <- add.indicator(strat, label="slow.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=12))
strat <- add.indicator(strat, label="fast.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=2))
strat <- add.indicator(strat, label="adx", name='ADX', arguments=list(HLC=quote(HLC(mktdata))))

# mkt <- applyIndicators(strat, mkt)
# print(head(mkt, n=30))

# signals
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fast.EMA", "slow.EMA"), relationship="gte"), label="fast.cross.above.slow")
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fast.EMA", "slow.EMA"), relationship="lt"), label="fast.cross.below.slow")
strat <- add.signal(strat, name="sigThreshold", arguments = list(column="ADX", relationship="gte", threshold=28), label="adx.gte.threshold")
strat <- add.signal(strat, name="sigThreshold", arguments = list(column="ADX", relationship="lt", threshold=28), label="adx.lt.threshold") 

sigLongBuy <- function(label, data = mktdata) {
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  trend <- sigThreshold(label="", data=mktdata, column="adx.ADX", threshold=28, relationship="gte")
  crossup <- sigCrossover(label="", data=mktdata, columns=c("fast.EMA", "slow.EMA"), relationship="gte")
  signal <- trend & crossup
  colnames(signal) <- label
  return(signal)
}
strat <- add.signal(strat, label="long.buy", name="sigLongBuy", arguments=list(data=quote(mktdata)))

sigMarketClose <- function(label, data = mktdata, period) {
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  signal[period] <-1
  colnames(signal) <- label  
  return(signal)
}
strat <- add.signal(strat, name="sigMarketClose", arguments = list(data=quote(mktdata), period="T15:00/T15:15"), label="market.close") 

# mkt <- applySignals(strat, mkt)
# print(head(mkt, n=300))

# rules
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="long.buy", sigval=TRUE, orderqty=100, ordertype='market', orderside='long'), type='enter')
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="fast.lt.slow", sigval=TRUE, orderqty="all", ordertype='market', orderside='long'), type='exit')
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="market.close", sigval=TRUE, orderqty="all", ordertype='market', orderside='long'), type='risk')
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="market.close", sigval=TRUE, orderqty="all", ordertype='market', orderside='short'), type='risk')

print(strat)

out <- try(applyStrategy(strat , portfolios=portfolio.name, parameters=list()))
updatePortf(Portfolio=portfolio.name)

#chart.Posn(Portfolio=portfolio.name)
# plot(add_MACD(fast=fastMA, slow=slowMA, signal=signalMA,maType="EMA"))

#look at the order book
getOrderBook(portfolio.name)

# ######
# strat <- strategy("kd")
# 
# # indicators
# strat <- add.indicator(strat, name='stoch', arguments=list(x=quote(HLC(mktdata))), label="KD")
# 
# # signals
# strat <- add.signal(strat, name="sigThreshold", arguments = list(column="fastD", relationship="gte", threshold=90), label="overbought")
# strat <- add.signal(strat, name="sigThreshold", arguments = list(column="fastD", relationship="lte", threshold=10), label="oversold")
# strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fastK", "fastD"), relationship="gte"), label="k.cross.above.d")
# strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fastK", "fastD"), relationship="lte"), label="k.cross.below.d")
# 
# # rules
# 
# # getSymbols(strat, from=initDate)
# # 
# # out<-try(applyStrategy(strat, portfolios=portfolio.name, parameters=list(sd=SD, n=N)))
# # 
# # updatePortf(Portfolio=portfolio.name, Dates=paste('::', as.Date(Sys.time()), sep=''))
# # 
# # chart.Posn(Portfolio=portfolio.name, Symbol=stock.str)
# 
# strategy.obj <- strategy(strategy.name)
# strategy.obj <- add.indicator(strategy = strategy.obj, name = "BBands", arguments=list(HLC=quote(HLC(mktdata)), maType='SMA'))
# 
# chartSeries(mktdata, type="bar", yrange=c(2580, 2710), TA=c(addVo(),addBBands()), theme=chartTheme('white'))
# addTA(EMA(Cl(mktdata), n=12), on=1, col="pink")
# addTA(EMA(Cl(mktdata), n=2), on=1, col="blue")
# addRSI()
