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

mktdata <- sample.500ms
mktdata <- mktdata["T09:15/T15:14"]
mktdata <- to.period(mktdata, "seconds", k = 1)
mktdata <- to.minutes(mktdata)


strategy.name <- "adx"
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

the.signal <- function(HLC) {
  adx <- ADX(HLC)[,"ADX"]
  
  fast.ma <- EMA(Cl(HLC), n=2)
  signal <- ifelse(fast.ma >= slow.ma & lag(fast.ma) < lag(slow.ma) & adx > 80, TRUE, FALSE)
  return(signal)
}
  
initPortf(portfolio.name, symbols=contract, initDate=start.time)
initAcct(account.name, portfolios=portfolio.name, initDate=start.time)
initOrders(portfolio=portfolio.name, initDate=start.time)

strat <- strategy("adx")

# indicators
strat <- add.indicator(strat, label="slow.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=12))
strat <- add.indicator(strat, label="fast.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=2))
strat <- add.indicator(strat, name='ADX', arguments=list(x=quote(HLC(mktdata)), n=14))

strat <- add.indicator(strat, label="the.signal", name='the.signal', arguments=list(x=quote(HLC(mktdata))))

# signals
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fast.EMA", "slow.EMA"), relationship="gte"), label="fast.EMA.gte.slow.EMA")
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fast.EMA", "slow.EMA"), relationship="lt"), label="fast.EMA.lt.slow.EMA")
strat <- add.signal(strat, name="sigThreshold", arguments = list(column="ADX", relationship="gte", threshold=28, cross=TRUE), label="signal.gte.zero")
strat <- add.signal(strat, name="sigThreshold", arguments = list(column="ADX", relationship="lt", threshold=28, cross=TRUE), label="signal.lt.zero") 

strat <- add.signal(strat, name="sigThreshold", arguments = list(column="the.signal", relationship="eq", threshold=0, cross=TRUE), label="long") 
strat <- add.signal(strat, name="sigThreshold", arguments = list(column="the.signal", relationship="gt", threshold=0, cross=TRUE), label="short") 
strat <- add.signal(strat, name="sigThreshold", arguments = list(column="the.signal", relationship="lt", threshold=0, cross=TRUE), label="noop") 
sigPeriod <- function(data = mktdata, period) {
  signal <- xts(rep(0, nrow(data)), order.by=index(data))
  signal["T14:50/T15:00"] <-1 
  return(signal)
}
strat <- add.signal(strat, name="sigPeriod", arguments = list(period="T14:50/T15:00"), label="end.of.day") 

# rules
strat <- add.rule(strat, name='ruleSignal', arguments=list(sigcol="fast.EMA.gte.slow.EMA", sigval=TRUE, orderqty=100, ordertype='market', orderside='long'), type='enter')
strat <- add.rule(strat, name='ruleSignal', arguments=list(sigcol="fast.EMA.gte.slow.EMA", sigval=TRUE, orderqty='all', ordertype='stoplimit', orderside='long', threshold=-.05, tmult=TRUE), type='risk')

strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="long", sigval=TRUE, orderqty=100, ordertype='market', orderside='long'), type='enter')
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="short", sigval=TRUE, orderqty="all", ordertype='market', orderside='long'), type='exit')

strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="long", sigval=TRUE, orderqty="all", ordertype='stoptrailing', orderside='long'), type='risk')
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="short", sigval=TRUE, orderqty="all", ordertype='stoptrailing', orderside='short'), type='risk')


strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="end.of.day", sigval=TRUE, orderqty="all", ordertype='market', orderside='short'), type='risk')
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="end.of.day", sigval=TRUE, orderqty="all", ordertype='market', orderside='long'), type='risk')

print(strat)

######
strat <- strategy("kd")

# indicators
strat <- add.indicator(strat, name='stoch', arguments=list(x=quote(HLC(mktdata))), label="KD")

# signals
strat <- add.signal(strat, name="sigThreshold", arguments = list(column="fastD", relationship="gte", threshold=90), label="overbought")
strat <- add.signal(strat, name="sigThreshold", arguments = list(column="fastD", relationship="lte", threshold=10), label="oversold")
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fastK", "fastD"), relationship="gte"), label="k.cross.above.d")
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fastK", "fastD"), relationship="lte"), label="k.cross.below.d")

# rules

# getSymbols(strat, from=initDate)
# 
# out<-try(applyStrategy(strat, portfolios=portfolio.name, parameters=list(sd=SD, n=N)))
# 
# updatePortf(Portfolio=portfolio.name, Dates=paste('::', as.Date(Sys.time()), sep=''))
# 
# chart.Posn(Portfolio=portfolio.name, Symbol=stock.str)
