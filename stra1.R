current.dir <- dirname(parent.frame(2)$ofile)
source(file.path(current.dir, "quantstrat-addon.R"))

require(quantstrat)
require(qmao)
require(PerformanceAnalytics)
options(width=240)
## example

# db.fields <- c("date", "open", "high", "low", "close", "bid", "ask", "volume", "adjusted")
# field.names <- db.fields[-1]
# IF1207 <- getSymbols.MySQL("if1207", verbose=F, auto.assign=F, user="test", password="123456", dbname="", host="192.168.10.149", db.fields=db.fields, field.names=field.names)
# idx <- index(IF1207)
# m <- as.matrix(IF1207)
# IF1207 <- try.xts(m, order.by=idx)
# 
# # IF1207 <- IF1207["T09:14:00/T15:15"]
# IF1207 <- IF1207["T09:14/T10:00"]
# IF1207 <- IF1207[,c("close", "High", "low", "bid", "ask", "volume")]
# head(IF1207)
# IF1207 <- to.minutes(IF1207, OHLC=T)
# save(IF1207, file="IF1207.rda")

strategy.name <- "dma_adx"
# clear out evironment
try(rm(list=ls(pos=.blotter), pos=.blotter), silent=T)
try(rm(list=ls(pos=.strategy), pos=.strategy), silent=T)
try(rm(list=ls(pos=.instrument), pos=.instrument), silent=T)

contract <- 'IF1206'
portfolio.name <- strategy.name
account.name <- strategy.name

fastEMA <- 2 
slowEMA <- 16
adx.min <- 12
adx.max <- 58

qty <- 2
txnfee <- -.02

begin.date <- "2011-12-31"
end.date <- Sys.Date()
start.time <- '2012-05-21T10:14:59'

currency('USD')
print(ls_currencies())
stock(contract, currency='USD', multiplier=1)
# print(ls_stocks())

# getData
contract.data <- get(contract)

start.time <- '2012-07-17T09:14:59' 
start.time <- format(index(first(contract.data)), format="%Y-%m-%dT%H:%M:%S")
initial.equity <- 1000000

initPortf(portfolio.name, symbols=contract, initDate=start.time)
initAcct(account.name, portfolios=portfolio.name, initDate=start.time)
initOrders(portfolio=portfolio.name, initDate=start.time)

strat <- strategy(strategy.name)

ABC <- function(x) {
  return(cbind(As(x), Bi(x), Cl(x)))
}
  
# indicators
strat <- add.indicator(strat, label="slow.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=slowEMA))
strat <- add.indicator(strat, label="fast.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=fastEMA))
strat <- add.indicator(strat, label="adx", name='ADX', arguments=list(HLC=quote(ABC(mktdata))))

# signals
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fast.EMA", "slow.EMA"), relationship="gte"), label="fast.cross.above.slow")
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fast.EMA", "slow.EMA"), relationship="lt"), label="fast.cross.below.slow")
# strat <- add.signal(strat, name="sigThreshold", arguments = list(column="ADX", relationship="gte", threshold=28), label="adx.gte.threshold")
# strat <- add.signal(strat, name="sigThreshold", arguments = list(column="ADX", relationship="lt", threshold=28), label="adx.lt.threshold") 

sigRange <- function(label, data=mktdata, column, upper, lower) {
  xx <- data[, column]
  signal <- xx < upper & xx > lower
  if (!is.null(label)) {
    colnames(signal) <- label
  }
  return(signal)  
}

sigBuyLong <- function(label, data=mktdata) {
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  in.range <- sigRange(label="in.range", data=data, column="adx.ADX", upper=adx.max, lower=adx.min)
  crossup <- sigCrossover(label="", data=data, columns=c("fast.EMA", "slow.EMA"), relationship="gte")
  signal <- in.range & crossup
  colnames(signal) <- label
  return(signal)
}
strat <- add.signal(strat, label="buy.long", name="sigBuyLong", arguments=list(data=quote(mktdata)))

sigSellShort <- function(label, data = mktdata) {
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  in.range <- sigRange(label="in.range", data=data, column="adx.ADX", upper=adx.max, lower=adx.min)
  crossup <- sigCrossover(label="", data=data, columns=c("fast.EMA", "slow.EMA"), relationship="lt")
  signal <- in.range & crossup
  colnames(signal) <- label
  return(signal)
}
strat <- add.signal(strat, label="sell.short", name="sigSellShort", arguments=list(data=quote(mktdata)))

sigMarketClose <- function(label, data = mktdata, period) {
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  signal[period] <-1
  colnames(signal) <- label  
  return(signal)
}
strat <- add.signal(strat, name="sigMarketClose", arguments = list(data=quote(mktdata), period="T15:00/T15:15"), label="market.close") 

# mkt <- get(contract)
# mkt <- applySignals(strat, mktdata=applyIndicators(strat, mktdata=get(contract))) #for debugging
# print(tail(mkt, n=30))

# rules
# buy long
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="buy.long", sigval=TRUE, orderqty=qty, ordertype='market', orderside='long', pricemethod='market', replace=FALSE, TxnFees=txnfee), type='enter', path.dep=TRUE)
strat <- add.rule(strat, label='signalexit',name="ruleSignal", arguments = list(sigcol="fast.cross.below.slow", sigval=TRUE, orderqty="all", ordertype='market', orderside='long', pricemethod='market', replace=FALSE, TxnFees=txnfee, orderset="exit2"), type='exit', path.dep=TRUE)
strat <- add.rule(strat, label='trailingexit', name="ruleSignal", arguments = list(sigcol="buy.long", sigval=TRUE, orderqty=-qty, ordertype='stoptrailing', orderside='long', threshold=0.95, tmult=T, TxnFees=txnfee, orderset="exit2"), type='risk')

# sell short
# strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="sell.short", sigval=TRUE, orderqty=-qty, ordertype='market', orderside='short', pricemethod='market', replace=FALSE, TxnFees=txnfee), type='enter', path.dep=TRUE)
# strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="fast.cross.above.slow", sigval=TRUE, orderqty="all", ordertype='market', orderside='short', pricemethod='market', replace=FALSE, TxnFees=txnfee), type='exit', path.dep=TRUE)

# strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="market.close", sigval=TRUE, orderqty="all", ordertype='market', orderside='long'), type='risk')
# strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="market.close", sigval=TRUE, orderqty="all", ordertype='market', orderside='short'), type='risk')

# addPosLimit(portfolio.name, contract, timestamp=start.time, maxpos=qty, minpos=0)
# addPosLimit(portfolio.name, contract, timestamp=start.time, maxpos=qty, minpos=0)

out <- applyStrategy(strat, portfolios=portfolio.name)
updatePortf(Portfolio=portfolio.name, Dates="2011/2012")
chart.Posn(Portfolio=portfolio.name)
 
p <- getPortfolio(portfolio.name)
s <- p$symbols[[contract]]
mkt <- get(contract)
adx <- ADX(ABC(mkt))$ADX

plot(add_TA(EMA(Cl(mkt), n=slowEMA), col="red"))
plot(add_TA(EMA(Cl(mkt), n=fastEMA), on=4, col='blue'))
plot(add_TA(adx))
plot(add_TA(xts(rep(adx.min, nrow(mkt)), order.by=index(mkt)), on=5, col="red"))
plot(add_TA(xts(rep(adx.max, nrow(mkt)), order.by=index(mkt)), on=5, col="red"))
 
# #look at the order book
book <- getOrderBook(portfolio.name)
book <- book[[strategy.name]][[contract]]
print(book)
dailyEqPL(portfolio.name, drop.time=F)
dailyTxnPL(portfolio.name, drop.time=F)
tradeStats(portfolio.name)
dailyStats(portfolio.name)
getEndEq(account.name, Date=Sys.time())
getPosQty(portfolio.name, Symbol=contract, Date=Sys.time())
