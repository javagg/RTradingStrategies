current.dir <- dirname(parent.frame(2)$ofile)

require(quantstrat)
require(qmao)
require(PerformanceAnalytics)

# parallel computing
require(foreach)
require(doMC)
registerDoMC()

source(file.path(current.dir, "quantstrat-addon.R"))
source(file.path(current.dir, "quantstrat-myrule.R"))

options(width=240)

###############################################################################
# Parameters
###############################################################################
fastEMA <- 20 
slowEMA <- 60
adx.min <- 12
adx.max <- 58
stoplimit.threshold <- -0.4
stoptrailing.threshold <- -3
qty <- 1
txnfee <- -0.15/2

enter.timespan <- "T09:15/T15:10"
exit.timespan <- "T15:11/T15:15"

initial.equity <- 1000000

osOp <- function (timestamp, orderqty, portfolio, symbol, ruletype, ...) {
  if (orderqty == "all" && !(ruletype == "exit" || ruletype == "risk")) {
    stop(paste("orderqty 'all' would produce nonsense, maybe use osMaxPos instead?\n", "Order Details:\n", "Timestamp:", timestamp, "Qty:", orderqty, "Symbol:", symbol))
    orderqty = 0
  }
  return(orderqty)
}

ABC <- function(x) {
  return(cbind(As(x), Bi(x), Cl(x)))
}

sigCouldEnter <- function(label, data = mktdata, enter.delay) {
  if (is.null(enter.delay)) {
    timespan <- enter.timespan
  } else {
    freq <- periodicity(data)$scale
    switch(freq, minute = {
      should.start <- substr(enter.timespan, 2, 6)
      should.end <- substring(enter.timespan, 9)
      should.start <- as.POSIXct(should.start, format="%H:%M")
      should.start <- should.start + enter.delay*60
      should.start <- format(should.start, format="%H:%M")
      timespan <- paste("T", should.start, "/T", should.end, sep="")
    })
  }
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  signal[timespan] <- TRUE
  colnames(signal) <- label  
  return(signal)
}

sigRange <- function(label, data=mktdata, column, upper, lower) {
  xx <- data[, column]
  signal <- xx < upper & xx > lower
  if (!is.null(label)) {
    colnames(signal) <- label
  }
  return(signal)  
}

sigBuyLong <- function(label, data=mktdata, adx.range, enter.delay=NULL) {
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  cound.enter <- sigCouldEnter(label="", data, enter.delay=enter.delay)
  in.range <- sigRange(label="in.range", data=data, column="adx.ADX", upper=max(adx.range), lower=min(adx.range))
  crossup <- sigCrossover(label="", data=data, columns=c("fast.EMA", "slow.EMA"), relationship="gte")
  signal <- cound.enter & in.range & crossup
  colnames(signal) <- label
  return(signal)
}

sigSellShort <- function(label, data = mktdata, adx.range, enter.delay=NULL) {
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  cound.enter <- sigCouldEnter(label="", data, enter.delay=enter.delay)  
  in.range <- sigRange(label="in.range", data=data, column="adx.ADX", upper=max(adx.range), lower=min(adx.range))
  crossup <- sigCrossover(label="", data=data, columns=c("fast.EMA", "slow.EMA"), relationship="lt")
  signal <- cound.enter & in.range & crossup
  colnames(signal) <- label
  return(signal)
}

sigMarketClose <- function(label, data = mktdata, timespan) {
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  signal[timespan] <- TRUE
  colnames(signal) <- label  
  return(signal)
}

init.strat <- function() {
  
}



strategy.name <- "dma_adx"
portfolio.name <- strategy.name
account.name <- strategy.name
symbol <- "IF1201"

# clear out evironment
rm.strat(strategy.name)

currency('RMB')
currency('USD')
future(symbol, currency='RMB', multiplier=300)

getSymbols(symbol, src="RData", dir=current.dir,  col.names=c("High","Low","Close","Bid", "Ask", "Volume"))
mrk.data <- get(symbol)
period.data <- periodicity(mrk.data)
start.time <- period.data$start

initPortf(portfolio.name, symbols=symbol, initDate=start.time)
initAcct(account.name, portfolios=portfolio.name, initDate=start.time)
initOrders(portfolio=portfolio.name, initDate=start.time)

strat <- strategy(strategy.name)

# add init functions
strat <- add.init(strat, name="init.strat", arguments=list())
# indicators
strat <- add.indicator(strat, label="slow.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=slowEMA))
strat <- add.indicator(strat, label="fast.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=fastEMA))
strat <- add.indicator(strat, label="adx", name='ADX', arguments=list(HLC=quote(ABC(mktdata))))

# signals
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fast.EMA", "slow.EMA"), relationship="gte"), label="fast.cross.above.slow")
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fast.EMA", "slow.EMA"), relationship="lt"), label="fast.cross.below.slow")
strat <- add.signal(strat, label="buy.long", name="sigBuyLong", arguments=list(data=quote(mktdata), adx.range=c(adx.min, adx.max), enter.delay=slowEMA))
strat <- add.signal(strat, label="sell.short", name="sigSellShort", arguments=list(data=quote(mktdata), adx.range=c(adx.min, adx.max), enter.delay=slowEMA))
strat <- add.signal(strat, name="sigMarketClose", arguments = list(data=quote(mktdata), timespan=exit.timespan), label="market.close") 

# mkt <- get(symbol)
# mkt <- applySignals(strat, mktdata=applyIndicators(strat, mktdata=get(symbol))) #for debugging
# print(tail(mkt, n=30))

# rules
# buy long
strat <- add.rule(strat, label="signal.enter", name="ruleSignal", arguments = list(sigcol="buy.long", sigval=T, orderqty=qty, ordertype='market', orderside='long', pricemethod='market', replace=F, TxnFees=txnfee), type='enter', timespan=enter.timespan)
strat <- add.rule(strat, label='signal.exit', name="ruleSignal", arguments = list(sigcol="fast.cross.below.slow", sigval=T, orderqty="all", ordertype='market', orderside='long', pricemethod='market', replace=F, TxnFees=txnfee, orderset="exit"), type='exit')
# strat <- add.rule(strat, label='stoploss.exit', name="ruleSignal", arguments = list(sigcol="buy.long", sigval=T, orderqty="all", ordertype='stoplimit', orderside='long', threshold=stoploss.threshold, tmult=T, TxnFees=txnfee, orderset="exit2"), type='risk')
strat <- add.rule(strat, label='stoptrailing.exit', name="ruleSignal", arguments = list(sigcol="buy.long", sigval=T, orderqty="all", ordertype='stoptrailing', orderside='long', threshold=stoptrailing.threshold, tmult=F, TxnFees=txnfee, orderset="exit2"), type='risk')
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="market.close", sigval=T, orderqty="all", ordertype='market', orderside='long'), type='risk')

# sell short
strat <- add.rule(strat, label="signal.enter", name="ruleSignal", arguments = list(sigcol="sell.short", sigval=T, orderqty=-qty, ordertype='market', orderside='short', pricemethod='market', replace=F, TxnFees=txnfee), type='enter', timespan=enter.timespan)
strat <- add.rule(strat, label='signal.exit', name="ruleSignal", arguments = list(sigcol="fast.cross.above.slow", sigval=T, orderqty="all", ordertype='market', orderside='short', pricemethod='market', replace=F, TxnFees=txnfee, orderset="exit"), type='exit')
# strat <- add.rule(strat, label='stoploss.exit', name="ruleSignal", arguments = list(sigcol="sell.short", sigval=T, orderqty="all", ordertype='stoplimit', orderside='short', threshold=stoploss.threshold, tmult=T, TxnFees=txnfee, orderset="exit2"), type='risk')
strat <- add.rule(strat, label='stoptrailing.exit', name="ruleSignal", arguments = list(sigcol="sell.short", sigval=T, orderqty="all", ordertype='stoptrailing', orderside='short', threshold=-stoptrailing.threshold, tmult=F, TxnFees=txnfee, orderset="exit2"), type='risk')
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="market.close", sigval=T, orderqty="all", ordertype='market', orderside='short'), type='risk')

addPosLimit(portfolio.name, symbol, timestamp=start.time, maxpos=qty, minpos=-qty)

# out <- applyStrategy(strat, portfolios=portfolio.name, initStrat=F, updateStrat=F)
# updatePortf(Portfolio=portfolio.name)
# # plot results
# chart.Posn(Portfolio=portfolio.name)
# p <- getPortfolio(portfolio.name)
# s <- p$symbols[[symbol]]
# mkt <- get(symbol)
# adx <- ADX(ABC(mkt))$ADX
# plot(add_TA(EMA(Cl(mkt), n=slowEMA), col="red"))
# plot(add_TA(EMA(Cl(mkt), n=fastEMA), on=4, col='blue'))
# plot(add_TA(adx))
# plot(add_TA(xts(rep(adx.min, nrow(mkt)), order.by=index(mkt)), on=5, col="red"))
# plot(add_TA(xts(rep(adx.max, nrow(mkt)), order.by=index(mkt)), on=5, col="red"))
# 
# # look at the order book
# book <- getOrderBook(portfolio.name)
# book <- book[[strategy.name]][[symbol]]
# 
# dailyEqPL(portfolio.name, drop.time=F)
# dailyTxnPL(portfolio.name, drop.time=F)
# print(tradeStats(portfolio.name))
# dailyStats(portfolio.name)
# getEndEq(account.name, Date=Sys.time())
# getPosQty(portfolio.name, Symbol=symbol, Date=Sys.time())


#print(getParameterTable(strat))
rm(pd, pc)
pd <- setParameterDistribution(type='indicator', indexnum=1, distribution=list(n=(9:10)), label='fastEMA')
pd <- setParameterDistribution(pd, type='indicator', indexnum=2, distribution=list(n=(40:41)), label='slowEMA')
#pd <- setParameterDistribution(pd, type='exit', indexnum=1, distribution=list(threshold=seq(0.01, 0.03, by=0.005)), label='exit.threshold')

#pd <- setParameterDistribution(pd, type='signal', indexnum=1, distribution=list(relationship=c('gt', 'gte')), label='sig1.gtgte')

pc <- setParameterConstraint(constraintLabel='ma.pc', paramList=c('fastEMA', 'slowEMA'), relationship='lt')
testPackList <- applyParameter(strategy=strat, portfolios=portfolio.name, parameterPool=pd, method='expand', sampleSize=10, parameterConstraints=pc, verbose=T)

testPackList$eachRun
