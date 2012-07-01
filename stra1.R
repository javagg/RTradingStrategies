this.dir <- dirname(parent.frame(2)$ofile)

require(quantstrat)
require(qmao)

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

initPortf(portfolio.name, symbols=contract, initDate=start.time)
initAcct(account.name, portfolios=portfolio.name, initDate=start.time)
initOrders(portfolio=portfolio.name, initDate=start.time)

strategy.obj <- strategy(strategy.name)
strategy.obj <- add.indicator(strategy = strategy.obj, name = "BBands", arguments=list(HLC=quote(HLC(mktdata)), maType='SMA'))

chartSeries(mktdata, type="bar", yrange=c(2580, 2710), TA=c(addVo(),addBBands()), theme=chartTheme('white'))
addTA(EMA(Cl(mktdata), n=12), on=1, col="pink")
addTA(EMA(Cl(mktdata), n=2), on=1, col="blue")
addRSI()

strat <- strategy("example")
strat <- add.indicator(strat, label="slow.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=12))
strat <- add.indicator(strat, label="fast.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=2))

strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("Close", "slow.EMA"), relationship="lt"), label="Cl.lt.slow.EMA")

strat <- add.signal(strat, name="sigThreshold", arguments = list(column="signal",relationship="gt", threshold=0, cross=TRUE), label="signal.gt.zero")
strat <- add.signal(strat, name="sigThreshold", arguments = list(column="signal",relationship="lt", threshold=0, cross=TRUE), label="signal.lt.zero") 


# stratRSI <- add.indicator(strategy=stratRSI, name="RSI", arguments=list(price = quote(getPrice(mktdata))), label="RSI")
# stratRSI <- add.signal(strategy=stratRSI, name="sigThreshold", arguments=list(threshold=70, column="RSI", relationship="gt", cross=TRUE), label="RSI.gt.70")
# stratRSI <- add.signal(strategy=stratRSI, name="sigThreshold", arguments=list(threshold=30, column="RSI", relationship="lt", cross=TRUE), label="RSI.lt.30")

print(stra)