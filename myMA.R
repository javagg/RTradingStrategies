#########################################################################################################################################################################
#A simple moving average strategy to evaluate trade efficiency
#checks on SMA of 50 days and SMA of 200 days
#Author: R. Raghuraman("raghu"), Brian Peterson
#########################################################################################################################################################################

current.dir <- dirname(parent.frame(2)$ofile)
source(file.path(current.dir, "quantstrat-addon.R"))
source(file.path(current.dir, "quantstrat-myrule.R"))
require(quantstrat)

options(width=240)
suppressWarnings(rm("order_book.macross",pos=.strategy))
suppressWarnings(rm("account.macross","portfolio.macross",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratMACROSS","initDate","initEq",'start_t','end_t'))
stock.str='MSFT' # what are we trying it on
currency('USD')
stock(stock.str,currency='USD',multiplier=1)
initDate='2008-06-30'
initEq=1000000
portfolio.st='macross'
account.st='macross'
initPortf(portfolio.st,symbols=stock.str, initDate=initDate)
initAcct(account.st,portfolios=portfolio.st, initDate=initDate)
initOrders(portfolio=portfolio.st,initDate=initDate)

stratMACROSS<- strategy(portfolio.st)

stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=50),label= "ma50" )
stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=200),label= "ma200")

stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(columns=c("ma50","ma200"), relationship="gte"),label="ma50.gt.ma200")
stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(column=c("ma50","ma200"),relationship="lt"),label="ma50.lt.ma200")

stratMACROSS <- add.rule(strategy = stratMACROSS, name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty=100, ordertype='market', orderside='long'),type='enter')
stratMACROSS <- add.rule(strategy = stratMACROSS, label="signalexit", name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty="all", ordertype='market', orderside='long', orderset="exit2"),type='exit')
# take-profit exit

#stratMACROSS <- add.rule(strategy = stratMACROSS, label="takeprofitexit", name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200", sigval=TRUE, orderqty="all", ordertype='limit', orderside='long', threshold=+1, tmult=F, orderset="altexit1"),type='exit')
# stop-loss exit
#stratMACROSS <- add.rule(strategy = stratMACROSS, label="stoplossexit", name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty="all", ordertype='stoplimit', orderside='long', threshold=-2,tmult=F, orderset="altexit"),type='exit')

# trailing stop exit
stratMACROSS <- add.rule(strategy = stratMACROSS, label="stoptrailingexit", name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty="all", ordertype='stoptrailing', orderside='long', threshold=-2.5,tmult=F, orderset="altexit"), type='exit')

# if you want a long/short Stops and Reverse MA cross strategy, you'd add two more rules for the short side:

# stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty=-100, ordertype='market', orderside='short'),type='enter')
# stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty="all", ordertype='market', orderside='short'),type='exit')

getSymbols(stock.str,from=initDate)
for(i in stock.str)
  assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))

start_t <- Sys.time()
out <- try(applyStrategy(strategy=stratMACROSS , portfolios=portfolio.st))
end_t<-Sys.time()
print(end_t-start_t)

start_t<-Sys.time()
updatePortf(Portfolio='macross',Dates=paste('::',as.Date(Sys.time()),sep=''))
end_t<-Sys.time()
print("trade blotter portfolio update:")
print(end_t-start_t)

chart.Posn(Portfolio='macross',Symbol=stock.str)
plot(add_SMA(n=50 , on=1,col='blue'))
plot(add_SMA(n=200, on=1))

ob <- getOrderBook('macross')
print(ob)
