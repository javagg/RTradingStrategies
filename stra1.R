.updatePosPL <- function(Portfolio, Symbol, Dates=NULL, Prices=NULL, ConMult=NULL, ...)
{ # @author Peter Carl, Brian Peterson
  rmfirst=FALSE
  prices=NULL
  pname<-Portfolio
  Portfolio<-getPortfolio(pname) 
  p.ccy.str<-attr(Portfolio,'currency')
  if(is.null(p.ccy.str)) p.ccy.str<-'NA'
  tmp_instr<-try(getInstrument(Symbol))
  if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
    warning(paste("Instrument",Symbol," not found, things may break"))
    tmp_instr<-list(currency="USD",multiplier=1)
  }
  dargs <- list(...)
  if(!is.null(dargs$env)) {env <- dargs$env} else env=.GlobalEnv
  if(!is.null(dargs$symbol)) {symbol<-dargs$symbol} else symbol=NULL
  if(!is.null(dargs$prefer)) {prefer<-dargs$prefer} else prefer=NULL
  if(is.null(Prices)){
    prices=getPrice(get(Symbol, pos=env), symbol=symbol, prefer=prefer)[,1]
  } else {
    prices=Prices
  }
  
  # if no date is specified, get all available dates
  if(is.null(Dates)) {
    Dates = time(prices)
  } else if(!is.timeBased(Dates)) {
    Dates = time(prices[Dates])
  }

  if(.parseISO8601(Dates)$first.time < as.POSIXct(first(index(prices))) || is.na(.parseISO8601(Dates)$first.time)){
    last.time<-last(Dates)
    Dates<-index(prices[paste('/',last.time,sep='')])
  }
  
  if(ncol(prices)>1) prices=getPrice(Prices,Symbol)
  
  # line up Prices dates with Dates set/index/span passed in.
  startDate = first(xts:::.parseISO8601(Dates))$first.time-1 #does this need to be a smaller/larger delta for millisecond data?
  endDate   = last(xts:::.parseISO8601(Dates))$last.time
  if(is.na(endDate)) endDate<-NULL
  dateRange = paste(startDate,endDate,sep='::')
  
  #subset Prices by dateRange too...
  Prices<-prices[dateRange]
  
  if(nrow(Prices)<1) {
    Prices=xts(cbind(Prices=as.numeric(last(prices[paste('::',endDate,sep='')]))),as.Date(endDate))
    warning('no Prices available for ',Symbol,' in ',dateRange,' : using last available price and marking to ', endDate)
  }
  
  # Prices <- Prices[dateRange][,1] # only take the first column, if there is more than one
  
  colnames(Prices)<-'Prices' # name it so we can refer to it by name later
  
  #	***** Vectorization *****#
  # trim posPL slot to not double count, related to bug 831 on R-Forge 
  Portfolio$symbols[[Symbol]]$posPL<-Portfolio$symbols[[Symbol]]$posPL[paste('::',startDate,sep='')]
  Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]]<-Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]][paste('::',startDate,sep='')]
  priorPL<-last(Portfolio$symbols[[Symbol]]$posPL)
  if(nrow(priorPL)==0) {
    cn<-colnames(priorPL)
    priorPL = xts(t(rep(0,ncol(priorPL))),order.by=startDate-1)
    colnames(priorPL)<-cn
  }
  
  Txns <- Portfolio$symbols[[Symbol]]$txn[dateRange]
  # if there are no transactions, get the last one before the current dateRange, we'll discard later
  if(nrow(Txns)==0) {
    Txns <- last(Portfolio$symbols[[Symbol]]$txn[paste('::',startDate,sep='')])
  } 
  
  #	 line up transaction with Dates list
  tmpPL <- merge(Txns, priorPL, Prices) # most Txn columns will get discarded later, as will the rows from 'before' the startDate
  
  #browser()
  
  if(is.na(tmpPL$Prices[1])){
    #first price is NA, it would be nice to fill it in with a previous last valid price
    fprice <- last(prices[paste('::',startDate,sep='')])
    if (length(fprice)==1) tmpPL$Prices[1] <- fprice 
  }
  
  # na.locf any missing prices with last observation (this assumption seems the only rational one for vectorization)
  tmpPL$Prices <- na.locf(tmpPL$Prices)
  
  # na.locf Pos.Qty,Con.Mult,Pos.Avg.Cost to instantiate $posPL new rows	
  #tmpPL$Pos.Qty.1 <- na.locf(tmpPL$Pos.Qty.1)
  #lagPosQty<-Lag(tmpPL$Pos.Qty.1)
  tmpPL$Pos.Qty <- ifelse(is.na(tmpPL$Pos.Qty) & !is.na(tmpPL$Pos.Qty.1), tmpPL$Pos.Qty.1, tmpPL$Pos.Qty)
  #tmpPL$Pos.Qty <- ifelse(is.na(tmpPL$Pos.Qty) & !is.na(lagPosQty), tmpPL$Pos.Qty.1, tmpPL$Pos.Qty)
  tmpPL$Pos.Qty <- na.locf(tmpPL$Pos.Qty)
  
  
  #TODO check for instrument multiplier rather than doing all this messing around, if possible.
  tmpPL$Con.Mult.1 <- na.locf(tmpPL$Con.Mult.1)
  tmpPL$Con.Mult.1 <- ifelse(is.na(tmpPL$Con.Mult) & !is.na(tmpPL$Con.Mult.1) , tmpPL$Con.Mult.1, tmpPL$Con.Mult)
  tmpPL$Con.Mult <- na.locf(tmpPL$Con.Mult)
  tmpPL$Con.Mult <- na.locf(tmpPL$Con.Mult, fromLast=TRUE) # carry NA's backwards too, might cause problems with options contracts that change multiplier
  tmpPL$Con.Mult <- ifelse(is.na(tmpPL$Con.Mult) ,1, tmpPL$Con.Mult)
  
  tmpPL$Pos.Avg.Cost.1 <- na.locf(tmpPL$Pos.Avg.Cost.1)
  tmpPL$Pos.Avg.Cost <- ifelse(is.na(tmpPL$Pos.Avg.Cost) & !is.na(tmpPL$Pos.Avg.Cost.1) ,tmpPL$Pos.Avg.Cost.1, tmpPL$Pos.Avg.Cost)
  tmpPL$Pos.Avg.Cost <- na.locf(tmpPL$Pos.Avg.Cost)
  
  # zerofill Txn.Value, Txn.Fees
  tmpPL$Txn.Value <- ifelse(is.na(tmpPL$Txn.Value),0, tmpPL$Txn.Value)
  
  tmpPL$Txn.Fees  <- ifelse(is.na(tmpPL$Txn.Fees) ,0, tmpPL$Txn.Fees)
  
  # matrix calc Pos.Qty * Price * Con.Mult to get Pos.Value
  tmpPL$Pos.Value <- tmpPL$Pos.Qty * tmpPL$Con.Mult * tmpPL$Prices
  
  LagValue<-Lag(tmpPL$Pos.Value)
  LagValue<-ifelse(is.na(LagValue),0,LagValue) # needed to avoid a possible NA on the first value that would mess up the Gross.Trading.PL calc
  tmpPL$Gross.Trading.PL <- tmpPL$Pos.Value- LagValue - tmpPL$Txn.Value
  
  
  # alternate matrix calc for Realized&Unrealized PL that is only dependent on Txn PL and Gross.Trading.PL
  tmpPL$Net.Txn.Realized.PL <- ifelse(is.na(tmpPL$Net.Txn.Realized.PL),0,tmpPL$Net.Txn.Realized.PL)
  tmpPL$Gross.Txn.Realized.PL <- ifelse(is.na(tmpPL$Gross.Txn.Realized.PL),0,tmpPL$Gross.Txn.Realized.PL)
  
  #tmpPL$Gross.Trading.PL <- tmpPL$Pos.Value - (tmpPL$Pos.Qty*tmpPL$Pos.Avg.Cost) +  tmpPL$Gross.Txn.Realized.PL
  tmpPL$Period.Realized.PL <- tmpPL$Gross.Txn.Realized.PL
  tmpPL$Period.Unrealized.PL <- round(tmpPL$Gross.Trading.PL - tmpPL$Period.Realized.PL,2)
  
  # matrix calc Net.Trading.PL as Gross.Trading.PL + Txn.Fees
  tmpPL$Net.Trading.PL <- tmpPL$Gross.Trading.PL + tmpPL$Txn.Fees
  
  # Ccy.Mult for this step is always 1
  tmpPL$Ccy.Mult<-rep(1,nrow(tmpPL))
  
  # reorder,discard  columns for insert into portfolio object
  tmpPL <- tmpPL[,c('Pos.Qty', 'Con.Mult', 'Ccy.Mult', 'Pos.Value', 'Pos.Avg.Cost', 'Txn.Value',  'Period.Realized.PL', 'Period.Unrealized.PL','Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')]
  
  # rbind to $posPL slot
  tmpPL <- tmpPL[dateRange] #subset to get rid of any prior period Txn or PosPL rows we inserted
  Portfolio$symbols[[Symbol]]$posPL<-rbind(Portfolio$symbols[[Symbol]]$posPL,tmpPL)
  
  
  
  # now do the currency conversions for the whole date range
  TmpPeriods<-Portfolio$symbols[[Symbol]]$posPL[dateRange]
  
  CcyMult = NA 
  FXrate = NA
  invert=FALSE
  if(!is.null(attr(Portfolio,'currency'))) {
    if (tmp_instr$currency==p.ccy.str) {
      CcyMult<-1			
    } else {
      port_currency<-try(getInstrument(p.ccy.str))
      if(inherits(port_currency,"try-error") | !is.instrument(port_currency)){
        warning("Currency",p.ccy.str," not found, using currency multiplier of 1")
        CcyMult<-1
      } else { #convert from instr ccy to portfolio ccy
        FXrate.str<-paste(tmp_instr$currency, p.ccy.str, sep='') # currency quote convention is EURUSD which reads as "USD per EUR"
        FXrate<-try(get(FXrate.str), silent=TRUE)
        #TODO FIXME: this uses convention to sort out the rate, we should check $currency and $counter_currency and make sure directionality is correct 
        if(inherits(FXrate,"try-error")){
          FXrate.str<-paste(p.ccy.str, tmp_instr$currency, sep='')
          FXrate<-try(get(FXrate.str), silent=TRUE)
          if(inherits(FXrate,"try-error")){ 
            warning("Exchange Rate",FXrate.str," not found for symbol,',Symbol,' using currency multiplier of 1")
            CcyMult<-1
          } else {
            invert=TRUE
          }
        }
      }		
      
    }
  } else {
    message("no currency set on portfolio, using currency multiplier of 1")
    CcyMult =1
  }
  if(is.na(CcyMult) && !is.na(FXrate)) {
    if(inherits(FXrate,'xts')){
      CcyMult <- FXrate[dateRange]
      CcyMult <- na.locf(merge(CcyMult,index(TmpPeriods)))
      CcyMult <- drop(CcyMult[index(TmpPeriods)])
    } else {
      CcyMult<-as.numeric(FXrate)
    }
  } else {
    CcyMult<-1
  }
  if(isTRUE(invert)){
    # portfolio and instrument have different currencies, and FXrate was in the wrong direction
    CcyMult<-1/CcyMult
  }
  
  
  #multiply the correct columns 
  columns<-c('Pos.Value', 'Txn.Value', 'Pos.Avg.Cost', 'Period.Realized.PL', 'Period.Unrealized.PL','Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
  TmpPeriods[,columns]<-TmpPeriods[,columns]*CcyMult
  TmpPeriods[,'Ccy.Mult']<-CcyMult
  
  #add change in Pos.Value in base currency
  LagValue <- as.numeric(last(Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]]$Pos.Value))
  if(length(LagValue)==0) LagValue <- 0
  LagPos.Value <- lag(TmpPeriods$Pos.Value,1)
  LagPos.Value[1] <- LagValue
  CcyMove <- TmpPeriods$Pos.Value - LagPos.Value - TmpPeriods$Txn.Value - TmpPeriods$Period.Unrealized.PL - TmpPeriods$Period.Realized.PL
  TmpPeriods$Gross.Trading.PL <- TmpPeriods$Gross.Trading.PL + CcyMove
  TmpPeriods$Net.Trading.PL <- TmpPeriods$Net.Trading.PL + CcyMove
  TmpPeriods$Period.Unrealized.PL <- TmpPeriods$Period.Unrealized.PL + CcyMove
  
  #stick it in posPL.ccy
  Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]]<-rbind(Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]],TmpPeriods)
  
  # assign Portfolio to environment
  assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
}

assignInNamespace(".updatePosPL", .updatePosPL, ns="blotter")


require(quantstrat)
require(qmao)
require(PerformanceAnalytics)

# this.dir <- dirname(parent.frame(2)$ofile)
# load(file.path(this.dir, "sample.500ms.rda"))
# mkt <- sample.500ms
# mkt <- mkt["T09:14/T15:15"]
# mkt <- to.period(mkt, "seconds", k = 1)
# mkt <- to.minutes(mkt)

strategy.name <- "ma_adx"
# clear out evironment
try(rm(list=ls(pos=.blotter), pos=.blotter), silent=T)
try(rm(list=ls(pos=.strategy), pos=.strategy), silent=T)
try(rm(list=ls(pos=.instrument), pos=.instrument), silent=T)

contract <- 'IF1206'
portfolio.name <- strategy.name
account.name <- strategy.name

fastMA <- 2 
slowMA <- 12
qty <- 100
txnfee <- -20

begin.date <- "2011-12-31"
end.date <- Sys.Date()
start.time <- '2012-03-14T09:14:59'


currency('USD')
print(ls_currencies())
stock(contract, currency='USD', multiplier=1)
# print(ls_stocks())

# getData
contract.data <- get(contract)

start.time <- '2012-03-14T09:14:59' 
start.time <- format(index(first(contract.data)), format="%Y-%m-%dT%H:%M:%S")
initial.equity <- 1000000

initPortf(portfolio.name, symbols=contract, initDate=start.time)
initAcct(account.name, portfolios=portfolio.name, initDate=start.time)
initOrders(portfolio=portfolio.name, initDate=start.time)

strat <- strategy(strategy.name)

# indicators
strat <- add.indicator(strat, label="slow.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=12))
strat <- add.indicator(strat, label="fast.EMA", name='EMA', arguments=list(x=quote(Cl(mktdata)), n=2))
strat <- add.indicator(strat, label="adx", name='ADX', arguments=list(HLC=quote(HLC(mktdata))))

# signals
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fast.EMA", "slow.EMA"), relationship="gte"), label="fast.cross.above.slow")
strat <- add.signal(strat, name="sigCrossover", arguments = list(columns=c("fast.EMA", "slow.EMA"), relationship="lt"), label="fast.cross.below.slow")
# strat <- add.signal(strat, name="sigThreshold", arguments = list(column="ADX", relationship="gte", threshold=28), label="adx.gte.threshold")
# strat <- add.signal(strat, name="sigThreshold", arguments = list(column="ADX", relationship="lt", threshold=28), label="adx.lt.threshold") 

sigBuyLong <- function(label, data = mktdata) {
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  trend <- sigThreshold(label="", data=mktdata, column="adx.ADX", threshold=28, relationship="gte")
  crossup <- sigCrossover(label="", data=mktdata, columns=c("fast.EMA", "slow.EMA"), relationship="gte")
  signal <- trend & crossup
  colnames(signal) <- label
  return(signal)
}
strat <- add.signal(strat, label="buy.long", name="sigBuyLong", arguments=list(data=quote(mktdata)))

sigSellShort <- function(label, data = mktdata) {
  signal <- xts(rep(FALSE, nrow(data)), order.by=index(data))
  trend <- sigThreshold(label="", data=mktdata, column="adx.ADX", threshold=28, relationship="lt")
  crossup <- sigCrossover(label="", data=mktdata, columns=c("fast.EMA", "slow.EMA"), relationship="lt")
  signal <- trend & crossup
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
# mkt <- applySignals(strat, mktdata=applyIndicators(strat, mktdata=IF2016)) #for debugging
# print(head(mkt, n=300))

# rules
# buy long
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="buy.long", sigval=TRUE, orderqty=100, ordertype='market', orderside='long', pricemethod='market', replace=FALSE, TxnFees=-2), type='enter', path.dep=TRUE)
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="fast.cross.below.slow", sigval=TRUE, orderqty="all", ordertype='market', orderside='long', pricemethod='market', replace=FALSE, TxnFees=-2), type='exit', path.dep=TRUE)
# sell short
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="sell.short", sigval=TRUE, orderqty=-100, ordertype='market', orderside='short', pricemethod='market', replace=FALSE, TxnFees=-2), type='enter', path.dep=TRUE)
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="fast.cross.above.slow", sigval=TRUE, orderqty="all", ordertype='market', orderside='short', pricemethod='market', replace=FALSE, TxnFees=-2), type='exit', path.dep=TRUE)

strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="market.close", sigval=TRUE, orderqty="all", ordertype='market', orderside='long'), type='risk')
strat <- add.rule(strat, name="ruleSignal", arguments = list(sigcol="market.close", sigval=TRUE, orderqty="all", ordertype='market', orderside='short'), type='risk')

addPosLimit(portfolio.name, contract, timestamp=initDate, maxpos=qty, minpos=0)
addPosLimit(portfolio.name, contract, timestamp=initDate, maxpos=qty, minpos=0)

out <- applyStrategy(strat, portfolios=portfolio.name)

# print("updateProf")
#updatePortf(Portfolio=portfolio.name, Dates=paste(start.time, '::', format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), sep=''))
#updatePortf(Portfolio=portfolio.name, Dates="2012-05-21T09:15:59::2012-07-10T17:01:38")
updatePortf(Portfolio=portfolio.name, Dates="2011/2012")

chart.Posn(Portfolio=portfolio.name)
# plot(add_MACD(fast=fastMA, slow=slowMA, signal=signalMA,maType="EMA"))

#look at the order book
getOrderBook(portfolio.name)

dailyEqPL(portfolio.name, drop.time=F)
dailyTxnPL(portfolio.name, drop.time=F)
tradeStats(portfolio.name)
dailyStats(portfolio.name)
getEndEq(account.name, Date=Sys.time())
getPosQty(portfolio.name, Symbol=contract, Date=Sys.time())
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

