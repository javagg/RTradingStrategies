#########################################################################################################################################################################
#A simple moving average strategy to evaluate trade efficiency
#checks on SMA of 50 days and SMA of 200 days
#Author: R. Raghuraman("raghu"), Brian Peterson
#########################################################################################################################################################################

require(quantstrat)

ruleOrderProc <- function (portfolio, symbol, mktdata, timespan = NULL, ordertype = NULL, 
          ..., slippageFUN = NULL) 
{
  if (is.null(timespan)) 
    return()
  orderbook <- getOrderBook(portfolio)
  ordersubset <- orderbook[[portfolio]][[symbol]]
  OpenOrders.i = NULL
  OpenOrders.i <- getOrders(portfolio = portfolio, symbol = symbol, 
                            status = "open", timespan = timespan, ordertype = ordertype, 
                            which.i = TRUE)
  if (hasArg(prefer)) 
    prefer = match.call(expand.dots = TRUE)$prefer
  else prefer = NULL
  if (length(OpenOrders.i) >= 1) {
    prevtime <- time(mktdata[last(mktdata[timespan, which.i = TRUE]) - 1])
    timestamp <- time(last(mktdata[timespan]))
    freq = periodicity(mktdata)
    neworders <- NULL
    mktdataTimestamp <- mktdata[timestamp]
    if (NROW(mktdataTimestamp) > 1) 
      mktdataTimestamp <- last(mktdataTimestamp)
    isOHLCmktdata <- is.OHLC(mktdata)
    isBBOmktdata <- is.BBO(mktdata)
    for (ii in OpenOrders.i) {
      if (ordersubset[ii, "Order.Status"] != "open") {
        next()
      }
      txnprice = NULL
      txnfees = ordersubset[ii, "Txn.Fees"]
      orderPrice <- as.numeric(ordersubset[ii, "Order.Price"])
      orderQty <- ordersubset[ii, "Order.Qty"]
      if (orderQty == "all") {
        orderQty = -1 * getPosQty(Portfolio = portfolio, Symbol = symbol, Date = timestamp)
        print(orderQty)
        orderside <- ordersubset[ii, "Order.Side"]
        if (((orderQty > 0 && orderside == "long") || 
          (orderQty < 0 && orderside == "short"))) {
          orderQty = 0
        }
      }
      orderQty <- as.numeric(orderQty)
      orderThreshold <- as.numeric(ordersubset[ii, "Order.Threshold"])
      orderType <- ordersubset[ii, "Order.Type"]
      
      switch(orderType, market = {
        switch(freq$scale, yearly = , quarterly = , monthly = {
          txntime = as.character(index(ordersubset[ii,]))
          txnprice = as.numeric(getPrice(last(mktdata[txntime]), prefer = prefer)[, 1])
        }, {
          txntime = timestamp
          if (isBBOmktdata) {
            if (orderQty > 0) {
              txnprice = max(orderPrice, as.numeric(getPrice(mktdataTimestamp, prefer = "ask")[, 1]))
            } else {
              txnprice = min(orderPrice, as.numeric(getPrice(mktdataTimestamp, prefer = "bid")[, 1]))
            }
          } else txnprice = as.numeric(getPrice(mktdataTimestamp, prefer = prefer)[, 1])
        }) # switch(freq$scale)
      }, limit = , stoplimit = , iceberg = {
        if (!isBBOmktdata) {
          if (orderType == "iceberg") {
            stop("iceberg orders only supported for BBO data")
          }
          if ((orderQty > 0 && orderType != "stoplimit") || 
            (orderQty < 0 && orderType == "stoplimit")) {
            if ((has.Lo(mktdata) && orderPrice > as.numeric(Lo(mktdataTimestamp))) || 
              (!has.Lo(mktdata) && orderPrice >= as.numeric(getPrice(mktdataTimestamp, 
                                                                     prefer = prefer)))) {
              txnprice = orderPrice
              txntime = timestamp
            } else next()
          } else if ((orderQty < 0 && orderType != "stoplimit") || 
            (orderQty > 0 && orderType == "stoplimit")) {
            if ((has.Hi(mktdata) && orderPrice < as.numeric(Hi(mktdataTimestamp))) || 
              (!has.Hi(mktdata) && orderPrice <= as.numeric(getPrice(mktdataTimestamp, 
                                                                     prefer = prefer)))) {
              txnprice = orderPrice
              txntime = timestamp
            } else next()
          } else {
            warning("ignoring order with quantity of zero")
            next()
          }
        } else if (isBBOmktdata) {
          if (orderQty > 0) {
            if (orderType == "stoplimit") {
              if (orderPrice <= as.numeric(getPrice(mktdataTimestamp, 
                                                    prefer = "ask")[, 1])) {
                txnprice = orderPrice
                txntime = timestamp
              } else next()
            } else {
              if (orderPrice >= as.numeric(getPrice(mktdataTimestamp, 
                                                    prefer = "ask")[, 1])) {
                txnprice = as.numeric(getPrice(mktdataTimestamp, 
                                               prefer = "ask")[, 1])
                txntime = timestamp
              } else next()
            }
          } else {
            if (orderType == "stoplimit") {
              if (orderPrice >= as.numeric(getPrice(mktdataTimestamp, 
                                                    prefer = "bid")[, 1])) {
                txnprice = orderPrice
                txntime = timestamp
              } else next()
            } else {
              if (orderPrice <= as.numeric(getPrice(mktdataTimestamp, 
                                                    prefer = "bid")[, 1])) {
                txnprice = as.numeric(getPrice(mktdataTimestamp, 
                                               prefer = "bid")[, 1])
                txntime = timestamp
              } else next()
            }
          }
          if (orderType == "iceberg") {
            neworder <- addOrder(portfolio = portfolio, 
                                 symbol = symbol, timestamp = timestamp, 
                                 qty = orderQty, price = as.numeric(getPrice(mktdataTimestamp, 
                                                                             prefer = prefer)[, 1]), ordertype = orderType, 
                                 side = ordersubset[ii, "Order.Side"], threshold = orderThreshold, 
                                 status = "open", replace = FALSE, return = TRUE, 
                                 , ... = ..., TxnFees = txnfees)
            if (is.null(neworders)) neworders = neworder else neworders = rbind(neworders, 
                                                                                neworder)
            ordersubset[ii, "Order.Status"] <- "replaced"
            ordersubset[ii, "Order.StatusTime"] <- format(timestamp, 
                                                          "%Y-%m-%d %H:%M:%S")
            next()
          }
        }
      }, stoptrailing = {
        print("handle stoptrailing!")
        print(orderQty)
        if (orderQty > 0) {
          #if (isBBOmktdata) prefer = "offer"
          if (isBBOmktdata) prefer = "ask"
          if (orderPrice >= getPrice(mktdataTimestamp, 
                                     prefer = prefer)[, 1]) {
            txnprice = as.numeric(getPrice(mktdataTimestamp, 
                                           prefer = prefer)[, 1])
            txntime = timestamp
          }
        } else {
          if (isBBOmktdata) prefer = "bid"
          if (orderPrice <= getPrice(mktdataTimestamp, 
                                     prefer = prefer)[, 1]) {
            txnprice = as.numeric(getPrice(mktdataTimestamp, prefer = prefer)[, 1])
            txntime = timestamp
          }
        }
        if (isOHLCmktdata) {
          if (orderPrice > as.numeric(Lo(mktdataTimestamp)) & 
            orderPrice < as.numeric(Hi(mktdataTimestamp))) {
            txnprice = orderPrice
            txntime = timestamp
          }
        }
        if (is.null(txnprice)) {
          print("move?")
          if (isOHLCmktdata) {
            prefer = "close"
          } else if (isBBOmktdata) {
            if (orderQty > 0) {
              #prefer = "offer"
              prefer = "ask"
            } else {
              prefer = "bid"
            }
          } else {
            prefer = NULL
          }
          mvstop = FALSE

          if (orderQty > 0) {
            if (as.numeric(last(getPrice(x = mktdataTimestamp, prefer = prefer)[, 1])) + orderThreshold < orderPrice) {
              mvstop = TRUE
            } else {}
          } else {
            print("move?")
            if (as.numeric(last(getPrice(x = mktdataTimestamp,prefer = prefer)[, 1])) + orderThreshold > orderPrice) {
              mvstop = TRUE
              print("move?")
            } else { print("move?") }
          }
          
          if (isTRUE(mvstop)) {
            print("move?")
            neworder <- addOrder(portfolio = portfolio, symbol = symbol, timestamp = timestamp, 
                                 qty = orderQty, price = as.numeric(getPrice(mktdataTimestamp, prefer = prefer)[, 1]), ordertype = orderType, 
                                 side = ordersubset[ii, "Order.Side"], threshold = orderThreshold, 
                                 status = "open", replace = FALSE, return = TRUE, 
                                 , ... = ..., TxnFees = txnfees)
            if (is.null(neworders)) neworders = neworder else neworders = rbind(neworders, neworder)
            ordersubset[ii, "Order.Status"] <- "replaced"
            ordersubset[ii, "Order.StatusTime"] <- format(timestamp, "%Y-%m-%d %H:%M:%S")
            next()
          }
        } # is.null(txnprice)
      }) # switch
      
      if (!is.null(txnprice) && !isTRUE(is.na(txnprice))) {
        pos <- getPosQty(portfolio, symbol, timestamp)
        if (orderQty == 0) {
          ordersubset[ii, "Order.Status"] <- "rejected"
        }
        else {
          addTxn(Portfolio = portfolio, Symbol = symbol, 
                 TxnDate = txntime, TxnQty = orderQty, TxnPrice = txnprice, 
                 ... = ..., TxnFees = txnfees)
          ordersubset[ii, "Order.Status"] <- "closed"
        }
        ordersubset[ii, "Order.StatusTime"] <- format(timestamp, "%Y-%m-%d %H:%M:%S")
        OrdersetTag = toString(ordersubset[ii, "Order.Set"])
        OpenInOrderset.i = which(ordersubset[, "Order.Status"] == "open" & ordersubset[, "Order.Set"] == OrdersetTag)
        if (length(OpenInOrderset.i) > 0) {
          ordersubset[OpenInOrderset.i, "Order.Status"] = "canceled"
          ordersubset[OpenInOrderset.i, "Order.StatusTime"] <- format(timestamp, "%Y-%m-%d %H:%M:%S")
        }
      }
    }
    if (!is.null(neworders)) 
      ordersubset = rbind(ordersubset, neworders)
    orderbook[[portfolio]][[symbol]] <- ordersubset
    assign(paste("order_book", portfolio, sep = "."), orderbook, 
           envir = .strategy)
  }
}

assignInNamespace("ruleOrderProc", ruleOrderProc, ns="quantstrat")

options(width=240)
suppressWarnings(rm("order_book.macross",pos=.strategy))
suppressWarnings(rm("account.macross","portfolio.macross",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratMACROSS","initDate","initEq",'start_t','end_t'))
stock.str='MSFT' # what are we trying it on
currency('USD')
stock(stock.str,currency='USD',multiplier=1)
initDate='2007-12-31'
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
#stratMACROSS <- add.rule(strategy = stratMACROSS, label="takeprofitexit", name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty="all", ordertype='stoplimit', orderside='long', threshold=+1,tmult=F, orderset="altexit"),type='exit')
# stop-loss exit
#stratMACROSS <- add.rule(strategy = stratMACROSS, label="stoplossexit", name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty="all", ordertype='stoplimit', orderside='long', threshold=-2,tmult=F, orderset="altexit"),type='exit')

# trailing stop exit
stratMACROSS <- add.rule(strategy = stratMACROSS, label="stoptrailingexit", name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty="all", ordertype='stoptrailing', orderside='long', threshold=-2,tmult=F, orderset="altexit"), type='risk')


# if you want a long/short Stops and Reverse MA cross strategy, you'd add two more rules for the short side:

# stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty=-100, ordertype='market', orderside='short'),type='enter')
# stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty="all", ordertype='market', orderside='short'),type='exit')

getSymbols(stock.str,from=initDate)
for(i in stock.str)
  assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))

start_t<-Sys.time()
out<-try(applyStrategy(strategy=stratMACROSS , portfolios=portfolio.st))
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
###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson,
# Jeffrey Ryan, Joshua Ulrich, and Garrett See
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: maCross.R 639 2011-06-24 14:29:06Z gsee $
#
###############################################################################
