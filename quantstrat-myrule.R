applyRules <- function(portfolio, symbol, strategy, mktdata, Dates=NULL, indicators=NULL, signals=NULL, parameters=NULL, ..., path.dep=TRUE, rule.order=NULL) {
  # TODO check for symbol name in mktdata using Josh's code:
  # symbol <- strsplit(colnames(mktdata)[1],"\\.")[[1]][1]
  
  # TODO handle indicator and signal lists as well as indicators/signals that were cbound to mktdata
  
  # ported from IBrokers thanks to Jeff
  # environment for data to be stored/accessed during applyRules execution
  # an example of this functionality is for the "symbols" variable
  # that can be set (by default) to display contract names
  .Data <- new.env()
  get.dindex <- function() get("dindex",pos=.Data) # inherits=TRUE)
  assign.dindex <- function(dindex) {
    dindex<-sort(unique(dindex))
    assign("dindex", dindex, .Data)
  }

  if (!is.strategy(strategy)) {
    strategy<-try(getStrategy(strategy))
    if(inherits(strategy,"try-error"))
      stop ("You must supply an object of type 'strategy'.")
  }
  
  ret <- NULL
  nargs <-list(...)
  if (length(nargs) == 0) nargs=NULL
  if (length('...') == 0 | is.null('...')) {
    rm('...')
    nargs=NULL
  }
  
  Dates = unique(index(mktdata)) # should this be index() instead?  
  
  ruleProc <- function (ruletypelist, timestamp=NULL, path.dep, ruletype, ...){
    for (rule in ruletypelist) {
      #TODO check to see if they've already been calculated
      if (!rule$path.dep == path.dep) next()
      
      if (!is.function(rule$name)) {
        if (!is.function(get(rule$name))) {
          if (!is.function(get(paste("sig",rule$name, sep='.')))) {
            message(paste("Skipping rule",rule$name,"because there is no function by that name to call"))
            next()      
          } else {
            rule$name <- paste("sig", rule$name, sep='.')
          }
        }   
      }
      
      if(!isTRUE(rule$enabled)) next()
      
      # check to see if we should run in this timespan
      if (!is.null(rule$timespan) && nrow(mktdata[rule$timespan][timestamp]) == 0) next()
      
      # see 'S Programming' p. 67 for this matching
      if (is.function(rule$name)) fun <- rule$name
      else fun <- match.fun(rule$name)
      
      nargs <- list(...)
      if (length(nargs) == 0) nargs=NULL
      if (length('...') == 0 | is.null('...')) {
        rm('...')
        nargs=NULL
      }
      
      .formals <- formals(fun)
      
      onames <- names(.formals)
      rule$arguments$timestamp = timestamp
      rule$arguments$ruletype = ruletype
      rule$arguments$label = rule$label
      pm <- pmatch(names(rule$arguments), onames, nomatch = 0L)
      # if (any(pm == 0L)) message(paste("some arguments stored for",rule$name,"do not match"))
      names(rule$arguments[pm > 0L]) <- onames[pm]
      .formals[pm] <- rule$arguments[pm > 0L]
      
      # now add arguments from parameters
      if (length(parameters)) {
        pm <- pmatch(names(parameters), onames, nomatch = 0L)
        names(parameters[pm > 0L]) <- onames[pm]
        .formals[pm] <- parameters[pm > 0L]
      }
      
      #now add dots
      if (length(nargs)) {
        pm <- pmatch(names(nargs), onames, nomatch = 0L)
        names(nargs[pm > 0L]) <- onames[pm]
        .formals[pm] <- nargs[pm > 0L]
      }
      .formals$... <- NULL
      
      # any rule-specific prefer-parameters should override global prefer parameter
      if (!is.null(rule$arguments$prefer)) .formals$prefer = rule$arguments$prefer
      
      tmp_val <- do.call(fun,.formals)
      
      mktdata <<- mktdata
      ret <<- ret
      hold <<- hold #TODO FIXME hold processing doesn't work unless custom rule has set it with <<-
      holdtill <<- holdtill
    } #end rules loop
  } # end sub process function ruleProc
  
  #we could maybe do something more sophisticated, but this should work
  if (isTRUE(path.dep)) {
    dindex <- c(1,length(Dates))# -1) # set the dimension reduction/loop jumping index vector
    assign.dindex(dindex)
    
    #pre-process for dimension reduction here
    for (type in names(strategy$rules)) {
      # check if there's anything to do
      if (length(strategy$rules[[type]]) >= 1) {
        for (rule in strategy$rules[[type]]) {
          if (isTRUE(rule$path.dep)) { # only apply to path dependent rule
            # check for sigcol, sigval, otherwise use all
            if (is.null(rule$arguments$sigcol) | is.null(rule$arguments$sigval)) {
              assign.dindex(1:length(Dates))
            } else {
              if (is.null(rule$timespan)) {
                assign.dindex(c(get.dindex(), which(mktdata[,rule$arguments$sigcol] == rule$arguments$sigval)))
              } else {
                assign.dindex(c(get.dindex(), which(merge(.xts(,.index(mktdata)), mktdata[rule$timespan, rule$arguments$sigcol]) == rule$arguments$sigval)))
              }
            }
          }
        }
      }    
    }
    
    dindex <- get.dindex()
    if (length(dindex) == 0) dindex = 1
    
    #for debugging, set dindex to all index values:
    #assign.dindex(1:length(index(mktdata)))
    #print(dindex)
  } else {
    Dates = ''
    dindex = 1
  } # end dindex initialization
  
  nextIndex <- function(curIndex,...) {
#     cat("timeindex move forward, nextIndex called\n")

    if (!isTRUE(path.dep)){
      curIndex = FALSE
      return(curIndex)
    } 
    
    dindex <- get.dindex()
    #message(dindex," in nextIndex(), at ",curIndex)
    
    hasmktord <- FALSE
    nidx = FALSE
    neworders = NULL
    
    orderbook <- getOrderBook(portfolio)
    ordersubset <- orderbook[[portfolio]][[symbol]]
    
    oo.idx <- getOrders(portfolio=portfolio, symbol=symbol, status="open", which.i=TRUE) #, timespan=timespan, ordertype=ordertype,which.i=TRUE)
    if (length(oo.idx) == 0) {
      nidx=FALSE
    } else { # open orders, 
      isOHLCmktdata <- is.OHLC(mktdata)
      isBBOmktdata  <- is.BBO(mktdata)
      
      #check for open orders at curIndex
      timespan <- paste(timestamp, "::", sep='') #no check to see if timestamp came through dots? Does it come from the search path? -gsee
      if (nrow(ordersubset[oo.idx,][timespan]) == 0) {
        # no open orders between now and the next index
        nidx = FALSE
      } else {
        if (!length(grep('market',ordersubset[oo.idx,'Order.Type'])) == 0) {
          # if block above had a prefer exclusion, as below:
          # || hasArg('prefer')
          # 'prefer' arguments would loop through all observations.  
          # we could probably change the code below on finding price to handle prefer, but not sure it matters
          
          #if any type is market    
          # set to curIndex+1
          #curIndex<-curIndex+1
          if (is.na(curIndex) || (curIndex + 1) > length(index(mktdata))) curIndex = FALSE
          hasmktord <- TRUE
          #return(curIndex) # move to next index, a market order in this index would have trumped any other open order
        }
        if (!length(grep('limit', ordersubset[oo.idx, 'Order.Type'])) == 0) {
          stoplimitorders <- grep('stoplimit', ordersubset[oo.idx, 'Order.Type'])
          limitorders <- grep('limit', ordersubset[oo.idx, 'Order.Type'])
          if (length(stoplimitorders) > 0) {
            limitorders <- limitorders[-stoplimitorders]
          }
          for (slorder in stoplimitorders) {
            dindex <- get.dindex()
            tmpqty <- ordersubset[oo.idx[slorder], 'Order.Qty']
            if (tmpqty == 'all') {
              #tmpqty<-osNoOp(timestamp=timestamp, orderqty=tmpqty, portfolio=portfolio, symbol=symbol,ruletype='exit' )
              #set to 0, and let the next block figure it out from orderside
              tmpqty = 0
            } 
            if (tmpqty == 0) {
              #no position, so do some sleight of hand to figure out when the index may be needed
              side <- ordersubset[oo.idx[slorder], 'Order.Side']
              if (side == 'long') tmpqty = -1
              else tmpqty = 1
            }
            tmpqty <- as.numeric(tmpqty)
            tmpprice <- as.numeric(ordersubset[oo.idx[slorder], 'Order.Price'])
            if (tmpqty > 0) { #buy if mktprice moves above stoplimitorder price
              relationship = 'gte'  #if the Ask or Hi go above threshold our stop will be filled
              if (isBBOmktdata) {
                col <- first(colnames(mktdata)[has.Ask(mktdata, which=TRUE)])
              } else if (isOHLCmktdata) {
                col <- first(colnames(mktdata)[has.Hi(mktdata, which=TRUE)])
              } else { #univariate or something built with fn_SpreadBuilder  
                col <- first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
                # perhaps we need a has.Price check
              }
              if (is.na(col)) stop("no price discernable for stoplimit in applyRules")
            } else { #sell if mktprice moves below stoplimitorder price
              relationship = "lte" #if Bid or Lo go below threshold, our stop will be filled
              if (isBBOmktdata) {
                col <- first(colnames(mktdata)[has.Bid(mktdata, which=TRUE)])
              } else if (isOHLCmktdata) {
                col <- first(colnames(mktdata)[has.Lo(mktdata, which=TRUE)])
              } else {
                col <- first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
              }    
              if (is.na(col)) stop("no price discernable for stoplimit in applyRules")                            
            } 
            cross <- sigThreshold(label='tmpstop', column=col, threshold=tmpprice, relationship=relationship)
            if (any(cross[timespan])) {
              # find first index that would cross after this index
              newidx <- curIndex + which(cross[timespan])[1] - 1
              # insert that into dindex
              assign.dindex(c(get.dindex(), newidx))                  
            }
          }
          print(limitorders)
          for (lorder in limitorders) {
            dindex <- get.dindex()
            tmpqty <- ordersubset[oo.idx[lorder], 'Order.Qty']
            if (tmpqty == 'all') {
              #tmpqty<-osNoOp(timestamp=timestamp, orderqty=tmpqty, portfolio=portfolio, symbol=symbol,ruletype='exit' )
              #set to 0, and let the next block figure it out from orderside
              tmpqty = 0
            } 
            if (tmpqty == 0) {
              print("hhh")
              #no position, so do some sleight of hand to figure out when the index may be needed
              side <- ordersubset[oo.idx[lorder], 'Order.Side']
              if (side=='long') tmpqty = -1
              else tmpqty = 1
            }
            tmpqty <- as.numeric(tmpqty)
            
            tmpprice <- as.numeric(ordersubset[oo.idx[lorder], 'Order.Price'])
            
            if (tmpqty > 0) {
              #buying
              print("ere")
              relationship = "lte" #look for places where Mkt Ask <= our Bid
              if (isBBOmktdata) {
                col <- first(colnames(mktdata)[has.Ask(mktdata, which=TRUE)])
              } else if (isOHLCmktdata) {
                col <- first(colnames(mktdata)[has.Lo(mktdata, which=TRUE)])
              } else {
                col <- first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
              }    
              if (is.na(col)) stop("no price discernable for stoplimit in applyRules")
            } else {
              #selling
              relationship = "gte" #look for places where Mkt Bid >= our Ask
              if (isBBOmktdata) {
                col <- first(colnames(mktdata)[has.Bid(mktdata, which=TRUE)])
              } else if (isOHLCmktdata) {
                col <- first(colnames(mktdata)[has.Hi(mktdata, which=TRUE)])
              } else {
                col <- first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
              }    
              if (is.na(col)) stop("no price discernable for stoplimit in applyRules")
            }
            # use sigThreshold
            cross <- sigThreshold(label='tmplimit', column=col, threshold=tmpprice, relationship=relationship)
            if (any(cross[timespan])) {
              # find first index that would cross after this index
              newidx <- curIndex + which(cross[timespan])[1] #- 1  #curIndex/timestamp was 1 in the subset, we need a -1 offset?
              #if there are is no cross curIndex will be incremented on line 496
              # with curIndex<-min(dindex[dindex>curIndex]).                            
              #we cannot get filled at this timestamp. The soonest we could get filled is next timestamp...
              #see also that market order increments curIndex before returning it. Going by the docs,
              #I think this is by design. i.e. no instant fills. -gsee
              
              # insert that into dindex
              assign.dindex(c(get.dindex(),newidx))                  
            } else {
              # no cross, move ahead
              # nidx=TRUE #WHY WAS THIS HERE?
            }
          } # end loop over open limit orders
        } # end if for limit order handling
        
        if (!length(grep('trailing', ordersubset[oo.idx, 'Order.Type'])) == 0) { # process trailing orders
          #print("trailing")
          #else process trailing
          trailorders <- grep('trailing', ordersubset[oo.idx, 'Order.Type'])
          #print(curIndex)
          for (torder in trailorders) {
#             dindex <- get.dindex()
            firsttime <- NULL
            onum <- oo.idx[torder]
            orderThreshold <- as.numeric(ordersubset[onum, 'Order.Threshold'])
            tmpqty <- ordersubset[onum, 'Order.Qty']
            if (tmpqty == 'all') {
              #tmpqty<-osNoOp(timestamp=timestamp, orderqty=tmpqty, portfolio=portfolio, symbol=symbol,ruletype='exit' )
              #set to 0, and let the next block figure it out from orderside
              tmpqty = 0
            }
            tmpqty <- as.numeric(tmpqty)
            if (tmpqty == 0) {
              #no position, so do some sleight of hand to figure out when the index may be needed
              side <- ordersubset[onum, 'Order.Side']
              if (side == 'long') tmpqty = -1
              else tmpqty = 1
            }
            tmpqty <- as.numeric(tmpqty)
            tmpprice <- as.numeric(ordersubset[onum, 'Order.Price'])
            tmpidx <- format(index(ordersubset[onum,]), "%Y-%m-%d %H:%M:%OS6") #this is the time the order was entered
            #print(tmpidx)
            if (isBBOmktdata) {
              if (tmpqty > 0) { # positive quantity 'buy'
                prefer = 'offer'
              } else {
                prefer = 'bid'
              }
            } else if (isOHLCmktdata) {
              prefer = 'close'
            } 
            dindex <- get.dindex()
            if (is.null(firsttime)) firsttime <- timestamp # timestamp = Dates[curIndex]
            nextidx <- min(dindex[dindex > curIndex])
            nextidx <- first(dindex[dindex > curIndex])
            
            if (length(nextidx)) {
              nextstamp <- format(index(mktdata[nextidx, ]), "%Y-%m-%d %H:%M:%OS6")
              #print(nextstamp)
              timespan <- paste(format(firsttime, "%Y-%m-%d %H:%M:%OS6"), "::", nextstamp, sep='')
              #get the subset of prices
              mkt_price_series <- getPrice(mktdata[timespan], prefer=prefer)
              col <- first(colnames(mkt_price_series))
              orderloop <- TRUE
            } else {
              orderloop <- FALSE
            }

            ###################################
            # edit from here                  #
            ###################################
            mkt_price_series <- getPrice(mktdata[timespan], prefer = prefer)
            is.sell = tmpqty < 0
            if (is.sell) {
              drawdown <- mkt_price_series - cummax(mkt_price_series)
            } else {
              drawdown <- mkt_price_series - cummin(mkt_price_series)
            }
            colnames(drawdown) <- "drawdown"
            
            relationship = ifelse(is.sell, "lte", "gte")
            cross <- sigThreshold(data=drawdown, label="", column="drawdown", threshold=orderThreshold, relationship=relationship)
            if (any(cross)) {
              newidx <- curIndex + which(cross)[1] - 1
              assign.dindex(c(get.dindex(), newidx))
              
              #debug
              #add a some index
#               another.newidx <- newidx - 5
#               assign.dindex(c(get.dindex(), another.newidx))

              part.mkt.price <- mkt_price_series[1:which(cross)[1]-1,]
              last.peak.price <- ifelse(is.sell, max(part.mkt.price), min(part.mkt.price))
              max.market.price <- last.peak.price
              peak.i <- last(which(part.mkt.price==last.peak.price))
              peak.price.timestamp <- index(part.mkt.price[peak.i,])
              peak.index <- c(rep(F, peak.i-1), T, rep(NA,length(drawdown)-peak.i))
              drawdown <- cbind(drawdown, mkt_price_series, orderThreshold, cross, last.peak.price, peak.index)
              colnames(drawdown) <- c("drawdown", "mkt.price", "threshold", "cross", "last.peak.price","peak.index")
#               ddrawdown <<- drawdown
              
#               print(timestamp)
#               print("addddddddd")
              neworder <- addOrder(portfolio=portfolio, symbol=symbol, timestamp=peak.price.timestamp, qty=ordersubset[onum,"Order.Qty"], price=max.market.price, 
                ordertype=ordersubset[onum,"Order.Type"], side=ordersubset[onum,"Order.Side"], orderset=ordersubset[onum, "Order.Set"], threshold=orderThreshold,
                status="open", replace=T, return=T, ...=..., TxnFees=ordersubset[onum,"Txn.Fees"], label=ordersubset[onum,"Rule"])
              ordersubset[onum, "Order.Status"] <- "replaced"
              ordersubset[onum, "Order.StatusTime"] <- format(peak.price.timestamp, "%Y-%m-%d %H:%M:%S")
              
              orderbook[[portfolio]][[symbol]] <- rbind(ordersubset, neworder)
              assign(paste("order_book", portfolio, sep='.'), orderbook, envir=.strategy)
            }
          } # end loop over open trailing orders
        } # end if for trailing orders
      } # end else clause for any open orders in this timespan    
    } # end any open orders closure
    
    if (curIndex) {
      if(hasmktord) { 
        curIndex <- curIndex+1
        dindex <- get.dindex()
      } else {
        dindex <- get.dindex()
        if (any(dindex > curIndex)) {
          curIndex <- min(dindex[dindex>curIndex]) 
        } else curIndex <- FALSE
      }
    }
    
    if (is.na(curIndex) || curIndex > length(index(mktdata))) curIndex = FALSE
    
    #debug line
    #print(curIndex)
    
    return(curIndex)
  } # end function nextIndex
  
  hold=FALSE
  holdtill=first(time(Dates))-1 # TODO FIXME make holdtill default more robust?
  
  mktinstr <- getInstrument(symbol)
  
  curIndex <- 1
  
  while (curIndex) {
    timestamp = Dates[curIndex]    
#     cat("start.....\n")
    # check to see if we need to release a hold
    if (isTRUE(hold) & holdtill<timestamp) {
      hold = FALSE
      holdtill = NULL
    }
    # evaluate the rule types in the order listed in the documentation
    # thanks to Aleksandr Rudnev for tracking this down (R-SIG-Finance, 2011-01-25)
    if (is.null(rule.order)) {
      types <- sort(factor(names(strategy$rules), levels=c("pre", "risk", "order", "rebalance", "exit", "enter", "entry", "post")))
    } else {
      print("Be aware that order of operations matters, and poor choises in rule order can create unintended consequences.")
      types <- rule.order
    }
    for (type in types) {
      switch (type, pre = {
        if (length(strategy$rules[[type]]) >= 1) {
          ruleProc(strategy$rules$pre, timestamp=timestamp, path.dep=path.dep, mktdata=mktdata, portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
        }
      }, risk = {
        if (length(strategy$rules$risk) >= 1) {
          ruleProc(strategy$rules$risk, timestamp=timestamp, path.dep=path.dep, mktdata=mktdata, portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
        }
      }, order = {
        if (length(strategy$rules[[type]]) >= 1) {
          ruleProc(strategy$rules[[type]], timestamp=timestamp, path.dep=path.dep, mktdata=mktdata, portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
        } else {
          #(mktdata, portfolio, symbol, timestamp, slippageFUN=NULL)
          if (isTRUE(path.dep)) {
            timespan <- format(timestamp, "::%Y-%m-%d %H:%M:%OS6")
          } else timespan = NULL
          ruleOrderProc(portfolio=portfolio, symbol=symbol, mktdata=mktdata, timespan=timespan, ...)
        }
      }, rebalance =, exit = , enter = , entry = {
        if (isTRUE(hold)) next()
        if (length(strategy$rules[[type]]) >= 1) {
          ruleProc(strategy$rules[[type]], timestamp=timestamp, path.dep=path.dep, mktdata=mktdata, portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
        }
        if (isTRUE(path.dep) && length(getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timestamp, which.i=TRUE))) {
          ## TODO FIXME this doesn't appear to work correctly
          # we opened orders in this timestamp, make sure to increment dindex w/ curIndex+1 so the order slot gets checked next index ?
          #browser()
          #assign.dindex(c(get.dindex(),curIndex+1))
          #
        }
      }, post = {
        #TODO do we process for hold here, or not?
        if (length(strategy$rules$post) >= 1) {
          ruleProc(strategy$rules$post,timestamp=timestamp, path.dep=path.dep, mktdata=mktdata, portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
        }
      }) # end switch
    } #end type loop
    
    # see orderbook
#     if (path.dep) {
#       cat("Orderbook at ", format(timestamp, "%Y-%m-%d %H:%M:%OS2"), "\n")
#       ob <- getOrderBook(portfolio)[[portfolio]][[symbol]]
#       print(ob)
#     }
    
    if (isTRUE(path.dep)) curIndex <- nextIndex(curIndex, ...) #timestamp comes from environment, not dots? -gsee
    else curIndex=FALSE
  } # end index while loop
  
  mktdata<<-mktdata
  if(is.null(ret)) {
    return(mktdata)
  }
  else return(ret)
}

assignInNamespace("applyRules", applyRules, ns="quantstrat")

ruleOrderProc <- function(portfolio, symbol, mktdata, timespan=NULL, ordertype=NULL, ..., slippageFUN=NULL) {
  if (is.null(timespan)) return()
  
  timestamp <- time(last(mktdata[timespan]))
  
  orderbook <- getOrderBook(portfolio)
  ordersubset <- orderbook[[portfolio]][[symbol]]
  OpenOrders.i <- getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timespan, ordertype=ordertype, which.i=TRUE)
  
  if (hasArg(prefer)) prefer = match.call(expand.dots=TRUE)$prefer
  else prefer = NULL
  
  # check for open orders
  if (length(OpenOrders.i) >= 1) {
    # get previous bar
    prevtime <- time(mktdata[last(mktdata[timespan, which.i=TRUE])-1]) 
    timestamp <- time(last(mktdata[timespan]))
    #switch on frequency
    freq = periodicity(mktdata)
    neworders <- NULL
    mktdataTimestamp <- mktdata[timestamp]

    # Should we only keep the last observation per time stamp?
    if (NROW(mktdataTimestamp) > 1) mktdataTimestamp <- last(mktdataTimestamp)
    
    isOHLCmktdata <- is.OHLC(mktdata)
    isBBOmktdata <- is.BBO(mktdata)
    for (ii in OpenOrders.i) {
      if(ordersubset[ii, "Order.Status"] != "open") {	# need to check this bc sideeffects may have changed order.status in this loop
        #print("@@@@@@@@ status changed from open")
        next()
      }
      
      txnprice = NULL
      txnfees = ordersubset[ii, "Txn.Fees"]
      orderPrice <- as.numeric(ordersubset[ii, "Order.Price"])
      orderQty <- ordersubset[ii, "Order.Qty"]
      if (orderQty == 'all') {
        # this has to be an exit or risk order, so: 
        orderQty = -1*getPosQty(Portfolio=portfolio, Symbol=symbol, Date=timestamp)
        orderside <- ordersubset[ii, "Order.Side"]
        if (((orderQty > 0 && orderside == 'long') || (orderQty < 0 && orderside == 'short'))) {
          # this condition may occur if (for example) a signal triggers an 'increase LONG pos' and 'close all SHORT pos' simultaneously
          # hence this is legal condition, and we must 0 the orderQty to reject the order
          
          # warning('trying to exit/market/all position but orderQty sign ', orderQty,' does not match orderside ', orderside)
          orderQty = 0
        }
      }
      orderQty <- as.numeric(orderQty)
      
      orderThreshold <- as.numeric(ordersubset[ii,"Order.Threshold"])
      # mktdataTimestamp <- mktdata[timestamp]
      #FIXME Should we only keep the last observation per time stamp?
      #if( NROW(mktdataTimestamp) > 1 ) mktdataTimestamp <- last(mktdataTimestamp)
      
      orderType <- ordersubset[ii,"Order.Type"]
      
      switch (orderType, market = {
        switch (freq$scale, yearly =, quarterly =, monthly = {
          txntime = as.character(index(ordersubset[ii,])) # transacts on this bar, e.g. in the intraday cross, or leading into the end of month, quarter, etc.
                        # txntime=as.character(timestamp) # use this if you wanted to transact on the close of the next bar
          txnprice = as.numeric(getPrice(last(mktdata[txntime]), prefer=prefer)[, 1])
        }, #end daily
        {
          txntime = timestamp
          if (isBBOmktdata) {
            #An ordertype of market will *almost* trump pricemethod here. orderPrice was determined using pricemethod.
            #but, for buy orders you'll be filled at either orderPrice or the current mkt ask -- whichever is worse.
            #and, for sell orders you'll be filled at either orderPrice or the current mkt bid -- whichever is worse.
            if(orderQty > 0){ # positive quantity 'buy'
              #fill at max(orderPrice,newMktAsk price) 
              txnprice = max(orderPrice, as.numeric(getPrice(mktdataTimestamp,prefer='ask')[,1]))
            } else { # negative quantity 'sell'
              txnprice = min(orderPrice, as.numeric(getPrice(mktdataTimestamp,prefer='bid')[,1])) #presumes unique timestamp
            }
            #e.g. if pricemethod was opside, it sent a buy order at mktAsk. fill at greater of that ask, and current ask
          } else txnprice = as.numeric(getPrice(mktdataTimestamp, prefer=prefer)[,1]) #filled at 'price'
        }) # end switch on frequency
      },
      limit=, stoplimit =, iceberg = {
        if (!isBBOmktdata) { #(isOHLCmktdata){
          if (orderType == 'iceberg'){
                stop("iceberg orders only supported for BBO data")
          }
          # check to see if price moved through the limit                        
          if((orderQty > 0 && orderType != 'stoplimit') || (orderQty < 0 && orderType == 'stoplimit') ) {  
            # buy limit, or sell stoplimit
            if( (has.Lo(mktdata) && orderPrice > as.numeric(Lo(mktdataTimestamp))) || 
               (!has.Lo(mktdata) && orderPrice >= as.numeric(getPrice(mktdataTimestamp, prefer=prefer)))) {
               txnprice = orderPrice
               txntime = timestamp
            } else next() # price did not move through my order, should go to next order  
          } else if ((orderQty < 0 && orderType != 'stoplimit') || (orderQty > 0 && orderType == 'stoplimit')) { 
            # sell limit or buy stoplimit
            if ((has.Hi(mktdata) && orderPrice < as.numeric(Hi(mktdataTimestamp))) ||
              (!has.Hi(mktdata) && orderPrice <= as.numeric(getPrice(mktdataTimestamp, prefer=prefer)))) {
              txnprice = orderPrice
              txntime = timestamp
            } else next() # price did not move through my order, should go to next order 
          } else {
            warning('ignoring order with quantity of zero')
            next()
          }
        } else if (isBBOmktdata) {
          # check side/qty
          if (orderQty > 0) { # positive quantity 'buy'
            if (orderType == 'stoplimit') { # buy stoplimit
              if (orderPrice <= as.numeric(getPrice(mktdataTimestamp, prefer='ask')[,1])) {
                # mktprice moved above our stop buy price 
                txnprice = orderPrice #assume we got filled at our stop price
                #txnprice = as.numeric(getPrice(mktdataTimestamp,prefer='ask')[,1]) #presumes unique timestamps
                txntime = timestamp
              } else next()
            } else { # orderType == 'limit', sell limit 
              if (orderPrice >= as.numeric(getPrice(mktdataTimestamp, prefer='ask')[,1])) { # price reaches limit 
                # price we're willing to pay is higher than the offer price, so execute at the prevailing price
                txnprice = as.numeric(getPrice(mktdataTimestamp,prefer='ask')[,1]) #presumes unique timestamps
                txntime = timestamp
              } else next()
            }
          } else { # negative quantity 'sell'
            if (orderType == 'stoplimit') { # sell stoplimit
              if (orderPrice >= as.numeric(getPrice(mktdataTimestamp, prefer='bid')[,1])) {
                # mktprice moved below our stop sell price
                txnprice = orderPrice #assumption is that we're filled at our stop price
                #txnprice = as.numeric(getPrice(mktdataTimestamp,prefer='bid')[,1]) #presumes unique timestamp
                txntime = timestamp
              } else next()
            } else { # buy limit
              if (orderPrice <= as.numeric(getPrice(mktdataTimestamp, prefer='bid')[,1])) {
                # we're willing to sell at a better price than the bid, so execute at the prevailing price
               # txnprice = orderPrice
                txnprice = as.numeric(getPrice(mktdataTimestamp, prefer='bid')[,1]) #presumes unique timestamp
                txntime = timestamp
              } else next()
            } 
          }
        
          if (orderType == 'iceberg') {
            #we've transacted, so the old order was closed, put in a new one
            neworder <- addOrder(portfolio=portfolio, symbol=symbol, timestamp=timestamp, qty=orderQty, price=as.numeric(getPrice(mktdataTimestamp, prefer=prefer)[,1]), 
              ordertype=orderType, side=ordersubset[ii,"Order.Side"], threshold=orderThreshold,
              status="open", replace=FALSE, return=TRUE,...=..., TxnFees=txnfees)
            
            if (is.null(neworders)) neworders=neworder else neworders = rbind(neworders, neworder)
          
            ordersubset[ii,"Order.Status"] <- 'replaced'
            ordersubset[ii,"Order.StatusTime"] <- format(timestamp, "%Y-%m-%d %H:%M:%S")
            next()
          } 
        }
      },
      stoptrailing = {
        if (orderQty == 0) {
          warning('ignoring order with quantity of zero')
          next()
        }
        
        # We only process those orders whose timestamps are past
        if (as.POSIXct(index(ordersubset[ii,])) > as.POSIXct(timestamp)) next()
        
#         cat("\n\n\n\n................................\n")
#         print(timestamp)
#         cat("process stoptrailling order...\n")
#         print(ordersubset[ii,])
#         print("mkt.price")
#         print(mktdataTimestamp)
        
        is.buy <- orderQty > 0
        # if market moved through my price, execute
        if (orderQty > 0) { # positive quantity 'buy'
          if (isBBOmktdata) prefer = 'offer'
          
          if (orderPrice <= getPrice(mktdataTimestamp, prefer=prefer)[,1]) { #TODO maybe use last(getPrice) to catch multiple prints on timestamp?
            # price we're willing to pay is higher than the offer price, so execute at the prevailing price
            #txnprice = orderPrice
            txnprice = as.numeric(getPrice(mktdataTimestamp, prefer=prefer)[,1]) #presumes unique timestamps
            txntime = timestamp
          }
        } else { # negative quantity 'sell'
          if (isBBOmktdata) prefer = 'bid'
            
          if (orderPrice >= getPrice(mktdataTimestamp, prefer=prefer)[,1]) {
            # we're willing to sell at a better price than the bid, so execute at the prevailing price
            # txnprice = orderPrice
            txnprice = as.numeric(getPrice(mktdataTimestamp, prefer=prefer)[,1]) #presumes unique timestamp
           txntime = timestamp
          }
        }
         
        if (isOHLCmktdata) {
          # check to see if price moved through the limit
          if (orderPrice > as.numeric(Lo(mktdataTimestamp)) & orderPrice < as.numeric(Hi(mktdataTimestamp))) {
            txnprice = orderPrice
            txntime = timestamp
          } 
        }
         
        # if market is beyond price+(-threshold), replace order
        if (is.null(txnprice)) {
          # we didn't trade, so check to see if we need to move the stop
          # first figure out how to find a price
          if (isOHLCmktdata) {
            prefer = 'close'
          } else if (isBBOmktdata) {
            if (orderQty > 0) {
              prefer = 'offer'
            } else {
              prefer = 'bid'
            }
          } else {
            prefer = NULL # see if getPrice can figure it out
          }
        }
      })
        
      if (!is.null(txnprice) && !isTRUE(is.na(txnprice))) {
        #make sure we don't cross through zero
        pos <- getPosQty(portfolio, symbol, timestamp)
        
        if (orderQty == 0) {	# reject the order (should be exit/market/all)
          ordersubset[ii, "Order.Status"] <- 'rejected'
        } else { #add the transaction
          addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=txntime, TxnQty=orderQty, TxnPrice=txnprice, ...=..., TxnFees=txnfees)
          ordersubset[ii,"Order.Status"] <- 'closed'
        } 
        ordersubset[ii,"Order.StatusTime"] <- format(timestamp, "%Y-%m-%d %H:%M:%S")
        
        #close all other orders in the same order set
        OrdersetTag = toString(ordersubset[ii, "Order.Set"])
        OpenInOrderset.i = which(ordersubset[, "Order.Status"] == 'open' & ordersubset[,"Order.Set"] == OrdersetTag)
        
        # skip this if there are no orders
        if (length(OpenInOrderset.i) > 0) {
          ordersubset[OpenInOrderset.i, "Order.Status"] = 'canceled'
          ordersubset[OpenInOrderset.i, "Order.StatusTime"] <- format(timestamp, "%Y-%m-%d %H:%M:%S")
        }
      } #end loop over open orders
      
      if (!is.null(neworders)) ordersubset = rbind(ordersubset, neworders)
      
      # now put the orders back in
      # assign order book back into place (do we need a non-exported "put" function?)
      orderbook[[portfolio]][[symbol]] <- ordersubset
      assign(paste("order_book", portfolio, sep='.'), orderbook, envir=.strategy)
    } # end check for open orders
  }
}
    
assignInNamespace("ruleOrderProc", ruleOrderProc, ns="quantstrat")

# applyParameter<-function(strategy, portfolios, parameterPool, parameterConstraints, method, sampleSize, verbose=FALSE, ...) {
#   #need to create combination of distribution values in each slot of the parameterPool
#   
#   initialPortf<-getPortfolio(portfolios)
#   stock.str<-names(initialPortf$symbols)
#   initDate<-time(first(initialPortf$symbols[[1]]$posPL))
#   
#   tmp_strategy<-strategy
#   
#   testPackList<-list()
#   testPackList$stats<-NULL
#   
#   testPackListPRLStructure<-list()
#   testPackListPRLStructure$stats<-NULL
#   
#   
#   
#   if (!is.strategy(tmp_strategy)) {
#     tmp_strategy<-try(getStrategy(tmp_strategy))
#     if(inherits(tmp_strategy,"try-error"))
#       stop ("You must supply an object of type 'strategy'.")
#   } 
#   
#   
#   out<-list()
#   paramdist<-list()
#   paramweight<-list()
#   paramLabel<-list()
#   lvmatch<-list()
#   
#   
#   
#   for (i in 1:length(parameterPool)){
#     
#     distr<-parameterPool[[i]]
#     #paramdist[[i]]<-distr$distribution[[1]]
#     paramdist[[paste('Param',distr$type,distr$indexnum,names(distr$distribution),sep='.')]]<-distr$distribution[[1]]
#     paramweight[[paste('ParamWt',distr$type,distr$indexnum,names(distr$distribution),sep='.')]]<-distr$weight
#     #paramdist[[paste(i)]]<-distr$distribution[[1]]
#     
#     #Build label<->var name match.
#     lvmatch$label[i]<-distr$label
#     lvmatch$varName[i]<-paste('Param',distr$type,distr$indexnum,names(distr$distribution),sep='.')
#     
#   }
#   
#   paramLabel<-data.frame(lvmatch,stringsAsFactors=FALSE)
#   
#   #TODO make it take sample size etc.
#   
#   
#   
#   if (method=='expand') 
#   {
#     paramTable<-expand.grid(paramdist, stringsAsFactors=FALSE)
#   }
#   else if (method=='random')
#   {
#     if (missing(sampleSize)) {stop ("sampleSize is needed")} 
#     #paramTable<-data.frame()
#     
#     #genSample update the paramTable with more sample rows.
#     genSample<-function(iparamTable,paramdist,tsampleSize,remainSize)
#     {
#       if (missing(remainSize) ) remainSize=tsampleSize
#       
#       tparamTable<-data.frame()
#       
#       for( i in 1:length(paramdist))
#       {
#         ireplace<-(length(paramdist[i])<tsampleSize)
#         
#         if (nrow(tparamTable)==0)
#         {
#           tparamTable<-data.frame(sample(paramdist[[i]],remainSize,prob=paramweight[[i]],replace=ireplace),stringsAsFactors=FALSE)
#           
#         }	
#         else{
#           tparamTable<-cbind(tparamTable,data.frame(sample(paramdist[[i]],remainSize,prob=paramweight[[i]],replace=ireplace),stringsAsFactors=FALSE))
#         }										
#       }
#       
#       names(tparamTable)<-names(paramdist)
#       
#       # put constraint test on tparamTable, before rbind
#       for (k in 1:length(parameterConstraints))
#       {
#         constrintfill<-paramConstraint(label=parameterConstraints[[k]]$constraintLabel,
#                                        data=tparamTable,
#                                        columns=merge(paramLabel,data.frame(parameterConstraints[[k]]$paramList),by="label")$varName, #has to keep the order.
#                                        relationship=parameterConstraints[[k]]$relationship)				
#         
#         
#         #only keep the samples fulfill the constraints.
#         tparamTable<-tparamTable[which(constrintfill==TRUE),]
#       }
#       
#       
#       iparamTable<-rbind(iparamTable,tparamTable)
#       
#       iparamTable<-unique(iparamTable)
#       
#       #			if(verbose >=1) print("nnnnnnnnnnnnnnnnnnnnnnn")
#       #			if(verbose >=1) print(nrow(iparamTable))
#       
#       if (nrow(iparamTable)<tsampleSize)
#       {
#         iparamTable<-genSample(iparamTable,paramdist,tsampleSize,remainSize=tsampleSize-nrow(iparamTable))			
#       }
#       
#       names(iparamTable)<-names(paramdist)
#       return(iparamTable)
#     } #end define function
#     
#     paramTable<-NULL
#     paramTable<-genSample(paramTable,paramdist,sampleSize)		
#     
#   }
#   
#   
#   testPackList$paramTable<-paramTable
#   testPackList$paramdist<-paramdist
#   testPackList$paramweight<-paramweight
#   testPackList$paramLabel<-paramLabel
#   
#   strategyList<-list()
#   if(verbose >=1) print("ParamTable generated")
#   
#   
#   psize=nrow(paramTable)
#   if(verbose >=1) print(psize)
#   
#   
#   
#   instruments<-as.list(FinancialInstrument:::.instrument)
#   getSymbols<-as.list(.getSymbols)
#   blotter<-as.list(.blotter)
#   
#   #Pack all symbols downloaded in .GlobalEnv
#   symbols<-names(.getSymbols)
#   
#   testPackListPRL<-foreach (i = 1:psize, .export=c('instruments',symbols,'getSymbols','blotter','tmp_strategy'),.verbose=TRUE,...=...) %dopar% 
#     
# {
#   if(verbose) print(paste('===> now starting parameter test', i))
#   
#   require(quantstrat, quietly=TRUE)
#   
#   # loops must be run with an empty .blotter environment each, or .blotter appears to accumulate portfolios and accounts
#   # and passes them from one loop to the next on each CPU - JH July 2012
#   if (getDoParRegistered() && getDoParWorkers()>1)
#   {
#     rm(list=ls(pos=.blotter), pos=.blotter)
#     gc(verbose=verbose)
#   }
#   
#   testPack<-list()
#   
#   #Pass environments needed.
#   loadInstruments(instruments)
#   .getSymbols<-as.environment(getSymbols)
#   
#   #Unpack symbols to worker. change later.
#   #seems need to go through assign, rather than just .export the names...
#   
#   for (sym in symbols) {
#     assign(sym, eval(as.name(sym)), .GlobalEnv)
#   }
#   
#   #Create a copy of strategy object, so not to lock up on the sameone.
#   PLtmp_strategy<-tmp_strategy
#   
#   #Extract parameter from table and construct PLtmp_strategy.
#   for (j in 1:ncol(paramTable)){
#     
#     tmp_arg<-parameterPool[[j]]$distribution[1] #Just get the list form with name
#     #tmp_arg<-list(tmp_argName=paramTable[i,j])
#     tmp_arg[[1]]<-paramTable[i,j]
#     
#     tmp_index<-parameterPool[[j]]$indexnum
#     
#     switch(parameterPool[[j]]$type,
#            'indicator'={
#              #merge.list uses another package. PLtmp_strategy$indicators[[tmp_index]]$arguments<-merge.list(targ1,tmp_arg)
#              targ1<-PLtmp_strategy$indicators[[tmp_index]]$arguments
#              
#              pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
#              if( any(pnamepos>0)){
#                #just change the argument value itself will do ?or still need add.indicator??
#                PLtmp_strategy$indicators[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
#              }
#              else{
#                PLtmp_strategy$indicators[[tmp_index]]$arguments<-append(targ1,tmp_arg)
#                
#              }
#              #OR still need add.*??
#              #pass_arg<-append(,tmp_arg)
#              #PLtmp_strategy <- add.indicator(strategy = PLtmp_strategy,name=PLtmp_strategy$indicators[[tmp_index]]$name, arguments = pass_arg,indexnum=tmp_index)
#            },
#            'signal'={
#              
#              targ1<-PLtmp_strategy$signals[[tmp_index]]$arguments
#              
#              pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
#              if( any(pnamepos>0)){
#                #just change the argument value itself will do ?or still need add.indicator??
#                
#                PLtmp_strategy$signals[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
#              }
#              else{
#                PLtmp_strategy$signals[[tmp_index]]$arguments<-append(targ1,tmp_arg)
#                
#              }
#              
#              #						pass_arg<-append(PLtmp_strategy$signal[[tmp_index]]$arguments,tmp_arg)
#              #						PLtmp_strategy <- add.signal(strategy = PLtmp_strategy,name=PLtmp_strategy$signal[[tmp_index]]$name,arguments = tmp_arg,indexnum=tmp_index)
#              
#            },
#            'order'={
#              targ1<-PLtmp_strategy$rules$order[[tmp_index]]$arguments
#              
#              pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
#              if( any(pnamepos>0)){
#                #just change the argument value itself will do ?or still need add.indicator??
#                PLtmp_strategy$rules$order[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
#              }
#              else{
#                PLtmp_strategy$rules$order[[tmp_index]]$arguments<-append(targ1,tmp_arg)
#                
#              }
#            },
#            'enter'={
#              targ1<-PLtmp_strategy$rules$enter[[tmp_index]]$arguments
#              
#              pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
#              if( any(pnamepos>0)){
#                #just change the argument value itself will do ?or still need add.indicator??
#                PLtmp_strategy$rules$enter[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
#              }
#              else{
#                PLtmp_strategy$rules$enter[[tmp_index]]$arguments<-append(targ1,tmp_arg)
#                
#              }						
#            },
#            'exit'={
#              targ1<-PLtmp_strategy$rules$exit[[tmp_index]]$arguments
#              
#              pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
#              if( any(pnamepos>0)){
#                #just change the argument value itself will do ?or still need add.indicator??
#                PLtmp_strategy$rules$exit[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
#              }
#              else{
#                PLtmp_strategy$rules$exit[[tmp_index]]$arguments<-append(targ1,tmp_arg)
#                
#              }
#            }
#     )
#   } #loop j
#   
#   #Initial portfolio for each test		
#   #######################################################################################
#   
#   testPack$portfolio.st<-paste(portfolios,'p',i,sep='.')
#   testPack$account.st<-paste(portfolios,'p',i,sep='.')
#   
#   rmpstr<-paste('portfolio',testPack$portfolio.st,sep=".")
#   rmastr<-paste('account',testPack$account.st,sep=".")
#   
#   try(rm(list = rmpstr, pos = .blotter),silent=FALSE)
#   try(rm(list = rmastr, pos = .blotter),silent=FALSE)
#   try(rm(list=paste("order_book",testPack$account.st,sep="."),pos=.strategy),silent=FALSE)
#   
#   if(verbose >=1) print('Initial portf')
#   
#   #				Decide not to remove the main obj from .blotter, incase of non-parallel run.
#   #				try(rm(list=paste("order_book",portfolios,sep='.'),pos=.strategy),silent=TRUE)
#   ##				try(rm(paste("account",portfolio.st,sep='.'),paste("portfolio",portfolio.st,sep='.'),pos=.blotter),silent=TRUE)
#   #				try(rm(list=paste("account",portfolios,sep='.'),pos=.blotter))
#   #				try(rm(list=paste("portfolio",portfolios,sep='.'),pos=.blotter))
#   
#   try({initPortf(testPack$portfolio.st,symbols=stock.str, initDate=initDate)})
#   try({initAcct(testPack$account.st,testPack$portfolio.st, initDate=initDate)})
#   try({initOrders(portfolio=testPack$portfolio.st,initDate=initDate)})
#   
#   # Apply strategy ######################################################################################
#   if(verbose >=1) print("Apply strategy...")
#   
#   try(rm("PLtmp_strategy",pos=.strategy),silent=TRUE)
#   
#   if(verbose >=1) print(PLtmp_strategy$signals[[2]])
#   
#   assign("PLtmp_strategy1",PLtmp_strategy,envir=as.environment(.strategy))
#   
#   testPack$out<-try(applyStrategy(strategy=PLtmp_strategy , portfolios=testPack$portfolio.st ),...=...)
#   testPack$strategy<-PLtmp_strategy
#   
#   # 	Update portfolio ######################################################################################
#   
#   #out<-try(applyStrategy(strategy=stratBBands , portfolios=portfolios ))
#   #		try({
#   #					updatePortf(testPack$portfolio.st,Date=initDate)
#   #					updateAcct(testPack$account.st,Date=initDate)
#   #					updateOrders(portfolio=testPack$portfolio.st)
#   #				})
#   
#   
#   #try(updatePortf(Portfolio=testPack$portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep='')))
#   updatePortf(Portfolio=testPack$portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep=''))
#   
#   #no need to update account.
#   #updateAcct(account.st,Dates=paste(startDate,endDate,sep="::")) 
#   #updateEndEq(account.st,Dates=paste(startDate,endDate,sep="::"))
#   #getEndEq(account.st,Sys.time())
#   
#   testPack$parameters<-paramTable[i,]
#   
#   testPack$stats<-tradeStats(Portfolios=testPack$portfolio.st)
#   testPack$blotterl<-as.list(.blotter)
#   #				testPack$blotter<-as.environment(.blotter)
#   #				testPack$blotterr<-.blotter
#   
#   return(testPack)
#   
# }	# Loop i
#   gc(verbose=verbose)
#   
#   
#   for (k in 1: nrow(paramTable)){
#     testPackListPRLStructure$statsTable<-rbind(testPackListPRLStructure$stats,cbind(testPackListPRL[[k]]$parameters,testPackListPRL[[k]]$stats))
#     if(verbose >=1) print(names(testPackListPRL[[k]]$blotterl))
#     
#     for(nn in 1:length(testPackListPRL[[k]]$blotterl)){
#       #			if(verbose >=1) print(paste(names(testPackListPRL[[k]]$blotterl)[nn],'nnp',nn,sep='.'))
#       assign(names(testPackListPRL[[k]]$blotterl[nn]),testPackListPRL[[k]]$blotterl[[nn]],envir=as.environment(.blotter))
#     }
#     names(testPackListPRL)[k]<-testPackListPRL[[k]]$portfolio.st
#   }
#   
#   
#   testPackListPRLStructure$eachRun<-testPackListPRL
#   testPackListPRLStructure$paramTable<-paramTable
#   testPackListPRLStructure$paramConstrainTable<-data.frame(parameterConstraints)
#   
#   testPackListPRLStructure$parameterDistribution<-parameterPool
#   testPackListPRLStructure$parameterConstraints<-parameterConstraints
#   
#   return(testPackListPRLStructure)
#   
# }
# 
# assignInNamespace("applyParameter", applyParameter, ns="quantstrat")
# 
# # this function should not be edited, it is just called by applyParameter 
# paramConstraint <- function(label,data=mktdata, columns, relationship=c("gt","lt","eq","gte","lte")) {
#   relationship=relationship[1] #only use the first one
#   #	if(verbose >=1) print(columns)
#   if (length(columns)==2){
#     ret_sig=NULL
#     if (relationship=='op'){
#       # (How) can this support "Close"? --jmu
#       if(columns[1] %in% c("Close","Cl","close"))
#         stop("Close not supported with relationship=='op'")
#       switch(columns[1],
#              Low =, 
#              low =, 
#              bid = { relationship = 'lt' },
#              Hi  =,
#              High=,
#              high=,
#              ask = {relationship = 'gt'}
#       )
#     }
#     
#     colNums <- match.names(columns,colnames(data))
#     
#     opr <- switch( relationship,
#                    gt = , '>' = '>', 
#                    lt =, '<' = '<', 
#                    eq =, "==" =, "=" = "==",
#                    gte =, gteq =, ge =, ">=" = ">=",
#                    lte =, lteq =, le =, "<=" = "<="
#     )
#     
#     ret_sig$tname <- do.call( opr, list(data[,colNums[1]], data[,colNums[2]]))
#     
#   } else {
#     stop("comparison of more than two columns not supported, see sigFormula")
#   }
#   names(ret_sig)<-label
#   return(data.frame(ret_sig))
# }
# 
# assignInNamespace("paramConstraint", paramConstraint, ns="quantstrat")