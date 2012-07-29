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
    if (!is.null(timestamp)) {
#       cat(timestamp, ": in ruleProc for ", ruletype, "\n")
    }
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
            print("hhhhh")
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
#             if (tmpqty > 0) { # positive quantity 'buy'
#               move_order <- ifelse((mkt_price_series+orderThreshold) < tmpprice, TRUE, FALSE)
#               #this ifelse creates a logical xts vector 
#               relationship = "gte"
#             } else {  # negative quantity 'sell'
#               move_order <- ifelse((mkt_price_series+orderThreshold) > tmpprice, TRUE, FALSE)
#               relationship = "lte"
#             }
                        
#             tmpidx <- NULL
#             if (any(move_order)) {
#               dindex <- get.dindex()
#               #print(firsttime)
#               # find first index where we would move an order
#               orderidx <- first(which(move_order))
#               if (is.null(tmpidx))
#                 tmpidx <- format(index(move_order[orderidx,]), "%Y-%m-%d %H:%M:%OS6")
#               
#               trailspan <- paste(format(firsttime, "%Y-%m-%d %H:%M:%OS6"), "::", tmpidx, sep='')
#               #make sure we don't cross before then 
#               # use sigThreshold
#               cross <- sigThreshold(data=mkt_price_series, label='tmptrail', column=col, threshold=tmpprice, relationship=relationship)
#               # find first index that would cross after this index
#               if (any(cross[trailspan])) {
#                 newidx <- curIndex + which(cross[trailspan])[1] - 1  #curIndex/firsttime was 1 in the subset, we need a -1 offset?
#                 newidx <- index(mktdata[index(which(cross[trailspan])[1]), which.i=TRUE])
#                 # insert that into dindex
#                 assign.dindex(c(get.dindex(),newidx))
#               } else {
#                 #if we don't cross, do this
#                 moveidx <- index(mktdata[index(move_order[orderidx,]), which.i=TRUE])
#                 assign.dindex(c(get.dindex(), moveidx))
#               }    
#             } # end any(move_order) check 
            
            mkt_price_series <- getPrice(mktdata[timespan], prefer = prefer)
            drawdown <- mkt_price_series - cummax(mkt_price_series)
            drawdown <- drawdown * ifelse(tmpqty < 0, 1, -1)
            colnames(drawdown) <- "drawdown"
            
            relationship = ifelse(tmpqty > 0, "gte", "lte")
            cross <- sigThreshold(data=drawdown, label="", column="drawdown", threshold=orderThreshold, relationship=relationship)
            if (any(cross)) {
              newidx <- curIndex + which(cross)[1] - 1
              assign.dindex(c(get.dindex(), newidx))
            }
            
            part.mkt.price <- mkt_price_series[1:which(cross)[1]-1,]
            max.market.price <- max(part.mkt.price)
            max.i <- last(which(part.mkt.price==max.market.price))
            peak.timestamp <- index(part.mkt.price[max.i,])
            print(peak.timestamp)
            peak.index <- c(rep(F, max.i-1), T, rep(NA,length(drawdown)-max.i))
            drawdown <- cbind(drawdown, mkt_price_series, orderThreshold, cross, max.market.price, peak.index)
            colnames(drawdown) <- c("drawdown", "mkt.price", "threshold", "cross", "max.mkt.price","peak.index")
            ddrawdown <<- drawdown
            
            neworder <- addOrder(portfolio=portfolio, symbol=symbol, timestamp=peak.timestamp, qty=ordersubset[onum,"Order.Qty"], price=max.market.price, 
              ordertype=ordersubset[onum,"Order.Type"], side=ordersubset[onum,"Order.Side"], orderset=ordersubset[onum, "Order.Set"], threshold=orderThreshold,
                     status="open", return=TRUE, ...=..., TxnFees=ordersubset[onum,"Txn.Fees"])
            ordersubset[onum, "Order.Status"] <- "replaced"
            ordersubset[onum,"Order.StatusTime"] <- format(peak.timestamp, "%Y-%m-%d %H:%M:%S")
            print("oldorder")
            print(ordersubset[onum,])
            print("neworder")
            print(neworder)

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
#   cat(format(timestamp, "%Y-%m-%d %H:%M:%OS6"), ": in ruleOrderProc\n")

  orderbook <- getOrderBook(portfolio)
  ordersubset <- orderbook[[portfolio]][[symbol]]
  
  # get open orders
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
      if(ordersubset[ii, "Order.Status"] != "open")	# need to check this bc sideeffects may have changed order.status in this loop
      {
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
        
        # if market moved through my price, execute
        if (orderQty > 0) { # positive quantity 'buy'
           if (isBBOmktdata) prefer = 'offer'
           if (orderPrice >= getPrice(mktdataTimestamp, prefer=prefer)[,1]) { #TODO maybe use last(getPrice) to catch multiple prints on timestamp?
             # price we're willing to pay is higher than the offer price, so execute at the prevailing price
             #txnprice = orderPrice
             txnprice = as.numeric(getPrice(mktdataTimestamp, prefer=prefer)[,1]) #presumes unique timestamps
             txntime = timestamp
           }
         } else { # negative quantity 'sell'
           if (isBBOmktdata) prefer = 'bid'
           if (orderPrice <= getPrice(mktdataTimestamp, prefer=prefer)[,1]) {
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
         
         cat("\n\n\n\n................................\n")
         print(timestamp)
         cat("process stoptrailling order...\n")
         print(ordersubset[ii,])
         print("mkt.price")
         print(mktdataTimestamp)
         cat("................................\n\n\n\n")
         
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
         
          # check if we need to move the stop
          mvstop = FALSE
          if (orderQty > 0) { # positive quantity 'buy'
            if (as.numeric(last(getPrice(x=mktdataTimestamp, prefer=prefer)[,1]))+orderThreshold < orderPrice) 
              mvstop = TRUE
          } else {  # negative quantity 'sell'
            if (as.numeric(last(getPrice(x=mktdataTimestamp, prefer=prefer)[,1]))+orderThreshold > orderPrice) 
              mvstop = TRUE
          }
          if (isTRUE(mvstop)) {
            neworder <- addOrder(portfolio=portfolio, symbol=symbol, timestamp=timestamp, qty=orderQty, price=as.numeric(getPrice(mktdataTimestamp,prefer=prefer)[,1]), 
              ordertype=orderType, side=ordersubset[ii,"Order.Side"], threshold=orderThreshold, status="open", replace=FALSE, return=TRUE, ...=..., TxnFees=txnfees)
            if (is.null(neworders)) neworders = neworder else neworders = rbind(neworders, neworder)
            
            ordersubset[ii,"Order.Status"] <- 'replaced'
            ordersubset[ii,"Order.StatusTime"] <- format(timestamp, "%Y-%m-%d %H:%M:%S")
            next()
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
        ordersubset[ii,"Order.StatusTime"]<-format(timestamp, "%Y-%m-%d %H:%M:%S")
        
        #close all other orders in the same order set
        OrdersetTag = toString(ordersubset[ii, "Order.Set"])
        OpenInOrderset.i = which(ordersubset[, "Order.Status"] == 'open' & ordersubset[,"Order.Set"] == OrdersetTag)
        
        # skip this if there are no orders
        if (length(OpenInOrderset.i) > 0) {
          ordersubset[OpenInOrderset.i, "Order.Status"] = 'canceled'
          ordersubset[OpenInOrderset.i, "Order.StatusTime"] <- format(timestamp, "%Y-%m-%d %H:%M:%S")
        } 
      }
#       cat("an ", orderType, " order handled(closed,replaced,rejected\n")
    } #end loop over open orders
    
    if (!is.null(neworders)) ordersubset = rbind(ordersubset, neworders)
    
    # now put the orders back in
    # assign order book back into place (do we need a non-exported "put" function?)
    orderbook[[portfolio]][[symbol]] <- ordersubset
    assign(paste("order_book", portfolio, sep='.'), orderbook, envir=.strategy)
  } # end check for open orders
}

assignInNamespace("ruleOrderProc", ruleOrderProc, ns="quantstrat")

ruleSignal <- function(data=mktdata, timestamp, sigcol, sigval, orderqty=0, ordertype, orderside=NULL, orderset=NULL, threshold=NULL, tmult=FALSE, replace=TRUE, delay=0.0001, osFUN='osNoOp', pricemethod=c('market','opside','active'), portfolio, symbol, ..., ruletype, TxnFees=0, prefer=NULL, sethold=FALSE, label='')
{
  if(!is.function(osFUN)) osFUN<-match.fun(osFUN)
  #print(paste(symbol,timestamp, sigval))
  #print(data[timestamp][,sigcol])
  #browser()
  if (!is.na(timestamp) && !is.na(data[timestamp][,sigcol]) && data[timestamp][,sigcol] == sigval) {
    #calculate order price using pricemethod
    pricemethod<-pricemethod[1] #only use the first if not set by calling function
    
    if(hasArg(prefer)) prefer=match.call(expand.dots=TRUE)$prefer
    else prefer = NULL
    
    #if(hasArg(TxnFees)) TxnFees=match.call(expand.dots=TRUE)$TxnFees
    #else TxnFees=0
    
    switch(pricemethod,
           market = ,
           opside = ,
           active = {
             if(is.BBO(data)){
               if (orderqty>0) 
                 prefer='ask'  # we're buying, so pay what they're asking
               else
                 prefer='bid'  # we're selling, so give it to them for what they're bidding  
             } 
             orderprice <- try(getPrice(x=data, prefer=prefer))[timestamp] 
           },
           passive =,
           work =,
           join = {
             if(is.BBO(data)){
               if (orderqty>0) 
                 prefer='bid'  # we're buying, so work the bid price
               else
                 prefer='ask'  # we're selling, so work the ask price
             }
             orderprice <- try(getPrice(x=data, prefer=prefer))[timestamp]
           },
           maker = {
             if(hasArg(price) & length(match.call(expand.dots=TRUE)$price)>1) {
               # we have prices, just use them
               orderprice <- try(match.call(expand.dots=TRUE)$price)
             } else {
               if(!is.null(threshold)) {
                 baseprice<- last(getPrice(x=data)[timestamp]) # this should get either the last trade price or the Close
                 if(hasArg(tmult) & isTRUE(match.call(expand.dots=TRUE)$tmult)) {
                   baseprice<- last(getPrice(x=data)[timestamp]) # this should get either the last trade price or the Close
                   # threshold is a multiplier of current price
                   if (length(threshold)>1){
                     orderprice <- baseprice * threshold # assume the user has set proper threshold multipliers for each side
                   } else {
                     orderprice <- c(baseprice*threshold,baseprice*(1+1-threshold)) #just bracket on both sides
                   }
                 } else {
                   # tmult is FALSE or NULL, threshold is numeric
                   if (length(threshold)>1){
                     orderprice <- baseprice + threshold # assume the user has set proper threshold numerical offsets for each order
                   } else {
                     orderprice <- c(baseprice+threshold,baseprice+(-threshold)) #just bracket on both sides
                   }
                 }
               } else{
                 # no threshold, put it on the averages?
                 stop('maker orders without specified prices and without threholds not (yet?) supported')
                 if(is.BBO(data)){
                   
                 } else {
                   
                 }
               }
             }
             if(length(orderqty)==1) orderqty <- c(orderqty,-orderqty) #create paired market maker orders at the same size
           }
    )
    if(inherits(orderprice,'try-error')) orderprice<-NULL
    if(length(orderprice>1) && !pricemethod=='maker') orderprice<-last(orderprice[timestamp])
    if(!is.null(orderprice) && !is.null(ncol(orderprice))) orderprice <- orderprice[,1]
    
    if(is.null(orderside) & !isTRUE(orderqty == 0)){
      curqty<-getPosQty(Portfolio=portfolio, Symbol=symbol, Date=timestamp)
      if (curqty>0 ){
        #we have a long position
        orderside<-'long'
      } else if (curqty<0){
        #we have a short position
        orderside<-'short'
      } else {
        # no current position, which way are we going?
        if (orderqty>0) 
          orderside<-'long'
        else
          orderside<-'short'
      }
    }
    
    if(is.null(orderset)) orderset=NA
    
    ## now size the order
    #TODO add fancy formals matching for osFUN
    if(orderqty!='all')
    {
      orderqty <- osFUN(strategy=strategy, data=data, timestamp=timestamp, orderqty=orderqty, ordertype=ordertype, orderside=orderside, portfolio=portfolio, symbol=symbol,...=...,ruletype=ruletype, orderprice=as.numeric(orderprice))
    }
    
    if(!is.null(orderqty) && orderqty!=0 && !is.null(orderprice)) #orderprice could have length > 1
    {
      addOrder(portfolio=portfolio, 
               symbol=symbol, 
               timestamp=timestamp, 
               qty=orderqty, 
               price=as.numeric(orderprice), 
               ordertype=ordertype, 
               side=orderside, 
               orderset=orderset, 
               threshold=threshold, 
               status="open", 
               replace=replace , 
               delay=delay, 
               tmult=tmult, 
               ...=..., 
               prefer=prefer, 
               TxnFees=TxnFees,
               label=label)
      
#       cat(ordertype, " order added!\n")
    }
  }
  if(sethold) hold <<- TRUE
}

assignInNamespace("ruleSignal", ruleSignal, ns="quantstrat")
