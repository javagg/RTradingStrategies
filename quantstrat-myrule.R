applyRules <- function (portfolio, symbol, strategy, mktdata, Dates = NULL, indicators = NULL, signals = NULL, parameters = NULL, ..., path.dep = TRUE, rule.order = NULL) 
{
  .Data <- new.env()
  get.dindex <- function() get("dindex", pos = .Data)
  assign.dindex <- function(dindex) {
    dindex <- sort(unique(dindex))
    assign("dindex", dindex, .Data)
  }
  
  if (!is.strategy(strategy)) {
    strategy <- try(getStrategy(strategy))
    if (inherits(strategy, "try-error")) 
      stop("You must supply an object of type 'strategy'.")
  }
  
  ret <- NULL
  nargs <- list(...)
  if (length(nargs) == 0) 
    nargs = NULL
  if (length("...") == 0 | is.null("...")) {
    rm("...")
    nargs = NULL
  }
  
  Dates = unique(index(mktdata))
  
  ruleProc <- function(ruletypelist, timestamp = NULL, path.dep, ruletype, ...) {
    for (rule in ruletypelist) {
      if (!rule$path.dep == path.dep) 
        next()
      if (!is.function(rule$name)) {
        if (!is.function(get(rule$name))) {
          if (!is.function(get(paste("sig", rule$name, sep = ".")))) {
            message(paste("Skipping rule", rule$name, "because there is no function by that name to call"))
            next()
          }
          else {
            rule$name <- paste("sig", rule$name, sep = ".")
          }
        }
      }
      if (!isTRUE(rule$enabled)) 
        next()
      if (!is.null(rule$timespan) && nrow(mktdata[rule$timespan][timestamp]) == 0) 
        next()
      if (is.function(rule$name)) 
        fun <- rule$name
      else fun <- match.fun(rule$name)
      
      nargs <- list(...)
      if (length(nargs) == 0) 
        nargs = NULL
      if (length("...") == 0 | is.null("...")) {
        rm("...")
        nargs = NULL
      }
      .formals <- formals(fun)
      onames <- names(.formals)
      rule$arguments$timestamp = timestamp
      rule$arguments$ruletype = ruletype
      rule$arguments$label = rule$label
      pm <- pmatch(names(rule$arguments), onames, nomatch = 0L)
      names(rule$arguments[pm > 0L]) <- onames[pm]
      .formals[pm] <- rule$arguments[pm > 0L]
      if (length(parameters)) {
        pm <- pmatch(names(parameters), onames, nomatch = 0L)
        names(parameters[pm > 0L]) <- onames[pm]
        .formals[pm] <- parameters[pm > 0L]
      }
      if (length(nargs)) {
        pm <- pmatch(names(nargs), onames, nomatch = 0L)
        names(nargs[pm > 0L]) <- onames[pm]
        .formals[pm] <- nargs[pm > 0L]
      }
      .formals$... <- NULL
      if (!is.null(rule$arguments$prefer)) 
        .formals$prefer = rule$arguments$prefer
      tmp_val <- do.call(fun, .formals)
      mktdata <<- mktdata
      ret <<- ret
      hold <<- hold
      holdtill <<- holdtill
    } # for
  }
  
  if (isTRUE(path.dep)) {
    dindex <- c(1, length(Dates))
    assign.dindex(dindex)
    for (type in names(strategy$rules)) {
      if (length(strategy$rules[[type]]) >= 1) {
        for (rule in strategy$rules[[type]]) {
          if (isTRUE(rule$path.dep)) {
            if (is.null(rule$arguments$sigcol) | is.null(rule$arguments$sigval)) {
              assign.dindex(1:length(Dates))
            } else {
              if (is.null(rule$timespan)) {
                assign.dindex(c(get.dindex(), which(mktdata[, rule$arguments$sigcol] == rule$arguments$sigval)))
              } else {
                assign.dindex(c(get.dindex(), which(merge(.xts(, .index(mktdata)), mktdata[rule$timespan, rule$arguments$sigcol]) == rule$arguments$sigval)))
              }
            }
          }
        } # for each rule
      }
    } # for each type
    
    dindex <- get.dindex()
    if (length(dindex) == 0) 
      dindex = 1
  } else {
    Dates = ""
    dindex = 1
  }
  
  nextIndex <- function(curIndex, ...) {
    if (!isTRUE(path.dep)) {
      curIndex = FALSE
      return(curIndex)
    }
    
    dindex <- get.dindex()
    hasmktord <- FALSE
    nidx = FALSE
    neworders = NULL
    orderbook <- getOrderBook(portfolio)
    ordersubset <- orderbook[[portfolio]][[symbol]]
    oo.idx <- getOrders(portfolio = portfolio, symbol = symbol, status = "open", which.i = TRUE)
    if (length(oo.idx) == 0) {
      nidx = FALSE
    } else {
      isOHLCmktdata <- is.OHLC(mktdata)
      isBBOmktdata <- is.BBO(mktdata)
      timespan <- paste(timestamp, "::", sep = "")
      if (nrow(ordersubset[oo.idx, ][timespan]) == 0) {
        nidx = FALSE
      } else {
        if (!length(grep("market", ordersubset[oo.idx, "Order.Type"])) == 0) {
          if (is.na(curIndex) || (curIndex + 1) > length(index(mktdata))) {
            curIndex = FALSE
          }
          hasmktord <- TRUE
        }
        
        if (!length(grep("limit", ordersubset[oo.idx, "Order.Type"])) == 0) {
          stoplimitorders <- grep("stoplimit", ordersubset[oo.idx, "Order.Type"])
          limitorders <- grep("limit", ordersubset[oo.idx, "Order.Type"])
          
          #limitorders <- limitorders[-stoplimitorders]
          
          # stoplimitorders
          for (slorder in stoplimitorders) {
            print("handle stoplimitorders!")
            dindex <- get.dindex()
            tmpqty <- ordersubset[oo.idx[slorder], "Order.Qty"]
            if (tmpqty == "all") {
              tmpqty = 0
            }
            if (tmpqty == 0) {
              side <- ordersubset[oo.idx[slorder], "Order.Side"]
              if (side == "long") 
                tmpqty = -1
              else tmpqty = 1
            }
            tmpqty <- as.numeric(tmpqty)
            tmpprice <- as.numeric(ordersubset[oo.idx[slorder], "Order.Price"])
            
            if (tmpqty > 0) {
              relationship = "gte"
              if (isBBOmktdata) {
                col <- first(colnames(mktdata)[has.Ask(mktdata, which = TRUE)])
              }
              else if (isOHLCmktdata) {
                col <- first(colnames(mktdata)[has.Hi(mktdata, which = TRUE)])
              }
              else {
                col <- first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
              }
              if (is.na(col)) 
                stop("no price discernable for stoplimit in applyRules")
            } else {   
              relationship = "lte"
              if (isBBOmktdata) {
                col <- first(colnames(mktdata)[has.Bid(mktdata, which = TRUE)])
              }
              else if (isOHLCmktdata) {
                col <- first(colnames(mktdata)[has.Lo(mktdata, which = TRUE)])
              }
              else {
                col <- first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
              }
              if (is.na(col)) 
                stop("no price discernable for stoplimit in applyRules")
            }
            
            # Find dindexes that order can be filled
            cross <- sigThreshold(label = "tmpstop", column = col, threshold = tmpprice, relationship = relationship)
            if (any(cross[timespan])) {
              newidx <- curIndex + which(cross[timespan])[1] - 1
              assign.dindex(c(get.dindex(), newidx))
            }
          }
          
          # limitorders
          for (lorder in limitorders) {
            print("handle limitorders!")
            dindex <- get.dindex()
            tmpqty <- ordersubset[oo.idx[lorder], "Order.Qty"]
            if (tmpqty == "all") {
              tmpqty = 0
            }
            if (tmpqty == 0) {
              side <- ordersubset[oo.idx[lorder], "Order.Side"]
              if (side == "long") 
                tmpqty = -1
              else tmpqty = 1
            }
            tmpqty <- as.numeric(tmpqty)
            tmpprice <- as.numeric(ordersubset[oo.idx[lorder], "Order.Price"])
            if (tmpqty > 0) {
              relationship = "lte"
              if (isBBOmktdata) {
                col <- first(colnames(mktdata)[has.Ask(mktdata, which = TRUE)])
              }
              else if (isOHLCmktdata) {
                col <- first(colnames(mktdata)[has.Lo(mktdata, which = TRUE)])
              }
              else {
                col <- first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
              }
              if (is.na(col)) 
                stop("no price discernable for stoplimit in applyRules")
            } else {
              relationship = "gte"
              if (isBBOmktdata) {
                col <- first(colnames(mktdata)[has.Bid(mktdata, which = TRUE)])
              }
              else if (isOHLCmktdata) {
                col <- first(colnames(mktdata)[has.Hi(mktdata, which = TRUE)])
              }
              else {
                col <- first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
              }
              if (is.na(col)) 
                stop("no price discernable for stoplimit in applyRules")
            }
            
            cross <- sigThreshold(label = "tmplimit", column = col, threshold = tmpprice, relationship = relationship)
            if (any(cross[timespan])) {
              newidx <- curIndex + which(cross[timespan])[1]
              assign.dindex(c(get.dindex(), newidx))
            }
          }
        }
        
        if (!length(grep("trailing", ordersubset[oo.idx, "Order.Type"])) == 0) {
          trailorders <- grep("trailing", ordersubset[oo.idx, "Order.Type"])
          for (torder in trailorders) {
            dindex <- get.dindex()
            firsttime <- NULL
            neworders <- NULL
            onum <- oo.idx[torder]
            orderThreshold <- as.numeric(ordersubset[onum, "Order.Threshold"])
            tmpqty <- ordersubset[onum, "Order.Qty"]
            if (tmpqty == "all") {
              tmpqty = 0
            }
            if (tmpqty == 0) {
              side <- ordersubset[oo.idx[torder], "Order.Side"]
              if (side == "long") 
                tmpqty = -1
              else tmpqty = 1
            }
            tmpqty <- as.numeric(tmpqty)
            if (tmpqty == 0) {
              side <- ordersubset[onum, "Order.Side"]
              if (side == "long") 
                tmpqty = -1
              else tmpqty = 1
            }
            tmpqty <- as.numeric(tmpqty)
            tmpprice <- as.numeric(ordersubset[onum, "Order.Price"])
            tmpidx <- format(index(ordersubset[onum,]), "%Y-%m-%d %H:%M:%OS6")
            if (isBBOmktdata) {
              if (tmpqty > 0) {
                #prefer = "offer"
                prefer = "ask"
              }
              else {
                prefer = "bid"
              }
            } else if (isOHLCmktdata) {
              prefer = "close"
            }
            
            if (is.null(firsttime)) { 
              firsttime <- timestamp
            }
            dindex <- get.dindex()
            nextidx <- min(dindex[dindex > curIndex])
            if (length(nextidx)) {
              nextstamp <- format(index(mktdata[nextidx,]), "%Y-%m-%d %H:%M:%OS6")
              timespan <- paste(format(firsttime, "%Y-%m-%d %H:%M:%OS6"), "::", nextstamp, sep = "")
              print(timespan)
              mkt_price_series <- getPrice(mktdata[timespan], prefer = prefer)
              col <- first(colnames(mkt_price_series))
              orderloop <- TRUE
            } else {
              orderloop <- FALSE
            }
            
            mkt_price_series <- getPrice(mktdata[timespan], prefer = prefer)
            drawdown <<- mkt_price_series - cummax(mkt_price_series)
            colnames(drawdown) <- "drawdown"
            ddrawdown <<- drawdown
            cross <- sigThreshold(data=drawdown, label="", column="drawdown", threshold=orderThreshold, relationship="lte")
            if (any(cross)) {
              newidx <- curIndex + which(cross)[1] - 1
              assign.dindex(c(get.dindex(), newidx))
            }
          }
        }
        
        #         if (!length(grep("trailing", ordersubset[oo.idx, "Order.Type"])) == 0) {
        #           trailorders <- grep("trailing", ordersubset[oo.idx, "Order.Type"])
        #           for (torder in trailorders) {
        #             dindex <- get.dindex()
        #             firsttime <- NULL
        #             neworders <- NULL
        #             onum <- oo.idx[torder]
        #             orderThreshold <- as.numeric(ordersubset[onum, "Order.Threshold"])
        #             tmpqty <- ordersubset[onum, "Order.Qty"]
        #             if (tmpqty == "all") {
        #               tmpqty = 0
        #             }
        #             if (tmpqty == 0) {
        #               side <- ordersubset[oo.idx[torder], "Order.Side"]
        #               if (side == "long") 
        #                 tmpqty = -1
        #               else tmpqty = 1
        #             }
        #             tmpqty <- as.numeric(tmpqty)
        #             if (tmpqty == 0) {
        #               side <- ordersubset[onum, "Order.Side"]
        #               if (side == "long") 
        #                 tmpqty = -1
        #               else tmpqty = 1
        #             }
        #             tmpqty <- as.numeric(tmpqty)
        #             tmpprice <- as.numeric(ordersubset[onum, "Order.Price"])
        #             tmpidx <- format(index(ordersubset[onum,]), "%Y-%m-%d %H:%M:%OS6")
        #             if (isBBOmktdata) {
        #               if (tmpqty > 0) {
        #                 #prefer = "offer"
        #                 prefer = "ask"
        #               }
        #               else {
        #                 prefer = "bid"
        #               }
        #             } else if (isOHLCmktdata) {
        #               prefer = "close"
        #             }
        #             dindex <- get.dindex()
        #             if (is.null(firsttime)) 
        #               firsttime <- timestamp
        #             nextidx <- min(dindex[dindex > curIndex])
        #             if (length(nextidx)) {
        #               nextstamp <- format(index(mktdata[nextidx,]), "%Y-%m-%d %H:%M:%OS6")
        #               timespan <- paste(format(firsttime, "%Y-%m-%d %H:%M:%OS6"), "::", nextstamp, sep = "")
        #               
        #               mkt_price_series <- getPrice(mktdata[timespan], prefer = prefer)
        #               col <- first(colnames(mkt_price_series))
        #               orderloop <- TRUE
        #             } else {
        #               orderloop <- FALSE
        #             }
        #             
        #             if (tmpqty > 0) {
        #               move_order <- ifelse((mkt_price_series + orderThreshold) < tmpprice, TRUE, FALSE)
        #               relationship = "gte"
        #             } else {
        #               move_order <- ifelse((mkt_price_series + orderThreshold) > tmpprice, TRUE, FALSE)
        #               relationship = "lte"
        #             }
        #             tmpidx <- NULL
        #             if (any(move_order)) {
        #               dindex <- get.dindex()
        #               orderidx <- first(which(move_order))
        #               if (is.null(tmpidx)) 
        #                 tmpidx <- format(index(move_order[orderidx,]), "%Y-%m-%d %H:%M:%OS6")
        #               trailspan <- paste(format(firsttime, "%Y-%m-%d %H:%M:%OS6"), "::", tmpidx, sep = "")
        #               cat("trailspan",trailspan,"\n")
        #               cross <- sigThreshold(data = mkt_price_series, label = "tmptrail", column = col, threshold = tmpprice, relationship = relationship)
        #               if (any(cross[trailspan])) {
        #                 newidx <- curIndex + which(cross[trailspan])[1] - 1
        #                 moveidx <- index(mktdata[index(which(cross[trailspan])[1]), which.i = TRUE])
        #                 assign.dindex(c(get.dindex(), newidx))
        #               } else {
        #                 moveidx <- index(mktdata[index(move_order[orderidx,]), which.i = TRUE])
        #                 assign.dindex(c(get.dindex(), moveidx))
        #               }
        #             }
        #           }
        #         }
        
        
        
      }
    }
    
    if (curIndex) {
      if (hasmktord) {
        curIndex <- curIndex + 1
        dindex <- get.dindex()
      } else {
        dindex <- get.dindex()
        if (any(dindex > curIndex)) {
          curIndex <- min(dindex[dindex > curIndex])
        } else { 
          curIndex <- FALSE
        }
      }
    }
    if (is.na(curIndex) || curIndex > length(index(mktdata))) 
      curIndex = FALSE
    return(curIndex)
  }
  
  # path.dep testing starts here.
  hold = FALSE
  holdtill = first(time(Dates)) - 1
  mktinstr <- getInstrument(symbol)
  curIndex <- 1
  
  while (curIndex) {
    timestamp = Dates[curIndex]
    if (isTRUE(hold) & holdtill < timestamp) {
      hold = FALSE
      holdtill = NULL
    }
    if (is.null(rule.order)) {
      types <- sort(factor(names(strategy$rules), levels = c("pre", "risk", "order", "rebalance", "exit", "enter", "entry", "post")))
    }
    else {
      print("Be aware that order of operations matters, and poor choises in rule order can create unintended consequences.")
      types <- rule.order
    }
    for (type in types) {
      switch(type, pre = {
        if (length(strategy$rules[[type]]) >= 1) {
          ruleProc(strategy$rules$pre, timestamp = timestamp, path.dep = path.dep, mktdata = mktdata, portfolio = portfolio, symbol = symbol, ruletype = type, mktinstr = mktinstr, ...)
        }
      }, risk = {
        if (length(strategy$rules$risk) >= 1) {
          ruleProc(strategy$rules$risk, timestamp = timestamp, path.dep = path.dep, mktdata = mktdata, portfolio = portfolio, symbol = symbol, ruletype = type, mktinstr = mktinstr, ...)
        }
      }, order = {
        if (length(strategy$rules[[type]]) >= 1) {
          ruleProc(strategy$rules[[type]], timestamp = timestamp, path.dep = path.dep, mktdata = mktdata, portfolio = portfolio, symbol = symbol, ruletype = type, mktinstr = mktinstr, ...)
        } else {
          if (isTRUE(path.dep)) {
            timespan <- format(timestamp, "::%Y-%m-%d %H:%M:%OS6")
          } else {
            timespan = NULL
          }
          ruleOrderProc(portfolio = portfolio, symbol = symbol, mktdata = mktdata, timespan = timespan, ...)
        }
      }, rebalance = , exit = , enter = , entry = {
        if (isTRUE(hold)) next()
        if (length(strategy$rules[[type]]) >= 1) {
          ruleProc(strategy$rules[[type]], timestamp = timestamp, path.dep = path.dep, mktdata = mktdata, portfolio = portfolio, symbol = symbol, ruletype = type, mktinstr = mktinstr, ...)
        }
        if (isTRUE(path.dep) && length(getOrders(portfolio = portfolio, symbol = symbol, status = "open", timespan = timestamp, which.i = TRUE))) {
        }
      }, post = {
        if (length(strategy$rules$post) >= 1) {
          ruleProc(strategy$rules$post, timestamp = timestamp, path.dep = path.dep, mktdata = mktdata, portfolio = portfolio, symbol = symbol, ruletype = type, mktinstr = mktinstr, ...)
        }
      })
    }
    
    if (isTRUE(path.dep)) {
      curIndex <- nextIndex(curIndex, ...)
    } else {
      curIndex = FALSE
    }
  }
  
  mktdata <<- mktdata
  if (is.null(ret)) {
    return(mktdata)
  }
  else return(ret)
}

assignInNamespace("applyRules", applyRules, ns="quantstrat")

ruleOrderProc <- function (portfolio, symbol, mktdata, timespan = NULL, ordertype = NULL, ..., slippageFUN = NULL) {
  if (is.null(timespan)) 
    return()
  orderbook <- getOrderBook(portfolio)
  ordersubset <- orderbook[[portfolio]][[symbol]]
  OpenOrders.i = NULL
  OpenOrders.i <- getOrders(portfolio = portfolio, symbol = symbol, status = "open", timespan = timespan, ordertype = ordertype, which.i = TRUE)
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
      orderThreshold <- as.numeric(ordersubset[ii, "Order.Threshold"])
      orderType <- ordersubset[ii, "Order.Type"]
      posQty <- getPosQty(Portfolio = portfolio, Symbol = symbol, Date = timestamp)
      orderQty <- ordersubset[ii, "Order.Qty"]
      
      if (orderQty == "all") {
        orderQty = -1 * getPosQty(Portfolio = portfolio, Symbol = symbol, Date = timestamp)
        orderside <- ordersubset[ii, "Order.Side"]
        if (((orderQty > 0 && orderside == "long") || (orderQty < 0 && orderside == "short"))) {
          orderQty = 0
        }
      }
      orderQty <- as.numeric(orderQty)
      
      # Find out whether the order be be filled at the timestamp
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
          if ((orderQty > 0 && orderType != "stoplimit") || (orderQty < 0 && orderType == "stoplimit")) {
            if ((has.Lo(mktdata) && orderPrice > as.numeric(Lo(mktdataTimestamp))) || (!has.Lo(mktdata) && orderPrice >= as.numeric(getPrice(mktdataTimestamp, prefer = prefer)))) {
              txnprice = orderPrice
              txntime = timestamp
            } else next()
          } else if ((orderQty < 0 && orderType != "stoplimit") || (orderQty > 0 && orderType == "stoplimit")) {
            if ((has.Hi(mktdata) && orderPrice < as.numeric(Hi(mktdataTimestamp))) || 
              (!has.Hi(mktdata) && orderPrice <= as.numeric(getPrice(mktdataTimestamp, prefer = prefer)))) {
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
              if (orderPrice <= as.numeric(getPrice(mktdataTimestamp, prefer = "ask")[, 1])) {
                txnprice = orderPrice
                txntime = timestamp
              } else next()
            } else {
              if (orderPrice >= as.numeric(getPrice(mktdataTimestamp, prefer = "ask")[, 1])) {
                txnprice = as.numeric(getPrice(mktdataTimestamp, prefer = "ask")[, 1])
                txntime = timestamp
              } else next()
            }
          } else { # (orderQty <= 0)
            if (orderType == "stoplimit") {
              if (orderPrice >= as.numeric(getPrice(mktdataTimestamp, prefer = "bid")[, 1])) {
                txnprice = orderPrice
                txntime = timestamp
              } else next()
            } else {
              if (orderPrice <= as.numeric(getPrice(mktdataTimestamp, prefer = "bid")[, 1])) {
                txnprice = as.numeric(getPrice(mktdataTimestamp, prefer = "bid")[, 1])
                txntime = timestamp
              } else next()
            }
          }
          if (orderType == "iceberg") {
            neworder <- addOrder(portfolio = portfolio, symbol = symbol, timestamp = timestamp, 
                                 qty = orderQty, price = as.numeric(getPrice(mktdataTimestamp, prefer = prefer)[, 1]), ordertype = orderType, 
                                 side = ordersubset[ii, "Order.Side"], threshold = orderThreshold, 
                                 status = "open", replace = FALSE, return = TRUE, ... = ..., TxnFees = txnfees)
            if (is.null(neworders)) neworders = neworder else neworders = rbind(neworders, neworder)
            
            ordersubset[ii, "Order.Status"] <- "replaced"
            ordersubset[ii, "Order.StatusTime"] <- format(timestamp, "%Y-%m-%d %H:%M:%S")
            next()
          }
        }
      }, stoptrailing = {
        #         print("debug.start-------------------------")
        #         print(ordersubset[ii,])
        #         print("marketprice")
        #         print(mktdataTimestamp)
        #         print("debug.end-------------------------")        
        if (orderQty > 0) {
          #if (isBBOmktdata) prefer = "offer"
          if (isBBOmktdata) prefer = "ask"
          if (orderPrice >= getPrice(mktdataTimestamp, prefer = prefer)[,1]) {
            txnprice = as.numeric(getPrice(mktdataTimestamp, prefer = prefer)[,1])
            txntime = timestamp
          }
        } else {
          if (isBBOmktdata) prefer = "bid"
          prefer.price <- as.numeric(getPrice(mktdataTimestamp, prefer = prefer)[, 1])
          if (orderPrice <= prefer.price) {
            txnprice = prefer.price
            txntime = timestamp
          }
        }
        
        if (isOHLCmktdata) {
          if (orderPrice > as.numeric(Lo(mktdataTimestamp)) & orderPrice < as.numeric(Hi(mktdataTimestamp))) {
            txnprice = orderPrice
            txntime = timestamp
          }
        }
        
        txnprice = NULL
        
        if (is.null(txnprice)) { # The order cannot be filled because of null txnprice
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
            if (as.numeric(last(getPrice(x = mktdataTimestamp,prefer = prefer)[, 1])) + orderThreshold > orderPrice) {
              mvstop = TRUE
            } else {}
          }
          
          if (isTRUE(mvstop)) {
            neworder <- addOrder(portfolio = portfolio, symbol = symbol, timestamp = timestamp, 
                                 qty = orderQty, price = as.numeric(getPrice(mktdataTimestamp, prefer = prefer)[, 1]), ordertype = orderType, 
                                 side = ordersubset[ii, "Order.Side"], threshold = orderThreshold, 
                                 status = "open", replace = FALSE, return = TRUE, ... = ..., TxnFees = txnfees)
            #             colnames(neworder) <- colnames(ordersubset[ii,])
            #             print("old")
            #             print(ordersubset[ii,])
            #             print("new")
            #             print(neworder)
            #             print("next")
            if (is.null(neworders)) neworders = neworder else neworders = rbind(neworders, neworder)
            
            ordersubset[ii, "Order.Status"] <- "replaced"
            ordersubset[ii, "Order.StatusTime"] <- format(timestamp, "%Y-%m-%d %H:%M:%S")
            next()
          }
        }
      }) # switch
      
      if (!is.null(txnprice) && !isTRUE(is.na(txnprice))) { # txnprice is valid
        pos <- getPosQty(portfolio, symbol, timestamp)
        if (orderQty == 0) {
          ordersubset[ii, "Order.Status"] <- "rejected"
        }
        else {
          addTxn(Portfolio = portfolio, Symbol = symbol, TxnDate = txntime, TxnQty = orderQty, TxnPrice = txnprice, ... = ..., TxnFees = txnfees)
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
    if (!is.null(neworders)) {
      ordersubset = rbind(ordersubset, neworders)
    }
    orderbook[[portfolio]][[symbol]] <- ordersubset
    assign(paste("order_book", portfolio, sep = "."), orderbook, envir = .strategy)
  }
}

assignInNamespace("ruleOrderProc", ruleOrderProc, ns="quantstrat")
