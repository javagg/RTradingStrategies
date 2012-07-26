library(quantstrat)
convert.time.series <- function (fr, return.class) {
  if ("quantmod.OHLC" %in% return.class) {
    class(fr) <- c("quantmod.OHLC", "zoo")
    return(fr)
  }
  else if ("xts" %in% return.class) {
    return(fr)
  }
  if ("zoo" %in% return.class) {
    return(as.zoo(fr))
  }
  else if ("ts" %in% return.class) {
    fr <- as.ts(fr)
    return(fr)
  }
  else if ("data.frame" %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  }
  else if ("matrix" %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  }
  else if ("its" %in% return.class) {
    if ("package:its" %in% search() || suppressMessages(require("its", 
                                                                quietly = TRUE))) {
      fr.dates <- as.POSIXct(as.character(index(fr)))
      fr <- its::its(coredata(fr), fr.dates)
      return(fr)
    }
    else {
      warning(paste("'its' from package 'its' could not be loaded:", 
                    " 'xts' class returned"))
    }
  }
  else if ("timeSeries" %in% return.class) {
    if ("package:timeSeries" %in% search() || suppressMessages(require("timeSeries", 
                                                                       quietly = TRUE))) {
      fr <- timeSeries(coredata(fr), charvec = as.character(index(fr)))
      return(fr)
    }
    else {
      warning(paste("'timeSeries' from package 'timeSeries' could not be loaded:", 
                    " 'xts' class returned"))
    }
  }
}

getSymbols.mysql <- function (Symbols, env, return.class = "xts", db.fields = c("date", "o", "h", "l", "c", "v", "a"), 
    field.names = NULL, user = NULL, password = NULL, dbname = NULL, ...) {
  
  importDefaults("getSymbols.MySQL")
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  if (missing(verbose)) 
    verbose <- FALSE
  if (missing(auto.assign)) 
    auto.assign <- TRUE
  if ("package:DBI" %in% search() || require("DBI", quietly = TRUE)) {
    if ("package:RMySQL" %in% search() || require("RMySQL", quietly = TRUE)) {
    }
    else {
      warning(paste("package:", dQuote("RMySQL"), "cannot be loaded"))
    }
  } else {
    stop(paste("package:", dQuote("DBI"), "cannot be loaded."))
  }
  if (is.null(user) || is.null(password) || is.null(dbname)) {
    stop(paste("At least one connection argument (", sQuote("user"), 
               sQuote("password"), sQuote("dbname"), ") is not set"))
  }
  con <- dbConnect(MySQL(), user = user, password = password, dbname = dbname)
  db.Symbols <- dbListTables(con)
  if (length(Symbols) != sum(Symbols %in% db.Symbols)) {
    missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
    warning(paste("could not load symbol(s): ", paste(missing.db.symbol, collapse = ", ")))
    Symbols <- Symbols[Symbols %in% db.Symbols]
  }
  for (i in 1:length(Symbols)) {
    if (verbose) {
      cat(paste("Loading ", Symbols[[i]], paste(rep(".", 10 - nchar(Symbols[[i]])), collapse = ""), sep = ""))
    }
    
    query <- paste("SELECT ", paste(db.fields, collapse = ","), " FROM ", Symbols[[i]], " ORDER BY date")
    rs <- dbSendQuery(con, query)
    fr <- fetch(rs, n = -1)
    fr <- xts(as.matrix(fr[, -1]), order.by = as.POSIXct(fr[, 1], origin = "1970-01-01"), src = dbname, updated = Sys.time())
    #colnames(fr) <- paste(Symbols[[i]], c("Open", "High", "Low", "Close", "Volume", "Adjusted"), sep = ".")
    colnames(fr) <- field.names
    
    fr <- convert.time.series(fr = fr, return.class = return.class)
    if (auto.assign) 
      assign(Symbols[[i]], fr, env)
    if (verbose) 
      cat("done\n")
  }
  dbDisconnect(con)
  if (auto.assign) 
    return(Symbols)
  return(fr)
}

getSymbols.MySQL <- getSymbols.mysql

# assignInNamespace("getSymbols.MySQL", getSymbols.MySQL, ns="quantmod")
# assignInNamespace("getSymbols.mysql", getSymbols.MySQL, ns="quantmod")
# rm(getSymbols.MySQL)
# rm(getSymbols.mysql)

# These functions should be included into the 'xts' package
to.seconds <- function(x, k, name, ...) {
  if (missing(name)) 
    name <- deparse(substitute(x))
  if (missing(k)) 
    k <- 1
  to.period(x, "seconds", k = k, name = name, ...)
}

to.seconds5 <- function(x, name, ...) {
  to.seconds(x, k = 5, name = name, ...)
}

to.seconds10 <- function(x, name, ...) {
  to.seconds(x, k = 10, name = name, ...)
}

to.seconds15 <- function(x, k, name, ...) {
  to.seconds(x, k = 15, name = name, ...)
}

to.seconds30 <- function(x, k, name, ...) {
  to.seconds(x, k = 30, name = name, ...)
}

Delt_ <- function(x1,x2=NULL,k=0,type=c('arithmetic','log')) {
  x1 <- try.xts(x1, error=FALSE)
  type <- match.arg(type[1],c('log','arithmetic'))
  if(length(x2)!=length(x1) && !is.null(x2)) stop('x1 and x2 must be of same length');
  if(is.null(x2)){
    x2 <- x1 #copy for same symbol deltas
    if(length(k) < 2) {
      k <- max(1,k)
    }
  }
  dim(x2) <- NULL  # allow for multiple k matrix math to happen
  if(type=='log') {
    xx <- lapply(k, function(K.) {
      log(unclass(x2)/lag(x1,K.))
    })
  } else {
    xx <- lapply(k, function(K.) {
      unclass(x2)/lag(x1,K.)-1
    })
  }
  xx <- do.call("cbind", xx)
  colnames(xx) <- paste("Delt", k, type,sep=".")
  reclass(xx, x1)
}

# The modified function should be included into the 'quantmod' package
periodReturn <- function (x, period = "monthly", subset = NULL, type = "arithmetic", leading = TRUE, ...) {
  xx <- try.xts(x)
  if (inherits(x, "ts")) {
    x <- na.omit(try.xts(x))
    xtsAttributes(x) <- CLASS(x) <- NULL
    xx <- x
    TS <- TRUE
  }
  else TS <- FALSE
  if (has.Op(xx) & has.Cl(xx)) {
    getFirst <- function(X) Op(X)
    getLast <- function(X) Cl(X)
  }
  else getFirst <- getLast <- function(X) X[, 1]
  on.opts <- list(secondly = "seconds", minutely = "minutes", hourly = "hours", daily = "days", weekly = "weeks", monthly = "months", 
                  quarterly = "quarters", yearly = "years", annually = "years")
  ep <- endpoints(xx, on = on.opts[[period]])
  ret <- Delt_(Cl(to_period(x, period = on.opts[[period]], ...)), type = type)
  if (leading) {
    firstval <- as.numeric(Delt_(getFirst(xx[1]), getLast(xx[ep[2]]), type = type))
    ret[1] <- firstval
  }
  colnames(ret) <- paste(period, "returns", sep = ".")
  if (TS) 
    xx <- 1
  tmp.ret <- reclass(ret, xx[ep[-1]])
  if (is.null(subset)) 
    subset <- "/"
  reclass(as.xts(tmp.ret)[subset])
}

getSymbols.myfile <- function(Symbols, env) {
  importDefaults("getSymbols.myfile")
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  
  if (missing(verbose)) 
    verbose <- FALSE
  if (missing(auto.assign)) 
    auto.assign <- TRUE
  
  for (i in 1:length(Symbols)) {
    return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
    return.class <- ifelse(is.null(return.class), default.return.class, return.class)
    
    format <- getSymbolLookup()[[Symbols[[i]]]]$format
    if (is.null(format)) 
      format <- ""    
    if (verbose) 
      cat("loading ", Symbols[[i]], ".....")
    
    fr <- read.csv(sym.file)
    if (verbose) 
      cat("done.\n")
    fr <- xts(fr[, -1], as.Date(fr[, 1], format = format, origin = "1970-01-01"), src = "csv", updated = Sys.time())
    colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols[[i]])), 
                          c("Open", "High", "Low", "Close", "Volume", "Adjusted"), 
                          sep = ".")
    fr <- convert.time.series(fr = fr, return.class = return.class)
    Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
    if (auto.assign) 
      assign(Symbols[[i]], fr, env)
  }
  if (auto.assign) 
    return(Symbols)
  return(fr)
}

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
  if (!is.null(dargs$env)) {env <- dargs$env} else env=.GlobalEnv
  if (!is.null(dargs$symbol)) {symbol<-dargs$symbol} else symbol=NULL
  if (!is.null(dargs$prefer)) {prefer<-dargs$prefer} else prefer=NULL
  if (is.null(Prices)) {
    prices = getPrice(get(Symbol, pos=env), symbol=symbol, prefer=prefer)[,1]
  } else {
    prices = Prices
  }
  
  # if no date is specified, get all available dates
  if(is.null(Dates)) {
    Dates = time(prices)
  } else if(!is.timeBased(Dates)) {
    Dates = time(prices[Dates])
  }
  
  if(.parseISO8601(Dates)$first.time < as.POSIXct(first(index(prices))) || is.na(.parseISO8601(Dates)$first.time)){
    last.time<-last(Dates)
    Dates <- index(prices[paste('/',last.time,sep='')])
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
  
  #  ***** Vectorization *****#
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

