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

# assignInNamespace("getSymbols.MySQL", getSymbols.MySQL, ns="quantmod")
# rm(getSymbols.MySQL)

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
    fr <- xts(fr[, -1], as.Date(fr[, 1], format = format, 
                                origin = "1970-01-01"), src = "csv", updated = Sys.time())
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

# example
# getSymbols.MySQL <- getSymbols.mysql
# db.fields <- c("date", "open", "high", "low", "close", "bid", "ask", "volume", "adjusted")
# field.names <- db.fields[-1]
# IF1207 <- getSymbols.MySQL("IF1207", verbose=F, auto.assign=F, user="test", password="123456", dbname="stockdata", host="192.168.10.5", db.fields=db.fields, field.names=field.names)
# 
# IF1207<-IF1207["T09:14/T15:15"]
# IF1207 <- IF1207[, c("high", "low", "close", "bid","ask","volume")]
# IF1207 <- to.minutes(IF1207)
# save(IF1207, file="IF1207.rda")