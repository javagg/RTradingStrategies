symbols <- c(paste("IF110", 4:9, sep=""), paste("IF111", 0:2, sep=""), paste("IF120", 1:6, sep=""))
symbols <- c("IF1101")
symbols <- c("IF1101", "IF1102", "IF1103", "IF1011", "IF1012")
fields <- c("high", "low", "close", "bid", "ask", "volume")
getSymbols(symbols, src="MySQL", user="test", password="123456", dbname="historydata", db.fields=c("date", "open", "high", "low", "close", "bid", "ask", "volume", "adjusted"), field.names=c("open", "high", "low", "close", "bid", "ask", "volume", "adjusted"))

for (symbol in symbols) {
  ts <- get(symbol)
  ts <- ts["T09:15/T15:15"]
  ts <- ts[, fields]
  ts <- to.minutes(ts, OHLC=F)
  assign(symbol, ts)
  print(head(ts))
#   save(symbol, file=paste(symbol, ".rda", sep=""))
}