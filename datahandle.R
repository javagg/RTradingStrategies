library(reshape2)
library(xts)

data.dir <- "~/workspace/data"
files <- list.files(data.dir, full.names=T)
# print(files)
symbols <- paste("IF120", as.character(1:6), sep="")


for (symbol in symbols) {
  fs <- grep(symbol, files, value=T)
  colnums <- c(5, 6, 7, 8, 10, 11, 9)
  ts <- NULL
  for (file in fs) {
    raw <- read.table(file)
    #   print(head(raw))
    date <- paste(raw[,2], raw[,3], sep=" ")
    date <- paste(date, raw[,4], sep=".")
    date <- as.POSIXct(date, format="%Y%m%d %H:%M:%OS")
    df <- data.frame(raw[,c(5, 6, 7, 10, 11, 8)])
    df <- as.xts(df, order.by=date,  dateFormat="POSIXct")
    colnames(df) <- c("close", "high", "low", "bid", "ask", "volume")
    ts <- rbind(ts, df)
  }
  ts <- ts["T09:14/T15:15"]
  ts <- to.minutes(ts, OHLC=F)
  assign(symbol, ts)
}

save(IF1201,IF1202,IF1203,IF1204,IF1205,IF1206, file="rawdata.rda")
