this.dir <- dirname(parent.frame(2)$ofile)
raw <- read.table(file.path(this.dir, "IF1203_20120314.txt"))

dt <- paste(format(as.POSIXct(as.character(raw[,"V2"]), format="%Y%M%d"), format="%Y-%M-%d"), raw[,"V3"])
dt <- paste(dt, raw[,"V4"], sep=".")
dt <- as.POSIXct(dt)

sample.500ms <- data.frame(High=raw[,"V6"], Low=raw[,"V7"], Close=raw[,"V5"], Bid=raw[,"V10"], Ask=raw[,"V11"],Volume=raw[,"V8"])
sample.500ms <- as.xts(sample.500ms, order.by=dt, dateFormat="POSIXct")

save(sample.500ms, file="sample.500ms.rda")