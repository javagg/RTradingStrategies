# symbols <- c("IF1207")
# for (symbol in symbols) {
#   mkt <- get(symbol)
#   mkt <- TimeOfDaySubset(mkt, open.timespan)
#   idx <- index(mkt)
#   days <- format(unique(as.Date(idx)), format="%Y-%m-%d")
#   for (day in days) {
#     daydata <- mkt[day]
#     assign(symbol, daydata)
#     png(paste(symbol, day, "png", sep="."))
#     sink(paste(symbol, day, "txt", sep="."))
#     trade(symbol, day)
#     sink()
#     dev.off()
#   }
#   rm(symbol)
# }
# 
# sink()