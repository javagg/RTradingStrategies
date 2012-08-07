fields <- c("Stkcd","Trddt", "Opnprc", "Hiprc", "Loprc", "Clsprc", "Dnshrtrd")
colClasses <- c("character","Date", "numeric", "numeric", "numeric", "numeric", "numeric")
data.dir <- "~/Downloads/data"
zipfiles <- list.files(data.dir, full.names=T)

raw.all <- NULL
for (zipfile in zipfiles) {
  files <- unzip(zipfile)
  filename <- grep(".csv$", files, value=T)
  rawdata <- read.table(filename, header=T, colClasses=colClasses, fileEncoding="UCS-2LE")
  if (is.null(raw.all)) {
    raw.all <- rawdata
  } else { 
    raw.all <- rbind(raw.all, rawdata)
  }
  print(head(rawdata))
}

