current.dir <- dirname(parent.frame(2)$ofile)

require(FinancialInstrument)

getSymbols("F", src="RData", dir=current.dir)

F.close.ts <- as.ts(getPrice(F))
F.model <- structSSM(y=F.close.ts)
F.fit<- fitSSM(inits=c(0.5*log(var(F.close.ts)),0.5*log(var(F.close.ts))), model=F.model)

F.kfs <- KFS(F.fit$model, smoothing="state")
plot(F.kfs, col=1:2)

lows <- c(F.kfs$alphahat-qnorm(0.95)*sqrt(c(F.kfs$V)))
ups <- c(F.kfs$alphahat+qnorm(0.95)*sqrt(c(F.kfs$V)))
plot.ts(cbind(F.close.ts, c(F.kfs$alphahat), lows, ups), plot.type="single", col=c(1:2,3,3),  ylab="Predicted Annual flow", main="River Nile")

st.m <- matrix(c(3, 1, 0, -3, 0, 1, 1, 0, 0), nrow=3, ncol=3)
