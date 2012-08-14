current.dir <- dirname(parent.frame(2)$ofile)

require(KFAS)
require(FKF)
require(dlm)
require(quantmod)

getSymbols("F", src="RData", dir=current.dir,  col.names=c("High","Low","Close","Bid", "Ask", "Volume"))

F.close <- getPrice(F)

state <- cbind(F.close, lag(F.close, k=1), lag(F.close, k=2))

# matrix(c(1,-3, 0, 1, 0, 0, 0, 1, 0), nrow=3, byrow=T)
m1.dlm <- dlm(FF=1, V=0.8, GG=matrix(c(1,-3, 0, 1, 0, 0, 0, 1, 0), nrow=3, byrow=T), W=diag(3), m0=0, C0=100)

