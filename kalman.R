current.dir <- dirname(parent.frame(2)$ofile)

require(dse)
require(KFAS)
require(sspir)
require(FKF)
require(dlm)
require(quantmod)

source(file.path(current.dir, "debug.R"))

getSymbols("F", src="RData", col.names=c("High","Low","Close","Bid", "Ask", "Volume"))

F.close <- getPrice(F)
F.close.lag1 <- lag(F.close, k=1)
F.close.lag2 <- lag(F.close, k=2)

state <- cbind(F.close, F.close.lag1, F.close.lag2)
state <- state[-1*1:2,]

set.seed(2)
state <- rnorm(300)
dim(state)<-c(100,3)
# 
FF <- matrix(c(1, 0, 0), nrow=1)
GG <- matrix(c(3,-3, 0, 1, 0, 0, 0, 1, 0), nrow=3, byrow=T)
V <- matrix(0.1)
W <- matrix(1, nrow=3, ncol=3)
m0 <- c(0,0,0)
C0 <- matrix(1, nrow=3, ncol=3)

model <- dlm(FF=FF, V=V, GG=GG, W=W, m0=m0, C0=C0)

F.close <- as.matrix(F.close)
filtered <- dlmFilter(F.close, model)
smoothed <- dlmSmooth(filtered)

plot(F.close, type="b")
lines(dropFirst(filtered$m)[,1], lty="longdash")

# data(Nile)
# 
# y <- Nile
# model <- structSSM(y=y)
# fit <- fitSSM(inits=c(0.5*log(var(y)), 0.5*log(var(y))), model=model)
# kfs <- KFS(fit$model, smoothing="state")
# 
# m1b.dse <- dse::SS(F=matrix(1, 1, 1), Q=matrix(40, 1, 1), H=matrix(1, 1, 1), R=matrix(130, 1, 1), constants=list(Q=matrix(TRUE, 1, 1), P0=matrix(TRUE, 1, 1)), z0=matrix(0, 1, 1), P0=matrix(10^5, 1, 1))
# m1b.dse.est <- estMaxLik(m1b.dse, TSdata(output=Nile))
# 
# m1.sspir <- sspir::SS(Fmat=function(tt, x, phi) { return(matrix(1)) }, Gmat=function(tt, x, phi) { return(matrix(1)) }, Vmat = function(tt, x, phi) { return(matrix(exp(phi[1]))) }, Wmat=function(tt, x, phi) { return(matrix(exp(phi[2]))) }, y = as.matrix(Nile, ncol = 1))
# str(m1.sspir)
# 
# C0(m1.sspir) <- matrix(10^7, 1, 1)
# phi(m1.sspir) <- c(9, 7)
# m1.sspir.f <- kfilter(m1.sspir)
# 
# mod2.sspir <- ssm(Nile~tvar(1), family="gaussian", C0=diag(1)*10^7, phi=exp(c(9, 7)))
# # class(mod2.sspir)
# # 
# m1.dlm <- dlm(FF=1, V=0.8, GG=1, W=0.1, m0=0, C0=100)
# # 
# #m2.dlm <- dlm(FF=matrix(c(1, 0, 0), 1, 3), JFF=matrix(c(0, 1, 2), 1, 3), GG=diag(3), W=0.1*diag(3), V=4, m0=c(0, 0, 0), C0=10^9*diag(3), X=X)
# # 
# m3.dlm <- dlmModARMA(ar=c(0.3, 0.2, 0.4), ma=c(0.9, 0.1))
# 
# m4.dlm <- dlmModPoly(order = 1) + dlmModSeas(4)
# m3Plusm4.dlm <- m4.dlm %+% m3.dlm
# GG(m3Plusm4.dlm)
# FF(m3Plusm4.dlm)
# 
# 
# fit <- StructTS(Nile, type="level")


