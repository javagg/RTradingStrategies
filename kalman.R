require(KFAS)
require(sspir)
require(FKF)
require(dlm)

data(Nile)

y <- Nile
model <- structSSM(y=y)
fit <- fitSSM(inits=c(0.5*log(var(y)), 0.5*log(var(y))), model=model)
kfs <- KFS(fit$model, smoothing="state")