library(survival)
library(MASS)

fitwei <- survreg(Surv(time, cens) ~ treat, gehan, dist='weibull')
X <- model.matrix(~ treat,data=gehan)
llgamma <- function(par,y,d,X){
  a <- exp(par[1])
  beta <- par[-1]
  b <- exp(X%*%beta)
  -sum(d*dgamma(y,a,b,log=TRUE)+
    (1-d)*pgamma(y,a,b,log.p =TRUE,lower.tail = FALSE))
}
X
opt <- optim(numeric(3),llgamma,y=gehan$time,d=gehan$cens,X=X,control = list(maxit=5000))

logLik(fitwei)
exp(opt$par[1])
