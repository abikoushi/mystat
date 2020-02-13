library(readr)
dat <- read_csv("~/projects/mystat/data/dat_7_4.csv")

mind <- as.Date("2013/1/1")
maxd <- as.Date("2015/11/1")
t=as.numeric(dat$cvdate-mind)
loglik <- function(par,t,maxT){
  a <- exp(par[1])
  b <- exp(par[2])
  lambda <- log(a)+dexp(t,b,log=TRUE)
  Lambda <- a*pexp(maxT,b)
  sum(lambda) - Lambda
}

dloglik <- function(par,t,maxT){
  a <- exp(par[1])
  b <-exp(par[2])
  da <- length(t)-a*pexp(maxT,b)
  db <- sum(1 - t*b) +
    a*exp(-b*maxT)*(b*maxT)
  c(da,db)
}

numDeriv::grad(loglik,c(0.1,4),t=t,maxT=as.numeric(maxd-mind))
dloglik(c(0.1,4),t=t,maxT=as.numeric(maxd-mind))

curve(pexp(x),0,10)

fit1 <-optim(c(3,0),
             loglik,
             t=t,
             maxT=as.numeric(maxd-mind),
             control = list(fnscale=-1))
fit1
plot(t,1:length(t),xlab="date",ylab = "cumulative CV",xaxt="n",type="s",lwd=2)
curve(exp(fit1$par[1])*pexp(x,exp(fit1$par[2])),add=TRUE,col="royalblue",lwd=2)
axis(1, at = t, labels=dat$cvdate)

loglik <- function(par,t,maxT){
  a <- exp(par[1])
  m <-exp(par[2])
  eta <- exp(par[3])
  lambda <- log(a)+dweibull(t,m,eta,log=TRUE)
  Lambda <- a*pweibull(maxT,m,eta)
  sum(lambda) - Lambda
}
dloglik <- function(par,t,maxT){
  a <- exp(par[1])
  m <-exp(par[2])
  eta <- exp(par[3])
  da <- length(t)-a*pweibull(maxT,m,eta)
  dm <- sum(1+m*(log(t)-log(eta))-(m*(t/eta)^(m))*(log(t)-log(eta))) +
    a*m*exp(-(maxT/eta)^m)*log(maxT/eta)*(maxT/eta)^m
  deta <- sum(m + m*(t/eta)^m) +
    a*m*exp(-(maxT/eta)^m)*(maxT/eta)^m
  c(da,dm,deta)
}
numDeriv::grad(loglik,c(0.1,4,2),t=t,maxT=as.numeric(maxd-mind))
dloglik(c(0.1,4,2),t=t,maxT=as.numeric(maxd-mind))

fit1 <-optim(c(1,0,0),
             loglik,
             t=t,
             maxT=as.numeric(maxd-mind),
             control = list(fnscale=-1))
fit1$par
plot(t,1:length(t),xlab="date",ylab = "cumulative CV",xaxt="n",type="s",lwd=2)
curve(exp(fit1$par[1])*pweibull(x,exp(fit1$par[2]),exp(fit1$par[3])),add=TRUE,col="royalblue",lwd=2)
axis(1, at = t, labels=dat$cvdate)

