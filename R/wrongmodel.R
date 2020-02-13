MLEnorm_sim <- function(i,n){
  x <- rgamma(n,4,1)
  muhat <- mean(x)
  n <- length(x)
  s2hat <-  n*var(x)/(n-1)
  c(muhat,s2hat)
}

library(parallel)
library(vioplot)
res10 <- t(simplify2array(mclapply(1:10000,MLEnorm_sim,n=10,
                                   mc.cores = detectCores())))
res100 <- t(simplify2array(mclapply(1:10000,MLEnorm_sim,n=100,
                                    mc.cores = detectCores())))
res1000 <- t(simplify2array(mclapply(1:10000,MLEnorm_sim,n=1000,
                                     mc.cores = detectCores())))
setwd("~/projects/mystat")
pdf("./img/wrongmodel_mu.pdf")
vioplot(data.frame(res10[,1],res100[,1],res1000[,1]),
        main=expression(hat(mu)),
        names = c("n=10","n=100","n=1000"),col="white",lwd=2)
abline(h=4,lty=2)
dev.off()

pdf("./img/wrongmodel_sigma2.pdf")
vioplot(data.frame(res10[,2],res100[,2],res1000[,2]),
        main=expression(hat(sigma^2)),
        names = c("n=10","n=100","n=1000"),col="white",lwd=2)
abline(h=4,lty=2)
dev.off()
