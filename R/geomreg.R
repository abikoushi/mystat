geomreg <- function(formula,weights,data,method="BFGS",control=NULL){
  mf <-model.frame(formula = formula, data = data)
  y <-model.extract(mf,"response")
  X <-model.matrix(formula,data = data)
  w <-data[,weights]
  ll <- function(beta,y,X,w){
    logprob <- plogis(X %*% beta,log.p = TRUE)
    logprob2 <- plogis(-X %*% beta,log.p = TRUE)
    -sum(w*((y-1)*logprob2+logprob))
  }
  dll <- function(beta,y,X,w){
    prob <- plogis(X %*% beta)
    prob2 <- plogis(-X %*% beta)
    -t(w*(-(y-1)*prob+prob2))%*%X
  }
  ini <-numeric(ncol(X))
  names(ini) <- colnames(X)
  optim(ini,fn = ll, gr=dll,
        method = method,
        control = control,
        y=y,X=X,w=w,hessian = TRUE)
}

library(tidyverse)
# library(xtable)
# setwd("~/projects/mystat")
gadata <- read_csv("./data/dat_7_1.csv") 
# xtable(head(gadata),digits = 0)
fit <-geomreg(pageDepth~userGender+userAgeBracket+userType,weights = "sessions",data = gadata)

gadata_fit <-gadata %>% 
  unite(x,c("userGender","userAgeBracket","userType")) %>% 
  spread(pageDepth,sessions,fill=0) %>% 
  gather(pageDepth,sessions,-x) %>% 
  separate(x,c("userGender","userAgeBracket","userType"),sep = "_") %>% 
  mutate(pageDepth=as.integer(pageDepth)) %>% 
  group_by(userGender,userAgeBracket,userType) %>% 
  mutate(density=sessions/sum(sessions))

X <- model.matrix(~userGender+userAgeBracket+userType,data = gadata_fit)

fitted_prob <- dgeom(gadata_fit$pageDepth-1,plogis(X %*% fit$par))

p1 <- ggplot(gadata_fit,aes(x=pageDepth,y=density))+
  geom_col(fill="gray")+
  geom_point(aes(y=fitted_prob))+
  facet_wrap(~userGender+userAgeBracket+userType)+
  theme_classic()

ggsave(p1,filename = "./img/fitgeomreg.pdf")

dfcoef<-data.frame(beta=fit$par,se=sqrt(diag(solve(fit$hessian)))) %>% 
  rownames_to_column()

p2 <- ggplot(dfcoef,aes(x=rowname,y=beta,ymin=beta-2*se,ymax=beta+2*se))+
  geom_pointrange()+
  geom_hline(yintercept = 0,linetype=2)+
  xlab("")+
  coord_flip()+
  theme_classic(20)+
  theme(axis.text = element_text(colour = "black"))
  
ggsave(p2,filename = "./img/coefgeomreg.pdf")

