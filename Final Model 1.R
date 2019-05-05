#install.packages("rmeta")

library(rmeta)
library(rjags)

data(cochrane)
cochrane

model.FE <- meta.MH(n.trt,n.ctrl,ev.trt,ev.ctrl, names=name,data=cochrane)

et<- cochrane$ev.trt # rt
et
nt<- cochrane$n.trt #nt
nt
  
ec<-cochrane$ev.ctrl #rc
ec
nc<-cochrane$n.ctrl #nc
nc

#simulation of data
#function to generate random numbers 
random<- function(nt, nc){
  n<- length(nt)
  t<-rep(NA, n)
  c<-rep(NA, n)
  for( i in 1:n ){
    
    t[i]= rbinom(1,nt[i], .1)
    c[i]= rbinom(1,nc[i], .1)
  }
    return(cbind(t,c))
}


# et<- rt
# nt<-  #nt
# ec<- #rc
# nc<- #nc

#function to run jags method
model<-function(et, nt, ec, nc){
  n<-7
  data <- list(n=n, y0= et, x0 = nt, y1= ec, x1= nc)
  
  model_string <- textConnection("model{
                                 
   # Likelihood
   for(i in 1:n){
    y1[i]~dbin(c[i], x1[i])
    y0[i]~dbin(t[i], x0[i])
    
    logit(c[i])<- mu[i]
    logit(t[i])<- mu[i] + delta[i]
    
    mu[i]~dnorm(0, .001)
    delta[i]~dt(alpha,taub, 4)
    
   }
   
   # Priors
     alpha ~ dnorm(0,0.001)
     taub ~ dgamma(0.1, 0.1)
     deltaN~ dt(alpha,taub, 4) 
     sigma<- 1/sqrt(taub)

   }")
  
  model <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE) 
  update(model, 1000, progress.bar="none") 
  params <- c("alpha", "deltaN", "sigma") 
  samples <- coda.samples(model,variable.names=params,n.iter=5000,thin=5, progress.bar="none")
  
  return(samples)
}


sam<- model(et, nt, ec, nc)

plot(sam)
summary(sam)

#plot(samples)

gelman.diag(sam)

effectiveSize(sam)



for( n in 1:1000){
  r<- random(nt,nc)
  sam<- model(r[,1], nt, r[,2], nc)
}


