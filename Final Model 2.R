
library(rmeta)
library(rjags)

data(cochrane)
cochrane


# et<- rt
# nt<-  #nt
# ec<- #rc
# nc<- #nc
# 
# # #text book
# ec<- c(3	,10	,40,	5	,5,	2	,19	,59	,5,	16,	8)
# nc<- c(55	,94	,573,	75	,69	,61,	419,	782	,81	,226,	66)
# 
# et<- c(1,	3	,32,	5,	3,	3	,20,	52,	2	,12,	6)
# nt<- c(55,	95,	565,	75,	71,	62,	421,	790	,81	,225,	71)

##web page
# et = c(3, 7, 5, 102, 28, 4, 98, 60, 25, 138, 64, 45, 9, 57, 25, 33, 28, 8, 6, 32, 27, 22 )
# nt = c(38, 114, 69, 1533, 355, 59, 945, 632, 278,1916, 873, 263, 291, 858, 154, 207, 251, 151, 174, 209, 391, 680)
# ec = c(3, 14, 11, 127, 27, 6, 152, 48, 37, 188, 52, 47, 16, 45, 31, 38, 12, 6, 3, 40, 43, 39)
# nc = c(39, 116, 93, 1520, 365, 52, 939, 471, 282, 1921, 583, 266, 293, 883, 147, 213, 122, 154, 134, 218, 364, 674)

#hw
et<- cochrane$ev.trt # rt
et
nt<- cochrane$n.trt #nt
nt

ec<-cochrane$ev.ctrl #rc
ec
nc<-cochrane$n.ctrl #nc
nc



#function to run jags method
model<-function(et, nt, ec, nc){
  n<-length(et)
  data <- list(n=n, y1= et, n1 = nt, y0= ec, n0= nc)
  
  model_string <- textConnection("model{
                                 
 # Likelihood
   for(i in 1:n){
      y0[i]~dbin(c[i], n0[i])
      y1[i]~dbin(t[i], n1[i])
   
      logit(c[i])<- mu[i]
      logit(t[i])<- mu[i] + delta[i]
   
      mu[i]~dnorm(0, .0001)
      delta[i]~dnorm(d,taub)
   }
   
   # Priors
   d ~ dnorm(0,0.0001)
   taub <- 1/(sigma*sigma)
   sigma~ dunif(0,2)
   or <- exp(d)  
                                 
}")
  
  model <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE) 
  update(model, 10000, progress.bar="none") 
  params <- c( "d", "sigma", "or") 
  samples <- coda.samples(model,variable.names=params,n.iter=20000,thin=5, progress.bar="none")
  
  return(samples)
}



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


# number of simulations 
set.seed(1)

nsims<- 1000

#hold 

median<- rep(0, nsims)

means <- rep(0, nsims)

count = 0

for(i in 1:nsims){
  r= random(nt,nc)
  
  su= summary(model(r[,1], nt, r[,2], nc))
  
  means[i] = su$statistics[1,1]

  median[i]= su$quantiles[1,3]
  
  low= su$quantiles[2,1]
  
  upper= su$quantiles[2,5]
  
  #type 1 error check 
  if(low > 0 || upper < 0 ){
    count = count + 1
  }
}

count

#9 where 0 was not in the interval

su<- model(et, nt, ec, nc)
line<- summary(su)
sum<- su[[1]]

line$quantiles
title<- c("Pooled log odds ratio(d)", "Pooled odds ratio(od)")
ma <- c("Posterior of log odds ratio(d)", "Posterior of odds ratio(od)")

op <- par(pty="m", mfrow=c(2, 1), mar=c(4, 4, 2, 2))
for(i in 1: 2){
  d= density(sum[,i])
  plot(d,xlab = title[i], main= ma[i])
  polygon(d, col="wheat")
  # median
  abline(v= line$quantiles[i,3], col = "red", lwd = 3)
  # ci low
  abline(v= line$quantiles[i,1], col = "blue")
  # high
  abline(v= line$quantiles[i,5], col = "green")
  # mean
  abline(v= line$statistics[i,1] ,col = "orange", lwd = 3)
  legend("topright", legend = c("low CI", "Upper CI", "Mean", "Median"),col=c("blue", "green", "red", "orange"), lwd=c(1,1,3,3), bty = "n")

}

summary(model(et, nt, ec, nc))


plot(su)

gelman.diag(su)

effectiveSize(su)
