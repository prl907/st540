load("C:/Users/prl90/Desktop/ST540/E2data.RData")

library(rjags)

da<- matrix(nrow = 50, ncol = 61)

#names for dataset

name<- rep("", 61)
name[1] = "Y"
count = 2
for(i in 1:10){
  for(j in 1:6){
    name[count] = paste("m", "_", j,"_", "l", "_",i, sep = "")
    count = count + 1
  }
}

colnames(da)<- name

count = 2
for(i in 1:50){
  #data[i, 1] = Y[i]
  da[i, 1] = Y[i]
  for(j in 1:10){
    for(k in 1:6){
        da[i, count] = X[k,j,i]
      count = 1 + count
    }
  }
  count = 2
}


#data
dat<-scale(da[,-1])
x<- dat[1:44, -1]
y<- da[1:44, 1]

n <- length(y) 
p <- ncol(x)

#ppd

x_<- dat[45:50, -1]
y_<- da[45:50, 1]

n_ <- length(y_) 


data <- list(n=n,p=p,Y=y,X=x, n_ = n_, X_= x_)


model_string <- textConnection("model{
                               
 # Likelihood
 for(i in 1:n){
  Y[i] ~ dpois(pr[i])
  log(pr[i]) =  alpha +inprod(X[i,],beta[]) 
 }
 
 # Priors
 for(j in 1:p){
  beta[j] ~  dnorm(0, taue * taub)
 }

   alpha ~ dnorm(0,0.001)
   taue ~ dgamma(0.1, 0.1) 
   taub ~ dgamma(0.1, 0.1)
  
 # Predictions
   for(i in 1:n_){
    Y_[i] ~ dpois(pr_[i])
    log(pr_[i]) =  alpha +inprod(X_[i,],beta[]) 
 }

 }")

model <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE) 
update(model, 10000, progress.bar="none") 
params <- c("beta", "Y_") 
samples <- coda.samples(model,variable.names=params,n.iter=20000,thin=10, progress.bar="none")

DIC<- dic.samples(model,n.iter=20000,n.thin = 10, progress.bar="none")


summary(samples)

plot(samples)

gelman.diag(samples)

effectiveSize(samples)

sub<- samples[[1]]

plot(density(sub[,2]),xlab="Y", xlim = c(0, 20))
abline(v = 15, col= "red")

