library(rjags)
library(geoR)

data("gambia")


y<- c(2, 15, 14, 16, 18, 22, 28)
x<- c(29.9,1761, 1807, 2984, 3230, 5040, 5654)
n<- length(y)


data   <- list(Y=y,X=x,n=n)


model_string <- textConnection("model{
   for(i in 1:n){
    Y[i]~ dgamma((a*mu[i]*mu[i]),(a*mu[i]))
    logit(mu[i]) <- inprod(X[i],beta)

   }
   beta ~ dnorm(0,0.01)
   a ~ dgamma(0.1, 0.1)
}")


model <- jags.model(model_string,data = data, n.chains=2 ,quiet=TRUE)



update(model, 10000, progress.bar="none")


params  <- c("a", "beta")
samples <- coda.samples(model, variable.names=params, n.iter=25000, progress.bar="none")

summary(samples)

plot(samples)

# Low ESS indicates poor convergence, size sample apperas to be large
effectiveSize(samples)

# R greater than 1.1 indicates poor convergence 
gelman.diag(samples)

sub<- samples[[1]]

plot(density(y))
lines(density(sub[,2]), col = "red")


#question 7
#part a



y<- gambia$pos

x<- as.matrix(gambia[-3])

data <- list(n=nrow(x),p=ncol(x),Y=y,X=x)


model_string <- textConnection("model{

   # Likelihood
   for(i in 1:n){
     Y[i] ~ dbern(pr[i])
     logit(pr[i]) =  inprod(X[i,],beta[])
   }

   # Priors
   for(j in 1:p){beta[j] ~  dnorm(0, 0.01)}
 }")



model <- jags.model(model_string,data = data, n.chains=2 ,quiet=TRUE)



update(model, 10000, progress.bar="none")


params  <- c("beta")
samples <- coda.samples(model, variable.names=params, n.iter=25000, progress.bar="none")

summary(samples)

plot(samples)

# Low ESS indicates poor convergence, size sample apperas to be large
effectiveSize(samples)

# R greater than 1.1 indicates poor convergence 
gelman.diag(samples)

sub<- samples[[1]]


#part b
gam<- gambia

y<- gam$pos

x<- as.matrix(gam[-3])

a<- 0
b<- 0
id<- 0

r<- 65
tag<- rep(0, r)
x_<- rep(0, r)
y_<- rep(0, r)

#creating id of all the various locations 1-65
for(i in 1:nrow(x)){
  if(x[i,1] != a && x[i,2] != b){
    id= id + 1
    x_[id]= x[i,1]
    y_[id]=x[i,2]
  }
  tag[i]= id
  a= x[i,1]
  b= x[i,2]
}



data <- list(n=nrow(x),p=ncol(x),Y=y,X=x, r= r, tag = tag)


model_string <- textConnection("model{
                               
 # Likelihood
 for(i in 1:n){
  Y[i] ~ dbern(pr[i])
  logit(pr[i]) =  inprod(X[i,],beta[]) + re[tag[i]]
 }
 
 # Priors
  for(j in 1:p){
    beta[j] ~  dnorm(0, 0.01)
  }
  for(j in 1:r){
    re[j] ~  dnorm(0, tau1)
  }
   tau1 ~ dgamma(0.01,0.01)
 }")



model <- jags.model(model_string,data = data, n.chains=2 ,quiet=TRUE)



update(model, 10000, progress.bar="none")


params  <- c("beta", "re")
samples <- coda.samples(model, variable.names=params, n.iter=25000, progress.bar="none")

summary(samples)
su<- summary(samples)

# Low ESS indicates poor convergence, size sample apperas to be large
effectiveSize(samples)

# R greater than 1.1 indicates poor convergence 
gelman.diag(samples)

#mean of random effects
re<- (su$statistics)

re_<- re[8:nrow(re),1]

#plot(x= x_, y =y_, pch = 16, col = "red" )
plot(x_, y_, type='n', ylab = "Y", xlab= "X", main = "Posterior of mean based on spartial location")
text(x_, y_, label = 1:65, cex=0.6)









