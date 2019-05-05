library("MASS")
library("rjags")

data(Boston)

#View(Boston)


y<- Boston$medv

x<- Boston[1:13]


x<- as.matrix(x)


n <- length(y)
p <- ncol(x)

data   <- list(Y=y,X=x,n=n,p=p)


model_string <- textConnection("model{
   # Likelihood
     for(i in 1:n){
          Y[i] ~ dnorm(alpha+inprod(X[i,],beta[]),taue)
     }
     # Priors
     for(j in 1:p){
          beta[j] ~ dnorm(0,0.001)
     }
     alpha ~ dnorm(0,0.001)
     taue  ~ dgamma(0.1, 0.1)
}")


model <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE)


update(model, 10000, progress.bar="none")


params <- c("beta", "alpha")
samples <- coda.samples(model,variable.names=params,n.iter=20000,thin=10,  progress.bar="none")

summary(samples)

# plots
plot(samples)

# Low ESS indicates poor convergence, beta10 and the intercept have low sample size
effectiveSize(samples)

# R greater than 1.1 indicates poor convergence, therefore we have good convergence.
gelman.diag(samples)

#b

#normal liner model
lsModel<- lm(medv~., data= Boston)
summary(lsModel)
confint(lsModel)


#c

model_string <- textConnection("model{
   # Likelihood
   for(i in 1:n){
      Y[i] ~ dnorm(alpha+inprod(X[i,],beta[]),taue)
   }
   # Priors
   for(j in 1:p){
      beta[j] ~ dnorm(0,taue * taub)
   }
   alpha ~ dnorm(0,0.001)
   taue  ~ dgamma(0.1, 0.1)
   taub  ~ dgamma(0.1, 0.1)
}")


model <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE)


update(model, 10000, progress.bar="none")


params <- c("beta", "alpha")
samples <- coda.samples(model,variable.names=params,n.iter=20000,thin=10,  progress.bar="none")

summary(samples)



#d

##data
#taking the frist 500 rows 
y<- Boston$medv[1:500]

#taking the covar, 500 rows
x<- (Boston[1:13])[1:500,]

##ppd data to be passed to JAGS
yPp<- y[495:500]
xPp<- x[495:500, ]
#scaling matrix
X_<- as.matrix(xPp)


#obs Data to be passed to JAGS
Y<- y[1:494]
xob<- x[1:494, ]
#scaling 
X<- as.matrix(xob)


# of obs in obs matrix
n <- length(yob)
p <- ncol(xob)

# of obs in predi matrix
n_<- length(yPp)

data   <- list(Y=Y,X=X,n=n,p=p, n_= n_, X_= X_)

#jags model
model_string <- textConnection("model{
   # Likelihood
   for(i in 1:n){
    Y[i] ~ dnorm(alpha+inprod(X[i,],beta[]),taue)
   }

   # Priors
   for(j in 1:p){
    beta[j] ~ dnorm(0,0.001)
   
   }
   
   alpha ~ dnorm(0,0.001)
   taue  ~ dgamma(0.1, 0.1)
    
  #prediction
   for(i in 1:n_){
    Y_[i] ~ dnorm(alpha+inprod(X_[i,],beta[]),taue)
   }

}")

                              


model <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE)
update(model, 10000, progress.bar="none")
params <- c("Y_")


samples <- coda.samples(model,variable.names=params,n.iter=20000,thin=10,  progress.bar="none")

preM<- summary(samples)

pMean<- preM$statistics[,1]

sub<- samples[[1]]

#plot the ppd 
for(i in 1:length(yPp)){
  plot(density(sub[,i]),xlab="Y",main=paste("PPD", i))
  #true means
  abline(v = yPp[i], col= "red")
  abline(v= pMean[i], col = "blue")
  legend("topright", legend = c("True", "Predicted"),col=c("red", "blue"), lty=c(1,1), bty = "n")
}





