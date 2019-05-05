library(rjags)



#question 4 part c
#yi
Y<- c(1:10)

#n
n<- length(Y)

data<- list(Y= Y, n = n)

model_string <- textConnection("model{
   # Likelihood
    for(i in 1:n){
      Y[i] ~ dnorm(0, tau[i])
    }
   # Priors
    for(i in 1:n){
      tau[i] ~ dgamma(1,b)
      sigma[i]<- 1/tau[i]
    }

    b ~ dgamma(1,1)

 }")

inits <- list( b = .1)
model <- jags.model(model_string,data = data, inits=inits, n.chains=2 ,quiet=TRUE)



update(model, 10000, progress.bar="none")


params  <- c("sigma","b")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=25000, progress.bar="none")


summary(samples)


#Question 6

#  overall proportions 
q<- c(.845, .847, .880, .674, .909, .898, .770, .801, .802, .875)
# number of made
Y<- c(64, 72, 55, 27, 75, 24, 28, 66, 40, 13)
# number of attempts
n<- c(75,95,63, 39, 83, 26, 41, 82, 54, 16)

N<-length(Y)

data<- list(Y= Y, n = n, q = q , N=N)



model_string <- textConnection("model{
   # Likelihood
    for(i in 1:N){
      Y[i] ~ dbinom(theta[i], n[i] )
    }
   # Priors
    for(i in 1:N){
      theta[i] ~ dbeta(exp(m)* q[i], exp(m)* (1-q[i]))
    }

    m ~ dnorm(0,10)

 }")


model <- jags.model(model_string,data = data, n.chains=2 ,quiet=TRUE)

update(model, 10000, progress.bar="none")


params  <- c("theta","m")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=25000, progress.bar="none")


summary(samples)













