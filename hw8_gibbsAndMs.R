
#set.seed(1)
#  overall proportions 
q<- c(.845, .847, .880, .674, .909, .898, .770, .801, .802, .875)
# number of made
Y<- c(64, 72, 55, 27, 75, 24, 28, 66, 40, 13)
# number of attempts
n<- c(75,95,63, 39, 83, 26, 41, 82, 54, 16)

N<-length(Y)

S<- 25000
#set intial values 

theta<- .5

m<- .03


log_post<- function(theta, n, q, m, Y){
  like= sum(dbinom(Y, size = n,theta, log = T ))
  a = q * exp(m)
  b = (1-q)* exp(m)
  prior1= sum(dbeta(q, a,b, log = T))
  prior2= dnorm(m,0, 10, log = T)
  return(like + prior1 + prior2)
}


#matrix to hold results
samples<- matrix(NA, nrow = S, ncol = 11)
aRate<-rep(0, S)

can_sd<- 1

#tuning 
burn  <- 5000      # Length of burn-in period for tuning
check <- 100  # Iterations between checks of the acceptance rate     
att   <- 0  # Keep track of the number of MH attempts
acc   <- 0




for(i in 1: S){
  #metro sampling 
  can = rnorm(1,m, can_sd)
  logR   <- log_post(theta, n, q, can, Y)-log_post(theta, n, q, m, Y) 
  #record attemps
  att = att + 1
  if(log(runif(1))<logR){
    m <- can
    acc<- acc + 1
  }

  
    #tunning
  if(i<burn & att==check){
    if(acc/att<0.2){can_sd<-can_sd*0.8}
    if(acc/att>0.6){can_sd<-can_sd*1.2}
    acc <- att <- 0
  }

  #gibbs
  for( j in 1: N){
    a<- Y[j]+ q[j]* exp(m)
    b<- n[j] - Y[j] + exp(m) * (1-q[j])
    theta[j]= rbeta(1,a, b)
  }
  
  samples[i,]= c(m, theta)
}


for(i in 1:11){
  temp=samples[,i]
  print(mean(temp))
}

sd(samples[,1])



plot(samples[,1])