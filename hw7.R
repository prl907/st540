

library(invgamma)

Y<- c(1:10)

n<- length(Y)

S<- 25000

samples<- matrix(NA, nrow = S, ncol = 11)
colnames(samples)<- c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "B")



sigma <- 1
B  <- 1


a<- .1
b<- .1

for(s in 1:S){
  for(i in 1:n){
    sigma[i] <- 1/(rgamma(1,.5 + a, (Y[i]^2 + 2* b)/2))
  }
  
  B <- rgamma(1,n*a + 1, 1+sum(1/sigma)) 
  samples[s,] <- c(sigma, B)
}


# getting the inverse gamma rate and shape
para<- function(x){
  m = mean(x)
  v = var(x)
  a = (m^2/v) + 2
  b = m * (a- 1)
  return(c(a, b))
}

op <- par(pty="m", mfrow=c(5, 2), mar=c(4.2, 4.2, 1, 1))
for(i in 1:10){
  temp=samples[,i]
  values= temp[temp<200]
  hist(values, freq = F, col = "Wheat", xlab =paste("sigma", i), main =  paste("Histogram of sigma", i))
  x= seq(0, 200, length = length(values))
  
  curve(dinvgamma(x,para(values)[1],para(values)[2]), add = TRUE, col = "red");
}
par(op)

hist(samples[,11], freq = F, col = "Wheat", main = "Histogram of b")






