
#question 3

set.seed(6)

b<- seq(0,1, by = .0001)

a_ <- 0
b_ <- 0

for(i in 1:length(b)){
  a = (100*b[i]^2)
  med = qgamma(.5,a,b[i])
  if(med%>%round(0) == 75){
      a_= a;b_ = b[i]
      break
  }
}

#condition 1

var<- a_/b_^2
var

#condition 2

med_<- rgamma(10000, a_, b_)%>%median()
med_%>%round()





#question 4

c= c(1,1,2,2)
a<- c(.1,1,.1,1)

data<- matrix(.0,nrow = 4, ncol = 9)

data[,1]<- c
data[,2]<- a
data[,3]<- a

sse <- function(){
  x2=15;x=-2;n= 20
  ((1/(n-1))* x2- (x/n)^2)%>%return()
}

mean<- function(a, b){
  n = 20
  ((sse() + b)/(n + 2*a - 2))%>%return()
}



#calcuating the std
for(i in 1:nrow(data)){
  data[i,4]= 20/2 + data[i,3]
  data[i,5] = (data[i,3]  + sse())
  #P(sigma>c)
  data[i,6] = 1- pinvgamma(data[i,1], data[i,4], data[i,5])
  #mean
  data[i,7] = mean(data[i,3], data[i,3])
  #std
  data[i,8] = (sse()/(20-1))^.5
  #adding random a and b to see what effect the prior had on the posterior
  data[i,9] = mean(10, 10)
  
}

df2<- as.data.frame(data)

names(df2)<- c("c", "a", "b", "A", "B",  "postProb", "mean", "std", "priorTest")

df2

#P(sigma>c | c= 1, a=b=0.1) / P(sigma>c | c=1, a=b=1)

data[1,6]/data[2,6]

#P(sigma>c | c= 2, a=b=0.1) / P(sigma>c | c=1, a=b=1)

data[3,6]/data[4,6]










