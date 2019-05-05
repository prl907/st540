
library(ggplot2)



#import file from desktop
ozone<- read.csv("C:/Users/prl90/Documents/ST540/ozone.csv", header=T)

r <- nrow(ozone)
c <- ncol(ozone)

str(ozone)

ozoneR<- ozone[2:ncol(ozone)]


#part a
mean<- apply(ozoneR,2,function(x)mean(x, na.rm = T))
std<- apply(ozoneR,2,function(x)sd(x, na.rm = T))
pMiss<- apply(ozoneR, 2, function(x)(sum(1 * is.na(x))/nrow(ozoneR)))

table<- data.frame(mean = mean , std = std, pMiss = pMiss)



#part b
#using the loop approach to get values
#value extracted from the columns 

mPlot<- rep(0, r)
sPlot<- rep(0, r)
pPlot<- rep(0,r)

for(i in 1:r){
  v=rep(0, c-1)
  for(j in 2:c){
    v[j - 1] = ozone[i,j]
    if(j == c){
      mPlot[i] = ifelse(is.nan(mean(v, na.rm = T)),0,mean(v, na.rm = T) )
      sPlot[i] = ifelse(is.nan(sd(v, na.rm = T)) ,0,sd(v, na.rm = T)) 
      pPlot[i] = (sum(1 * is.na(v))/c)
    }
  }
}



df<- map(list(mPlot,sPlot, pPlot), ~data.frame(x= .x))
dfC<- list("green", "red", "blue")

map2(df,dfC, ~(ggplot()+ geom_histogram(data = ..1, bins = 5,aes(x = ..1$x),  fill = ..2 )))

