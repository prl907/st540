library(ggplot2)



#import file from desktop
ozone<- read.csv("C:/Users/prl90/Documents/ST540/ozone.csv", header=T, na.rm = T)

r <- nrow(ozone)
c <- ncol(ozone)

str(ozone)

ozoneR[1,1]


ozoneR<- ozone[2:ncol(ozone)]
data<- rep(NA,nrow(ozoneR) * ncol(ozoneR) )
count = 1;
for(i in 1:nrow(ozoneR)){
  for(j in 1:ncol(ozoneR)){
    data[count] = ozoneR[i,j]
    count = count + 1;
    
  }
}

mean(data, na.rm = T)
std<- apply(data,2,function(x)sd(x, na.rm = T))
pMiss<- apply(data, 2, function(x)(sum(1 * is.na(x))/length(data)))


#part b

#value extracted from the columns storeage vector

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



