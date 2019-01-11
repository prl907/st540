library(ggplot2)



#import file from desktop
ozone<- read.csv("C:/Users/prl90/Documents/st540/ozone.csv", header=T)

r <- nrow(ozone)
c <- ncol(ozone)


ozoneR<- ozone[2:ncol(ozone)]


data<- rep(NA,nrow(ozoneR) * ncol(ozoneR) )
count = 1;
for(i in 1:nrow(ozoneR)){
  for(j in 1:ncol(ozoneR)){
    data[count] = ozoneR[i,j]
    count = count + 1;
    
  }
}
data<- unlist(ozoneR)
#statistics
mean(data, na.rm = T)
sd(data, na.rm = T)
sum(1 * is.na(data))/length(data)


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



walk(list(mPlot, sPlot, pPlot), hist)



