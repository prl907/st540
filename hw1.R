library(ggplot2)
library(purrr)
library(tidyr)



#import file from desktop
ozone<- read.csv("C:/Users/prl90/Documents/st540/ozone.csv", header=T)


#part a

#flatten data frame
data<- ozone[2:ncol(ozone)]%>%unlist

#statistics
mean(data, na.rm = T)
sd(data, na.rm = T)
sum(1 * is.na(data))/length(data)


#part b

#value extracted from the columns storage vector
r <- nrow(ozone)
c <- ncol(ozone)

#mplot = vector for mean, sPlot = vector for ds, pPlot = percent missing
mPlot<- rep(0, r); sPlot<- rep(0, r); pPlot<- rep(0,r)

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

#plotting the histograms
list(list(mPlot, sPlot, pPlot),list("red", "green", "blue"),list("mean", "sd", "% missing"))%>%
pwalk(~hist(x = ..1, col =..2, xlab = ..3 ))

#linear regression model with mean as response
model<- lm(mPlot~sPlot + pPlot)
summary(model)


