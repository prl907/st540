


player<- c("Russell Westbrook","James Harden", "Kawhi Leonard", "Lebron James", "Isaiah Thomas", "Stephen Curry", "Giannis Antetokounmpo", "John Wall", "Anthony Davis", "Kevin Durant")
overall<- c(.845, .847, .880, .674, .909, .898, .770, .801, .802, .875)
cMakes<- c(64, 72, 55, 27, 75, 24, 28, 66, 40, 13)
cAtt<- c(75,95,63, 39, 83, 26, 41, 82, 54, 16)
id<- c(1:10)

data<- list( "player" = player, "overall"= overall, "cMakes" = cMakes, "cAtt"= cAtt, "id" = id)



#b
#clutch

m<- seq(0,1, by = .01)

betaPost<-function( x,n){
  a= x +1
  b = n-x + 1
  return(cbind("x" = m, "y" = dbeta(m, a ,b )))
}

clutch<- pmap(data, ~betaPost( ..3, ..4))

op <- par(pty="m", mfrow=c(5, 2), mar=c(4.2, 4.2, 1, 1))

  for(i in 1:length(player)){
      plot(x = clutch[[i]][,1],y = clutch[[i]][,2], type = "l",col = "red", xlab = "Theta", ylab= "Post", main = player[i])
  }

par(op)


#c
#summ
postSum<-function( x,n, nam, over){
  a= x +1
  b = n-x + 1
  pMean=a/(a+b)
  pStd = sqrt((a*b)/(((a+b)**2)*(a+b+1)))
  ci = qbeta(c(.025, .975), a,b)
  lowCi=ci[1]
  hiCi=ci[2]
  less= pbeta(.5,a,b)
  return(cbind("name" = nam, "over"= over, "pMean" = pMean, "pStd" = pStd, "lowCi"= lowCi, "hiCi"= hiCi, "less"= less)%>%as.data.frame)
}

df_c<- pmap_df(data, ~postSum(..3,..4, ..5, ..2))

df_c$name<- player[df_c$name]
df_c
#d

df_d<- pmap_df(data, ~{cbind("name" = ..5, "pBelow" = pbeta(..2,..3,..4), "pAbove" = 1- pbeta(..2,..3,..4))}%>%as.data.frame)


df_d$name<- player[df_d$name]
df_d
#e
#TODO

#mcmc

#seed so experiment can be reproduced
set.seed(100)

#function that allows for random draws
mcmc<- function(x,n){
  a= x +1
  b = n-x + 1
  rbeta(100000, a,b)%>% return()
}

# Using the data data frame with raw information, random draws were made for each of the 10 players
df_e<- pmap(data, ~mcmc(..3,..4))

#a matrix to hold all the comparison scores for each player
l<-matrix(nrow= 10, ncol = 10)
#comparison 
for(i in 1:10){
  var<- double(10)
  for(j in 1:10){
    #all players were compared to each other and 
    #scores were recorded
    l[i,j]= (mean(df_e[[i]]> df_e[[j]]))
  }
}

#transofm each player results so it can be analysed row wise
tr<- t(l)

#max score of each player was determined and the player was recorded
rank<- as.integer(rep(0,10))
per<- as.double(rep(0, 10))
for(i in 1:10){
  rank[i]= tr[i,]%>%which.max()
  per[i] = tr[i,]%>%max()
  
}

#summary of best player against each player
results<- data.frame("Player" = player, "Best Score" = player[rank], "Percentage" = per)
results

