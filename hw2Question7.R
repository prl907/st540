#question 7 Plot


#building the posterior function with parameters
#theta given in the question
#y given in the question

postE<- function(theta, y){
  n= seq(1:10) + y
  like =dbinom(y,n,theta)
  prior= dpois(n,5)
  fy = sum(like*prior)
  post= like*prior/fy
  return(cbind(theta= theta, y = y, n = n, post = post))
}

#loop throught theta and y to create list of various combinations
df<- map2(c(.2,.5)%>%rep(3)%>%map(~..1),c(0,5.,10.)%>%rep(2)%>%sort()%>%map(~..1), ~postE(..1, ..2))

#flaten out dataframes in list as one dataframe
data<- df[[1]]%>%rbind(df[[2]])

for(i in 3:6){
  data= rbind(data, df[[i]])
}

#plot the dataframes
ggplot(as.data.frame(data), aes(y = post,x = n))+ geom_line()+ facet_grid(theta~y)+labs(title = "Posterior of binominal with prior poi(5)")

  
  

