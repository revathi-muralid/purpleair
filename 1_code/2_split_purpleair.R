
rm(list=ls())

setwd('purpleair')

# Get sensor list

sites <- read.csv('./sensorlist_ca_2021-04-08.csv', as.is = T)

#Split into groups of 100

sites_grpn<-ceiling(nrow(sites)/100)

for (i in 1:sites_grpn){
  a=100*i-99
  b=100*i
  sites_i<-sites[c(a:b),]
  write.csv(sites_i,paste("sites/sites_",i,".csv",sep=""),row.names=F,na="")
}
