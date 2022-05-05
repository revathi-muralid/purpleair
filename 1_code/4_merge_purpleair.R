
# Created: by RM
# Updated: by RM on 4/11/21

### MUST REQUEST 5GB MEMORY IN LONGLEAF IF TRYING TO CREATE DFS_ALL

rm(list=ls())

if (!require('data.table')) {
  install.packages('data.table')
  library(data.table)
}

if (!require('purrr')) {
  install.packages('purrr')
  library(purrr)
}

#Read in files through loop

mydfs<-list()
for(i in 1:40){
  l1<-list.files(paste("/nas/longleaf/home/revathi/purpleair/output/",i,sep=""))
  #assign(paste("sites_",i,sep=""),l1)
  for(j in 1:length(l1)){
    #d1 <- read.csv.sql(paste("/nas/longleaf/home/revathi/purpleair/output/",i,"/",l1[j],sep=""), "select * from file where ")
    d1<-fread(paste("/nas/longleaf/home/revathi/purpleair/output/",i,"/",l1[j],sep=""))
    d1$created_at<-ifelse(grepl("-",d1$created_at)==FALSE,as.POSIXct(as.numeric(d1$created_at), origin = '1970-01-01', tz = 'GMT'),d1$created_at)
    mydfs[[i*j]]<-d1
    #assign(paste('site_',i,'_',j,sep=""),d1)
  }
} #9750 DFs

dfs_to_bind<-purrr::discard(mydfs, ~any(dim(.x)[1]==0)) #9705
dfs_to_bind<-purrr::discard(dfs_to_bind, ~any(is.null(.x)==T)) #3977
dfs_all<-rbindlist(dfs_to_bind, fill=TRUE)

write.csv(dfs_all, 'CA_PM25_allPurpleAir_2020.csv',na="",row.names=F)

pm_in<-dfs_all[which(dfs_all$Location=="inside"),]
pm_out<-dfs_all[which(dfs_all$Location=="outside"),]

write.csv(pm_in, 'CA_PM25_indoorPurpleAir_2020.csv',na="",row.names=F)
write.csv(pm_out, 'CA_PM25_outdoorPurpleAir_2020.csv',na="",row.names=F)

