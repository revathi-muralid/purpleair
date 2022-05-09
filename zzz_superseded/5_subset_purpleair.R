
rm(list=ls())

if (!require('data.table')) {
  install.packages('data.table')
  library(data.table)
}

if (!require('purrr')) {
  install.packages('purrr')
  library(purrr)
}

setwd('purpleair')

dat<-fread('output/CA_PM25_allPurpleAir_2020.csv')

pm_in<-dat[which(dat$Location=="inside"),]
write.csv(pm_in, 'CA_PM25_indoorPurpleAir_2020.csv',na="",row.names=F)
fire<-read.csv('CALFIRE/mapdataall.csv')


pm_out<-dat[which(dat$Location=="outside"),]
write.csv(pm_out, 'CA_PM25_outdoorPurpleAir_2020.csv',na="",row.names=F)

