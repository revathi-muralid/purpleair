
rm(list=ls())

if (!require('data.table')) {
  install.packages('data.table')
  library(data.table)
}

if (!require('rgdal')) {
  install.packages('rgdal')
  library(rgdal)
}

if (!require('tidyverse')) {
  install.packages('tidyverse')
  library(tidyverse)
}

if (!require('sf')) {
  install.packages('sf')
  library(sf)
}

setwd('purpleair')

d1 = fread("/nas/longleaf/home/revathi/chaq/revathi/CA_wildfire_PM25_fires241-250_PurpleAir_540km.csv")
d2 = fread("/nas/longleaf/home/revathi/chaq/revathi/CA_wildfire_PM25_fires251-258_PurpleAir_540km.csv")

dat = rbind(d1,d2)
dat = dat[which(!duplicated(dat)),] #10 million obs

rm(d1,d2)
d1 = fread("9_DELIVERABLES/data/CA_wildfire_PM25_fires231-240_PurpleAir_540km.csv")
d2 = rbind(d1,dat)
dat = d2[which(!duplicated(d2)),] #10 million obs

rm(d1,d2)
d1 = fread("9_DELIVERABLES/data/CA_wildfire_PM25_fires221-230_PurpleAir_540km.csv")
d2 = rbind(d1,dat)
dat = d2[which(!duplicated(d2)),] #10 million obs

rm(d1,d2)
d1 = fread("9_DELIVERABLES/data/CA_wildfire_PM25_fires211-220_PurpleAir_540km.csv")
d2 = rbind(d1,dat)
dat = d2[which(!duplicated(d2)),] #10 million obs

rm(d1,d2)
d1 = fread("9_DELIVERABLES/data/CA_wildfire_PM25_fires191-200_PurpleAir_540km.csv")
d2 = rbind(d1,dat)
dat = d2[which(!duplicated(d2)),] #10 million obs

#5067519

rm(d1,d2)
d1 = fread("9_DELIVERABLES/data/CA_wildfire_PM25_fires31-40_PurpleAir_540km.csv")
d2 = rbind(d1,dat)
dat = d2[which(!duplicated(d2)),] #10 million obs

rm(d1,d2)
d1 = fread("9_DELIVERABLES/data/CA_wildfire_PM25_fires1-10_PurpleAir_540km.csv")
d2 = rbind(d1,dat)
dat = d2[which(!duplicated(d2)),] #10 million obs

fwrite(dat,"9_DELIVERABLES/data/CA_wildfire_PM25_PurpleAir_540km.csv")

pm_in<-dat%>%filter(Location=="inside")
pm_out<-dat%>%filter(Location=="outside")

fwrite(pm_in,"9_DELIVERABLES/data/CA_WF_PM25_indoorPurpleAir_540km.csv")
fwrite(pm_out,"9_DELIVERABLES/data/CA_WF_PM25_outdoorPurpleAir_540km.csv")

test<-fread("9_DELIVERABLES/data/superseded/CA_wildfire_PM25_allPurpleAir_540km.csv")
pm_in<-test%>%filter(Location=="inside")
pm_out<-test%>%filter(Location=="outside")
