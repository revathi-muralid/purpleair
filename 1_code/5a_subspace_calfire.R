
rm(list=ls())

if (!require('data.table')) {
  install.packages('data.table')
  library(data.table)
}

if (!require('purrr')) {
  install.packages('purrr')
  library(purrr)
}

if (!require('rgdal')) {
  install.packages('rgdal')
  library(rgdal)
}

if (!require('tidyverse')) {
  install.packages('tidyverse')
  library(tidyverse)
}

pm_all<-fread("7_output/CA_PM25_allPurpleAir_2020.csv")
sensors<-as.data.frame(unique(pm_all$ID))
names(sensors)<-c("ID")
all_sensors<-read.csv("2_sensor_lists/sensorlist_ca_2021-04-08.csv")
my_sensors<-merge(sensors, all_sensors, by="ID",all.x=T, all.y=F)

# Read in wildfire Purple Air shapefile

pa_shp<-readOGR(dsn="8_PA_wildfires_shp")
pa_data<-pa_shp@data

fire<-read.csv('6_CALFIRE/mapdataall.csv')
fire_20<-fire[which(year(fire$incident_dateonly_created)==2020),]
write.csv(fire_20,'CA_fires_2020.csv',na="",row.names=F)

# 3967/3977 sensors were kept by ArcGIS subset of 100km from wildfire event

pm_all_final<-pm_all%>%filter(ID %in% pa_data$ID)

pm_in_final=pm_all_final[which(pm_all_final$Location=="inside"),]
pm_out_final=pm_all_final[which(pm_all_final$Location=="outside"),]

write.csv(pm_all_final,'CA_wildfire_PM25_allPurpleAir.csv',na="",row.names=F)
write.csv(pm_in_final,'CA_wildfire_PM25_indoorPurpleAir.csv',na="",row.names=F)
write.csv(pm_out_final,'CA_wildfire_PM25_outdoorPurpleAir.csv',na="",row.names=F)
