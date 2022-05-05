
rm(list=ls())
setwd("/nas/longleaf/home/revathi/purpleair/9_DELIVERABLES/")

library(data.table)
library(sf)
library(tidyverse)
library(revgeo)

# Read in file of all PA monitors
all_monitors<-fread("PA_WF_monitors_geocoded.csv")

# Read in file of paired PA monitors
monitors<-fread("/nas/longleaf/home/revathi/purpleair/2_sensor_lists/WF_io_pairs_540km_Dec21.csv" )
names(monitors)<-c("ID","Dist","lon","lat","indoor_ID")
monitors$lon<-gsub("c[(]","",monitors$lon)
monitors$lat<-gsub("[)]","",monitors$lat)

monitors2<-merge(monitors,all_monitors[,c(1:2,6:11)],by="ID")
names(monitors2)<-c("outdoor_ID","Dist","out_lon","out_lat","ID",
                    "out_name","out_incident_id","out_incident_name",
                    "out_start_date","out_end_date","out_temp","out_humidity")
monitors3<-merge(monitors2,all_monitors[,c(1:2,4:11)],by="ID")
names(monitors3)<-c("indoor_ID","outdoor_ID","Dist","out_lon","out_lat",
                    "out_name","out_incident_id","out_incident_name",
                    "out_start_date","out_end_date","out_temp","out_humidity",
                    "in_name","in_lon","in_lat","in_incident_id",
                    "in_incident_name","in_start_date","in_end_date",
                    "in_temp","in_humidity")

write.csv(monitors3,'PA_WF_monitors_addresses.csv',na="",row.names=F,quote=FALSE)
