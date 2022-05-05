
rm(list=ls())

if (!require('data.table')) {
  install.packages('data.table')
  library(data.table)
}
if (!require('boot')) {
  install.packages('boot')
  library(boot)
}
if (!require('dplyr')) {
  install.packages('dplyr')
  library(dplyr)
}

if (!require('sf')) {
  install.packages('sf')
  library(sf)
}

if (!require('rgdal')) {
  install.packages('rgdal')
  library(rgdal)
}
#########################################################################################

setwd('/nas/longleaf/home/revathi/purpleair/')

# For calculating WF-associated Finf, uncomment the following line:
d5<-fread('7b_analytic_datasets/WF_associated_data_May22.csv')

# For calculating non-WF Finf, uncomment the following line:
#d5<-fread('7b_analytic_datasets/nonWF_data.csv')

#########################################################################################

###Indoor/Outdoor monitor pairing

# Check Bi et al. 2020b
# Pairing strategy: match indoor monitor to nearest outdoor monitor w/in 500-m radius

sensors<-d5%>%select(ID,Location,Lon,Lat)
sensors<-unique(sensors)

indoor<-sensors[which(sensors$Location=="inside"),]
outdoor<-sensors[which(sensors$Location=="outside"),]

site_coordinates_crs<-CRS("+proj=longlat +datum=WGS84")

point_sf <- st_as_sf(indoor, coords = c("Lon", "Lat"),crs=site_coordinates_crs)
st_crs(point_sf) <- 4326
#names(point_sf)[1]<-c("indoor_ID")

# Specify the source of X and Y coordinates
target_sf <- st_as_sf(outdoor, coords = c("Lon", "Lat"),crs=site_coordinates_crs)
st_crs(target_sf) <- 4326
#names(target_sf)[1]<-c("outdoor_ID")

# Loop through point_sf row by row to get closest outdoor monitor for each indoor monitor
# Save df from each loop iteration to df list mydfs

mydfs<-list()
for(i in 1:nrow(point_sf)){
  # Get list of indoor monitors within 500 m of outdoor monitor i
  target_sf2<-target_sf
  Dist=as.numeric(st_distance(point_sf[c(i),], target_sf, by_element = TRUE))
  #Dist<-as.numeric(st_distance(point_sf[c(i),], target_sf, by_element = TRUE))
  target_sf2<-cbind(target_sf,Dist)
  # Filter the records with Dist <= 500 (m)
  target_sf2<-target_sf2%>%filter(Dist<=500)
  target_sf2<-target_sf2%>%select(ID,Dist)
  names(target_sf2)[1]<-c("outdoor_ID")
  # # Reattach wildfire data to df of monitors within 100km of wildfire i
  if((nrow(target_sf2)>0)==T){
    target_sf2$indoor_ID<-point_sf$ID[i]
    mydfs[[i]]<-target_sf2
  }
}

pairs<-rbindlist(mydfs)

#write.csv(pairs,'2_sensor_lists/WF_io_pairs_540km_May22.csv',row.names=F,na="")

#test<-read.csv('2_sensor_lists/WF_io_pairs_540km_May22.csv')
#names(pairs)<-c("outdoor_ID","Dist","geom_1","geom_2","indoor_ID")

finf_dat<-d5%>%filter((ID %in% pairs$outdoor_ID)|(ID %in% pairs$indoor_ID)) #348103 obs
#finf_dat<-finf_dat[,-c(1)]

#write.csv(finf_dat,'7b_analytic_datasets/nonWF_finf_dataset.csv',na="",row.names=F)

# Pair observations

# As the outdoor measurements made an hour earlier had the highest correlation 
# coefficient with indoor measurements, the indoor measurements were paired with 
# outdoor measurements an hour earlier to calculate the hourly I/O ratios.

#Get indoor ID

pm_pairs<-list()
for(i in 1:nrow(pairs)){
  ID_in<-pairs$indoor_ID[i]
  ID_out<-as.integer(pairs$outdoor_ID[i])
  t_pm_in<-finf_dat[which(finf_dat$ID==ID_in),]
  t_pm_out<-finf_dat[which(finf_dat$ID==ID_out),]
  #t_pm_in<-t_pm_in[,c(1,3,19)]
  #t_pm_out<-t_pm_out[,c(1,3,19)]
  t_pm_pair<-list()
  for(j in 1:nrow(t_pm_in)){
    in_time=t_pm_in$created_at[j]
    pair_time=in_time-259200
    #Get obs from 3 days earlier from outdoor monitor
    t_pair<-t_pm_out[which(t_pm_out$created_at==pair_time),]
    names(t_pair)[1]<-c("out_time")
    names(t_pair)[3]<-c("out_PM2.5_CF_1_ug/m3_A")
    names(t_pair)[19]<-c("out_PM2.5_CF_1_ug/m3_B")
    names(t_pair)[53]<-c("out_CPM2.5_A")
    names(t_pair)[54]<-c("out_CPM2.5_B")
    names(t_pair)[55]<-c("out_CPM2.5_adj_A")
    names(t_pair)[56]<-c("out_CPM2.5_adj_B")
    t_pair$in_time<-in_time
    t_pair$'in_PM2.5_CF_1_ug/m3_A'<-t_pm_in$`PM2.5_CF_1_ug/m3_A`[j]
    t_pair$'in_PM2.5_CF_1_ug/m3_B'<-t_pm_in$`PM2.5_CF_1_ug/m3_B`[j]
    t_pair$in_CPM2.5_A<-t_pm_in$CPM2.5_A[j]
    t_pair$in_CPM2.5_B<-t_pm_in$CPM2.5_B[j]
    t_pair$in_CPM2.5_adj_A<-t_pm_in$CPM2.5_adj_A[j]
    t_pair$in_CPM2.5_adj_B<-t_pm_in$CPM2.5_adj_B[j]
    t_pm_pair[[j]]<-t_pair
  }
  t_pm_pair_all<-rbindlist(t_pm_pair)
  t_pm_pair_all$indoor_ID<-ID_in
  t_pm_pair_all$outdoor_ID<-ID_out
  pm_pairs[[i]]<-t_pm_pair_all
}

pm_paired<-rbindlist(pm_pairs)

qa<-pm_paired
qa<-qa[which(!duplicated(qa)),]

write.csv(pm_paired,'7b_analytic_datasets/CA_WF_PM25_pairedPurpleAir_540km_May22.csv')

#as.POSIXct(t_pm_in$created_at[i],origin="1970-01-01")
