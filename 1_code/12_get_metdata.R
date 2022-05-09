
rm(list=ls())
setwd("/nas/longleaf/home/revathi/purpleair/9_DELIVERABLES/")

library(stationaRy)
library(data.table)
library(sf)
library(tidyverse)

# Get all stations that have met data
# https://rich-iannone.github.io/stationaRy/reference/get_station_metadata.html

dat<-as.data.frame(get_station_metadata())
dat<-dat[which(dat$end_year>=2020),]
dat<-dat[which(dat$state=="CA"),]

# Read in file of paired PA monitors
monitors<-fread("/nas/longleaf/home/revathi/purpleair/2_sensor_lists/WF_io_pairs_540km_May22.csv" )

names(monitors)<-c("outdoor_ID","Dist","lon","lat","indoor_ID")
monitors$lon<-gsub("c[(]","",monitors$lon)
monitors$lat<-gsub("[)]","",monitors$lat)

#### Find closest met station to each PA monitor

site_coordinates_crs<-CRS("+proj=longlat +datum=WGS84")

point_sf <- st_as_sf(monitors, coords = c("lon", "lat"),crs=site_coordinates_crs)
# Set the projection to EPSG 4326 (long-lat)
st_crs(point_sf) <- 4326

# Specify the source of X and Y coordinates
target_sf <- st_as_sf(dat, coords = c("lon", "lat"),crs=site_coordinates_crs)
# Set the projection to EPSG 4326 (long-lat)
st_crs(target_sf) <- 4326

# Loop through point_sf row by row to get a df for each PA monitor
# Save df from each loop iteration to df list mydfs

mydfs<-list()

for(i in 1:nrow(point_sf)){
  target_sf2 <- target_sf %>% # target_sf stays the same
    mutate(Dist = as.numeric(st_distance(point_sf[c(i),], target_sf, by_element = TRUE))) #%>%
  # Order target_sf2 by closest to furthest pair
  target_sf3<-target_sf2[order(target_sf2$Dist),]
  # Grab first (closest) pair record
  target_sf4<-target_sf3[c(1),]
  # # Reattach PA data to df of monitors near met station i
  if(nrow(target_sf4)>0){
    target_sf4$outdoor_ID<-point_sf$outdoor_ID[i]
    target_sf4$indoor_ID<-point_sf$indoor_ID[i]
    target_sf4$PA_Dist<-point_sf$Dist[i]
    
    mydfs[[i]]<-target_sf4
  }
}
met_clean<-rbindlist(mydfs)

# After getting appropriate USAF/WBAN station IDs from above, get 
# data from this link: https://www.ncei.noaa.gov/pub/data/noaa/2020/

# If you need hourly data then please go through the site 
# "https://www.ncei.noaa.gov/pub/data/noaa/2020/". Here, 
# you need to select the data by United State Air Force Station (USAF) id 
# (Example 722950 - Los Angeles) or Weather Bureau Army Navy Station (WBAN) id 
# (Example 23174 - Los Angeles).

metpg="https://www.ncei.noaa.gov/pub/data/noaa/2020/"
metfiles=readLines(metpg)

mydfs<-list()

for(i in 1:nrow(met_clean)){
  mygz=paste(metpg,met_clean$id[i],"-2020.gz",sep="")
  tmp<-tempfile()
  download.file(mygz,tmp)
  data <- read.csv(
    gzfile(tmp),
    sep="\t",
    header=TRUE,
    stringsAsFactors=FALSE)
  data2<-data
  names(data2)<-c("ALL")
  #data2$nchar<-substr(data2$ALL,1,4)
  data2$usaf<-substr(data2$ALL,5,10)
  data2$wban<-substr(data2$ALL,11,15)
  data2$date<-substr(data2$ALL,16,23)
  data2$time<-substr(data2$ALL,24,27)
  data2$geoflag<-substr(data2$ALL,28,28)
  data2$lat<-substr(data2$ALL,29,34)
  data2$lon<-substr(data2$ALL,35,41)
  data2$obs_type<-substr(data2$ALL,42,46)
  data2$elev<-substr(data2$ALL,47,51)
  data2$FWS_ID<-substr(data2$ALL,52,56)
  #data2$QC_name<-substr(data2$ALL,57,60)
  data2$wind_angle<-substr(data2$ALL,61,63)
  data2$wind_ang_qc<-substr(data2$ALL,64,64)
  data2$wind_type<-substr(data2$ALL,65,65)
  data2$wind_speed<-substr(data2$ALL,66,69)
  data2$wind_sp_qc<-substr(data2$ALL,70,70)
  data2$air_temp<-substr(data2$ALL,88,92)
  data2$air_temp_qc<-substr(data2$ALL,93,93)
  data2$sealvl_atm<-substr(data2$ALL,100,104)
  data2$sealvl_atm_qc<-substr(data2$ALL,105,105)
  data3<-data2[,c(2:20)]
  mydfs[[i]]<-data3
}

met_out<-rbindlist(mydfs)

met_clean2<-met_clean[,c(1:13)]
met_clean2<-met_clean2[!duplicated(met_clean2)]



met_fin<-merge(met_out,met_clean,by=c("usaf","wban"),all.x=TRUE,allow.cartesian=TRUE)

# Join PA data back to met


# Output hourly met data
met_fin2<-met_fin[,c(1:25,32:36)]
fwrite(met_fin2, 'Hourly_MET_2020_May22.csv',na="",row.names=F,quote=FALSE)


# Output PA indoor/outdoor monitor data with associated met stations
met_clean3<-met_clean
met_clean3<-met_clean3[,c(1:8,15:19)]
PA_pairs<-read.csv("")
met_clean3<-as.matrix(met_clean3)
write.csv(met_clean3,'MET_WF_io_pairs_540km_May22.csv',na="",row.names=F)
