
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

# Read in wildfire Purple Air shapefile - 3967 monitors

pa_shp<-readOGR(dsn="8_PA_wildfires_shp")
pa_data<-pa_shp@data
pa_data<-setDT(pa_data)

pm_all<-fread("7_output/CA_PM25_allPurpleAir_2020.csv")
pm_all$date_only<-as.POSIXct(as.numeric(as.character(pm_all$created_at)), origin="1970-01-01", tz="GMT")
pm_all$date_only<-as.Date(pm_all$date_only,origin="1970-01-01",format='%Y-%m-%d')

# Find closest wildfire to each monitor

fire_20<-fread('6_CALFIRE/CA_fires_2020.csv')

# Find all dates for wildfires
fire_dates<-fire_20[,c("incident_name","incident_id","incident_longitude",
                       "incident_latitude","incident_dateonly_created","incident_dateonly_extinguished")]
date_ranges<-fire_dates
names(date_ranges)[5:6]<-c("start_dates","end_dates")
date_ranges<-date_ranges[!duplicated(date_ranges),]
row.names(date_ranges)<-1:nrow(date_ranges)
for(i in 1:nrow(date_ranges)){
  date_ranges$end_dates[i]<-ifelse(is.na(date_ranges$end_dates[i])==T,date_ranges$start_dates[i],date_ranges$end_dates[i])
}

# Find all sensors within 540km of wildfire for each date range

site_coordinates_crs<-CRS("+proj=longlat +datum=WGS84")

point_sf <- st_as_sf(date_ranges, coords = c("incident_longitude", "incident_latitude"),crs=site_coordinates_crs)
# Set the projection to EPSG 4326 (long-lat)
st_crs(point_sf) <- 4326

# Specify the source of X and Y coordinates
target_sf <- st_as_sf(pa_data, coords = c("Lon", "Lat"),crs=site_coordinates_crs)
# Set the projection to EPSG 4326 (long-lat)
st_crs(target_sf) <- 4326

#st_crs(coordinates)
#coordinates_aes<-st_transform(coordinates,crs(nlcd)) 

# 12/13/21 UPDATE

# Loop through point_sf row by row to get a df for each wildfire
# Save df from each loop iteration to df list mydfs

mydfs<-list()

dt_points<-data.table(point_sf,key="incident_id")
dt_target<-data.table(target_sf,key="ID")

dt_points[dt_target,distance:=st_distance(dt_points[geometry],dt_target[geometry],by_element=TRUE)]
#dt=dt[, test:= 0]
for (i in 1:nrow(dt_points)){
  dt_target[, Dist:= as.numeric(st_distance(dt_points[c(i),], dt_target, by_element=TRUE))]
  dt_target[Dist <= 540000]
}

# DT method
for(i in 1:nrow(dt_points)){
  target_sf2 <- dt_target 
  target_sf2[, Dist:= as.numeric(st_distance(dt_points[c(i),], target_sf2, by_element=TRUE))]
  target_sf2[Dist <= 540000]
  # 

  # # Reattach wildfire data to df of monitors within 540km of wildfire i
  if((nrow(target_sf2)>0)==T){
    target_sf2$incident_id<-dt_points$incident_id[i]
    target_sf2$incident_name<-dt_points$incident_name[i]
    target_sf2$start_date<-dt_points$start_dates[i]
    target_sf2$end_date<-dt_points$end_dates[i]
    # 
    # # Subset dataset of PA observations for only monitors selected from above
    # # Also select for time range of relevant wildfire
    # pm_temp is df of all PA obs w/in 540km of wildfire i for dates wildfire i took place
    
    pm_temp = pm_all[ID %in% target_sf2$ID]
    pm_temp2 = pm_temp[date_only >= "2020-07-01" & date_only <= "2020-11-01"]
    
    
    #pm_temp<-pm_all%>%filter(ID %in% target_sf2$ID & date_only >= "2020-07-01" & date_only <= "2020-11-01")
    mydfs[[i]]<-pm_temp2
  }
}

for(i in 1:nrow(dt_points)){
  target_sf2 <- dt_target %>% # target_sf stays the same
    mutate(Dist = as.numeric(st_distance(dt_points[c(i),], dt_target, by_element = TRUE))) %>%
    # Filter the records with Dist <= 540000 (m)
    filter(Dist <= 540000)
  # 
  # # Reattach wildfire data to df of monitors within 540km of wildfire i
  if((nrow(target_sf2)>0)==T){
    target_sf2$incident_id<-dt_points$incident_id[i]
    target_sf2$incident_name<-dt_points$incident_name[i]
    target_sf2$start_date<-dt_points$start_dates[i]
    target_sf2$end_date<-dt_points$end_dates[i]
    # 
    # # Subset dataset of PA observations for only monitors selected from above
    # # Also select for time range of relevant wildfire
    # pm_temp is df of all PA obs w/in 540km of wildfire i for dates wildfire i took place
    
    pm_temp = pm_all[ID %in% target_sf2$ID]
    pm_temp2 = pm_temp[date_only >= "2020-07-01" & date_only <= "2020-11-01"]
    
    
    #pm_temp<-pm_all%>%filter(ID %in% target_sf2$ID & date_only >= "2020-07-01" & date_only <= "2020-11-01")
    mydfs[[i]]<-pm_temp2
  }
}
#4/28/22 update
#target_sf2<-fread('9_DELIVERABLES/PA_WF_paired_monitors_addresses.csv')
#pm_temp = pm_all[ID %in% target_sf2$outdoor_ID | ID %in% target_sf2$indoor_ID]
#pm_temp<-pm_temp[which(!duplicated(pm_temp)),]
pm_clean<-rbindlist(mydfs)#6696675 obs - 48 million
pm_final<-pm_clean[which(!duplicated(pm_clean)),] #7.8 million obs

pm_in<-pm%>%filter(Location=="inside")
pm_out<-pm%>%filter(Location=="outside")

write.csv(pm_final,"9_DELIVERABLES/data/CA_wildfire_PM25_allPurpleAir_540km.csv")
write.csv(pm_in,"9_DELIVERABLES/data/CA_wildfire_PM25_insidePurpleAir_540km.csv")
write.csv(pm_out,"9_DELIVERABLES/data/CA_wildfire_PM25_outsidePurpleAir_540km.csv")


#######PREP FOR 5/11/21 MEETING
##GET LIST OF # OF INDOOR SENSORS IMPACTED BY WILDFIRE EVENTS IN EACH COUNTY

pm2<-fread("9_DELIVERABLES/data/CA_wildfire_PM25_allPurpleAir.csv")
pm_in<-fread("9_DELIVERABLES/data/CA_wildfire_PM25_insidePurpleAir_170km.csv")
sensors2<-unique(pm2[,c(35:38)])
write.csv(sensors,'9_DELIVERABLES/wildfire_indoor_sensors_170km.csv',na="",row.names=F)

# Get county shapefile

county_shp<-readOGR(dsn="8b_ca-county-boundaries")
county_data<-county_shp@data

# Read .csv file

d1<-fread("9_DELIVERABLES/PA_WF_IndoorSensorsWithCounties.csv")
d2<-d1%>%group_by(NAMELSAD)%>%summarise(county_n=length(unique(PA_SENSOR_ID)))
names(d2)[1]<-c("COUNTY")
names(d2)[2]<-c("N_SENSORS")
d2<-rbind(d2,c("TOTAL",sum(d2$N_SENSORS)))
write.csv(d2,'9_DELIVERABLES/PA_WF_IndoorSensorSummary.csv',row.names=F,na="")
