#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec  9 17:16:23 2021

@author: revathi
"""
import pandas as pd
import numpy as np
import geopandas as gpd
import math
import haversine

Dir = "/nas/longleaf/home/revathi/purpleair/"

# Read in wildfire Purple Air shapefile - 3967 monitors

pa_shp = gpd.read_file(Dir+"8_PA_wildfires_shp")
#pa_data<-pa_shp@data

# Get lon/lat data
pa_shp['lon'] = pa_shp['geometry'].x
pa_shp['lat'] = pa_shp['geometry'].y

pm_all = pd.read_csv(Dir+"7_output/CA_PM25_allPurpleAir_2020.csv")
pm_all['created_at'] = pm_all['created_at'].astype('int64')
pm_all['date_only'] = pd.to_datetime(pm_all['created_at'], unit='s',infer_datetime_format=True, origin="1970-01-01")
#pm_all$date_only<-as.POSIXct(as.numeric(as.character(pm_all$created_at)), origin="1970-01-01", tz="GMT")

# Find closest wildfire to each monitor

fire_20 = pd.read_csv(Dir+'6_CALFIRE/CA_fires_2020.csv')

# Find all dates for wildfires
fire_dates=fire_20[["incident_name","incident_id","incident_longitude",
"incident_latitude","incident_dateonly_created","incident_dateonly_extinguished"]]
date_ranges = fire_dates
date_ranges = date_ranges.rename({"incident_dateonly_created":"start_dates",
                    "incident_dateonly_extinguished":"end_dates"},axis='columns')
date_ranges=date_ranges.drop_duplicates()
#row.names(date_ranges)<-1:nrow(date_ranges)

end_dates_new = []

#date_ranges_2 = np.where(date_ranges['end_dates'].loc(i)=="",date_ranges['start_dates'].loc(i),date_ranges['end_dates'].loc(i))

date_ranges['end_dates'] = date_ranges['end_dates'].fillna(0)

for i in range(0,date_ranges.shape[0]):
    end_dates_new.append(np.where(date_ranges['end_dates'].loc[i]==0,date_ranges['start_dates'].loc[i],date_ranges['end_dates'].loc[i]))

date_ranges['end_dates']=end_dates_new

# Find all sensors within 540km of wildfire for each date range
from shapely.geometry import Point

points_df = gpd.GeoDataFrame({'geometry': [pnt1, pnt2]}, crs='EPSG:4326')
points_df = pa_shp.to_crs('EPSG:5234')
points_df2 = points_df.shift() #We shift the dataframe by 1 to align pnt1 with pnt2
points_df.distance(points_df2)
haversine(date_ranges[['incident_longitude','incident_latitude']],
pa_shp[['lon','lat']].loc[1])

point_sf = date_ranges[['incident_id','lon_rad','lat_rad']]


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

# Loop through point_sf row by row to get a df for each wildfire
# Save df from each loop iteration to df list mydfs

mydfs = []
for i in range(0,point_sf.shape[0]):
  target_sf2 = target_sf %>% # target_sf stays the same
    mutate(Dist = as.numeric(st_distance(point_sf[c(i),], target_sf, by_element = TRUE))) %>%
    # Filter the records with Dist <= 540000 (m)
    filter(Dist <= 540000)
  # 
  # # Reattach wildfire data to df of monitors within 540km of wildfire i
  if((nrow(target_sf2)>0)==T){
    target_sf2$incident_id<-point_sf$incident_id[i]
    target_sf2$incident_name<-point_sf$incident_name[i]
    target_sf2$start_date<-point_sf$start_dates[i]
    target_sf2$end_date<-point_sf$end_dates[i]
    # 
    # # Subset dataset of PA observations for only monitors selected from above
    # # Also select for time range of relevant wildfire
    # pm_temp is df of all PA obs w/in 540km of wildfire i for dates wildfire i took place
    
    pm_temp = pm_all%>%filter(ID %in% target_sf2$ID & date_only >= target_sf2$start_date[1] & date_only <= target_sf2$end_date[1])
    mydfs[[i]]<-pm_temp
  }
}

# 11/21/21 UPDATE

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


last = 1
idx = rep(0, nrow(dt))
for (curr in 1:nrow(point_sf)) {
  if (dist(dt[c(curr, last), .(easting, northing)]) <= maxDist) {
    idx[curr] = curr
    last = curr
  }
}

for(i in 1:nrow(point_sf)){
  target_sf2 <- target_sf %>% # target_sf stays the same
    mutate(Dist = as.numeric(st_distance(point_sf[c(i),], target_sf, by_element = TRUE))) %>%
    # Filter the records with Dist <= 540000 (m)
    filter(Dist <= 540000)
  # 
  # # Reattach wildfire data to df of monitors within 540km of wildfire i
  if((nrow(target_sf2)>0)==T){
    target_sf2$incident_id<-point_sf$incident_id[i]
    target_sf2$incident_name<-point_sf$incident_name[i]
    target_sf2$start_date<-point_sf$start_dates[i]
    target_sf2$end_date<-point_sf$end_dates[i]
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
pm_clean<-rbindlist(mydfs)#6696675 obs - 48 million
pm_final<-pm_clean[which(!duplicated(pm_clean)),] #7.8 million obs

pm_in<-pm%>%filter(Location=="inside")
pm_out<-pm%>%filter(Location=="outside")

write.csv(pm_final,"9_DELIVERABLES/data/CA_wildfire_PM25_allPurpleAir_540km.csv")
write.csv(pm_in,"9_DELIVERABLES/data/CA_wildfire_PM25_insidePurpleAir_540km.csv")
write.csv(pm_out,"9_DELIVERABLES/data/CA_wildfire_PM25_outsidePurpleAir_540km.csv")


#######PREP FOR 5/11/21 MEETING
##GET LIST OF # OF INDOOR SENSORS IMPACTED BY WILDFIRE EVENTS IN EACH COUNTY

pm2 = pd.read_csv(Dir+"9_DELIVERABLES/data/CA_wildfire_PM25_allPurpleAir.csv")
pm_in=pd.read_csv(Dir+"9_DELIVERABLES/data/CA_wildfire_PM25_insidePurpleAir_170km.csv")
sensors2<-unique(pm2[,c(35:38)])
write.csv(sensors,'9_DELIVERABLES/wildfire_indoor_sensors_170km.csv',na="",row.names=F)

# Get county shapefile

county_shp<-readOGR(dsn="8b_ca-county-boundaries")
county_data<-county_shp@data

# Read .csv file

d1<-pd.read_csv(Dir+"9_DELIVERABLES/PA_WF_IndoorSensorsWithCounties.csv")
d2<-d1%>%group_by(NAMELSAD)%>%summarise(county_n=length(unique(PA_SENSOR_ID)))
names(d2)[1]<-c("COUNTY")
names(d2)[2]<-c("N_SENSORS")
d2<-rbind(d2,c("TOTAL",sum(d2$N_SENSORS)))
write.csv(d2,'9_DELIVERABLES/PA_WF_IndoorSensorSummary.csv',row.names=F,na="")
