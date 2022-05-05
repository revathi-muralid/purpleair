
rm(list=ls())

if (!require('data.table')) {
  install.packages('data.table')
  library(data.table)
}

if (!require('psych')) {
  install.packages('psych')
  library(psych)
}

if (!require('dplyr')) {
  install.packages('dplyr')
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

if (!require('xts')) {
  install.packages('xts')
  library(xts)
}

if (!require('leaflet')) {
  install.packages('leaflet')
  library(leaflet)
}

if (!require('ggplot2')) {
  install.packages('ggplot2')
  library(ggplot2)
}

setwd('purpleair')

fire_20<-read.csv('6_CALFIRE/CA_fires_2020.csv')

# Calculate months each WF spanned across
fire_20$month_created<-month(fire_20$incident_dateonly_created)
fire_20$incident_dateonly_extinguished<-ifelse(fire_20$incident_dateonly_extinguished=="",NA,fire_20$incident_dateonly_extinguished)
fire_20$month_end<-ifelse(is.na(fire_20$incident_dateonly_extinguished)==T,fire_20$month_created,month(fire_20$incident_dateonly_extinguished))
fire_20$month_created<-as.numeric(fire_20$month_created)
fire_20$month_end<-as.numeric(fire_20$month_end)

fire_20$duration_mo<-fire_20$month_end-fire_20$month_created
fire_20$months<-list(fire_20$month_created,fire_20$month_end)
master<-list()
for(i in 1:nrow(fire_20)){
  test<-list()
  test<-seq(fire_20$month_created[i],fire_20$month_end[i],1)
  master[[i]]<-test
}
fire_20$months<-master

### Make histogram of # of events per month
fire_hist<-fire_20%>%group_by(month)
p <- ggplot(fire_20, aes(x=month_created)) + 
  geom_histogram(bins=11)
p
q <- ggplot(fire_20, aes(x=month_end)) + 
  geom_histogram(breaks=seq(1,12,1)) +
  scale_x_continuous(breaks=seq(1,12,1)) +
  labs(y="Wildfire Events", x="Month") +
  ggtitle("Count of Wildfire Events by Month") +
  theme(plot.title = element_text(hjust=0.5))
q

pm<-fread("9_DELIVERABLES/data/CA_wildfire_PM25_PurpleAir_540km.csv")
sensors<-unique(pm[,c("ID","Lon","Lat","Location")])
sensors_in<-sensors[which(sensors$Location=="inside"),]
sensors_out<-sensors[which(sensors$Location=="outside"),]

pm_all<-fread("7_output/CA_PM25_allPurpleAir_2020.csv")
pm_all$date_only<-as.POSIXct(as.numeric(as.character(pm_all$created_at)), origin="1970-01-01", tz="GMT")
pm_all$date_only<-as.Date(pm_all$date_only,origin="1970-01-01",format='%Y-%m-%d')

finf_dat<-fread('7b_analytic_datasets/WF_finf_dataset.csv')

# Find all dates for wildfires
fire_dates<-fire_20[,c("incident_name","incident_id","incident_longitude",
                       "incident_latitude","incident_dateonly_created","incident_dateonly_extinguished")]
date_ranges<-fire_dates
names(date_ranges)[5:6]<-c("start_dates","end_dates")
date_ranges<-date_ranges[!duplicated(date_ranges),]
row.names(date_ranges)<-1:nrow(date_ranges)
for(i in 1:nrow(date_ranges)){
  date_ranges$end_dates[i]<-ifelse(date_ranges$end_dates[i]=="",date_ranges$start_dates[i],date_ranges$end_dates[i])
}

target_sf<-as.data.frame(unique(pm$ID))
names(target_sf)<-c("ID")
target_sf<-unique(merge(target_sf,pm[,c("ID","Lon","Lat")],by="ID"))

# Find all sensors within 540km of wildfire for each date range

site_coordinates_crs<-CRS("+proj=longlat +datum=WGS84")

# Dataframe of wildfire coords
point_sf <- st_as_sf(date_ranges, coords = c("incident_longitude", "incident_latitude"),crs=site_coordinates_crs)
# Set the projection to EPSG 4326 (long-lat)
st_crs(point_sf) <- 4326

# Filter for SCU Lightning Complex, August Complex, North Complex, CZU Lightning Complex
mywfs<-list("SCU Lightning Complex","CZU Lightning Complex (Including Warnella Fire) ",
            "August Complex (includes Doe Fire)", "Creek Fire")
point_sf<-point_sf[which(point_sf$incident_name %in% mywfs),]

# Specify the source of X and Y coordinates
# Dataframe of monitor X,Y coords
target_sf <- st_as_sf(target_sf, coords = c("Lon", "Lat"),crs=site_coordinates_crs)
# Set the projection to EPSG 4326 (long-lat)
st_crs(target_sf) <- 4326

#st_crs(coordinates)
#coordinates_aes<-st_transform(coordinates,crs(nlcd)) 

# Loop through point_sf row by row to get a df for each wildfire
# Save df from each loop iteration to df list mydfs

mydfs<-list()
for(i in 1:nrow(point_sf)){
  target_sf2 <- target_sf %>% # target_sf stays the same
    mutate(Dist = as.numeric(st_distance(point_sf[c(i),], target_sf, by_element = TRUE))) %>%
    # Filter the records with Dist <= 540000 (m)
    filter(Dist <= 540000)
  # # Reattach wildfire data to df of monitors within 540km of wildfire i
  if((nrow(target_sf2)>0)==T){
    target_sf2$incident_id<-point_sf$incident_id[i]
    target_sf2$incident_name<-point_sf$incident_name[i]
    target_sf2$start_date<-point_sf$start_dates[i]
    target_sf2$end_date<-point_sf$end_dates[i]
    #pm_temp<-pm%>%filter(ID %in% target_sf2$ID & date_only >= target_sf2$start_date[1] & date_only <= target_sf2$end_date[1])
    mydfs[[i]]<-target_sf2
  }
}

dfbind<-rbindlist(mydfs)#17060 obs
dfbind2<-dfbind%>%group_by(ID)%>%summarise(min(Dist))
names(dfbind2)[2]<-c("Dist")
dfbind3<-left_join(dfbind2,dfbind,by=c("ID","Dist"))
write.csv(dfbind3,'7b_analytic_datasets/PA_monitors_wildfires_Dec21.csv',na="",row.names=F)

# sensor ID 1

dfbind3<-fread('7b_analytic_datasets/PA_monitors_wildfires_Dec21.csv')
names(dfbind3)[2]<-c("Dist")
names(dfbind3)[1]<-c("ID")
names(dfbind3)[3]<-c("Lon")
names(dfbind3)[4]<-c("Lat")
dfbind2<-fread('7b_analytic_datasets/CA_PM25_allPurpleAir_2020.csv')

dfbind4<-merge(dfbind3,dfbind2[,c("ID","Name","Temperature_F_A","Humidity_%_A")],by="ID",all.x=F,all.y=F,no.dups=T)
names(dfbind4)[10]<-c("Temperature")
names(dfbind4)[11]<-c("Humidity")
dfbind5<-dfbind4%>%group_by(ID,Name,Dist,Lon,Lat,incident_id,incident_name,start_date,end_date)%>%summarise_at(vars(Temperature,Humidity),mean,na.rm=T)

write.csv(dfbind5,'9_DELIVERABLES/PA_WF_monitors_geocoded.csv',na="",row.names=F)

ID1 = 1004

d2<-pm[which(pm$ID==ID1),] #wildfire-associated observations 
timeseries1a<-ts(d2[,c("PM2.5_CF_ATM_ug/m3_A")])
plot.ts(timeseries1a)

d2_all<-pm_all[which(pm_all$ID==ID1),] #all observations 

tt <- ts(d2_all[,c("PM2.5_CF_ATM_ug/m3_A")],start=c(2020,1), frequency=24*365)

xts1 <- xts(d2_all[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_all$date_only)
plot.xts(xts1,main=paste("Monitor ID ",ID1, " - CZU Lightning Complex"),ylab="PM2.5 Channel A (ug/m3)")


# CZU Lightning Complex: 8/16 - 9/22
d2_during<-d2_all[which(d2_all$date_only >= "2020-07-01" & d2_all$date_only <= "2020-10-31"),]
xts1_during <- xts(d2_during[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_during$date_only)
plot.xts(xts1_during,main=paste("Monitor ID",ID1,"During WF Season (CZU LC)"),ylab="PM2.5 Channel A (ug/m3)")

d2_before<-d2_all[which(d2_all$date_only < "2020-07-01"),]
xts1_before <- xts(d2_before[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_before$date_only)
plot.xts(xts1_before,main=paste("Monitor ID",ID1,"Before WF Season (CZU LC)"),ylab="PM2.5 Channel A (ug/m3)")

d2_after<-d2_all[which(d2_all$date_only > "2020-10-31"),]
xts1_after <- xts(d2_after[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_after$date_only)
plot.xts(xts1_after,main=paste("Monitor ID",ID1,"Before WF Season (CZU LC)"),ylab="PM2.5 Channel A (ug/m3)")


# sensor ID 2

ID1 = 1186

d2<-pm[which(pm$ID==ID1),] #wildfire-associated observations 
timeseries1a<-ts(d2[,c("PM2.5_CF_ATM_ug/m3_A")])
plot.ts(timeseries1a)

d2_all<-pm_all[which(pm_all$ID==ID1),] #all observations 

tt <- ts(d2_all[,c("PM2.5_CF_ATM_ug/m3_A")],start=c(2020,1), frequency=24*365)

xts1 <- xts(d2_all[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_all$date_only)
plot.xts(xts1,main=paste("Monitor ID ",ID1, " - August Complex"),ylab="PM2.5 Channel A (ug/m3)")


# August Complex: 8/16 - 11/11
d2_during<-d2_all[which(d2_all$date_only >= "2020-07-01" & d2_all$date_only <= "2020-10-31"),]
xts1_during <- xts(d2_during[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_during$date_only)
plot.xts(xts1_during,main=paste("Monitor ID",ID1,"During WF Season (August C)"),ylab="PM2.5 Channel A (ug/m3)")

d2_before<-d2_all[which(d2_all$date_only < "2020-07-01"),]
xts1_before <- xts(d2_before[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_before$date_only)
plot.xts(xts1_before,main=paste("Monitor ID",ID1,"Before WF Season (August C)"),ylab="PM2.5 Channel A (ug/m3)")

d2_after<-d2_all[which(d2_all$date_only > "2020-10-31"),]
xts1_after <- xts(d2_after[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_after$date_only)
plot.xts(xts1_after,main=paste("Monitor ID",ID1,"After WF Season (August C)"),ylab="PM2.5 Channel A (ug/m3)")

# sensor ID 11416

ID1 = 11416

d2<-pm[which(pm$ID==ID1),] #wildfire-associated observations 
timeseries1a<-ts(d2[,c("PM2.5_CF_ATM_ug/m3_A")])
plot.ts(timeseries1a)

d2_all<-pm_all[which(pm_all$ID==ID1),] #all observations 

tt <- ts(d2_all[,c("PM2.5_CF_ATM_ug/m3_A")],start=c(2020,1), frequency=24*365)

xts1 <- xts(d2_all[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_all$date_only)
plot.xts(xts1,main=paste("Monitor ID ",ID1, " - SCU Lightning Complex"),ylab="PM2.5 Channel A (ug/m3)")


# August Complex: 8/18 - 10/01
d2_during<-d2_all[which(d2_all$date_only >= "2020-07-01" & d2_all$date_only <= "2020-10-31"),]
xts1_during <- xts(d2_during[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_during$date_only)
plot.xts(xts1_during,main=paste("Monitor ID",ID1,"During WF Season (SCU LC)"),ylab="PM2.5 Channel A (ug/m3)")

d2_before<-d2_all[which(d2_all$date_only < "2020-07-01"),]
xts1_before <- xts(d2_before[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_before$date_only)
plot.xts(xts1_before,main=paste("Monitor ID",ID1,"Before WF Season (SCU LC)"),ylab="PM2.5 Channel A (ug/m3)")

d2_after<-d2_all[which(d2_all$date_only > "2020-10-31"),]
xts1_after <- xts(d2_after[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_after$date_only)
plot.xts(xts1_after,main=paste("Monitor ID",ID1,"After WF Season (SCU LC)"),ylab="PM2.5 Channel A (ug/m3)")

# sensor ID 40639

ID1 = 40639

d2<-pm[which(pm$ID==ID1),] #wildfire-associated observations 
timeseries1a<-ts(d2[,c("PM2.5_CF_ATM_ug/m3_A")])
plot.ts(timeseries1a)

d2_all<-pm_all[which(pm_all$ID==ID1),] #all observations 

tt <- ts(d2_all[,c("PM2.5_CF_ATM_ug/m3_A")],start=c(2020,1), frequency=24*365)

xts1 <- xts(d2_all[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_all$date_only)
plot.xts(xts1,main=paste("Monitor ID ",ID1, " - Dam Fire"),ylab="PM2.5 Channel A (ug/m3)")


# Dam Fire: 7/30 - 8/14
d2_during<-d2_all[which(d2_all$date_only >= "2020-07-01" & d2_all$date_only <= "2020-10-31"),]
xts1_during <- xts(d2_during[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_during$date_only)
plot.xts(xts1_during,main=paste("Monitor ID",ID1,"During WF Season (Dam Fire)"),ylab="PM2.5 Channel A (ug/m3)")

d2_before<-d2_all[which(d2_all$date_only < "2020-07-01"),]
xts1_before <- xts(d2_before[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_before$date_only)
plot.xts(xts1_before,main=paste("Monitor ID",ID1,"Before WF Season (Dam)"),ylab="PM2.5 Channel A (ug/m3)")

d2_after<-d2_all[which(d2_all$date_only > "2020-10-31"),]
xts1_after <- xts(d2_after[,c("PM2.5_CF_ATM_ug/m3_A")], order.by=d2_after$date_only)
plot.xts(xts1_after,main=paste("Monitor ID",ID1,"After WF Season (Dam)"),ylab="PM2.5 Channel A (ug/m3)")

