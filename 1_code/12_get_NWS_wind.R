

rm(list=ls())

#TracMyAir uses the API web service from weather.gov. Their information site 
#is https://www.weather.gov/documentation/services-web-api

library(httr)
library(jsonlite)
library(data.table)
library(RANN)

setwd('/nas/longleaf/home/revathi/purpleair/6b_wind/')

#To get weather data, first we get the list of stations closest to a lat-lon 
#coordinate: https://api.weather.gov/points/35.8801,-78.788/stations

dat<-fread('/nas/longleaf/home/revathi/purpleair/2_sensor_lists/WF_io_pairs_540km_Dec21.csv')
names(dat)<-c("out_ID","dist","lon","lat","in_ID")
dat$lon<-gsub("c","",dat$lon)
dat$lon<-gsub("\\(","",dat$lon)
dat$lat<-gsub("\\)","",dat$lat)


#http://weather.rap.ucar.edu/surface/stations.txt
stations<-fread('UCAR_stations.csv')

dist<-as.data.frame(RANN::nn2(stations[,c(8,11)],dat[,c(4,3)],k=1))
dist2<-cbind(dist,dat[,c("out_ID","in_ID","lon","lat")])
unique(dist$nn.idx)
mysites<-c(32,107,166,93,163,106,191,83,23,200,115,126,7,129)
mystns<-stations[c(mysites),]
for(i in 1:nrow(dist2)){
  myrow<-dist2$nn.idx[i]
  dist2$STATION<-stations$STATION[myrow]
  dist2$ICAO<-stations$ICAO[myrow]
  dist2$IATA<-stations$IATA[myrow]
  dist2$LAT_DEC<-stations$LAT_DEC[myrow]
  dist2$LON_DEC<-stations$LON_DEC[myrow]
  dist2$ELEV<-stations$ELEV[myrow]
}

# Get wind speed data


for(i in 1){
  mylon<-dat$lon[i]
  mylat<-dat$lat[i]
  stn_api=paste("https://api.weather.gov/points/",mylat,",",mylon,"/stations",sep="")
  res = GET(stn_api)
  stations = fromJSON(rawToChar(res$content))
  stations<-stations$features
  for(j in 1:length(stations$properties$stationIdentifier)){
    mystations<-stations$properties$stationIdentifier
    obs_api=paste("https://api.weather.gov/stations/",mystations[j],"/observations?start=2020-01-01T00%3A00%3A00Z&end=2022-02-01T00%3A00%3A00Z",sep="")
    res2 = GET(obs_api)
    myobs = fromJSON(rawToChar(res2$content))
    myobs$features$properties$timestamp
  }
}

# for(i in 1){
#   mylon<-dat$lon[i]
#   mylat<-dat$lat[i]
#   stn_api=paste("https://api.weather.gov/points/",mylat,",",mylon,"/stations",sep="")
#   res = GET(stn_api)
#   stations = fromJSON(rawToChar(res$content))
#   stations<-stations$features
#   for(j in 1:length(stations$properties$stationIdentifier)){
#     mystations<-stations$properties$stationIdentifier
#     obs_api=paste("https://api.weather.gov/stations/",mystations[j],"/observations?start=2020-01-01T00%3A00%3A00Z&end=2022-02-01T00%3A00%3A00Z",sep="")
#     res2 = GET(obs_api)
#     myobs = fromJSON(rawToChar(res2$content))
#     myobs$features$properties$timestamp
#   }
# }

# This returns a list of the nearest stations to the given point, e.g. KRDU, KTTA.
# Then for a given station, we can get observations within a time period:

names(res2)
obs<-res2$request

# The start and end date are GMT and formatted as YYYY-MM-DDThh:mm:ssZ, with the 
# colons URL-encoded (%3A). This query returns a list of observations which includes 
# temperature, wind speed, humidity, etc.

