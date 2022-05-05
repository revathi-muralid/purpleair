# clear workspace

rm(list=ls())

# Set working directory

setwd('Documents/PurpleAir')

# Load functions

source("load_purpleair.R", echo = TRUE)


if (!require('rgdal')) {
  install.packages('rgdal')
  library(rgdal)
}


getPurpleairLst('Documents/PurpleAir')
d1<-read.csv('sensorlist_2021-03-04.csv') # n=32226

# Throw out rows that don't have indoor/outdoor location specified
#d1<-d1[which(is.na(d1$DEVICE_LOCATIONTYPE)==F),] #n=16035 - sensor count halved!

# Read in California shapefile\

ca_shp<-readOGR(dsn="ca_shp",layer="ca_shp_wgs84gcs")

# proj4string(mollcoords) <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")
# wgs<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# wgscoords <- spTransform(mollcoords, wgs)
# wgscoords<-coordinates(wgscoords)

# Subset to CA bounding box

d2 <- subset(d1, Lon >= ca_shp@bbox[1, 1] &
               Lon <= ca_shp@bbox[1, 2] &
               Lat >= ca_shp@bbox[2, 1] &
               Lat <= ca_shp@bbox[2, 2]) #19193 obs

ca_d1<-cutByShp(ca_shp, d1, 'Lat', 'Lon') #18853 obs

write.csv(ca_d1, 'sensorlist_ca_2021-04-08.csv', row.names=FALSE)

