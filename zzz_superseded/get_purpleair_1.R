
# clear workspace

rm(list=ls())

# Load packages

library(jsonlite)
library(httr)
library(rgdal)

# Set working directory

setwd('purpleair')

# Load functions

source("load_purpleair.R", echo = TRUE)

#Download hourly PM2.5 observations from Purple Air sensor network at: 
#https://www2.purpleair.com/
#You may find an API to help with the download, and be smart about it, 
#https://api.purpleair.com/#api-groups-get-groups-list
#Include all locations in California for the year 2020

# GET
# My API Keys:
# Read Key: E231FA19-7B6C-11EB-8C3A-42010A800259
# Write Key: E23306E9-7B6C-11EB-8C3A-42010A800259

# From API website:
#   GET https://api.purpleair.com/v1/sensors?fields=pm2.5_60minute&nwlng=-124.47&nwlat=42&selng=-114.117&selat=32.517 HTTP/1.1
# X-API-Key: E231FA19-7B6C-11EB-8C3A-42010A800259

sites <- read.csv('./sensorlist_ca_2021-03-04.csv', as.is = T)

source("purpleairDownloadRM.R", echo = TRUE)

purpleairDownloadRM('./sites_3.csv', '2020-01-01', '2020-12-31','./' ,60,
                      time.zone = "GMT", n.thread=4)

#data=fromJSON(rawToChar(res$content))
#df=as.data.frame(data$channel)

#fields of interest
#sensor_index, name, location_type, latitude, longitude, altitude, model,
#pm2.5_a, pm2.5_b, humidity_a, humidity_b, temperature_a, temperature_b,
#pressure_a, pressure_b, primary_key_a, primary_key_b, secondary_key_a, secondary_key_b,
#pm2.5, pm2.5_60minute, pm2.5_24hour, pm2.5_1week, time_stamp



