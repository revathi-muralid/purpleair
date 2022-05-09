url.csv.A <- "https://thingspeak.com/channels/553460/feeds.csv?api_key=1ZIXYGGG1W61J6OI&average=60&round=3&start=00:00:00&end=23:59:59&timezone=GMT"
url.csv.A.sec <- "https://thingspeak.com/channels/553463/feeds.csv?api_key=NYFHXX29D0MHNI9F&average=60&round=3&start=00:00:00&end=23:59:59&timezone=GMT"

d2<-fread(url.csv.A)

rm(list=ls())

setwd('purpleair')

sites<-read.csv('./sites_1.csv')

start_date <- as.Date('2020-01-01')
end_date <- as.Date('2020-01-03')

# Output directory
out.path <- getwd()
if (!file.exists(out.path)) {
  dir.create(out.path, recursive = T)
}

# Time zone
timezone <- 'GMT'

# Average level
# Don't use 'daily' since the time is UTC !!!
average <- 60

# Field names
# Primary
fieldnames.pri.A <- c("PM1.0_CF_1_ug/m3_A","PM2.5_CF_1_ug/m3_A","PM10.0_CF_1_ug/m3_A","Uptime_Minutes_A","RSSI_dbm_A","Temperature_F_A","Humidity_%_A","PM2.5_CF_ATM_ug/m3_A")
# Secondary
fieldnames.sec.A <- c("0.3um/dl_A","0.5um/dl_A","1.0um/dl_A","2.5um/dl_A","5.0um/dl_A","10.0um/dl_A","PM1.0_CF_ATM_ug/m3_A","PM10_CF_ATM_ug/m3_A")

#alldat<-data.frame()
#datalist=vector(mode = "list", length = nrow(sites))
foreach (i = 1 : nrow(sites)) %dopar% {
  name <- trimws(sites$Label[i]) # Remove Leading/Trailing Whitespace
  Lat <- sites$Lat[i]
  Lon <- sites$Lon[i]
  Location <- sites$DEVICE_LOCATIONTYPE[i]
  Type <- sites$Type[i]
  # Channel A (Primary)
  ID.A <- sites$ID[i]
  channelID.A <- sites$THINGSPEAK_PRIMARY_ID[i]
  channelKey.A <- sites$THINGSPEAK_PRIMARY_ID_READ_KEY[i]
  channelID.A.sec <- sites$THINGSPEAK_SECONDARY_ID[i]
  channelKey.A.sec <- sites$THINGSPEAK_SECONDARY_ID_READ_KEY[i]
  
  dat.final <- data.frame()
  
  for (j in 0 : as.numeric(end_date - start_date)) {
    
    this.day <- start_date + j
    cat(as.character(this.day), '\r')
    
    # --- Time range for a day ---
    starttime <- httpuv::encodeURI(paste(this.day, '00:00:00')) # UTC Time !!!
    endtime <- httpuv::encodeURI(paste(this.day, '23:59:59')) # UTC Time !!!
    
    # --- URL ---
    # Channel A
    url.csv.A <- paste("https://thingspeak.com/channels/", channelID.A, "/feeds.csv?api_key=", channelKey.A, '&average=', average, "&round=3&start=", starttime, "&end=", endtime, '&timezone=', timezone, sep = '')
    url.csv.A.sec <- paste("https://thingspeak.com/channels/", channelID.A.sec, "/feeds.csv?api_key=", channelKey.A.sec, '&average=', average, "&round=3&start=", starttime, "&end=", endtime, '&timezone=', timezone, sep = '')
    
    # --- Load CSV data ---
    # Download URL A
    url.idx <- T
    while (url.idx) {
      try.txt <- try(expr = { # Try to connect the link
        dat.A <- read.csv(url.csv.A)
      }, silent = T)
      closeAllConnections()
      if (!inherits(try.txt, 'try-error')) url.idx <- F
    }
    url.idx <- T
    while (url.idx) {
      try.txt <- try(expr = { # Try to connect the link
        dat.A.sec <- read.csv(url.csv.A.sec)
      }, silent = T)
      closeAllConnections()
      if (!inherits(try.txt, 'try-error')) url.idx <- F
    }
    
    # Change the column names
    names(dat.A)[2 : ncol(dat.A)] <- c(fieldnames.pri.A)
    names(dat.A.sec)[2 : ncol(dat.A.sec)] <- c(fieldnames.sec.A)
    
    # --- Combine data frame ---
    dat.final <- rbind(dat.final, dat.A)
    
  }
  
  # --- Add basic information --- #
  if (nrow(dat.final) != 0) {
    dat.final$ID <- ID.A
    dat.final$Name <- name
    dat.final$Lat <- Lat
    dat.final$Lon <- Lon
    dat.final$Location <- Location
    dat.final$Type <- Type
  }
  assign(paste("CA_PM25_",i,sep=""),dat.final)
  #alldat <<- rbind.data.frame(alldat, dat.final)
  #datalist[[i]] <- dat.final # add it to your list
  #assign("dfi",alldat,envir=globalenv())
  # --- Save CSV data --- 
  #assign(paste(ID.A,sep=""),dat.final, envir = globalenv() )
  #file.name <- paste(ID.A, '.csv', sep = '')
  #write.csv(dat.final, file.path(out.path, file.name), row.names = F)
  
}

#big_data = do.call("rbind",datalist)
