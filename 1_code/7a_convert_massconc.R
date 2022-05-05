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

setwd('purpleair')

pm_all<-fread("7_output/CA_PM25_allPurpleAir_2020.csv")

d1<-read.csv('9_DELIVERABLES/data/CA_WF_PM25_allPurpleAir_540km_May22.csv')
d2<-d1

#########################################################################################

### NON-WF ASSOCIATED OBS - GET d5 FROM RUNNING ENTIRE SCRIPT FOR WF-ASSOCIATED OBS

`%nin%` = Negate(`%in%`)
pm_nonwf<-pm_all[which(pm_all$ID %nin% d5$ID),]
d2<-pm_nonwf

#########################################################################################

# Exclude PM2.5 data from older single-channel monitors 

#qc<-d2%>%group_by(ID)%>%summarise(all(is.na(d2$`PM2.5_CF_1_ug/m3_B`==T)))

# No single-channel monitors

# Convert 6 size cut-off bins to 6 size categories
# 1) 0.3-0.5 um, 2) 0.5-1 um, 3) 1-2.5 um, 4) 2.5-5 um, 5) 5-10 um, 6) >10 um
# https://www2.purpleair.com/community/faq#hc-primary-and-secondary-data-header

# PM2.5 sample includes particle number densities per dL in 3 smallest categories

### Mass concentration conversion from number densities - Wallace et al. 2020
# Cbin=Nbin*4/3*pi*(dbin/2)^3*rhoH2O
# Nbin=number density per dL in specific size cat
# dbin=arithmetic mid-point diam of size range - particle arithmetic mean diam=mid-point of size range
# rhoH2O=water density (1000 kg/cubic m)
# Cbin=converted mass concentration in size cat

d2$'0.3-0.5um_A'<-d2$`X0.3um.dl_A`-d2$`X0.5um.dl_A`
d2$'0.5-1.0um_A'<-d2$`X0.5um.dl_A`-d2$`X1.0um.dl_A`
d2$'1.0-2.5um_A'<-d2$`X1.0um.dl_A`-d2$`X2.5um.dl_A`

d2$'0.3-0.5um_B'<-d2$`X0.3um.dl_B`-d2$`X0.5um.dl_B`
d2$'0.5-1.0um_B'<-d2$`X0.5um.dl_B`-d2$`X1.0um.dl_B`
d2$'1.0-2.5um_B'<-d2$`X1.0um.dl_B`-d2$`X2.5um.dl_B`

### NON-WF

d2$'0.3-0.5um_A'<-d2$`0.3um/dl_A`-d2$`0.5um/dl_A`
d2$'0.5-1.0um_A'<-d2$`0.5um/dl_A`-d2$`1.0um/dl_A`
d2$'1.0-2.5um_A'<-d2$`1.0um/dl_A`-d2$`2.5um/dl_A`

d2$'0.3-0.5um_B'<-d2$`0.3um/dl_B`-d2$`0.5um/dl_B`
d2$'0.5-1.0um_B'<-d2$`0.5um/dl_B`-d2$`1.0um/dl_B`
d2$'1.0-2.5um_B'<-d2$`1.0um/dl_B`-d2$`2.5um/dl_B`

# #/dL * (dl/10,000 ug) * (1e9 ug/kg) * (1000 kg/m^3) * (1e-6 m^3/um^3) * (um^3)

d2$cbin_0.5_A<-(d2$'0.3-0.5um_A')*(4/3)*pi*((0.4/2)^3)*1/100 # was 1/100 before this
d2$cbin_1.0_A<-(d2$'0.5-1.0um_A')*(4/3)*pi*((0.75/2)^3)*1/100
d2$cbin_2.5_A<-(d2$'1.0-2.5um_A')*(4/3)*pi*((1.75/2)^3)*1/100

d2$cbin_0.5_B<-(d2$'0.3-0.5um_B')*(4/3)*pi*((0.4/2)^3)*1/100
d2$cbin_1.0_B<-(d2$'0.5-1.0um_B')*(4/3)*pi*((0.75/2)^3)*1/100
d2$cbin_2.5_B<-(d2$'1.0-2.5um_B')*(4/3)*pi*((1.75/2)^3)*1/100

# Exclude zero readings in size cats 0.3-0.5 um and 0.5-1 um

d2[which(d2$cbin_0.5_A<=0),]$cbin_0.5_A<-NA
d2[which(d2$cbin_1.0_A<=0),]$cbin_1.0_A<-NA
d2[which(d2$cbin_2.5_A<0),]$cbin_2.5_A<-NA

d2[which(d2$cbin_0.5_B<=0),]$cbin_0.5_B<-NA
d2[which(d2$cbin_1.0_B<=0),]$cbin_1.0_B<-NA
d2[which(d2$cbin_2.5_B<0),]$cbin_2.5_B<-NA

# Add particle mass concentrations in 3 smallest size cats
#CPM2.5 = C0.3-0.5um + C0.5-1um + C1-2.5um

d2$CPM2.5_A=d2$cbin_0.5_A+d2$cbin_1.0_A+d2$cbin_2.5_A
d2$CPM2.5_B=d2$cbin_0.5_B+d2$cbin_1.0_B+d2$cbin_2.5_B

# Multiply converted mass concentration units by 3.0 - empirical factor to reflect more realistic scale of PM2.5 in ug/cubic m
# All quantitative analyses were based on original CMCU, unaffected by adjustment

d2$CPM2.5_adj_A=d2$CPM2.5_A*3
d2$CPM2.5_adj_B=d2$CPM2.5_B*3

#######################################################################################

# Filter PM2.5 samples based on agreement of dual-channel readings

d2$A_B_0.5_diff<-abs(d2$cbin_0.5_A-d2$cbin_0.5_B)/d2$cbin_0.5_A
d2$A_B_1.0_diff<-abs(d2$cbin_1.0_A-d2$cbin_1.0_B)/d2$cbin_1.0_A
d2$A_B_2.5_diff<-abs(d2$cbin_2.5_A-d2$cbin_2.5_B)/d2$cbin_2.5_A

d2$A_B_CPM2.5_diff<-abs(d2$CPM2.5_adj_A-d2$CPM2.5_adj_B)/d2$CPM2.5_adj_A

# Remove samples w/ dual-channel difference >30%

d3<-d2[which(d2$A_B_CPM2.5_diff<=0.3),] # 7.7 million-->5.5 million

# Sensitivity analysis

# Exclude monitors with total operating time of < 3 weeks over entire study period
d3$date_only<-as.POSIXct(as.numeric(as.character(d3$created_at)), origin="1970-01-01", tz="GMT")
d3$date_only<-as.Date(d3$date_only,origin="1970-01-01",format='%Y-%m-%d')

PA_dates0<-d3 %>% select(ID, date_only)
PA_dates0<-PA_dates0[which(!duplicated(PA_dates0)),]

PA_dates <- PA_dates0 %>%
  group_by(ID) %>%
  summarise(
    start = min(date_only, na.rm = T),
    end = max(date_only, na.rm = T)
  ) %>%
  arrange(ID)

PA_dates$op_time<-difftime(PA_dates$end, PA_dates$start, units="days") #2871 monitors
PA_dates<-PA_dates[which(PA_dates$op_time>=21),] #2654 monitors

d4<-d3[which(d3$ID %in% PA_dates$ID),] #5.5 mill-->5.4 mill

# Identify potentially mislabeled indoor/outdoor monitors based on temp and humidity 
# Outdoor monitors should have larger variations in T and H

sensors<-d4%>%group_by(ID,Location,Lon,Lat)%>%summarise(min_T=min(Temperature_F_A), max_T=max(Temperature_F_A),min_H=min(`Humidity_%_A`),max_H=max(`Humidity_%_A`))

# Remove indoor monitors that show significant variations in T and H
# Remove outdoor monitors w/ narrow range of T (e.g. 70-80 deg F)

sensors$diff_T<-sensors$max_T-sensors$min_T
nrow(sensors[which(sensors$Location=="inside"),]) #245 in/75 non
nrow(sensors[which(sensors$Location=="outside"),]) #2408 out/644 non
sensors<-sensors[which((sensors$diff_T>20 & sensors$Location=="outside")|(sensors$diff_T<40 & sensors$Location=="inside")),] #2276-->2093
nrow(sensors[which(sensors$Location=="inside"),]) #207 in/39 non
nrow(sensors[which(sensors$Location=="outside"),]) #2199 out/459 non

# 1 sensor has minimum temperature that doesn't make sense (old analytic dataset)
# ID 19727 (old)

sensors<-sensors[which(sensors$ID!=19727),]

d5<-d4[which(d4$ID %in% sensors$ID),] #5.4 milli-->4.8 milli


d5_in_s<-c(d5[which(d5$Location=="inside"),]$CPM2.5_A, d5[which(d5$Location=="inside"),]$CPM2.5_B)
d5_out_s<-c(d5[which(d5$Location=="outside"),]$CPM2.5_A, d5[which(d5$Location=="outside"),]$CPM2.5_B)

summary(d5_in_s)
summary(d5_out_s)

d5_in_ug_s<-c(d5[which(d5$Location=="inside"),]$`PM2.5_CF_ATM_ug/m3_A`, d5[which(d5$Location=="inside"),]$`PM2.5_CF_ATM_ug/m3_B`)
d5_out_ug_s<-c(d5[which(d5$Location=="outside"),]$`PM2.5_CF_ATM_ug/m3_A`, d5[which(d5$Location=="outside"),]$`PM2.5_CF_ATM_ug/m3_B`)

summary(d5_in_ug_s)
summary(d5_out_ug_s)

summary(d5[which(d5$Location=="inside"),]$Temperature_F_A)
summary(d5[which(d5$Location=="inside"),]$`Humidity_%_A`)
summary(d5[which(d5$Location=="outside"),]$Temperature_F_A)
summary(d5[which(d5$Location=="outside"),]$`Humidity_%_A`)

fwrite(d5,'7b_analytic_datasets/nonWF_associated_data_May22.csv')

d5<-fread('7b_analytic_datasets/PA_monitors_wildfires_Dec21.csv')
d6<-fread('7b_analytic_datasets/WF_associated_data_Dec21.csv')
dat100<-d6[which(d6$ID %in% d5$V1 | d6$ID %in% d5$ID),]
length(unique(d6[which(d6$`PM2.5_CF_1_ug/m3_B`>100),]$ID)) #2041 monitors

paired<-fread('7b_analytic_datasets/CA_WF_PM25_pairedPurpleAir_540km_Dec21.csv')
