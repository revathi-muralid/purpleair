
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

dat<-fread('7b_analytic_datasets/CA_WF_PM25_pairedPurpleAir_540km.csv')

finfs<-fread('7b_analytic_datasets/site_specific_Finfs.csv')
names(finfs)<-c("indoor_ID","outdoor_ID","finf")

dat2<-merge(dat,finfs,by=c("indoor_ID","outdoor_ID"))

# Cig=Ci-Finf*Co
# Ci=total indoor conc
# Co=corresponding outdoor conc
# Cig=estimated conc of indoor generated PM2.5

dat2$cig_A<-dat2$in_CPM2.5_A-(dat2$finf*dat2$out_CPM2.5_A)

# Calculate proportion of indoor-generated PM2.5 (Cig/Ci*100%) at hourly level based on
# paired I/O concs and site-specific Finf est

dat2$pCig<-as.numeric(dat2$cig_A)/as.numeric(dat2$in_CPM2.5_A)

# Median of hourly proportions is treated as estimated long-term avg proportion of indoor-
#generated concs at this site

median(as.numeric(dat2$pCig)) #0.4575369

write.csv(dat2,'7b_analytic_datasets/exposure_analytic_dataset.csv',row.names=F,na="")

# Ratio b/w exp to indoor-gen PM2.5 and exp to outdoor-infiltrated PM2.5

# For this analysis, the ratios between exposure to indoor-generated
# PM2.5 and exposure to outdoor-infiltrated PM2.5 (exposure was defined to be 
# mean concentration multiplied by total exposure/measurement time), a metric of 
# exposure errors when using ambient PM2.5 concentrations as an exposure proxy, were
# shown against different concentrations of outdoor PM2.5. 

# We further estimated the lower bounds of outdoor PM2.5 concentrations at which the 
# indoor-generated PM2.5 concentrations were negligible for individual sites (<5% of 
# total indoor concentrations). These outdoor PM2.5 threshold values are important 
# indicators for personal exposure assessment, which can help further analyze exposure 
# misclassification at a finer scale. The outdoor threshold value was estimated by 
# fitting a LOWESS curve with outdoor PM2.5 concentrations as the independent variable 
# and indoor source contributions (proportions) as the dependent variable. 
