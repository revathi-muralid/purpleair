
rm(list=ls())

library(data.table)
library(tidyverse)

d3<-fread('/nas/longleaf/home/revathi/purpleair/9_DELIVERABLES/data/CA_WF_PM25_allPurpleAir_540km_May22.csv')

pm_all<-fread("7_output/CA_PM25_allPurpleAir_2020.csv")

pm_all<-d3
pm_all$date_only<-as.POSIXct(as.numeric(as.character(pm_all$created_at)), origin="1970-01-01", tz="GMT")
pm_all$date_only<-as.Date(pm_all$date_only,origin="1970-01-01",format='%Y-%m-%d')

### QA OF DOWNLOAD

# Check date range for every monitor

PA_dates_all0<-pm_all %>% select(ID, date_only)
PA_dates_all0<-PA_dates_all0[which(!duplicated(PA_dates_all0)),]

PA_dates_all <- PA_dates_all0 %>%
  group_by(ID) %>%
  summarise(
    start = min(date_only, na.rm = T),
    end = max(date_only, na.rm = T)
  ) %>%
  arrange(ID)

# Check operating time of every monitor to see if final N=394 monitors makes sense

PA_dates_all$op_time<-difftime(PA_dates_all$end, PA_dates_all$start, units="days")

# 4/3742 monitors had <3 weeks operating time
# 0/3742 monitors had 365 day operating time
# 2860/3742 monitors had >1 month operating time
# 1044/3742 monitors had >100 days operating time
# 0/3742 monitors had >200 days operating time
