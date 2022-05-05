
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

# For calculating WF-associated Finf, uncomment the following line:
pm_paired<-read.csv('7b_analytic_datasets/CA_WF_PM25_pairedPurpleAir_540km_May22.csv')

# For calculating non-WF Finf, uncomment the following line:
pm_paired<-fread('7b_analytic_datasets/CA_nonWF_PM25_pairedPurpleAir_540km.csv')

#########################################################################################

dat<-pm_paired

#dat<-fread('7b_analytic_datasets/CA_WF_PM25_pairedPurpleAir_540km.csv')

#dat$in_CPM2.5_AB<-rowMeans(dat[,c(in_CPM2.5_A,in_CPM2.5_B)], na.rm=TRUE)
dat$in_CPM2.5_AB<-rowMeans(dat[,c(63,64)], na.rm=TRUE)
dat$out_CPM2.5_AB<-rowMeans(dat[,c(54,55)], na.rm=TRUE)

dat$ratio_AB<-as.numeric(dat$in_CPM2.5_AB)/as.numeric(dat$out_CPM2.5_AB)
#dat$ratio_B<-as.numeric(dat$in_CPM2.5_B)/as.numeric(dat$out_CPM2.5_B)

# Exclude records with an I/O ratio>1.2
dat2<-dat[which(dat$ratio_AB<1.2),] #135k-->125k

# dat2$`out_PM2.5_CF_1_ug/m3_A`<-as.numeric(dat2$`out_PM2.5_CF_1_ug/m3_A`)
# dat2$ratio<-as.numeric(dat2$ratio)
# dat2<-dat2[which(dat2$ratio!=Inf),]

# We used the linear regression method widely adopted in previous
# studies (Ott et al., 2000) as a baseline reference 

# A linear regression model was fitted with outdoor PM2.5 concentrations as the 
# independent variable and total indoor concentrations as the dependent variable, 
# in which the fitted slope was the estimation of Finf. 


# Required the maximum outdoor PM2.5 concentrations to exceed 8 converted mass 
# concentration units (the 95th percentile of outdoor concentrations) 

quantile(c(dat2$out_CPM2.5_AB),0.95) #22.3799    CMCUs

max_outs<-dat2%>%group_by(ID)%>%summarise(max_out = max(c(out_CPM2.5_A,out_CPM2.5_B), na.rm = T))
drops<-max_outs[which(max_outs$max_out<21.04708 ),]$ID

`%notin%` <- Negate(`%in%`)
#dat2<-dat2%>%filter(ID %notin% drops) 

# LOWESS: For each I/O monitor pair, N pairs of hourly I/O measurements
# with the highest outdoor PM2.5 concentrations were first selected.

n_pairs<-dat2[which(dat2$out_CPM2.5_A>91.5),]

# ratio is the dependent variable
# outdoor PM2.5 measurements is the independent variable
linear_finf<-lm(ratio_AB~out_CPM2.5_AB, data=dat2)
linear_finf<-lm(in_CPM2.5_AB~out_CPM2.5_AB, data=dat2)
summary(linear_finf)

# y-intercept is the Finf
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    4.562e-01  6.989e-04   652.7   <2e-16 ***
#   out_CPM2.5_AB -9.198e-03  5.632e-05  -163.3   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2971 on 253679 degrees of freedom
# Multiple R-squared:  0.09515,	Adjusted R-squared:  0.09515 
# F-statistic: 2.668e+04 on 1 and 253679 DF,  p-value: < 2.2e-16

# function to obtain slope from the data
slope <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$coefficients[2])
}
# bootstrapping with 1000 replications
results <- boot(data=dat2, statistic=slope,
                R=1000, formula=out_CPM2.5_AB~ratio_AB)

# view results
plot(results, main=NA, sub=NA)

mean(results$t)
median(results$t)
quantile(results$t,0.25)
quantile(results$t,0.75)
min(results$t)
max(results$t)

########################################################################################

pairs<-fread('2_sensor_lists/WF_io_pairs_540km_May22.csv')
names(pairs)<-c("outdoor_ID","Dist","geom1","geom2","indoor_ID")

# site-specific Finf estimates

site_finfs<-list()
for(i in 1:nrow(pairs)){
  mypair<-pairs[c(i),]
  
  tdat<-dat2[which(dat2$ID %in% mypair$outdoor_ID | dat2$ID %in% mypair$indoor_ID),]
  
  if(nrow(tdat)>0){
    
    results <- boot(data=tdat, statistic=slope,
                    R=1000, formula=in_CPM2.5_A~out_CPM2.5_A)
    
    tdf<-as.data.frame(mypair$indoor_ID)
    names(tdf)<-c("in_ID")
    
    tdf$out_ID<-mypair$outdoor_ID
    tdf$finf<-mean(results$t)
    
    site_finfs[[i]]<-tdf
  }
  
}

site_finf_final<-rbindlist(site_finfs)

write.csv(site_finf_final,'7b_analytic_datasets/site_specific_Finfs_May22.csv',row.names=F,na="")

########################################################################################

# Bi et al. got N=20 via a sensitivity analysis

# A bootstrap sample (with a sample size of N) was then obtained
# from these N measurements to fit a LOWESS curve. The mean value
# of the fitted LOWESS curve was treated as the Finf estimate of this
# bootstrap sample. Finally, the bootstrapping and LOWESS fitting
# steps were repeated M times to obtain M bootstrap Finf estimates

options(na.action="na.exclude")

lmFinf = lm(out_PM2.5_CF_1_ug.m3_A~ratio, data = dat2) #Create the linear regression
summary(lmFinf) #Review the results

