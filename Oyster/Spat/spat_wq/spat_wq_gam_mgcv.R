#### Modeling monthly spat count relationships to monthly wq variables of interest ####

library(dplyr)
library(readr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(mgcv)


#download latest wq.csv file from: https://github.com/LCRoysterproject/water_quality_shiny/tree/master/wq_app/data
#run beck salinity script to reduce wq dataset to monthly variables of interest, import file 
wq=read.csv("oyster/spat/spat_wq/sal_temp.csv")

#match wq date range to spat date range
wq=wq[-which(wq$year<2018),]
wq=wq[-which(wq$year==2018&wq$month<6),]
wq=wq[-which(wq$year>2019),]

#remove stations with no spat collectors
#wq=wq[-which(wq$Site>10),]

#run beck tile summary script, import summary file
spat=read.csv("oyster/spat/report/report_table_data/tile_summary3.csv")

#match month column name
spat$month=spat$month_num
#numeric station value from full label so spat matches wq
spat$Site=parse_number(spat$station)
spat$Site=as.integer(spat$Site)

str(wq)
str(spat)

#merge wq and spat files, remove additional wq rows that don't match spat
spat=spat[with(spat,order(year,month,Site)),]
wq=wq[with(wq,order(year,month,Site)),]

spat2=merge(wq,spat,by=c("year","month","Site"))

#explore data distributions

#density plots
ggdensity(spat2$total,
          main="spat_count")
ggdensity(spat2$rel_total,
          main="rel_spat_count")
ggdensity(spat2$temp_mean,
          main="temp_mean")
ggdensity(spat2$sum_25,
          main="temp readings > 25C")
ggdensity(spat2$sal_mean,
          main="sal_mean")
ggdensity(spat2$sum_5,
          main="sal readings > 5ppt")
ggdensity(spat2$sum_15,
          main="sal readings > 15ppt")

#qqplots
ggqqplot(spat2$total,
          main="spat_count")
ggqqplot(spat2$rel_total,
          main="rel_spat_count")
ggqqplot(spat2$temp_mean,
          main="temp_mean")
ggqqplot(spat2$sum_25,
          main="temp readings > 25C")
ggqqplot(spat2$sal_mean,
          main="sal_mean")
ggqqplot(spat2$sum_5,
          main="sal readings > 5ppt")
ggqqplot(spat2$sum_15,
          main="sal readings > 15ppt")

#normality test
shapiro.test(spat2$total)
shapiro.test(spat2$rel_total)
shapiro.test(spat2$temp_mean)
shapiro.test(spat2$sum_25)
shapiro.test(spat2$sal_mean)   #only monthly mean salinity normal
shapiro.test(spat2$sum_5)
shapiro.test(spat2$sum_15)


#plot total spat for each variable of interest
par(mfrow=c(1,1))
plot(spat2$temp_mean,spat2$total) #monthly mean temp
plot(spat2$sal_mean,spat2$total)  #monthly mean salinity
plot(spat2$sum_25,spat2$total)    #number monthly temp meas > 25C (common oys reproduction threshold)
plot(spat2$sum_5,spat2$total)     #number monthly salinity meas > 5ppt (common oys reproduction threshold)
plot(spat2$sum_15,spat2$total)    #number monthly salinity meas > 15ppt (common oys predation threshold)

#####GAM#####
#determine which combination of sal and temp metrics best predicts total spat, repeat for relative total spat
#running each with 2 predictors (1 sal metric and 1 temp metric) therefor all df=2

#gam1: sal_mean, temp_mean 
#gam2: sum_5, temp_mean 
#gam3: sum 15, temp_mean 
#gam4: mean_sal, sum_25   *** best model using gam package
#gam5: sum_5, sum_25
#gam6: sum 15, sum_25

#### Response = Total spat (s)

#starting with best model from gam package 

s_gam4=gam(total~s(sal_mean)+s(sum_25),method="REML",data=spat2)
par(mfrow=c(2,2)) 
gam.check(s_gam4)
summary(s_gam4)
