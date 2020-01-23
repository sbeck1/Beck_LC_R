#### Modeling monthly spat count relationships to monthly wq variables of interest ####

library(dplyr)
library(readr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(gam)


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
#gam4: mean_sal, sum_25 
#gam5: sum_5, sum_25
#gam6: sum 15, sum_25

#### Response = Total spat (s)
s_gam1=gam(total~s(sal_mean,df=2)+s(temp_mean,df=2),data=spat2)
summary(s_gam1)
par(mfrow=c(1,2)) 
plot(s_gam1,se = TRUE)
#AIC: 2267.667
#parm: *****BOTH predictors sig at p < 0.001
#nonparm: sal predictor signifant (sal_mean, p<0.01)
### rerun with no temp spline
s_gam1_2=gam(total~s(sal_mean,df=2)+temp_mean,df=2,data=spat2)
summary(s_gam1_2)
plot(s_gam1_2,se = TRUE)
#AIC: 2266.097
### rerun with no splines
s_gam1_3=gam(total~sal_mean,df=2+temp_mean,df=2,data=spat2)
summary(s_gam1_3)
plot(s_gam1_3,se = TRUE)
#AIC: 2286.984 

s_gam2=gam(total~s(sum_5,df=2)+s(temp_mean,df=2),data=spat2)
summary(s_gam2)
plot(s_gam2,se = TRUE)
#AIC: 2271.599
#parm: *****BOTH predictors sig at p < 0.001
#nonparm: sal predictor signifant (sum_5, p<0.05) 
### rerun with no temp spline
s_gam2_2=gam(total~s(sum_5,df=2)+temp_mean,df=2,data=spat2)
summary(s_gam2_2)
plot(s_gam2_2,se = TRUE)
#AIC: 2270.721
### rerun with no splines
s_gam2_3=gam(total~sum_5,df=2+temp_mean,df=2,data=spat2)
summary(s_gam2_3)
plot(s_gam2_3,se = TRUE)
#AIC: 2286.504

s_gam3=gam(total~s(sum_15,df=2)+s(temp_mean,df=2),data=spat2)
summary(s_gam3)
plot(s_gam3,se = TRUE)
#AIC: 2272.345
#parm: *****BOTH predictors sig at p < 0.001
#nonparm: sal predictor signifant (sum_15 p<0.01)
###  with no temp spline
s_gam3_2=gam(total~s(sum_15,df=2)+temp_mean,df=2,data=spat2)
summary(s_gam3_2)
plot(s_gam3_2,se = TRUE)
#AIC: 2271.22
### rerun with no splines
s_gam3_3=gam(total~sum_15,df=2+temp_mean,df=2,data=spat2)
summary(s_gam3_3)
plot(s_gam3_3,se = TRUE)
#AIC: 2293.114 

s_gam4=gam(total~s(sal_mean,df=2)+s(sum_25,df=2),data=spat2)
summary(s_gam4)
plot(s_gam4,se = TRUE)
#AIC: 2261.733
#parm: *****BOTH predictors sig at p < 0.001
#nonparm: *****BOTH sal and temp predictors significant (sal_mean p<0.05,sum_25 p<0.001)
### no rerun 


s_gam5=gam(total~s(sum_5,df=2)+s(sum_25,df=2),data=spat2)
summary(s_gam5)
plot(s_gam5,se = TRUE)
#AIC: 2270.258
#parm: *****BOTH predictors sig at p < 0.001
#nonparm: temp predictor significan (sum_25 p<0,05)
### rerun with no sal spline
s_gam5_2=gam(total~sum_5,df=2+s(sum_25,df=2),data=spat2)
summary(s_gam5_2)
plot(s_gam5_2,se = TRUE)
#AIC: 2286.504

s_gam6=gam(total~s(sum_15,df=2)+s(sum_25,df=2),data=spat2)
summary(s_gam6)
plot(s_gam6,se = TRUE)
#AIC: 2266.864   
#parm:     *****BOTH sal and temp predictors significant (sum_15 p<0.001,sum_25 p<0.01)
#nonparm:  *****BOTH sal and temp predictors significant (sum_15 p<0.05,sum_25 p<0.001)
### no rerun




#### Response =  RELATIVE Total spat (rs)
rs_gam1=gam(rel_total~s(sal_mean,df=2)+s(temp_mean,df=2),data=spat2)
summary(rs_gam1)
par(mfrow=c(1,2)) 
plot(rs_gam1,se = TRUE)
#AIC: -154.7902, 
#parm: temp predictor sig p<0.001
#nonparm: no predictors signifant
### rerun with no splines
rs_gam1_2=gam(rel_total~sal_mean,df=2+temp_mean,df=2,data=spat2)
summary(rs_gam1_2)
plot(rs_gam1_2,se = TRUE)
#AIC: -143.6238

rs_gam2=gam(rel_total~s(sum_5,df=2)+s(temp_mean,df=2),data=spat2)
summary(rs_gam2)
plot(rs_gam2,se = TRUE)
#AIC: -155.0751
#parm: temp predictor sig p<0.001
#nonparm: no predictors signifant
### rerun with no splines
rs_gam2_2=gam(rel_total~sum_5,df=2+temp_mean,df=2,data=spat2)
summary(rs_gam2_2)
plot(rs_gam2_2,se = TRUE)
#AIC: -143.4622 

rs_gam3=gam(rel_total~s(sum_15,df=2)+s(temp_mean,df=2),data=spat2)
summary(rs_gam3)
plot(rs_gam3,se = TRUE)
#AIC: -154.9957
#parm: temp predictor sig p<0.001
#nonparm: no predictors signifant
####rerun with no splines
rs_gam3_2=gam(rel_total~sum_15,df=2+temp_mean,df=2,data=spat2)
summary(rs_gam3_2)
plot(rs_gam3_2,se = TRUE)
#AIC: -143.5518 

rs_gam4=gam(rel_total~s(sal_mean,df=2)+s(sum_25,df=2),data=spat2)
summary(rs_gam4)
plot(rs_gam4,se = TRUE)
#AIC: -152.0325
#parm: temp predictor sig p<0.001
#nonparm: no predictors signifant
####rerun with no splines
rs_gam4_2=gam(rel_total~sal_mean,df=2+sum_25,df=2,data=spat2)
summary(rs_gam4_2)
plot(rs_gam4_2,se = TRUE)
#AIC: -143.6238

rs_gam5=gam(rel_total~s(sum_5,df=2)+s(sum_25,df=2),data=spat2)
summary(rs_gam5)
plot(rs_gam5,se = TRUE)
#AIC: -152.3953
#parm: temp predictor sig p<0.001
#nonparm: no predictors signifant
####rerun with no splines
rs_gam5_2=gam(rel_total~sum_5,df=2+sum_25,df=2,data=spat2)
summary(rs_gam5_2)
plot(rs_gam5_2,se = TRUE)
#AIC: -143.4622

rs_gam6=gam(rel_total~s(sum_15,df=2)+s(sum_25,df=2),data=spat2)
summary(rs_gam6)
plot(rs_gam6,se = TRUE)
#AIC: -152.2699
#parm: temp predictor sig p<0.001
#nonparm: no predictors signifant
####rerun with no splines
rs_gam6_2=gam(rel_total~sum_15,df=2+sum_25,df=2,data=spat2)
summary(rs_gam6_2)
plot(rs_gam6_2,se = TRUE)
#AIC: -143.5518 

##### Best Predictor of Total Spat:  sal_mean,sum_25 (s_gam4)
##### Best Predictor of Relative Total Spat:  None, no model significant



