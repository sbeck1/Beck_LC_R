####import precon bathymetric survey data exported from ArcMap
bathy=read.csv("elevation/data/development/contractor/precon_bathy_survey/precon_bathy_utm_exp.csv")
#points within elements and labeled
bathy_clip=read.csv("elevation/data/development/contractor/precon_bathy_survey/precon_bathy_utm_clip_exp.csv")
#combine
bathy=merge(bathy_clip, bathy, by=c("point"), all=TRUE)

#clean/format
bathy$locality="LC"
bathy$site="O"
bathy$elev_m=bathy$depth_ft*0.3048
bathy=subset(bathy,select=-c(FID.x,FID.y,depth_ft))
bathy=bathy[with(bathy,order(point)),]
bathy=rename(bathy, c("point"="field_point","POINT_X"="easting","POINT_Y"="northing"))
bathy=bathy[c("field_point","easting","northing","elev_m","date","locality","site","bar","station")]
write.csv(bathy,"elevation/data/development/contractor/precon_bathy_survey/precon_bathy_format.csv")
#manually added to "elevation_prod.csv"



####import epoch 1 elevation data, manual cleaning:  un-needed columns removed, remaining columns renamed
e1=read.csv("elevation/data/development/uf/epoch1/epoch1_elevation_pf.csv")

#remove stations with no elevation data
e1[e1==""]=NA
e1_clean=e1[complete.cases(e1),]

summary(e1_clean$station)

#rename stations
e1_clean$station=as.character(e1_clean$station)
e1_clean$station[e1_clean$station=='LCN3']<-'LCN6'
e1_clean$station[e1_clean$station=='LCN4']<-'LCN3' #no elevation data
e1_clean$station[e1_clean$station=='LCN5']<-'LCN9'
e1_clean$station[e1_clean$station=='LCO1']<-'LCN9B' #no elevation data
e1_clean$station[e1_clean$station=='LCO2']<-'LCN8A' #no elevation data

e1_clean$station=as.factor(e1_clean$station)

write.csv(e1_clean, "elevation/data/development/uf/epoch1/epoch1_elevation_clean.csv")
#manual changes:  rename bars, remove duplicate elevation data from multiple oyster sampling trips (differing end lengths and repeat elevations)
#notes from manual cleanup: no 2.5m segment for CKO3, CRN1 data entered twice, deleted HBO1 due to differing elevations at same tran_length between dates/no way to choose which is correct

#final epoch 1 data:  epoch1_elevation_prod.csv


###prep table for Arc 
e_prod=read.csv("elevation/data/development/elevation_prod.csv")

#elevation size bins/no negative values allowed
e_neg=e_prod[which(e_prod$elev_m<=0),]
e_neg$neg_e_abs=abs(e_neg$elev_m)
#just postcon elevations

summary(e_neg$neg_e_abs)

library(FSA)
library(magrittr)
library(dplyr)
library(plotrix)
library(Matching)

e_neg %<>% mutate(elev_cat=lencat(neg_e_abs,w=0.1))
e_neg$elev_cat=-(e_neg$elev_cat)
#just postcon elevations
e_postcon=e_neg[which(e_neg$year>2017),]
write.csv(e_postcon, "elevation/data/development/e_postcon.csv")
#just epoch1 elevations
e_epoch1=e_neg[which(e_neg$year<2011),]
write.csv(e_epoch1, "elevation/data/development/e_epoch1.csv")


