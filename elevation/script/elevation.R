#import precon bathymetric survey data exported from ArcMap
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
