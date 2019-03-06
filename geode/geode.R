
#import revised Geode field .csv

g=read.csv("geode/geode_points.csv")

#split Geode App field label (Description) into station, transect, type columns

library (tidyr)
g2=separate(data=g, col=Description, into=c("station","transect","type"))


#Create table for ArcMap import

write.csv(g2, "geode/geode_points_sep.csv")


#need to figure oout how to cut type=end rows and insert as new columns, currently proceeding in excel

#in Arc Map
#generated lines from points to calculate transect lengths 
g_t_length=read.csv("geode/geode_point_lines_exp.csv")
#calculated distance from USGS Wilcox gauge to transect start points
g_t_usgs=read.csv("geode/geode_point_trans_dist_usgs_exp.csv")
#calculated transect distances from eachother 
g_t_dist=read.csv("geode/geode_point_trans_dist_exp.csv")


#merge length table with USGS table to assign stations/transects to FID values
g_t_spatial=merge(g_t_length,g_t_usgs,by=c("FID"))
#clean table
g_t_spatial=subset(g_t_spatial, select=-c(Number.x,utm_e_st.x,utm_n_st.x,utm_e_en.x,utm_n_en.x,Field1,Number.y,Date,Time,Lat_st,Long_st,Lat_end,Long_end,NEAR_FID))

library(plyr)
g_t_spatial=rename(g_t_spatial, c("LENGTH"="g_t_lng_m","utm_e_st.y"="utm_e_st","utm_n_st.y"="utm_n_st","utm_e_en.y"="utm_e_en","utm_n_en.y"="utm_n_en","NEAR_DIST"="wilcox_t_st_dist_m"))
write.csv(g_t_spatial, "geode/g_t_spatial.csv")


#assign 2 sets of stations/transects to transect distance table
#IN_FID stations/transects
g_t_dist=rename(g_t_dist, c("IN_FID"="FID"))
g_t_dist=merge(g_t_dist,g_t_spatial,by=c("FID"))
g_t_dist=subset(g_t_dist, select=-c(FID,Rowid_,OBJECTID,g_t_lng_m,utm_e_st,utm_n_st,utm_e_en,utm_n_en,wilcox_t_st_dist_m))
g_t_dist=rename(g_t_dist, c("station"="station1","transect"="transect1"))
#NEAR_FID stations/transects
g_t_dist=rename(g_t_dist, c("NEAR_FID"="FID"))
g_t_dist=merge(g_t_dist,g_t_spatial,by=c("FID"))
g_t_dist=subset(g_t_dist, select=-c(FID,g_t_lng_m,utm_e_st,utm_n_st,utm_e_en,utm_n_en,wilcox_t_st_dist_m))
g_t_dist=rename(g_t_dist, c("NEAR_DIST"="dist_m","NEAR_RANK"="rank","station"="station2","transect"="transect2"))
g_t_dist=g_t_dist[with(g_t_dist,order(station1,transect1,rank)),]
write.csv(g_t_dist, "geode/g_t_dist.csv")
