# Script for reading water quality data from FWC, FDACS, and Fraizer, 
# and combining into single file with columns:
#   Year
#   Month
#   Day
#   Lat [decimal degrees]
#   Lon [decimal degrees]
#   Depth [bottom depth, in m]
#   Bottom_temp [C]
#   Bottom_sal
#   Bottom_DO
#   Surface_temp [C; available only in FDACS]
#   Surface_sal
#   Surface_DO
#   pH
#   Data_source [FWC, FDACS, or Frazier]


# QUESTIONS:
#   - in FWC data, does Depth column in waterquality table mean bottom depth or measurement depth?
#   - in FWC data, what does Stratum column in physicaltable refer to? (A, B, D)
#   - in Frazier data, does Depth column mean bottom depth or measurement depth?
#   - in FDACS data, what are the units on W_depth column?


library(dplyr)
library(ggmap)
library(stringr)
library(raster)
library(rgdal)

# =========================================================================
# read in data; connect data file to location file
fwc_dat = read.csv('Cooperator_WQ/FWC_waterquality.csv')
fwc_loc = read.csv('Cooperator_WQ/FWC_physicaltable.csv',na.strings=c('','.'))
fwc = merge(fwc_dat,fwc_loc,by='Reference') %>% dplyr::select(Reference,Longitude,Latitude,Depth,Stratum,Temperature,Conductivity,pH,Salinity,DissolvedO2)

fdacs_dat = read.csv('Cooperator_WQ/FDACS_28SUWA_SEA.csv')
fdacs_loc = read.csv('Cooperator_WQ/FDACS_locations.csv')
fdacs = merge(fdacs_dat,fdacs_loc,by.x='Station',by.y='station') %>% dplyr::select(Station,lat,lon,Date_samp,W_depth,Surface_temp,Surface_sal,Surface_DO,Bottom_temp,Bottom_sal,Bottom_DO,pH)

fraz = read.csv('Cooperator_WQ/frazier_suwannee_97_15.csv',na.strings=c('','.')) %>% dplyr::select(LATITUDE,LONGITUDE,YYYY,MM,DD,DEPTH..m.,TEMP...C.,SAL.......,DO..mg.L.,pH)

# ======================================================================
# add year, month, day columns to data frames; standardize column names

# FWC data
fwc$Year = substr(fwc$Reference,4,7) %>% as.numeric()
fwc$Month = substr(fwc$Reference,8,9) %>% as.numeric()
fwc$Day = substr(fwc$Reference,10,11) %>% as.numeric()
fwc = rename(fwc,Lon=Longitude,
             Lat=Latitude)
# find deepest measurement for each Reference ID
fwc_bottom_surface = data.frame()
numsamps = c()
nosurface=c()
for (ID in unique(fwc$Reference)) {
  samples = dplyr::filter(fwc,Reference==ID)
  numsamps = rbind(numsamps,dim(samples)[1])
  bottom = samples[samples$Depth==max(samples$Depth),]
  if (dim(samples)[1] > 1 & min(samples$Depth)<.4) {
    surface = samples[samples$Depth==min(samples$Depth),]
  } else {surface = data.frame(Temperature=NA,Salinity=NA,DissolvedO2=NA); if(min(samples$Depth)>=.4) {nosurface=rbind(nosurface,ID)}}
  dat = data.frame(Reference=ID,Year=bottom$Year,Month=bottom$Month,Day=bottom$Day,Lon=bottom$Lon,Lat=bottom$Lat,Depth=bottom$Depth,
                   Bottom_temp=bottom$Temperature,Bottom_sal=bottom$Salinity,Bottom_DO=bottom$DissolvedO2,
                   Surface_temp=surface$Temperature,Surface_sal=surface$Salinity,Surface_DO=surface$DissolvedO2,
                   pH=bottom$pH)
  fwc_bottom_surface = rbind(fwc_bottom_surface,dat)
}
fwc_bottom_surface$Data_source = rep("FWC")
fwc_bottom_surface = fwc_bottom_surface[order(fwc_bottom_surface$Reference),]



# FDACS data
fdacs$Date_samp = as.character(fdacs$Date_samp) %>% str_pad(6, pad = "0")
# year is in yy format: convert to yyyy
fdacs$Year = substr(fdacs$Date_samp,1,2) %>% as.numeric()
fdacs$Year[fdacs$Year<50] = fdacs$Year[fdacs$Year<50] + 2000
fdacs$Year[(fdacs$Year>50 & fdacs$Year<100)] = fdacs$Year[(fdacs$Year>50 & fdacs$Year<100)] +1900

fdacs$Month = substr(fdacs$Date_samp,3,4) %>% as.numeric()
fdacs$Day = substr(fdacs$Date_samp,5,6) %>% as.numeric()
fdacs$Data_source = rep('FDACS')
fdacs = rename(fdacs,Lon=lon,
               Lat=lat)
# data is recorded without decimal points, needs to be divided by 10
fdacs$Surface_temp = fdacs$Surface_temp/10
fdacs$Surface_sal = fdacs$Surface_sal/10
fdacs$Surface_DO = fdacs$Surface_DO/10
fdacs$Bottom_DO = fdacs$Bottom_DO/10
fdacs$Bottom_sal = fdacs$Bottom_sal/10
fdacs$Bottom_temp = fdacs$Bottom_temp/10
fdacs$W_depth = fdacs$W_depth/10
fdacs$pH = fdacs$pH/10
# placeholder - how to handle depth
fdacs = rename(fdacs,Depth=W_depth)
fdacs = fdacs[order(fdacs$Year,fdacs$Month,fdacs$Day,fdacs$Depth),]

# Frazier data
fraz = rename(fraz,Lon=LONGITUDE,
              Lat=LATITUDE,
              Year=YYYY,
              Month=MM,
              Day=DD,
              Depth=DEPTH..m.,
              Bottom_temp=TEMP...C.,
              Bottom_sal=SAL.......,
              Bottom_DO=DO..mg.L.)
fraz$Surface_temp = rep(NA)
fraz$Surface_sal = rep(NA)
fraz$Surface_DO = rep(NA)
fraz$Data_source = rep("Frazier")
fraz = fraz[order(fraz$Year,fraz$Month,fraz$Day,fraz$Depth),]


gpspoints = rbind(fwc_bottom_surface[,c('Year','Month','Day','Lat','Lon','Depth','Data_source')],
                  fdacs[,c('Year','Month','Day','Lat','Lon','Depth','Data_source')],
                  fraz[,c('Year','Month','Day','Lat','Lon','Depth','Data_source')])

waterquality = rbind(fwc_bottom_surface[,c('Year','Month','Day','Lat','Lon','Depth','Bottom_temp','Bottom_sal','Bottom_DO','Surface_temp','Surface_sal','Surface_DO','pH','Data_source')],
                     fdacs[,c('Year','Month','Day','Lat','Lon','Depth','Bottom_temp','Bottom_sal','Bottom_DO','Surface_temp','Surface_sal','Surface_DO','pH','Data_source')],
                     fraz[,c('Year','Month','Day','Lat','Lon','Depth','Bottom_temp','Bottom_sal','Bottom_DO','Surface_temp','Surface_sal','Surface_DO','pH','Data_source')])

#write.csv(waterquality,'Cooperator_WQ/CooperatorWaterQuality.csv',row.names=F,quote=F)

# ====================================================================
# plot locations
midpoint = data.frame(lon=mean(gpspoints$Lon),lat=mean(gpspoints$Lat))

# map what we have
mapgilbert <- get_map(location = midpoint, zoom = 11,
                      maptype = "satellite", scale = 2)


# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = gpspoints, aes(x = Lon, y = Lat, fill = Depth, colour=Data_source), alpha = 0.3, size = 3, shape = 21) +
  scale_fill_gradient(low='white',high='black')




# ================================================================
# making a spatial grid 

# projection in UTM for cedar key
#data = data.frame(X=gpspoints$lon,Y=gpspoints$lat)
#coordinates(data) <- c("X", "Y")
#proj4string(data) <- CRS("+proj=longlat +datum=WGS84")
#cedarkey_crs=CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
#data_utm <- spTransform(data, cedarkey_crs)
#xmin = data_utm@bbox[1,1]
#xmax = data_utm@bbox[1,2]
#ymin = data_utm@bbox[2,1]
#ymax = data_utm@bbox[2,2]
