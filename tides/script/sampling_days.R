### Choosing best days for sampling Lone Cabbage Reef Area ###

tides=read.csv("tides/noaa_predictions/noaa_tide_predict.csv",header=T)
head(tides)
str(tides)

tides=read.csv("tides/noaa_predictions/noaa_tide_predict.csv",header=T,
                  colClasses=c(rep("character",3),"character","numeric","character"))

#tides where LC reef should be exposed 
good_days=tides[which(tides$pred<=-1.45),]

#daylight hours
#good_times=good_days[which(good_days$time>"5:00"),]
