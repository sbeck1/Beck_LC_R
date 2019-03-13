quad <- read.csv("quadrat_combined.csv", header = T)
names <- read.csv("station_name_change_124.csv", header = T)

#put epoch in for the quad data
for(i in 1:nrow(quad)){
  year <- quad$Year[i]
  ifelse(year < 2013, quad$Epoch[i] <- 1, 
         ifelse(year < 2016, quad$Epoch[i] <- 2, quad$Epoch[i] <- 3))
}

#combine data from quad file with new station names
for (i in 1:nrow(quad)){
  e <- quad$Epoch[i]
  oldID <- as.character(quad$Substation[i])
  newID <- as.character(names$New_Station[names$Epoch == e & names$Original_Station == oldID])
  if(length(newID)>1)print(i)
  if(newID == "Same"){newID <- oldID}
  quad$NewStation[i] <- newID
}


#errors that need to be manually fixed
################

#station HBN5
#epoch 1 - sampling date 12/8/2019 - station name should be HBN2 - mistake in entry
quad$NewStation[quad$Substation == "HBN5" & quad$Date == "12/8/10"]  <- "HBN2"

write.csv(quad, "quadrat_combined_newstations.csv")
