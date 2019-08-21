#### stratify epoch 3 stations into live oyster density based quartiles  ####

library(dplyr)

#import Bill's file with compiled transect counts converted to densities for epoch 3
trans=read.csv("oyster/transect/data/LC_transect_data_2018_2019.csv")
trans2=trans %>%
  select(season:strata) %>%
  filter(!(locality=="LC" & site=="O"))

#group stations by quartiles and seperate
summary(trans2$density) #insert these values into next line of code

trans2$quart=ifelse(trans2$density>100.42,(ifelse(trans2$density>203.16,(ifelse(trans2$density>283.69,4,3)),2)),1)

quart1=trans2[which(trans2$quart==1),]
quart1$count=seq(1,11)

quart2=trans2[which(trans2$quart==2),]
quart2$count=seq(1,11)

quart3=trans2[which(trans2$quart==3),]
quart3$count=seq(1,11)

quart4=trans2[which(trans2$quart==4),]
quart4$count=seq(1,11)

#random draw of 4 reefs per strata
sample(1:11,4,replace=F)

#repeat above code 4X for each quartile strata

#### Reef Draw:
#Quartile 1:  LCI8, LCI19, LCN8, LCN3
#Quartile 2:  LCI12, LTI1, NNI1, LCI6
#Quartile 3:  LTI6, BTI1, LTI5, LCI1
#Quartile 4:  LCN7, BTI5, LCI2, BTI6

# after building grids in Arc for selected reefs above (clip reef to fishnet label point file, add xy), 
# randomly draw 4 points for each reef using code below.  Extract/compile points, create final shapefile in Arc.

grid=read.csv("oyster/reefmass/data/reefmass_grid.csv")

#create station column
grid$station=with(grid,paste0(Locality,Site,Bar))

library(plyr)
grid_draw=ddply(grid,.(station),function(x) x[sample(nrow(x),4),])





