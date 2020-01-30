#### stratify epoch 3 stations into live oyster density based quartiles  ####

library(dplyr)

#import Bill's file with compiled transect counts converted to densities for epoch 3
trans=read.csv("oyster/transect/data/LC_transect_data_2018_2019.csv")

#remove LCO, no reef mass samples on rocks
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
#Quartile 2:  LCI6, LTI1, NNI1, LCI2
#Quartile 3:  LTI6, BTI1, LTI5, LCI1
#Quartile 4:  LCN7, BTI5, BTI6, LCI2

trans3=trans2[which(trans2$station=="LCI8"|
                      trans2$station=="LCI19"|
                      trans2$station=="LCN8"|
                      trans2$station=="LCN3"|
                      trans2$station=="LCI12"|
                      trans2$station=="LTI1"|
                      trans2$station=="NNI1"|
                      trans2$station=="LCI6"|
                      trans2$station=="LTI6"|
                      trans2$station=="BTI1"|
                      trans2$station=="LTI5"|
                      trans2$station=="LCI1"|
                      trans2$station=="LCN7"|
                      trans2$station=="BTI5"|
                      trans2$station=="BTI6"|
                      trans2$station=="LCI2"),]
write.csv(trans3,"oyster/cultchmass/data/development/cultchmass_reefdraw.csv")


# after building grids in Arc for selected reefs above (start with LC_reefmass.mxd, clip reef to fishnet label point file, add xy), 
# randomly draw 4 points for each reef using code below.  Extract/compile points, create final shapefile in Arc.

grid=read.csv("oyster/cultchmass/data/development/supp_mass_grid.csv")

#create station column
grid$station=with(grid,paste0(Locality,Site,Bar))

library(plyr)
grid_draw=ddply(grid,.(STATION),function(x) x[sample(nrow(x),3),])
#only do this step once!  import exported file below to maintain original draw.

#rename exported file above and import
grid_draw2=read.csv("oyster/cultchmass/data/development/grid_draw.csv")

#add labels for Arc
grid_draw$quadrat=seq(1,3)

grid_draw$label=with(grid_draw,paste(STATION,quadrat),sep="-")

#export for Arc
write.csv(grid_draw,"oyster/cultchmass/data/development/supp_mass_draw.csv")

# Had to fix issue with LCI2 (Definitely should be Quartile 4, not 2...got swapped in Arc)




