#### Create a file with transect oyster densities and coordinates for Arc import

#import Bill's file with compiled transect counts converted to densities for epoch 3
trans=read.csv("oyster/transect/data/LC_transect_data_2018_2019.csv")

#import coordinates from master geospatial file
coord=read.csv("oyster/transect/data/trans_bar_coord.csv")

#remove addtional transects per bar since densities are collapsed
coord=coord[which(coord$transect==1),]

trans_coord=merge(trans,coord,by=c("station"))
write.csv(trans_coord,"oyster/transect/data/trans_bar_coord_dens.csv")
