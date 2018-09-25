####  Copy data from "tile_spat_count_entry.xlxs" into "tile_spat_count.csv"

tile=read.csv("oyster/spat/data/development/tile_spat_count.csv") 
tile=tile[which(tile$cnt>=0),] 


#Simple summaries to look for general errors (typos)...

##### Station Check #####

summary(tile$station)

##### Date Range Check #####

summary(tile$date_dep)
summary(tile$date_ret)
summary(tile$date_prc)

##### Month Check #####

summary(tile$month)

##### Tile Side Check #####

summary(tile$side)

##### Tile Count Check #####

hist(tile$cnt)
cnt_check=tile[which(tile$cnt>1000),]

##### Max Size Check #####

mx1=tile[which(tile$mx_mm1>0),]
hist(mx1$mx_mm1)
mx1_check=mx1[which(mx1$m_mm1>50),]

mx2=tile[which(tile$mx_mm2>0),]
hist(mx2$mx_mm2)
mx2_check=mx2[which(mx2$m_mm2>50),]

mx3=tile[which(tile$mx_mm3>0),]
hist(mx3$mx_mm3)
mx3_check=mx3[which(mx3$m_mm3>50),]

##### Barnacle check #####

summary(tile$barnacle)

##### status check #####

summary(tile$status)

##### multiple recruitment check #####

summary(tile$mult_rec)

##### Other check #####

summary(tile$other)
