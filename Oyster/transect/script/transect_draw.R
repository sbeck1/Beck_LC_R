### Transect selection for post construction oyster sampling:  Question 2 ###

### For repeat draws, start at "Random Draw" step below

library (dplyr)

### WILD REEFS ###

### Determine area of reef in harvestable/non-harvestable area

reef=read.csv("oyster/transect/data/intertidal_reef_draw.csv")
reef_summary = reef %>%
  group_by(Harvest)%>%
  summarise(total=sum(POLY_AREA))
reef_summary2 = reef_summary %>%
  summarise(total=sum(total))
reef_summary$percent = (reef_summary$total/reef_summary2$total)*100

### Determine total lengths of epoch 1 transects by harvest strata

trans=read.csv("oyster/transect/data/trans_length.csv")
trans2=trans[which(trans$locality=="LC"),]
trans3=trans2[which(trans2$site=="I" | trans2$site=="N"),]

trans_max = trans3 %>%
  group_by(locality,site,bar) %>%
  slice(which.max(tran_length))

epoch1_nonharvest=trans_max[which(trans_max$station=="LCI1"),]

epoch1_harvest=trans_max[which(trans_max$station != "LCI1"),]
sum(epoch1_harvest$tran_length)

###  Winter 2018-2019

# Need 1056m of transect for epoch 3
# 46.6% in nonharvestable area (492.1m)
1056*.466
# 53.3% harvestable area (562.8m)
1056*.533

#Subtract epoch 1 transects
#nonharvestable:  
492.1 - 20
#harvestable:
562.8 - 200.6

### TOTAL TRANSECT LENGTH NEEDED FROM DRAW###
#nonharvestable: 472.1m
#harvestable: 362.2m

###  Winter 2019-2020

# Need 760m of transect for epoch 3
# 46.6% in nonharvestable area (354.7m)
760*.466
# 53.3% harvestable area (405.1m)
760*.533

#Subtract epoch 1 transects
#nonharvestable:  
354.7 - 20
#harvestable:
405.1 - 315.2

### TOTAL TRANSECT LENGTH NEEDED FROM DRAW###
#nonharvestable: 334.7m
#harvestable: 89.9m


### Random Draw 

### For repeat draws, start here.  harvest_trans.csv and nonharvest_trans.csv include all possible wild reef transects(2x2m grid) from Arc shapefile (wildreef_fishnet_clip_sep.shp) 

#nonharvestable area
nonharvest_trans=read.csv("oyster/transect/data/nonharvest_trans.csv")
nonharvest_trans=nonharvest_trans[which(nonharvest_trans$LENGTH>=10),]  #min transect length 10m per 11/15/18 email
nonharvest_rand=sample(nrow(nonharvest_trans))
nonharvest_trans2=nonharvest_trans[nonharvest_rand,1:10]  #randomizes ArcGIS output
#rename exported file below for each draw
write.csv(nonharvest_trans2, file="oyster/transect/data/nonharvest_trans_rand_2019.csv")

#harvestable area
harvest_trans=read.csv("oyster/transect/data/harvest_trans.csv")
harvest_trans=harvest_trans[which(harvest_trans$LENGTH>=10),]
harvest_rand=sample(nrow(harvest_trans))
harvest_trans2=harvest_trans[harvest_rand,1:10]
#rename exported file below for each draw
write.csv(harvest_trans2, file="oyster/transect/data/harvest_trans_rand_2019.csv")

# in exported files, each row is a transect. Start at the top and sum until target total transect length is reached. 
# Export to "transect_draw_final" file.  Format with the following columns:  station, transect, epoch, harvest, length, start_x, start_y, end_x, end_y
# Add Epoch 1/Spring 2018 transects: LCI1-1, LCI2, LCI3, LCI4, LCI5, LCI6, LCN1, LCN2, LCN3, LCN4, LCN5, LCN6, LCN9
# Import into Arc. Fill in missing values, ensuring unique bar/transect numbers from previous year draws


### LC REEF ###

#Transplant occurred on elements 2-4 so we excluded these

### Winter 2018-19

#proportional allocation of 1056m of transect by area
#Elements 5-12:  Non-harvest/Large Rock:  10,181 sq m   (37.2%)
#Elements 13-16: Non-harvest/Small Rock:  5,129 sq m    (18.8%)
#Elements 17-22: Harvest/Small Rock:      12,055 sq m   (44.1%)
#Total:                                   27,354 sq m

(10181/27354)*100
(5129/27354)*100
(12055/27354)*100

#transect length distribution:
1056*.372
1056*.188
1056*.441

#Elements 5-12:   392.8m  (18, 22m tansects)
        #Strata Length:  896.6 (41 22m segments)
        896.6/22
#Elements 13-16:  198.5m  (9, 22m transects)
        #Strata Length:  560.9 (25, 22m segments)
        560.9/22
#Elements 17-22:  465.7m  (22, 22m transects)
        #Strata Length:  1318.3 (60, 22m segments)
        1318.3/22

# if using 22m max transects
(1056*.372)/22
(1056*.188)/22
(1056*.441)/22

# LC Reef Random Draw        
        
#After splitting strata centerlines into 22m segments in ArcGiS:        
lc_seg=read.csv("oyster/transect/data/lc_centerline_seg.csv")
lc_seg5_12=lc_seg[which(lc_seg$ELEMENT=="str1"),]
lc_seg13_16=lc_seg[which(lc_seg$ELEMENT=="str2"),]
lc_seg17_22=lc_seg[which(lc_seg$ELEMENT=="str3"),]

#random draw elements 5-12
seg5_12_rand=sample(nrow(lc_seg5_12))
lc_seg5_12_2=lc_seg5_12[seg5_12_rand,1:5]  #randomizes ArcGIS output
trans_num=data.frame(TRANS_NUM=sample(1:5,41,replace=T))# 5 possible transects in each 22m reef segment 
trans_num$SEG=lc_seg5_12_2$SEG
lc_seg5_12_2=merge(lc_seg5_12_2,trans_num,by.x="SEG", sort = F)
lc_seg5_12_2$TOT_LEN=lc_seg5_12_2$LENGTH*lc_seg5_12_2$TRANS_NUM
write.csv(lc_seg5_12_2, file="oyster/transect/data/seg5_12_rand.csv")

#random draw elements 13-16
seg13_16_rand=sample(nrow(lc_seg13_16))
lc_seg13_16_2=lc_seg13_16[seg13_16_rand,1:5]  #randomizes ArcGIS output
trans_num=data.frame(TRANS_NUM=sample(1:5,25,replace=T))# 5 possible transects in each 22m reef segment 
trans_num$SEG=lc_seg13_16_2$SEG
lc_seg13_16_2=merge(lc_seg13_16_2,trans_num,by.x="SEG", sort = F)
lc_seg13_16_2$TOT_LEN=lc_seg13_16_2$LENGTH*lc_seg13_16_2$TRANS_NUM
write.csv(lc_seg13_16_2, file="oyster/transect/data/seg13_16_rand.csv")

#random draw elements 17-22
seg17_22_rand=sample(nrow(lc_seg17_22))
lc_seg17_22_2=lc_seg17_22[seg17_22_rand,1:5]  #randomizes ArcGIS output
trans_num=data.frame(TRANS_NUM=sample(1:5,60,replace=T))# 5 possible transects in each 22m reef segment 
trans_num$SEG=lc_seg17_22_2$SEG
lc_seg17_22_2=merge(lc_seg17_22_2,trans_num,by.x="SEG", sort = F)
lc_seg17_22_2$TOT_LEN=lc_seg17_22_2$LENGTH*lc_seg17_22_2$TRANS_NUM
write.csv(lc_seg17_22_2, file="oyster/transect/data/seg17_22_rand.csv")

### Winter 2019-20

#proportional allocation of 1049m of transect by area
#Elements 5-16:  Non-harvest/Large Rock:  15,310 sq m   (56%)
#Elements 17-22: Harvest/Small Rock:      12,055 sq m   (44%)
#Total:                                   27,355 sq m

(15310/27355)*100
(12055/27355)*100

#transect length distribution:
1049*.56
1049*.44

#Elements 5-16:   532.4m  (25, 22m tansects)
#Subtract epoch 1 transects
#nonharvestable:  
587.4 - 55
532.4/22

#Elements 17-22:  461.6m  (22, 22m transects)
#harvestable
461.6/22

# LC Reef Random Draw        

#After splitting strata centerlines into 22m segments in ArcGiS:        
lc_seg=read.csv("oyster/transect/data/lc_centerline_seg.csv")
lc_seg5_16=lc_seg[which(lc_seg$ELEMENT=="str1"|lc_seg$ELEMENT=="str2"),]
lc_seg17_22=lc_seg[which(lc_seg$ELEMENT=="str3"),]

#random draw elements 5-16
seg5_16_rand=sample(nrow(lc_seg5_16))
lc_seg5_16_2=lc_seg5_16[seg5_16_rand,1:5]  #randomizes ArcGIS output
trans_num=data.frame(TRANS_NUM=sample(1:5,66,replace=T))# 5 possible transects in each 22m reef segment 
trans_num$SEG=lc_seg5_16_2$SEG
lc_seg5_16_2=merge(lc_seg5_16_2,trans_num,by.x="SEG", sort = F)
lc_seg5_16_2$TOT_LEN=lc_seg5_16_2$LENGTH*lc_seg5_16_2$TRANS_NUM
write.csv(lc_seg5_16_2, file="oyster/transect/data/seg5_16_rand_2019.csv")

#random draw elements 17-22
seg17_22_rand=sample(nrow(lc_seg17_22))
lc_seg17_22_2=lc_seg17_22[seg17_22_rand,1:5]  #randomizes ArcGIS output
trans_num=data.frame(TRANS_NUM=sample(1:5,60,replace=T))# 5 possible transects in each 22m reef segment 
trans_num$SEG=lc_seg17_22_2$SEG
lc_seg17_22_2=merge(lc_seg17_22_2,trans_num,by.x="SEG", sort = F)
lc_seg17_22_2$TOT_LEN=lc_seg17_22_2$LENGTH*lc_seg17_22_2$TRANS_NUM
write.csv(lc_seg17_22_2, file="oyster/transect/data/seg17_22_rand_2019.csv")

# in exported files, each row is the north end of a plot, with the number of transects per plot and total lenght per plot shown. Start at the top and sum until target total transect length is reached. 
# Export to "transect_draw_final" file.  Format with the following columns:  station, transect, epoch, harvest, length, start_x, start_y, end_x, end_y
# Add Epoch 1 transects: LCO
# Import into Arc. Fill in missing values, ensuring unique bar/transect numbers from previous year draws