### Transect selection for post construction oyster sampling:  Question 2 ###

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

### Determine max lengths of epoch 1 transects in harvestable area

trans=read.csv("oyster/transect/data/trans_length.csv")
trans2=trans[which(trans$locality=="LC"),]
trans3=trans2[which(trans2$site=="I" | trans2$site=="N"),]

trans_max = trans3 %>%
  group_by(locality,site,bar) %>%
  slice(which.max(tran_length))

epoch1_nonharvest=trans_max[which(trans_max$station=="LCI1"),]

epoch1_harvest=trans_max[which(trans_max$station != "LCI1"),]
sum(epoch1_harvest$tran_length)

###

# Need 1056m of transect for epoch 3
# 46.6% in nonharvetable area (492.1m)
1056*.466
# 53.3% harvstable area (562.8m)
1056*.533

#Subtract epoch 1 transects
#nonharvestable:  
492.1 - 20
#harvestable:
562.8 - 200.6

### TOTAL TRANSECT LENGTH NEEDED FROM DRAW###
#nonharvestable: 472.1m
#harvestable: 362.2m



### Random Draw 

#nonharvestable area
nonharvest_trans=read.csv("oyster/transect/data/nonharvest_trans.csv")
nonharvest_trans=nonharvest_trans[which(nonharvest_trans$LENGTH>=10),]  #min transect length 10m per 11/15/18 email
nonharvest_rand=sample(nrow(nonharvest_trans))
nonharvest_trans2=nonharvest_trans[nonharvest_rand,1:10]  #randomizes ArcGIS output
write.csv(nonharvest_trans2, file="oyster/transect/data/nonharvest_trans_rand.csv")

#harvestable area
harvest_trans=read.csv("oyster/transect/data/harvest_trans.csv")
harvest_trans=harvest_trans[which(harvest_trans$LENGTH>=10),]
harvest_rand=sample(nrow(harvest_trans))
harvest_trans2=harvest_trans[harvest_rand,1:10]
write.csv(harvest_trans2, file="oyster/transect/data/harvest_trans_rand.csv")



### LC Reef proportional allocation of 1056m of transect by area
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


