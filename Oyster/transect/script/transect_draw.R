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

# Need 1056 m of transect for epoch 3
# 46.6% in nonharvetable area (492.1)
1056*.466
# 53.3% harvstable area (562.8)
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
nonharvest_rand=sample(nrow(nonharvest_trans))
nonharvest_trans2=nonharvest_trans[nonharvest_rand,1:10]
write.csv(nonharvest_trans2, file="oyster/transect/data/nonharvest_trans_rand.csv")

#harvestable area
harvest_trans=read.csv("oyster/transect/data/harvest_trans.csv")
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
#Elements 13-16:  198.5m  (9, 22m transects)
#Elements 17-22:  465.7m  (22, 22m transects)

# if using 22m max transects
(1056*.372)/22
(1056*.188)/22
(1056*.441)/22