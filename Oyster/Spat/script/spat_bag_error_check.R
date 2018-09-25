####  Copy data from "shell_spat_bag_entry.xlxs" into "shell_spat_bag.csv"

shell_bag=read.csv("oyster/spat/data/development/shell_spat_bag.csv") 
shell_bag2=shell_bag[which(shell_bag$status=="RETR"&shell_bag$drill_cnt>=0),]  #filters to include only successfully retrieved collectors
str(shell_bag2)



#Simple summaries to look for general errors (typos)...

##### Station Check #####

summary(shell_bag$station)

##### Date Range Check #####

summary(shell_bag2$date_dep)
summary(shell_bag2$date_ret)

##### Month Check #####

summary(shell_bag2$month)

##### Status Check #####

summary(shell_bag2$status)

##### Recorder Check #####

summary(shell_bag2$recorder)

##### Volume Check #####

hist(shell_bag2$t_vol_ml)   #look for outliers, set range below which will create datasets that contain extreme values
vol_check1=shell_bag2[which(shell_bag2$t_vol_ml>400)]
vol_check2=shell_bag2[which(shell_bag2$t_vol_ml<100)]

##### Weight Check #####

hist(shell_bag2$t_wt_g)     #look for outliers, set range below which will create datasets that contain extreme values
wt_check1=shell_bag2[which(shell_bag2$t_wt_g>600)]
wt_check2=shell_bag2[which(shell_bag2$t_wt_g<200)]

##### Drill Check #####

hist(shell_bag2$drill_cnt)  #look for outliers, set range below which will create datasets that contain extreme values
drill_check1=shell_bag2[which(shell_bag2$drill_cnt>10)]

##### Crab Check #####

hist(shell_bag2$crab_cnt)  #look for outliers, set range below
crab_check1=shell_bag2[which(shell_bag2$crab_cnt>20)]


