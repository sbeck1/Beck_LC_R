####  Copy data from "shell_spat_count_entry.xlxs" into "shell_spat_count.csv"

shell=read.csv("oyster/spat/data/development/shell_spat_count.csv") 
shell2=shell[which(shell$ex_cnt>=0),]  #filters to include only counted collectors
str(shell)



#Simple summaries to look for general errors (typos)...

##### Station Check #####

summary(shell2$station)

##### Date Range Check #####

summary(shell2$date_dep)
summary(shell2$date_ret)
summary(shell2$date_prc)

##### Month Check #####

summary(shell2$month)

##### Shell Number Check #####

hist(shell2$shell_num, breaks=c(0:10))

##### Shell Height Check #####

hist(shell2$sh_ht_mm)   #look for outliers, set range below which will create datasets that contain extreme values
ht_check1=shell2[which(shell2$sh_ht_mm>150),]
ht_check2=shell2[which(shell2$sh_ht_mm<50),]  

##### Shell Length Check #####

hist(shell2$sh_ln_mm)   #look for outliers, set range below which will create datasets that contain extreme values
ln_check1=shell2[which(shell2$sh_ln_mm>100),]
ln_check2=shell2[which(shell2$sh_ln_mm<20),]  

##### Shell Width Check #####

width=shell2[which(shell2$sh_wd_mm>=0),]
hist(width$sh_wd_mm)   #look for outliers, set range below which will create datasets that contain extreme values
wd_check1=width[which(width$sh_wd_mm>50),]

##### count check #####

hist(shell2$ex_cnt)
ex_count_check=shell2[which(shell2$ex_cnt>100),]
hist(shell2$in_cnt)
in_count_check=shell2[which(shell2$in_cnt>100),]  

##### barnacle check #####

summary(shell2$barnacle)

##### max size check #####

max_size=shell2[which(shell2$mx_mm>=0),]
hist(max_size$mx_mm)
max_size_check=max_size[which(max_size$mx_mm>50),]

max_ex_size=shell2[which(shell2$mx_ex_mm>=0),]
hist(max_ex_size$mx_ex_mm)
max_ex_size_check=max_ex_size[which(max_ex_size$mx_ex_mm>50),]

max_in_size=shell2[which(shell2$mx_in_mm>=0),]
hist(max_in_size$mx_in_mm)
max_in_size_check=max_in_size[which(max_in_size$mx_ex_mm>50),]

##### Recorder check #####

summary(shell2$recorder)

##### Multiple Recruitment Check #####

summary(shell2$mult_rec)

##### Mud Check #####

summary(shell2$mud)
