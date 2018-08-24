##############

#   Coop WQ Summary Tables
#   Steve Beck

##############

library(ggplot2)

wq = read.csv("Cooperator_WQ/CooperatorWaterQuality.csv",header=T)

samp_cnt = unique.matrix(wq[c("Data_source","Year","Month","Lat","Lon")])
samp_cnt_sum = table(samp_cnt$Data_source,samp_cnt$Year,samp_cnt$Month)
ftable(samp_cnt_sum)

samp_cnt2 = unique.matrix(wq[c("Data_source","Year","Lat","Lon")])
samp_cnt2_sum = table(samp_cnt2$Data_source,samp_cnt2$Year)
ftable(samp_cnt2_sum)

samp_cnt3 = unique.matrix(wq[c("Data_source","Lat","Lon")])
samp_cnt3_sum = table(samp_cnt3$Data_source)
ftable(samp_cnt3_sum)

wq_source = table(wq$Data_source)
ftable(wq_source)

wq_source_site = table(wq$Year,wq$Data_source)
ftable(wq_source_site)
as.data.frame(ftable(wq_source_site))
#matplot(wq_source_site,type="l",xlab="Year",ylab="Sample Count")
ggplot(data=wq_source_site,aes(x=Year))+
  geom_line(aes(y=FDACS))+
  geom_line(aes(y=FWC))+
  geom_line(aes(y=Frazier))

coop_wq_table = table(wq$Data_source,wq$Year,wq$Month) 
ftable(coop_wq_table)
coopwq_my=as.data.frame(ftable(coop_wq_table))
write.ftable(ftable(coop_wq_table),"Cooperator_WQ/CoopWQ_MY.csv",quote = FALSE, 
             sep = ";")

