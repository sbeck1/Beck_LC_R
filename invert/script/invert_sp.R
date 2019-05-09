#### invert species summaries ####

sp=read.csv("invert/data/production/20190502_lc_invert_sp_recon.csv") 

library(ggplot2)
library(dplyr)
library(plotrix) #for stderr

#### Taxa Counts ####

sp$sp_cnt=1 #each taxa is a row, sum taxa per station below

sp_summary = sp %>%
  group_by(station,sample)%>%
  summarise(total=sum(sp_cnt))
sp_summary$sample=as.factor(sp_summary$sample)
ggplot(sp_summary, aes(x=station,y=total,fill=sample))+
  geom_bar(stat="identity",colour="black")+
  scale_fill_brewer(palette="Greys")+
  labs(title="Species Counts Per Station/Sample",x="Station",y="Count")+
  scale_x_discrete(limits=c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14"))
str(sp_summary)
  
dev.copy2pdf(file="invert/fig/invert_sp_totals.pdf")

mean_sp = sp_summary %>%
  group_by(station)%>%
  summarise_all(funs(mean,sd,std.error))

ggplot(mean_sp, aes(x=station,y=total_mean))+
         geom_bar(stat="identity",colour="black")+
         labs(title="Mean Species Counts Per Station",x="Station",y="Mean +/- SE")+
  scale_x_discrete(limits=c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14"))+
  geom_errorbar(aes(ymin=total_mean-total_std.error,ymax=total_mean+total_std.error),width=.2)
dev.copy2pdf(file="invert/fig/invert_sp_mean.pdf")

#### Taxa Abundance ####

sp_abund = sp %>%
  group_by(station,fam_code,sp_code)%>%
  summarise(total=sum(count))

#derive proportional abundances to remove sp <1% of total
sp_abund2 = sp %>%
  group_by(fam_code,sp_code)%>%
  summarise(total=sum(count))
sp_abund2$gr_tot = sum(sp_abund2$total)
sp_abund2$prop=(sp_abund2$total/sp_abund2$gr_tot)*100

sp_abund2$prev="RARE"
sp_abund2$prev[sp_abund2$prop>=1]="COMMON"

sp2=merge(sp,sp_abund2)

comm_sp_abund_stn=sp2[which(sp2$prev=="COMMON"),] %>%
  group_by(station,sample,fam_code,sp_code)%>%
  summarise(total=sum(count))

comm_sp_abund_stn$fam_sp=paste(comm_sp_abund_stn$fam_code,comm_sp_abund_stn$sp_code)
write.csv(comm_sp_abund_stn,"invert/fig/invert_comm_sp_abund_stn.csv")

comm_sp_abund=sp2[which(sp2$prev=="COMMON"),] %>%
  group_by(station,fam_code,sp_code)%>%
  summarise(total=sum(count))

comm_sp_abund$fam_sp=paste(comm_sp_abund$fam_code,comm_sp_abund$sp_code)

ggplot(comm_sp_abund, aes(x=station,y=total,fill=fam_sp))+
  geom_bar(stat="identity", colour="black")+
  scale_fill_brewer(palette="Paired")+
  labs(title="Common Species Abundance by Station",x="Station",y="Total")+
  scale_x_discrete(limits=c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14"))
dev.copy2pdf(file="invert/fig/invert_comm_sp_abund.pdf")

#merge with station coordinates and create CSV for Arc
invert_stn=read.csv("master_geospatial/data/production/20190509_master_geospatial.csv")
invert_stn=invert_stn[which(invert_stn$data_type=="INVERT"),]
invert_stn=invert_stn[c("station","easting","northing")]
write.csv(invert_stn,"invert/fig/invert_stn.csv")

sp_summary=merge(sp_summary,invert_stn)
write.csv(sp_summary,"invert/fig/invert_sp_totals.csv")

mean_sp=merge(mean_sp,invert_stn)
write.csv(mean_sp,"invert/fig/invert_sp_mean.csv")

comm_sp_abund=merge(comm_sp_abund,invert_stn)
write.csv(mean_sp,"invert/fig/invert_comm_sp_abund.csv")

