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
ggplot(sp_summary, aes(x=station,y=total,fill=sample))+
  geom_bar(stat="identity")+
  labs(title="Species Counts Per Station/Sample",x="Station",y="Count")+
  scale_x_discrete(limits=c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14"))
dev.copy2pdf(file="invert/fig/invert_sp_totals.pdf")

mean_sp = sp_summary %>%
  group_by(station)%>%
  summarise_all(funs(mean,sd,std.error))

ggplot(mean_sp, aes(x=station,y=total_mean))+
         geom_bar(stat="identity")+
         labs(title="Mean Species Counts Per Station",x="Station",y="Mean +/- SE")+
  scale_x_discrete(limits=c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14"))+
  geom_errorbar(aes(ymin=total_mean-total_std.error,ymax=total_mean+total_std.error),width=.2)
dev.copy2pdf(file="invert/fig/invert_sp_mean.pdf")

#### Taxa Abundance ####

sp_abund = sp %>%
  group_by(station,sp_code)%>%
  summarise(total=sum(count))

ggplot(sp_abund, aes(x=station,y=total,fill=sp_code))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14"))
dev.copy2pdf(file="invert/fig/invert_sp_abund_stn.pdf")

### too many colors, need to change to proportional totals and filter to most common

fam_abund= sp %>%
  group_by(station,fam_code)%>%
  summarise(total=sum(count))

ggplot(fam_abund, aes(x=station,y=total,fill=fam_code))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14"))
dev.copy2pdf(file="invert/fig/invert_fam_abund_stn.pdf")
### too many colors, need to change to proportional totals and filter to most common

