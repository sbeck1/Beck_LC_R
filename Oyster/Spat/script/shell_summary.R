library (ggplot2)
library (dplyr)
library (plotly)
library(plotrix)
library(tidyr)

shell_spat=read.csv("oyster/spat/data/production/201810_shell_spat_count.csv")
shell_spat=shell_spat[which(shell_spat$ex_cnt>=0),]  #filters to include only counted collectors
shell_spat$count=shell_spat$in_cnt+shell_spat$ex_cnt  #total spat count
str(shell_spat)

shell_bag=read.csv("oyster/spat/data/production/201810_shell_spat_bag.csv")  
shell_bag2=shell_bag[which(shell_bag$status=="RETR"&shell_bag$snail_cnt>=0),]  #filters to include only successfully retrieved collectors

mo2num=function(x) match(tolower(x),tolower(month.name))  #month name to month number
shell_spat$month_num=mo2num(shell_spat$month)
shell_spat$month_num=factor(shell_spat$month_num)
shell_bag2$month_num=mo2num(shell_bag2$month)
shell_bag2$month_num=factor(shell_bag2$month_num)


###Shell totals

shell_summary = shell_spat %>%
  group_by(station,month_num)%>%
  summarise(total=sum(count), mean=mean(count), sd=sd(count))
shell_summary$type="SHELL"

#standardizing counts by weight/volume
shell_summary2=merge(shell_summary,shell_bag2,by=c("station","month_num"))
shell_summary2$total_kg=(shell_summary2$total/shell_summary2$t_wt_g)*1000
shell_summary2$total_L=(shell_summary2$total/shell_summary2$t_vol_ml)*1000

shell_summary2$station=factor(shell_summary2$station,levels=c("WQ6","WQ1","WQ7","WQ5","WQ2","WQ8","WQ4","WQ3","WQ9"))

#spat shell bag

shell_summary2_l = shell_summary2 %>% gather(type,meas,total_kg:total_L)

ggplot(shell_summary2_l, aes(x=month_num,y=meas,shape=type))+
  geom_point(size=2)+
  labs(title="Shell Spat Collectors:  Total Spat per Unit Shell Mass and Volume by Station/Month",x="Month", y="Total Spat Count")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))

dev.copy2pdf(file="oyster/spat/fig/shell_spat_total.pdf")

      #remove title for report

ggplot(shell_summary2_l, aes(x=month_num,y=meas,shape=type))+
  geom_point(size=2)+
  labs(x="Month", y="Total Spat Count")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_spat_total.pdf")

#format for report table
shell_summary3=subset(shell_summary2, select=c(station,month_num,total,total_kg,total_L))
write.csv(shell_summary3,"oyster/spat/report/report_table_data/shell_summary3.csv")




###Shell totals by side
shell_side=subset(shell_spat, select=c(station,month_num,ex_cnt,in_cnt))
shell_side=shell_side %>% gather(side,cnt,ex_cnt,in_cnt)

shell_side_all = shell_side %>%
  group_by(side)%>%
  summarise_all(funs(mean,sd,std.error))
shell_side_all = subset(shell_side_all, select=c(side,cnt_mean,cnt_std.error))

ggplot(shell_side_all, aes(x=side,y=cnt_mean))+
  geom_point(stat="identity",colour="black")+
  ylim(0,10)+
  labs(title="Shell Spat Collectors:  Mean Spat Totals by Shell Side",x="Shell Side",y="Mean Spat Count +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/fig/shell_side_all.pdf")
write.csv(shell_side_all,"oyster/spat/report/report_table_data/shell_side_all.csv")

      #remove title for report

ggplot(shell_side_all, aes(x=side,y=cnt_mean))+
  geom_point(stat="identity",colour="black")+
  ylim(0,10)+
  labs(x="Shell Side",y="Mean Spat Count +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_side_all.pdf")

#by month
shell_side_month = shell_side %>%
  group_by(side,month_num)%>%
  summarise_all(funs(mean,sd,std.error))
shell_side_month = subset(shell_side_month, select=c(side,month_num,cnt_mean,cnt_std.error))

ggplot(shell_side_month, aes(x=month_num,y=cnt_mean,shape=side))+
  geom_point(stat="identity",colour="black")+
  labs(title="Shell Spat Collectors:  Mean Spat Totals by Month/Shell Side",x="Month",y="Mean Spat Count +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/fig/shell_side_month.pdf")
write.csv(shell_side_month,"oyster/spat/report/report_table_data/shell_side_month.csv")

      #remove title for report

ggplot(shell_side_month, aes(x=month_num,y=cnt_mean,shape=side))+
  geom_point(stat="identity",colour="black")+
  labs(x="Month",y="Mean Spat Count +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_side_month.pdf")

#by station/month
shell_side$station=factor(shell_side$station,levels=c("WQ6","WQ1","WQ7","WQ5","WQ2","WQ8","WQ4","WQ3","WQ9"))

shell_side_mo_stn = shell_side %>%
  group_by(side,month_num,station)%>%
  summarise_all(funs(mean,sd,std.error))
shell_side_mo_stn = subset(shell_side_mo_stn, select=c(side,month_num,station,mean,std.error))

ggplot(shell_side_mo_stn, aes(x=month_num,y=mean,shape=side))+
  geom_point(size=1.5)+
  labs(title="Shell Spat Collectors:  Total Spat by Station/Month/Shell Side",x="Month", y="Mean Spat Count +/- SE")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))+
  geom_errorbar(aes(ymin=mean-std.error,ymax=mean+std.error),width=.2)

dev.copy2pdf(file="oyster/spat/fig/shell_side_mo_stn.pdf")
write.csv(shell_side_mo_stn,"oyster/spat/report/report_table_data/shell_side_mo_stn.csv")

      #remove title for report

ggplot(shell_side_mo_stn, aes(x=month_num,y=mean,shape=side))+
  geom_point(size=1.5)+
  labs(x="Month", y="Mean Spat Count +/- SE")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))+
  geom_errorbar(aes(ymin=mean-std.error,ymax=mean+std.error),width=.2)

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_side_mo_stn.pdf")




###Spat Size

shell_size=subset(shell_spat, select=c(station,month_num,mx_ex_mm,mx_in_mm,mx_mm))
shell_size=shell_size %>% gather(side,size,mx_ex_mm:mx_mm)
shell_size = shell_size[which(shell_size$size>=0),]
shell_size2 = shell_size[which(shell_size$sid=="mx_ex_mm"|shell_size$sid=="mx_in_mm"),]

#shell size was not recorded by shell side until June 2018

shell_size_side = shell_size2 %>%
  group_by(side)%>%
  summarise_all(funs(mean,sd,std.error))
shell_size_side = subset(shell_size_side, select=c(side,size_mean,size_std.error))

ggplot(shell_size_side, aes(x=side,y=size_mean))+
  geom_point(stat="identity",colour="black")+
  ylim(0,10)+
  labs(title="Shell Spat Collectors:  Mean Maximum Spat Height by Shell Side",x="Shell Side",y="Mean Spat Height (mm) +/- SE")+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/fig/shell_size_side.pdf")
write.csv(tile_size_side,"oyster/spat/report/report_table_data/shell_size_side.csv")

#remove title for report

ggplot(shell_size_side, aes(x=side,y=size_mean))+
  geom_point(stat="identity",colour="black")+
  ylim(0,10)+
  labs(x="Shell Side",y="Mean Spat Height (mm) +/- SE")+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_size_side.pdf")

#by month

shell_size_month= shell_size %>%
  group_by(month_num)%>%
  summarise_all(funs(mean,sd,std.error))
shell_size_month = subset(shell_size_month, select=c(month_num,size_mean,size_std.error))

ggplot(shell_size_month, aes(x=month_num,y=size_mean))+
  geom_point(stat="identity",colour="black")+
  ylim(0,17)+
  labs(title="Shell Spat Collectors:  Mean Maximum Spat Height by Month",x="Month",y="Mean Spat Height (mm) +/- SE")+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))

#April 2018 value is for 2 months (intitial collector deployment time was 2 months)

dev.copy2pdf(file="oyster/spat/fig/shell_size_month.pdf")
write.csv(tile_size_month,"oyster/spat/report/report_table_data/shell_size_month.csv")

#remove title for report

ggplot(shell_size_month, aes(x=month_num,y=size_mean))+
  geom_point(stat="identity",colour="black")+
  ylim(0,17)+
  labs(x="Month",y="Mean Spat Height (mm) +/- SE")+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_size_month.pdf")

#by month/station
shell_size_mo_stn = shell_size %>%
  group_by(station,month_num)%>%
  summarise_all(funs(mean,sd,std.error))
shell_size_mo_stn = subset(shell_size_mo_stn, select=c(station,month_num,size_mean,size_std.error))

shell_size_mo_stn$station=factor(shell_size_mo_stn$station,levels=c("WQ6","WQ1","WQ7","WQ5","WQ2","WQ8","WQ4","WQ3","WQ9"))

ggplot(shell_size_mo_stn, aes(x=month_num,y=size_mean))+
  geom_point(size=1.5)+
  labs(title="Shell Spat Collectors:  Mean Maximum Spat Height by Station/Month",x="Month", y="Mean Spat Height (mm) +/- SE")+
  facet_wrap(~station, ncol=3)+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))

dev.copy2pdf(file="oyster/spat/fig/shell_size_mo_stn.pdf")
write.csv(shell_size_mo_stn,"oyster/spat/report/report_table_data/shell_size_mo_stn.csv")

#remove title for report

ggplot(shell_size_mo_stn, aes(x=month_num,y=size_mean))+
  geom_point(size=1.5)+
  labs(x="Month", y="Mean Spat Height (mm) +/- SE")+
  facet_wrap(~station, ncol=3)+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_size_mo_stn.pdf")

###Barnacle Fouling
shell_spat2 = subset(shell_spat,select=c(station,month_num,ex_cnt,in_cnt,barnacle))
shell_spat2$station=factor(shell_spat2$station,levels=c("WQ6","WQ1","WQ7","WQ5","WQ2","WQ8","WQ4","WQ3","WQ9"))

ggplot(shell_spat2, aes(x=month_num,y=barnacle))+
  geom_point(size=1.5,position=position_jitter(w=0.1,h=0.1))+
  labs(title="Shell Spat Collectors:  Barnacle Coverage by Station/Month/Shell",x="Month", y="Barnacle Coverage")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))+
  scale_y_discrete(limits=c("NONE","LIGHT","MODERATE","HEAVY"))

dev.copy2pdf(file="oyster/spat/fig/shell_barnacle.pdf")
write.csv(tile_size_mo_stn,"oyster/spat/report/report_table_data/shell_barnacle.csv")

#remove title for report

ggplot(shell_spat2, aes(x=month_num,y=barnacle))+
  geom_point(size=1.5,position=position_jitter(w=0.1,h=0.1))+
  labs(x="Month", y="Barnacle Coverage")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))+
  scale_y_discrete(limits=c("NONE","LIGHT","MODERATE","HEAVY"))

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_barnacle.pdf")



###Spat count by shell dimensions
shell_dim=subset(shell_spat, select=c(station,month_num,sh_ht_mm,sh_ln_mm,sh_wd_mm,ex_cnt,in_cnt))
shell_dim=shell_dim %>% gather(dim,size,sh_ht_mm:sh_wd_mm)
#shell_dim=shell_dim %>% gather(side,cnt,ex_cnt:in_cnt)
shell_dim2= shell_dim[which(shell_dim$size>=0),]   #width not collected for April 2018
shell_dim2$tot_cnt=shell_dim2$ex_cnt + shell_dim2$in_cnt

ggplot(shell_dim2, aes(x=size,y=tot_cnt))+
  geom_point(size=1.5)+
  ylim(0,150)+
  xlim(0,150)+
  labs(title="Total Spat Count vs Shell Size", x="Shell Size (mm)", y="Total Spat Count")+
  facet_wrap(~dim,ncol=3)

dev.copy2pdf(file="oyster/spat/fig/shell_dim.pdf")

#remove title for report

ggplot(shell_dim2, aes(x=size,y=tot_cnt))+
  geom_point(size=1.5)+
  ylim(0,150)+
  xlim(0,150)+
  labs(x="Shell Size (mm)", y="Total Spat Count")+
  facet_wrap(~dim,ncol=3)

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_dim.pdf")




