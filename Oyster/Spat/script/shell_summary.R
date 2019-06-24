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
  labs(title="Shell Spat Collectors:  Total Spat per Mass and Volume Shell by Station/Month",x="Month", y="Total Spat")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))

dev.copy2pdf(file="oyster/spat/fig/shell_spat_total.pdf")

      #remove title for report

ggplot(shell_summary2_l, aes(x=month_num,y=meas,shape=type))+
  geom_point(size=2)+
  labs(x="Month", y="Total Spat")+
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
  labs(title="Shell Spat Collectors:  Mean Spat Totals by Shell Side",x="Shell Side",y="Mean +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/fig/shell_side_all.pdf")
write.csv(shell_side_all,"oyster/spat/report/report_table_data/shell_side_all.csv")

      #remove title for report

ggplot(shell_side_all, aes(x=side,y=cnt_mean))+
  geom_point(stat="identity",colour="black")+
  labs(x="shell Side",y="Mean +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_side_all.pdf")

#by month
shell_side_month = shell_side %>%
  group_by(side,month_num)%>%
  summarise_all(funs(mean,sd,std.error))
shell_side_month = subset(shell_side_month, select=c(side,month_num,cnt_mean,cnt_std.error))

ggplot(shell_side_month, aes(x=month_num,y=cnt_mean,shape=side))+
  geom_point(stat="identity",colour="black")+
  labs(title="Shell Spat Collectors:  Mean Spat Totals by Month/Shell Side",x="Month",y="Mean +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/fig/shell_side_month.pdf")
write.csv(shell_side_month,"oyster/spat/report/report_table_data/shell_side_month.csv")

      #remove title for report

ggplot(shell_side_month, aes(x=month_num,y=cnt_mean,shape=side))+
  geom_point(stat="identity",colour="black")+
  labs(x="Month",y="Mean +/- SE")+
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
  labs(title="Shell Spat Collectors:  Total Spat by Station/Month/Shell Side",x="Month", y="Mean +/- SE")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))+
  geom_errorbar(aes(ymin=mean-std.error,ymax=mean+std.error),width=.2)

dev.copy2pdf(file="oyster/spat/fig/shell_side_mo_stn.pdf")
write.csv(shell_side_mo_stn,"oyster/spat/report/report_table_data/shell_side_mo_stn.csv")

      #remove title for report

ggplot(shell_side_mo_stn, aes(x=month_num,y=mean,shape=side))+
  geom_point(size=1.5)+
  labs(x="Month", y="Mean +/- SE")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("4","6","7","8","9","10"))+
  geom_errorbar(aes(ymin=mean-std.error,ymax=mean+std.error),width=.2)

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_shell_side_mo_stn.pdf")
