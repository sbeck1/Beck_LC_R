##### Basic Salinity Summaries #####

wq=read.csv("wq/data/production/20190719_wq.csv")

library(ggplot2)
library(dplyr)
library(lubridate)
library(plotrix)
library(tidyr)

#remove columns
sal=subset(wq, select=c(Site,Salinity,Date))

#seperate date
sal = sal %>%
  dplyr::mutate(year=lubridate::year(Date),
                month=lubridate::month(Date),
                day=lubridate::day(Date),
                hour=lubridate::hour(Date))


#### daily summaries
sal_d= sal %>%
  group_by(year,month,day,Site) %>%
  summarise(mean=mean(Salinity),min=min(Salinity),max=max(Salinity))

#reformat for plots
sal_d2=sal_d %>%
  gather(metric,value,mean:max)

sal_d2$date=as.Date(with(sal_d2, paste(year,month,day,sep="-")))

sal_d2$Site=factor(sal_d2$Site,levels=c("6","1","7","5","2","8","4","3","9","10"))

ggplot(sal_d2, aes(x=date,y=value,color=metric))+
  geom_point(size=0.5)+
  labs(x="Date", y="Salinity (ppt)")+
  facet_wrap(~Site, ncol=3)



####LC Reef effect

#format
sal_reef=sal[which(sal$Site<7),]
sal_reef2=sal_reef[-c(16574,84,32968,3455,13988,47584,71432,40115,66687,58993,28358),]  #remove duplicates
sal_reef2=sal_reef2 %>%
  spread (Site,Salinity)

#paired sensor differences
sal_reef2$diff_6_1=(sal_reef2$"6"-sal_reef2$"1")
sal_reef2$diff_5_2=(sal_reef2$"5"-sal_reef2$"2")
sal_reef2$diff_4_3=(sal_reef2$"4"-sal_reef2$"3")

#format
sal_reef3=subset(sal_reef2,select=c(Date,diff_6_1,diff_5_2,diff_4_3))
sal_reef3=sal_reef3%>%
  gather(pair,diff,diff_6_1:diff_4_3)
sal_reef3=na.omit(sal_reef3)

sal_reef3 = sal_reef3 %>%
  dplyr::mutate(year=lubridate::year(Date),
                month=lubridate::month(Date),
                day=lubridate::day(Date),
                hour=lubridate::hour(Date))

sal_reef3$Date=as.Date(sal_reef3$Date) 

ggplot(sal_reef3, aes(x=Date,y=diff))+
  geom_rect(aes(xmin=as.Date(c("2018-07-01 01:00:00")),xmax=as.Date(c("2018-09-30 23:00:00")),ymin=-Inf,ymax=Inf),fill="dodgerblue")+
  geom_point(size=0.5)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=as.Date(c("2018-07-01 01:00:00","2018-09-30 23:00:00")))+
  scale_x_date(date_labels="%b-%y")+
  labs(x="Date", y="Salinity Difference (ppt)")+
  theme(axis.text.x=element_text(size=rel(0.8)),legend.position="none")+
  facet_wrap(~pair)
dev.copy2pdf(file="wq/fig/sal_diff_pre_post_reef_con.pdf")


# Metric looking at total measurements per day where inside (station 1,2,3) salinity < outside (stn 4,5,6)...pre/post-con

sal_reef4=sal_reef3
sal_reef4$cnt=ifelse(sal_reef4$diff>=0,1,0)

sal_reef5= sal_reef4 %>%
  group_by(Date,pair) %>%
  summarise(sum=sum(cnt))

#below not very descriptive
ggplot(sal_reef5, aes(x=Date,y=sum))+
  geom_rect(aes(xmin=as.Date(c("2018-07-01 01:00:00")),xmax=as.Date(c("2018-09-30 23:00:00")),ymin=-Inf,ymax=Inf),fill="dodgerblue")+
  geom_point(size=0.5)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=as.Date(c("2018-07-01 01:00:00","2018-09-30 23:00:00")))+
  scale_x_date(date_labels="%b-%y")+
  labs(x="Date", y="Number Measurements per Day Inside < Outside Salinity")+
  theme(axis.text.x=element_text(size=rel(0.8)),legend.position="none")+
  facet_wrap(~pair)

#try mean meas per day inside sal < outside sal
sal_reef6=sal_reef5
sal_reef6$con=ifelse(sal_reef6$Date>"2018-07-01",(ifelse(sal_reef6$Date>"2018-09-30","postcon","con")),"precon")

sal_reef7= sal_reef6 %>%
  group_by(pair,con)%>%
  summarise_all(funs(mean,sd,std.error))
sal_reef7 = subset(sal_reef7, select=c(pair,con,sum_mean,sum_std.error))

ggplot(sal_reef7, aes(x=con,y=sum_mean,shape=pair))+
  geom_point(stat="identity",colour="black",size=2)+
  labs(x="Construction Status",y="Mean Measurements per Day Inside Sal < Outside Sal +/- SE")+
  geom_errorbar(aes(ymin=sum_mean-sum_std.error,ymax=sum_mean+sum_std.error),width=.1)+
  scale_x_discrete(limits=c("precon","con","postcon"))+
  ylim(0,24)
dev.copy2pdf(file="wq/fig/mean_diff_pre_post_reef_con.pdf")



#### monthly summaries: mean
sal_m= sal %>%
  group_by(year,month,Site) %>%
  summarise(mean=mean(Salinity),min=min(Salinity),max=max(Salinity))

sal_m2=sal_m %>%
  gather(metric,value,mean:max)

sal_m2$day=1
sal_m2$date=as.Date(with(sal_m2, paste(year,month,day,sep="-")))

sal_m2$Site=factor(sal_m2$Site,levels=c("6","1","7","5","2","8","4","3","9","10"))

ggplot(sal_m2, aes(x=date,y=value,color=metric))+
  geom_point(size=0.5)+
  labs(x="Date", y="Salinity (ppt)")+
  scale_x_date(date_labels="%b-%y",date_breaks="2 months")+
  theme(axis.text.x=element_text(angle=90))+
  facet_wrap(~Site, ncol=3)

dev.copy2pdf(file="wq/fig/sal_mean_month.pdf")

sal_m$year=as.factor(sal_m$year)

ggplot(sal_m, aes(x=month,y=mean,shape=year))+
  geom_point(size=1)+
  labs(x="Month", y="Salinity (ppt)")+
  geom_errorbar(aes(ymin=min,ymax=max),width=.1)+
  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  facet_wrap(~Site, ncol=3)

dev.copy2pdf(file="wq/fig/sal_mean_month2.pdf")

#### monthly summaries: cumulative measurements per month >5 and 15 ppt

sal$cnt_5=ifelse(sal$Salinity>5,1,0)
sal$cnt_15=ifelse(sal$Salinity>15,1,0)

sal_range= sal %>%
  group_by(year,month,Site) %>%
  summarise(sum_5=sum(cnt_5),sum_15=sum(cnt_15))

sal_range$year=as.factor(sal_range$year)
sal_range$Site=factor(sal_range$Site,levels=c("6","1","7","5","2","8","4","3","9","10"))

ggplot(sal_range, aes(x=month,y=sum_5,shape=year,color=year))+
  geom_point(size=2)+
  labs(x="Month", y="Total Measurements >5ppt")+
  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  facet_wrap(~Site, ncol=3)

dev.copy2pdf(file="wq/fig/sal_month_gt5ppt.pdf")

ggplot(sal_range, aes(x=month,y=sum_15,shape=year,color=year))+
  geom_point(size=2)+
  ylim(0,800)+
  labs(x="Month", y="Total Measurements >15ppt")+
  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  facet_wrap(~Site, ncol=3)

dev.copy2pdf(file="wq/fig/sal_month_gt15ppt.pdf")


#merging
sal_merge=merge(sal_m,sal_range,by=c("year","month","Site"))
sal_merge=plyr::rename(sal_merge,c("mean"="sal_mean","min"="sal_min","max"="sal_max"))




##### temperature

#monthly summaries:  mean temperatures

#remove columns
temp=subset(wq, select=c(Site,Temperature,Date))

#seperate date
temp = temp %>%
  dplyr::mutate(year=lubridate::year(Date),
                month=lubridate::month(Date),
                day=lubridate::day(Date),
                hour=lubridate::hour(Date))

temp_m=temp %>%
  group_by(year,month,Site) %>%
  summarise(mean=mean(Temperature),min=min(Temperature),max=max(Temperature))

temp_m2=temp_m %>%
  gather(metric,value,mean:max)

temp_m2$day=1
temp_m2$date=as.Date(with(temp_m2, paste(year,month,day,sep="-")))

temp_m2$Site=factor(temp$Site,levels=c("6","1","7","5","2","8","4","3","9","10"))

ggplot(temp_m2, aes(x=date,y=value,color=metric))+
  geom_point(size=0.5)+
  labs(x="Date", y="Temperature (C)")+
  scale_x_date(date_labels="%b-%y",date_breaks="2 months")+
  theme(axis.text.x=element_text(angle=90))+
  facet_wrap(~Site, ncol=3)

dev.copy2pdf(file="wq/fig/temp_mean_month.pdf")

temp_m$year=as.factor(temp_m$year)

ggplot(temp_m, aes(x=month,y=mean,shape=year))+
  geom_point(size=1)+
  labs(x="Month", y="Temperature (C)")+
  geom_errorbar(aes(ymin=min,ymax=max),width=.1)+
  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  facet_wrap(~Site, ncol=3)

dev.copy2pdf(file="wq/fig/temp_mean_month2.pdf")



#### monthly summaries: cumulative measurements per month >25 degrees

temp$cnt_25=ifelse(temp$Temperature>25,1,0)

temp_range= temp %>%
  group_by(year,month,Site) %>%
  summarise(sum_25=sum(cnt_25))

temp_range$year=as.factor(temp_range$year)
temp_range$Site=factor(temp_range$Site,levels=c("6","1","7","5","2","8","4","3","9","10"))

ggplot(temp_range, aes(x=month,y=sum_25,shape=year,color=year))+
  geom_point(size=2)+
  ylim(0,800)+
  labs(x="Month", y="Total Measurements >25(C)")+
  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  facet_wrap(~Site, ncol=3)

dev.copy2pdf(file="wq/fig/temp_month_gt25C.pdf")

#merging

temp_merge=merge(temp_m,temp_range,by=c("year","month","Site"))
temp_merge=plyr::rename(temp_merge,c("mean"="temp_mean","min"="temp_min","max"="temp_max"))


#merge sal and temp files to combibe with spat data

sal_temp=merge(temp_merge,sal_merge,by=c("year","month","Site"))
write.csv(sal_temp,"wq/data/development/sal_temp.csv")


