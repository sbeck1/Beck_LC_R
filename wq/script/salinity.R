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

#daily summaries
sal_d= sal %>%
  group_by(year,month,day,Site) %>%
  summarise(mean=mean(Salinity),min=min(Salinity),max=max(Salinity))

#remformat for plots
sal_d2=sal_d %>%
  gather(metric,value,mean:max)

sal_d2$date=as.Date(with(sal_d2, paste(year,month,day,sep="-")))

sal_d2$Site=factor(sal_d2$Site,levels=c("6","1","7","5","2","8","4","3","9","10"))

ggplot(sal_d2, aes(x=date,y=value,color=metric))+
  geom_point(size=0.5)+
  labs(x="Date", y="Salinity (ppt)")+
  facet_wrap(~Site, ncol=3)

#LC Reef effect

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

