library (ggplot2)
library (dplyr)
library (plotly)
library(plotrix)
library(tidyr)

tile_spat=read.csv("oyster/spat/data/production/201905_tile_spat_count.csv")
tile_spat2=tile_spat[which(tile_spat$cnt>=0),]  #only include retrieved tiles

mo2num=function(x) match(tolower(x),tolower(month.name))  #month name to month number
tile_spat2$month_num=mo2num(tile_spat2$month)
tile_spat2$month_num=factor(tile_spat2$month_num)



###Tile totals

#reformat
tile_summary = tile_spat2 %>%
  group_by(station,month_num,year)%>%
  summarise(tile_total=sum(cnt))  

#surface area of tile = (2*(0.151m x 0.151m))+(4*(0.151 x 0.012m)) = 0.058286sq m

tile_summary2 = tile_spat2 %>%
  group_by(station,month_num,year)%>%
  summarise(total=sum(cnt)) #multiply total spat count by 17.15678 to obtain total spat per sq m
tile_summary2$type="TILE"
str(tile_summary2)

tile_summary2$year=as.factor(tile_summary2$year)
tile_summary2$station=factor(tile_summary2$station,levels=c("WQ6","WQ1","WQ7","WQ5","WQ2","WQ8","WQ4","WQ3","WQ9","WQ10"))

ggplot(tile_summary2, aes(x=month_num,y=total,shape=year))+
  geom_point(size=2)+
  labs(title="Tile Spat Collectors:  Total Spat by Station/Month/Year",x="Month", y="Spat Count per Tile")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))

dev.copy2pdf(file="oyster/spat/fig/tile_spat_total.pdf")

    #remove title for report

ggplot(tile_summary2, aes(x=month_num,y=total,shape=year))+
  geom_point(size=2)+
  labs(x="Month", y="Spat Count per Tile")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_tile_spat_total.pdf")

#report table format
tile_summary3=subset(tile_summary2, select=c(station,month_num,total))
tile_summary3 = tile_summary3 %>% spread(month_num,total)
write.csv(tile_summary2,"oyster/spat/report/report_table_data/tile_summary3.csv")




###Tile totals by side

#overall
tile_spat3 = subset(tile_spat2,select=c(station,month_num,year,side,cnt))

tile_side_all = tile_spat3 %>%
  group_by(side)%>%
  summarise_all(funs(mean,sd,std.error))
tile_side_all = subset(tile_side_all, select=c(side,cnt_mean,cnt_std.error))

ggplot(tile_side_all, aes(x=side,y=cnt_mean))+
  geom_point(stat="identity",colour="black")+
  labs(title="Tile Spat Collectors:  Mean Spat Totals by Tile Side",x="Tile Side",y="Mean Spat Count +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/fig/tile_side_all.pdf")
write.csv(tile_side_all,"oyster/spat/report/report_table_data/tile_side_all.csv")

      #remove title for report

ggplot(tile_side_all, aes(x=side,y=cnt_mean))+
  geom_point(stat="identity",colour="black")+
  labs(x="Tile Side",y="Mean Spat Count +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_tile_side_all.pdf")

#by month
tile_side_month =tile_spat3 %>%
  group_by(side,month_num)%>%
  summarise_all(funs(mean,sd,std.error))
tile_side_month = subset(tile_side_month, select=c(side,month_num,cnt_mean,cnt_std.error))

ggplot(tile_side_month, aes(x=month_num,y=cnt_mean,shape=side))+
  geom_point(stat="identity",colour="black")+
  labs(title="Tile Spat Collectors:  Mean Spat Totals by Month/Tile Side",x="Month",y="Mean Spat Count +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))

dev.copy2pdf(file="oyster/spat/fig/tile_side_month.pdf")
write.csv(tile_side_month,"oyster/spat/report/report_table_data/tile_side_month.csv")

      #remove title for report

ggplot(tile_side_month, aes(x=month_num,y=cnt_mean,shape=side))+
  geom_point(stat="identity",colour="black")+
  labs(x="Month",y="Mean Spat Count +/- SE")+
  geom_errorbar(aes(ymin=cnt_mean-cnt_std.error,ymax=cnt_mean+cnt_std.error),width=.2)+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_tile_side_month.pdf")

#by month/station
tile_spat3$station=factor(tile_spat3$station,levels=c("WQ6","WQ1","WQ7","WQ5","WQ2","WQ8","WQ4","WQ3","WQ9","WQ10"))

ggplot(tile_spat3, aes(x=month_num,y=cnt,shape=side))+
  geom_point(size=1.5)+
  labs(title="Tile Spat Collectors:  Total Spat by Station/Month/Tile Side",x="Month", y="Spat Count per Tile")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))

dev.copy2pdf(file="oyster/spat/fig/tile_side_mo_stn.pdf")
write.csv(tile_spat3,"oyster/spat/report/report_table_data/tile_side_mo_stn.csv")

      #remove title for report

ggplot(tile_spat3, aes(x=month_num,y=cnt,shape=side))+
  geom_point(size=1.5)+
  labs(x="Month", y="Spat Count per Tile")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_tile_side_mo_stn.pdf")



###Spat size

tile_spat4 = subset(tile_spat2,select=c(station,month_num,year,side,cnt,mx_mm1,mx_mm2,mx_mm3))
tile_spat4_l = tile_spat4 %>% gather(type,size,mx_mm1:mx_mm3)
tile_spat5 = tile_spat4_l[which(tile_spat4_l$size>=0),]

#by tile side
tile_size_side = tile_spat5 %>%
  group_by(side)%>%
  summarise_all(funs(mean,sd,std.error))
tile_size_side = subset(tile_size_side, select=c(side,size_mean,size_std.error))

ggplot(tile_size_side, aes(x=side,y=size_mean))+
  geom_point(stat="identity",colour="black")+
  ylim(0,10)+
  labs(title="Tile Spat Collectors:  Mean Maximum Spat Height by Tile Side",x="Tile Side",y="Mean Spat Height (mm) +/- SE")+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/fig/tile_size_side.pdf")
write.csv(tile_size_side,"oyster/spat/report/report_table_data/tile_size_side.csv")

      #remove title for report

ggplot(tile_size_side, aes(x=side,y=size_mean))+
  geom_point(stat="identity",colour="black")+
  ylim(0,10)+
  labs(x="Tile Side",y="Mean Spat Height (mm) +/- SE")+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_tile_size_side.pdf")

#by month
tile_size_month= tile_spat5 %>%
  group_by(year,month_num)%>%
  summarise_all(funs(mean,sd,std.error))
tile_size_month = subset(tile_size_month, select=c(year,month_num,size_mean,size_std.error))

tile_size_month$year=as.factor(tile_size_month$year)

ggplot(tile_size_month, aes(x=month_num,y=size_mean,shape=year))+
  geom_point(stat="identity",colour="black")+
  ylim(0,11)+
  labs(title="Tile Spat Collectors:  Mean Maximum Spat Height by Month",x="Month",y="Mean Spat Height (mm) +/- SE")+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))

dev.copy2pdf(file="oyster/spat/fig/tile_size_month.pdf")
write.csv(tile_size_month,"oyster/spat/report/report_table_data/tile_size_month.csv")

      #remove title for report

ggplot(tile_size_month, aes(x=month_num,y=size_mean,shape=year))+
  geom_point(stat="identity",colour="black")+
  ylim(0,11)+
  labs(x="Month",y="Mean Spat Height (mm) +/- SE")+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_tile_size_month.pdf")


#by month/station
tile_size_mo_stn = tile_spat5 %>%
  group_by(year,station,month_num)%>%
  summarise_all(funs(mean,sd,std.error))
tile_size_mo_stn = subset(tile_size_mo_stn, select=c(year,station,month_num,size_mean,size_std.error))

tile_size_mo_stn$year=as.factor(tile_size_mo_stn$year)
tile_size_mo_stn$station=factor(tile_size_mo_stn$station,levels=c("WQ6","WQ1","WQ7","WQ5","WQ2","WQ8","WQ4","WQ3","WQ9","WQ10"))

ggplot(tile_size_mo_stn, aes(x=month_num,y=size_mean,shape=year))+
  geom_point(size=1.5)+
  labs(title="Tile Spat Collectors:  Mean Maximum Spat Height by Station/Month",x="Month", y="Mean Spat Height (mm) +/- SE")+
  facet_wrap(~station, ncol=3)+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))

dev.copy2pdf(file="oyster/spat/fig/tile_size_mo_stn.pdf")
write.csv(tile_size_mo_stn,"oyster/spat/report/report_table_data/tile_size_mo_stn.csv")

      #remove title for report

ggplot(tile_size_mo_stn, aes(x=month_num,y=size_mean,shape=year))+
  geom_point(size=1.5)+
  labs(x="Month", y="Mean Spat Height (mm) +/- SE")+
  facet_wrap(~station, ncol=3)+
  geom_errorbar(aes(ymin=size_mean-size_std.error,ymax=size_mean+size_std.error),width=.2)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_tile_size_mo_stn.pdf")



###Barnacle Fouling
tile_spat6 = subset(tile_spat2,select=c(station,month_num,year,side,cnt,barnacle))
tile_spat6$station=factor(tile_spat6$station,levels=c("WQ6","WQ1","WQ7","WQ5","WQ2","WQ8","WQ4","WQ3","WQ9","WQ10"))

ggplot(tile_spat6, aes(x=month_num,y=barnacle,shape=side))+
  geom_point(size=1.5)+
  labs(title="Tile Spat Collectors:  Barnacle Coverage by Station/Month/Tile Side",x="Month", y="Barnacle Coverage")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))+
  scale_y_discrete(limits=c("NONE","LIGHT","MODERATE","HEAVY"))

dev.copy2pdf(file="oyster/spat/fig/tile_barnacle.pdf")
write.csv(tile_size_mo_stn,"oyster/spat/report/report_table_data/tile_barnacle.csv")

      #remove title for report

ggplot(tile_spat6, aes(x=month_num,y=barnacle,shape=side))+
  geom_point(size=1.5)+
  labs(x="Month", y="Barnacle Coverage")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))+
  scale_x_discrete(limits=c("6","7","8","9","10","11","12","1","2","3","4","5"))+
  scale_y_discrete(limits=c("NONE","LIGHT","MODERATE","HEAVY"))

dev.copy2pdf(file="oyster/spat/report/report_fig/rep_tile_barnacle.pdf")

