### Shell Spat Collectors ###

shell_spat=read.csv("oyster/spat/data/development/shell_spat_count.csv")
shell_spat=shell_spat[which(shell_spat$ex_cnt>=0),]  #filters to include only counted collectors
shell_spat$count=shell_spat$in_cnt+shell_spat$ex_cnt  #total spat count
str(shell_spat)

shell_bag=read.csv("oyster/spat/data/development/shell_spat_bag.csv")  
shell_bag2=shell_bag[which(shell_bag$status=="RETR"&shell_bag$snail_cnt>=0),]  #filters to include only successfully retrieved collectors

library (ggplot2)
library (Rmisc) #multiplot
library (dplyr)

#shell_spat_apr[!is.na(shell_spat_apr$count),]

shell_summary = shell_spat %>%
  group_by(station,month)%>%
  summarise(total=sum(count), mean=mean(count), sd=sd(count))
shell_summary$type="SHELL"

#standardizing counts by weight/volume
shell_summary2=merge(shell_summary,shell_bag2,by=c("station","month"))
shell_summary2$total_kg=(shell_summary2$total/shell_summary2$t_wt_g)*1000
shell_summary2$total_L=(shell_summary2$total/shell_summary2$t_vol_ml)*1000

#format for rbind 
shell_summary3=shell_summary2[,c(1,2,8,9,16)]
shell_summary3$type="SHELL"
colnames(shell_summary3)[5]="total"
as.integer(shell_summary3$total)
str(shell_summary3)






####### Tile Spat Collectors ########
tile_spat=read.csv("oyster/spat/data/development/tile_spat_count.csv")
tile_spat2=tile_spat[which(tile_spat$cnt>=0),]

#surface area of tile = (2*(0.151m x 0.151m))+(4*(0.151 x 0.012m)) = 0.058286sq m

#format for merge
tile_summary = tile_spat2 %>%
  group_by(station,month,year)%>%
  summarise(tile_total=sum(cnt))

#format for rbind
tile_summary2 = tile_spat2 %>%
  group_by(station,month,year)%>%
  summarise(total=sum(cnt)) #multiply total spat count by 17.15678 to obtain total spat per sq m
tile_summary2$type="TILE"
str(tile_summary2)

### combining shell and tile collector data ###
combined=merge(shell_summary2,tile_summary,by=c("station","month"))
combined2=bind_rows(shell_summary3,tile_summary2)
mo2num=function(x) match(tolower(x),tolower(month.name))  #month name to month number
combined2$month_num=mo2num(combined2$month)
combined2$month_num=factor(combined2$month_num)

#combined2=combined2[which(combined2$month_num>=8),]

#determine appropriate ymax for plots
summary(combined2$total)

#add gps coordinates
gps=read.csv("oyster/spat/data/development/wq_lc_stn.csv")
spat_combined_utm=merge(gps,combined2,by="station")
shell_spat_utm=spat_combined_utm[which(spat_combined_utm$type=="KG_SHELL"),]
tile_spat_utm=spat_combined_utm[which(spat_combined_utm$type=="TILE"),]
write.csv(shell_spat_utm,"oyster/spat/data/development/shell_spat_utm.csv")
write.csv(tile_spat_utm,"oyster/spat/data/development/tile_spat_utm.csv")



### Plots ###

### Shell Spat Collectors ###


#barplot of trends...not very good representation, line graph not working either...too few points?
#ggplot(shell_summary, aes(x=station,y=total,fill=month))+
  #labs(title="Shell Spat Collectors:  Total Spat by Station/Month", x="Station", y="Total Spat")+
  #geom_bar(position="dodge", colour="black", stat="identity", width=0.5)+
  #geom_text(size=3, position=position_dodge(0.9), aes(label=total, vjust=-1.5))+
  #scale_fill_brewer(palette="Greens")

#plot of time series
ggplot(shell_summary2, aes(x=month,y=total_kg,shape=station))+
  geom_point(size=2.5)+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10))+
  labs(title="Shell Spat Collectors:  Total Spat by Station/Month",x="Month", y="Spat/kg Shell")


### Tile Spat Collectors ###
ggplot(tile_summary, aes(x=month,y=tile_total,shape=station))+
  geom_point(size=2.5)+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10))+
  labs(title="Tile Spat Collectors:  Total Spat by Station/Month",x="Month", y="Spat/Tile")


### Combined Plots ###
com1=combined2[which(combined2$station=="WQ1"),]
com2=combined2[which(combined2$station=="WQ2"),]
com3=combined2[which(combined2$station=="WQ3"),]
com4=combined2[which(combined2$station=="WQ4"),]
com5=combined2[which(combined2$station=="WQ5"),]
com6=combined2[which(combined2$station=="WQ6"),]
com7=combined2[which(combined2$station=="WQ7"),]
com8=combined2[which(combined2$station=="WQ8"),]
com9=combined2[which(combined2$station=="WQ9"),]
com10=combined2[which(combined2$station=="WQ10"),]

WQ1=ggplot(com1, aes(x=month_num, y=total, shape=type))+
  ylim(0,3000)+
  geom_point(size=2.5)+
  labs(title="WQ1",x="", y="")+
  theme(legend.position=c(0.66,0.8),
        legend.background=element_rect(color="black",size=0.5))+
  scale_x_discrete(limits=c("4","5","6","7","8","9","10","11","12","1","2","3"))

WQ2=ggplot(com2, aes(x=month_num, y=total, shape=type))+
  ylim(0,3000)+
  geom_point(size=2.5)+
  expand_limits(x=4,y=0)+
  labs(title="WQ2",x="", y="")+
  theme(legend.position="none")+
  scale_x_discrete(limits=c("4","5","6","7","8","9","10","11","12","1","2","3"))

WQ3=ggplot(com3, aes(x=month_num, y=total, shape=type))+
  ylim(0,3000)+
  geom_point(size=2.5)+
  labs(title="WQ3",x="Month", y="")+
  theme(legend.position="none")+
  scale_x_discrete(limits=c("4","5","6","7","8","9","10","11","12","1","2","3"))

WQ4=ggplot(com4, aes(x=month_num, y=total, shape=type))+
  ylim(0,3000)+
  geom_point(size=2.5)+
  labs(title="WQ4",x="", y="")+
  theme(legend.position="none")+
  scale_x_discrete(limits=c("4","5","6","7","8","9","10","11","12","1","2","3"))

WQ5=ggplot(com5, aes(x=month_num, y=total, shape=type))+
  ylim(0,3000)+
  geom_point(size=2.5)+
  labs(title="WQ5",x="", y="Total Spat")+
  theme(legend.position="none")+
  scale_x_discrete(limits=c("4","5","6","7","8","9","10","11","12","1","2","3"))

WQ6=ggplot(com6, aes(x=month_num, y=total, shape=type))+
  ylim(0,3000)+
  geom_point(size=2.5)+
  labs(title="WQ6",x="", y="")+
  theme(legend.position="none")+
  scale_x_discrete(limits=c("4","5","6","7","8","9","10","11","12","1","2","3"))

WQ7=ggplot(com7, aes(x=month_num, y=total, shape=type))+
  ylim(0,3000)+
  geom_point(size=2.5)+
  labs(title="WQ7",x="", y="")+
  theme(legend.position="none")+
  scale_x_discrete(limits=c("4","5","6","7","8","9","10","11","12","1","2","3"))

WQ8=ggplot(com8, aes(x=month_num, y=total, shape=type))+
  ylim(0,3000)+
  geom_point(size=2.5)+
  labs(title="WQ8",x="", y="")+
  theme(legend.position="none")+
  scale_x_discrete(limits=c("4","5","6","7","8","9","10","11","12","1","2","3"))

WQ9=ggplot(com9, aes(x=month_num, y=total, shape=type))+
  ylim(0,3000)+
  geom_point(size=2.5)+
  labs(title="WQ9",x="", y="")+
  theme(legend.position="none")+
  scale_x_discrete(limits=c("4","5","6","7","8","9","10","11","12","1","2","3"))

#WQ10=ggplot(com10, aes(x=month_num, y=total, shape=type))+
  #ylim(0,3000)+
  #geom_point(size=2.5)+
  #labs(title="WQ10",x="", y="")+
  #theme(legend.position="none")+
  #scale_x_discrete(limits=c("4","5","6","7","8","9","10","11","12","1","2","3"))

multiplot(WQ6,WQ5,WQ4,WQ1,WQ2,WQ3,WQ7,WQ8,WQ9,cols=3)
dev.copy2pdf(file="oyster/spat/fig/shell_tile_spat_total.pdf")

### Comparison, run these 3 plots together ###

#weight vs Volume Relationship for standarized counts
par(mfrow=c(1,3))
wt_vs_vol=lm(combined$total_L~combined$total_kg)
plot(shell_summary2$total_kg, shell_summary2$total_L, xlab="Spat/kg Shell", 
     ylab="Spat/L Shell", ylim=c(0,5500), xlim=c(0,5500),
     main="Shell Mass vs Shell Volume", pch=16, col="black")
abline(wt_vs_vol)

#Shell vs Tile Spat Collectors 

wt_vs_tile=lm(combined$total_kg~combined$tile_total)
plot(combined$tile_total, combined$total_kg, xlab="Spat/Tile", ylab="Spat/kg Shell", 
     ylim=c(0,5500), xlim=c(0,5500),
     main="Tile vs Shell Mass", pch=16, col="black")
abline(wt_vs_tile)

vol_vs_tile=lm(combined$total_L~combined$tile_total)
plot(combined$tile_total, combined$total_L, xlab="Spat/Tile", ylab="Spat/L Shell",
     ylim=c(0,5500), xlim=c(0,5500),
     main="Tile vs Shell Volume", pch=16, col="black")
abline(vol_vs_tile)

dev.copy2pdf(file="oyster/spat/fig/shell_tile_comp.pdf")
