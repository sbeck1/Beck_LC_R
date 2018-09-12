### Shell Spat Collectors ###

shell_spat=read.csv("oyster/spat/data/shell_spat_count.csv")
shell_spat$count=shell_spat$in_cnt+shell_spat$ex_cnt
shell_spat_apr=shell_spat[which(shell_spat$month=="APRIL"),]
shell_spat_june=shell_spat[which(shell_spat$month=="JUNE"),]
shell_spat_july=shell_spat[which(shell_spat$month=="JULY"),]


shell_bag=read.csv("oyster/spat/data/shell_spat_bag.csv")
shell_bag2=shell_bag[which(shell_bag$status=="RETR"&shell_bag$drill_cnt>=0),]


library (dplyr)
library (ggplot2)

#shell_spat_apr[!is.na(shell_spat_apr$count),]

apr_spat = shell_spat_apr %>%
  group_by(station)%>%
  summarise(total=sum(count), mean=mean(count), sd=sd(count))
apr_spat$month = "APRIL"

june_spat = shell_spat_june %>%
  group_by(station)%>%
  summarise(total=sum(count), mean=mean(count), sd=sd(count))
june_spat$month = "JUNE"

july_spat = shell_spat_july %>%
  group_by(station)%>%
  summarise(total=sum(count), mean=mean(count), sd=sd(count))
july_spat$month = "JULY"

shell_summary=rbind(apr_spat,june_spat,july_spat)
shell_summary$type="SHELL"

#standardizing counts by weight/volume
shell_summary2=merge(shell_summary,shell_bag2,by=c("station","month"))
shell_summary2$total_kg=(shell_summary2$total/shell_summary2$t_wt_g)*1000
shell_summary2$total_L=(shell_summary2$total/shell_summary2$t_vol_ml)*1000



### Tile Spat Collectors ###
tile_spat=read.csv("oyster/spat/data/tile_spat_count.csv")
tile_spat2=tile_spat[which(tile_spat$cnt>=0),]

tile_summary = tile_spat2 %>%
  group_by(station,month)%>%
  summarise(tile_total=sum(cnt))




### combining shell and tile collector data ###
combined=merge(shell_summary2,tile_summary,by=c("station","month"))




### Plots ###

### Shell Spat Collectors ###

#weight vs Volume Relationship for standarized counts
par(mfrow=c(1,1))
wt_vs_vol=lm(shell_summary2$total_L~shell_summary2$total_kg)
plot(shell_summary2$total_kg, shell_summary2$total_L, xlab="Spat/kg", 
     ylab="Spat/L", main="Standardized Spat Counts:  Weight vs Volume", pch=16, col="black")+
abline(wt_vs_vol)

#barplot of trends...not very good representation, line graph not working either...too few points?
#ggplot(shell_summary, aes(x=station,y=total,fill=month))+
  #labs(title="Shell Spat Collectors:  Total Spat by Station/Month", x="Station", y="Total Spat")+
  #geom_bar(position="dodge", colour="black", stat="identity", width=0.5)+
  #geom_text(size=3, position=position_dodge(0.9), aes(label=total, vjust=-1.5))+
  #scale_fill_brewer(palette="Greens")

#plot of time series
ggplot(shell_summary2, aes(x=month,y=total_kg,shape=station))+
  geom_point(size=2.5)+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9))+
  labs(title="Shell Spat Collectors:  Total Spat by Station/Month",x="Month", y="Spat/kg Shell")


### Tile Spat Collectors ###
ggplot(tile_summary, aes(x=month,y=tile_total,shape=station))+
  geom_point(size=2.5)+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9))+
  labs(title="Tile Spat Collectors:  Total Spat by Station/Month",x="Month", y="Spat/Tile")



### Comparison:  Shell vs Tile Spat Collectors ###
par(mfrow=c(1,2))

wt_vs_tile=lm(combined$total_kg~combined$tile_total)
plot(combined$tile_total, combined$total_kg, xlab="Spat/Tile", ylab="Spat/kg Shell", 
     main="Shell Weight vs Tile Spat Count", pch=16, col="black")
abline(wt_vs_tile)

vol_vs_tile=lm(combined$total_L~combined$tile_total)
plot(combined$tile_total, combined$total_L, xlab="Spat/Tile", ylab="Spat/L Shell", 
     main="Shell Volume vs Tile Spat Count", pch=16, col="black")
abline(vol_vs_tile)