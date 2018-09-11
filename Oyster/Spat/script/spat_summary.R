### Shell Spat Collectors ###

shell_spat=read.csv("oyster/spat/data/shell_spat_count.csv")
shell_bag=read.csv("oyster/spat/data/shell_spat_bag.csv")

shell_spat$count=shell_spat$in_cnt+shell_spat$ex_cnt

shell_spat_apr=shell_spat[which(shell_spat$month=="APRIL"),]
shell_spat_june=shell_spat[which(shell_spat$month=="JUNE"),]
shell_spat_july=shell_spat[which(shell_spat$month=="JULY"),]

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

par(mfrow=c(3,1))
ggplot(shell_summary, aes(x=station,y=total,fill=month))+
  labs(title="Shell Spat Collectors:  Total Spat by Station/Month", x="Station", y="Total Spat")+
  geom_bar(position="dodge", colour="black", stat="identity", width=0.5)+
  geom_text(size=3, position=position_dodge(0.9), aes(label=total, vjust=-1.5))+
  scale_fill_brewer(palette="Greens")

### Tile Spat Collectors ###

