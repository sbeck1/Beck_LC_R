##### Comparison of reader count variability for August 2018 spat collector data #####

### shell collectors

shell_comp=read.csv("oyster/spat/data/development/entry/2018_aug/shell_reader_comp.csv")

shell_comp$count=shell_comp$in_cnt+shell_comp$ex_cnt  #total spat count

library (ggplot2)
library (Rmisc) #multiplot
library (dplyr)

shell_summary = shell_comp %>%
  group_by(station,month,recorder)%>%
  summarise(total=sum(count), mean=mean(count), sd=sd(count))
shell_summary$type="SHELL"

ggplot(shell_summary, aes(x=station,y=total,shape=recorder))+
  geom_point(size=2.5)+
  scale_shape_manual(values=c(1,2))+
  labs(title="Shell Spat Collectors:  August 2018 Reader Comparison",x="Station", y="Count")

dev.copy2pdf(file="oyster/spat/fig/reader_comp/shell_reader_comp.pdf")

### tile collectors

tile_comp=read.csv("oyster/spat/data/development/entry/2018_aug/tile_reader_comp.csv")
tile_comp=tile_comp[which(tile_comp$cnt>=0 & tile_comp$month=="AUGUST"),]

tile_summary = tile_comp %>%
  group_by(station,year,recorder)%>%
  summarise(total=sum(cnt))

ggplot(tile_summary, aes(x=station,y=total,shape=recorder))+
  geom_point(size=2.5)+
  scale_shape_manual(values=c(1,2))+
  labs(title="Tile Spat Collectors:  August 2018 Reader Comparison",x="Station", y="Count")

dev.copy2pdf(file="oyster/spat/fig/reader_comp/tile_reader_comp.pdf")
