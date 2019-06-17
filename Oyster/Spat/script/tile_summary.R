library (ggplot2)
library (dplyr)
library (plotly)

tile_spat=read.csv("oyster/spat/data/development/tile_spat_count.csv")
tile_spat2=tile_spat[which(tile_spat$cnt>=0),]

#surface area of tile = 

#format for merge
tile_summary = tile_spat2 %>%
  group_by(station,month,year)%>%
  summarise(tile_total=sum(cnt))  # multiple by xxxx to obtain spat per meter

#format for rbind
tile_summary2 = tile_spat2 %>%
  group_by(station,month,year)%>%
  summarise(total=sum(cnt)) 
tile_summary2$type="TILE"
str(tile_summary2)

mo2num=function(x) match(tolower(x),tolower(month.name))  #month name to month number
tile_summary2$month_num=mo2num(tile_summary2$month)
tile_summary2$month_num=factor(tile_summary2$month_num)

tile_summary2$year=as.factor(tile_summary2$year)
tile_summary2$station=factor(tile_summary2$station,levels=c("WQ6","WQ1","WQ7","WQ5","WQ2","WQ8","WQ4","WQ3","WQ9","WQ10"))

ggplot(tile_summary2, aes(x=month_num,y=total,shape=year))+
  geom_point(size=2)+
  labs(title="Tile Spat Collectors:  Total Spat by Station/Month/Year",x="Month", y="Spat per Tile")+
  facet_wrap(~station, ncol=3)+
  theme(axis.text.x=element_text(size=rel(0.8)))

dev.copy2pdf(file="oyster/spat/fig/tile_spat_total.pdf")

