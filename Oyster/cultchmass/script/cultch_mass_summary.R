### Correlations of Cultch Mass with Live Oyster Density ###

library(dplyr)
library(ggplot2)

#import cultch file
cultch=read.csv("oyster/cultchmass/data/development/cultchmass.csv")

#add station
cultch$station=with(cultch,paste0(locality,site,bar))

#remove bucket weight from small-large reef measurements. total reef measurement will not be used.
cultch$lg=cultch$lg_rf_kg-cultch$bucket_kg
cultch$sm=cultch$sm_rf_kg-cultch$bucket_kg

#sum small plus large measurements = all material >5cm
cultch$tot=cultch$lg+cultch$sm


#summarize by station
small = cultch %>%
  group_by(station)%>%
  summarise(total_kg=sum(sm), mean_kg=mean(sm), sd_kg=sd(sm))

large = cultch %>%
  group_by(station)%>%
  summarise(total_kg=sum(lg), mean_kg=mean(lg), sd_kg=sd(lg))

total = cultch %>%
  group_by(station)%>%
  summarise(total_kg=sum(tot), mean_kg=mean(tot), sd_kg=sd(tot))

#add quartile
small$quart=ifelse(small$station=="LCI19",1,(ifelse(small$station=="LCI2",4,(ifelse(small$station=="LCI1",3,2)))))
large$quart=ifelse(large$station=="LCI19",1,(ifelse(large$station=="LCI2",4,(ifelse(large$station=="LCI1",3,2)))))
total$quart=ifelse(total$station=="LCI19",1,(ifelse(total$station=="LCI2",4,(ifelse(total$station=="LCI1",3,2)))))

#import reef density data
reefs=read.csv("oyster/cultchmass/data/development/cultchmass_reefdraw.csv")

#merge mass density data, sort by quartile to make plot color scheme work
sm_dens=merge(small,reefs,by=c("station","quart"))
sm_dens=sm_dens[order(sm_dens$quart),]
lg_dens=merge(large,reefs,by=c("station","quart"))
lg_dens=lg_dens[order(lg_dens$quart),]
tot_dens=merge(total,reefs,by=c("station","quart"))
tot_dens=tot_dens[order(tot_dens$quart),]




#### Repeat the above using the first two quadrats to compare two vs four quadrats per station ####
cultch2=cultch[which(cultch$quadrat==1|cultch$quadrat==2),]

small2 = cultch2 %>%
  group_by(station)%>%
  summarise(total_kg=sum(sm), mean_kg=mean(sm), sd_kg=sd(sm))

large2 = cultch2 %>%
  group_by(station)%>%
  summarise(total_kg=sum(lg), mean_kg=mean(lg), sd_kg=sd(lg))

total2 = cultch2 %>%
  group_by(station)%>%
  summarise(total_kg=sum(tot), mean_kg=mean(tot), sd_kg=sd(tot))

#add quartile
small2$quart=ifelse(small2$station=="LCI19",1,(ifelse(small2$station=="LCI2",4,(ifelse(small2$station=="LCI1",3,2)))))
large2$quart=ifelse(large2$station=="LCI19",1,(ifelse(large2$station=="LCI2",4,(ifelse(large2$station=="LCI1",3,2)))))
total2$quart=ifelse(total2$station=="LCI19",1,(ifelse(total2$station=="LCI2",4,(ifelse(total2$station=="LCI1",3,2)))))

#merge mass density data, sort by quartile to make plot color scheme work
sm_dens2=merge(small2,reefs,by=c("station","quart"))
sm_dens2=sm_dens2[order(sm_dens2$quart),]
lg_dens2=merge(large2,reefs,by=c("station","quart"))
lg_dens2=lg_dens2[order(lg_dens2$quart),]
tot_dens2=merge(total2,reefs,by=c("station","quart"))
tot_dens2=tot_dens2[order(tot_dens2$quart),]


### Plot Density v Total Mass

### 4 Quadrats

par(mfrow=c(1,3))
sm_d_vs_m=lm(sm_dens$density~sm_dens$total_kg)
plot(sm_dens$total_kg, sm_dens$density, xlab="Small Cultch Mass (kg) 5-10cm", 
     ylab="Oyster Density (per sq m)", ylim=c(0,500), xlim=c(0,20),
     main="4 Quadrats Per Station",
     pch=sm_dens$quart)
abline(sm_d_vs_m)
legend("top",legend=c(1:4),pch=sm_dens$quart,title="Quartile")

lg_d_vs_m=lm(lg_dens$density~lg_dens$total_kg)
plot(lg_dens$total_kg, lg_dens$density, xlab="Large Cultch Mass (kg) >10cm", 
     ylim=c(0,500), xlim=c(0,20),
     ylab="",pch=sm_dens$quart)
abline(lg_d_vs_m)

tot_d_vs_m=lm(tot_dens$density~tot_dens$total_kg)
plot(tot_dens$total_kg, tot_dens$density, xlab="Total Cultch Mass (kg) >5cm", 
     ylim=c(0,500), xlim=c(0,20),
     ylab="",pch=sm_dens$quart)
abline(tot_d_vs_m)

dev.copy2pdf(file="oyster/cultchmass/fig/CultchMass_v_Density_4Quad.pdf")

### 2 Quadrats

par(mfrow=c(1,3))
sm_d_vs_m2=lm(sm_dens$density~sm_dens2$total_kg)
plot(sm_dens2$total_kg, sm_dens2$density, xlab="Small Cultch Mass (kg) 5-10cm", 
     ylab="Oyster Density (per sq m)", ylim=c(0,500), xlim=c(0,20),
     main="2 Quadrats Per Station",
     pch=sm_dens2$quart)
abline(sm_d_vs_m2)
legend("top",legend=c(1:4),pch=sm_dens2$quart,title="Quartile")

lg_d_vs_m2=lm(lg_dens2$density~lg_dens2$total_kg)
plot(lg_dens2$total_kg, lg_dens2$density, xlab="Large Cultch Mass (kg) >10cm", 
     ylim=c(0,500), xlim=c(0,20),
     ylab="",pch=sm_dens2$quart)
abline(lg_d_vs_m2)

tot_d_vs_m2=lm(tot_dens2$density~tot_dens2$total_kg)
plot(tot_dens2$total_kg, tot_dens2$density, xlab="Total Cultch Mass (kg) >5cm", 
     ylim=c(0,500), xlim=c(0,20),
     ylab="",pch=sm_dens$quart)
abline(tot_d_vs_m2)

dev.copy2pdf(file="oyster/cultchmass/fig/CultchMass_v_Density_2Quad.pdf")

#### linear model results
summary(sm_d_vs_m)
summary(lg_d_vs_m)
summary(tot_d_vs_m)
summary(sm_d_vs_m2)
summary(lg_d_vs_m2)
summary(tot_d_vs_m2)


### Boxplots:

par(mfrow=c(2,3))
boxplot(small$total_kg,data=small,xlab="Small (5-10cm)",ylab="Cultch Mass (kg)",ylim=c(0,20),main="4 Quadrats per Station" )
boxplot(large$total_kg,data=large,xlab="Large (>10cm)",ylim=c(0,20))
boxplot(total$total_kg,data=total,xlab="Total (>5cm)",ylim=c(0,20))
boxplot(small2$total_kg,data=small2,xlab="Small (5-10cm)",ylab="Cultch Mass (kg)",ylim=c(0,20),main="2 Quadrats per Station" )
boxplot(large2$total_kg,data=large2,xlab="Large (>10cm)",ylim=c(0,20))
boxplot(total2$total_kg,data=total2,xlab="Total (>5cm)",ylim=c(0,20))

### once more data collected set up figures by harvest strata


