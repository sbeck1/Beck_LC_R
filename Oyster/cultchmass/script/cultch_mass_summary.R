### Correlations of Cultch Mass with Live Oyster Density ###

library(dplyr)
library(ggplot2)

#import cultch file
cultch=read.csv("oyster/cultchmass/data/production/20191011_cultchmass.csv")

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

#import reef density data
reefs=read.csv("oyster/cultchmass/data/development/cultchmass_reefdraw.csv")

#merge mass density data, sort by quartile to make plot color scheme work
sm_dens=merge(small,reefs,by=c("station"))
sm_dens=sm_dens[order(sm_dens$quart),]
lg_dens=merge(large,reefs,by=c("station"))
lg_dens=lg_dens[order(lg_dens$quart),]
tot_dens=merge(total,reefs,by=c("station"))
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

#merge mass density data, sort by quartile to make plot color scheme work
sm_dens2=merge(small2,reefs,by=c("station"))
sm_dens2=sm_dens2[order(sm_dens2$quart),]
lg_dens2=merge(large2,reefs,by=c("station"))
lg_dens2=lg_dens2[order(lg_dens2$quart),]
tot_dens2=merge(total2,reefs,by=c("station"))
tot_dens2=tot_dens2[order(tot_dens2$quart),]


### Plot Density v Total Mass

### 4 Quadrats

par(mfrow=c(2,3))
plot(sm_dens$density, sm_dens$total_kg, ylab="Small Cultch Mass (kg) 5-10cm", 
     xlab="", xlim=c(0,500), ylim=c(0,30),
     main="4 Quadrats Per Station",
     pch=sm_dens$quart)
legend("topright",legend=c(1:4),pch=c(1:4),title="Quartile")

plot(lg_dens$density, lg_dens$total_kg, ylab="Large Cultch Mass (kg) >10cm", 
     xlim=c(0,500), ylim=c(0,30),pch=sm_dens$quart,
     xlab="Oyster Density (per sq m)")
    

plot(tot_dens$density, tot_dens$total_kg, ylab="Total Cultch Mass (kg) >5cm", 
     xlim=c(0,500), ylim=c(0,30),
     xlab="",pch=sm_dens$quart)

### 2 Quadrats

plot(sm_dens2$density, sm_dens2$total_kg, ylab="Small Cultch Mass (kg) 5-10cm", 
     xlab="", xlim=c(0,500), ylim=c(0,30),
     main="2 Quadrats Per Station",
     pch=sm_dens2$quart)

plot(lg_dens2$density, lg_dens2$total_kg, ylab="Large Cultch Mass (kg) >10cm", 
     xlim=c(0,500), ylim=c(0,30),
     pch=sm_dens2$quart,
     xlab="Oyster Density (per 0.5 sq m)")

plot(tot_dens2$density, tot_dens2$total_kg, ylab="Total Cultch Mass (kg) >5cm", 
     xlim=c(0,500), ylim=c(0,30),
     xlab="",pch=sm_dens$quart)

dev.copy2pdf(file="oyster/cultchmass/fig/CultchMass_v_Density_sum_quart.pdf")

### Boxplots:

par(mfrow=c(2,3))
boxplot(small$total_kg,data=small,xlab="Small (5-10cm)",ylab="Cultch Mass (kg)",ylim=c(0,30),main="4 Quadrats per Station" )
boxplot(large$total_kg,data=large,xlab="Large (>10cm)",ylim=c(0,30))
boxplot(total$total_kg,data=total,xlab="Total (>5cm)",ylim=c(0,30))
boxplot(small2$total_kg,data=small2,xlab="Small (5-10cm)",ylab="Cultch Mass (kg)",ylim=c(0,30),main="2 Quadrats per Station" )
boxplot(large2$total_kg,data=large2,xlab="Large (>10cm)",ylim=c(0,30))
boxplot(total2$total_kg,data=total2,xlab="Total (>5cm)",ylim=c(0,30))

dev.copy2pdf(file="oyster/cultchmass/fig/CultchMass_box.pdf")


### Plots maintaining individual quadrats per Bill:
cultch_dens=merge(cultch,reefs,by=c("station"))
cultch_dens=cultch_dens[order(cultch_dens$quart),]

par(mfrow=c(1,3))
plot(cultch_dens$density, cultch_dens$sm, xlab="", 
     ylab="Small Cultch Mass (kg) 5-10cm", ylim=c(0,12), xlim=c(0,500),
     pch=cultch_dens$quart)

plot(cultch_dens$density, cultch_dens$lg, xlab="Oyster Density (per sq m)", 
     ylab="Large Cultch Mass (kg) >10cm", ylim=c(0,12), xlim=c(0,500),
     pch=cultch_dens$quart)
legend("top",legend=c(1,2,3,4),pch=c(1,2,3,4),title="Density Quartile")

plot(cultch_dens$density, cultch_dens$tot, xlab="", 
     ylab="Total Cultch Mass (kg) >5cm", ylim=c(0,12), xlim=c(0,500),
     pch=cultch_dens$quart)

dev.copy2pdf(file="oyster/cultchmass/fig/CultchMass_v_Density_raw.pdf")

### plot of Small v Large cultch relationship per Bill

par(mfrow=c(1,1),pty="s")
plot(cultch_dens$sm, cultch_dens$lg, xlab="Small Cultch Mass (kg) 5-10cm", 
     ylab="Large Cultch Mass (kg) >10cm", ylim=c(0,10), xlim=c(0,10),
     pch=cultch_dens$quart)
legend("top",legend=c(1,2,3,4),pch=c(1,2,3,4),title="Density Quartile",cex=0.6)

dev.copy2pdf(file="oyster/cultchmass/fig/Sm_v_Lg_cultchmass_quart.pdf")

###Appears Quartile 1 and 4 show best relationship
quart1=cultch_dens[which(cultch_dens$quart==1),]
quart2=cultch_dens[which(cultch_dens$quart==2),]
quart3=cultch_dens[which(cultch_dens$quart==3),]
quart4=cultch_dens[which(cultch_dens$quart==4),]


par(mfrow=c(1,4))

lm1=lm(quart1$lg~quart1$sm)
plot(quart1$sm, quart1$lg, xlab="Small Cultch Mass (kg) 5-10cm", 
     ylab="Large Cultch Mass (kg) >10cm", ylim=c(0,10), xlim=c(0,10), 
     main = "Quartile 1*")
abline(lm1)

lm2=lm(quart2$lg~quart2$sm)
plot(quart2$sm, quart2$lg, xlab="", 
     ylab="", ylim=c(0,10), xlim=c(0,10), 
     main = "Quartile 2")
abline(lm2)

lm3=lm(quart3$lg~quart3$sm)
plot(quart3$sm, quart3$lg, xlab="", 
     ylab="", ylim=c(0,10), xlim=c(0,10), 
     main = "Quartile 3")
abline(lm3)

lm4=lm(quart4$lg~quart4$sm)
plot(quart4$sm, quart4$lg, xlab="", 
     ylab="", ylim=c(0,10), xlim=c(0,10), 
     main = "Quartile 4*")
abline(lm4)

dev.copy2pdf(file="oyster/cultchmass/fig/Sm_v_Lg_cultchmass_quart_lm.pdf")

#### linear model results
summary(lm1) #  P<0.05
summary(lm2)
summary(lm3)
summary(lm4) #  P<0.05

####### once all data collected set up figures by harvest strata


#merge mass density data, sort by strata to make plot color scheme work
sm_dens=sm_dens[order(sm_dens$strata),]
lg_dens=lg_dens[order(lg_dens$strata),]
tot_dens=tot_dens[order(tot_dens$strata),]

#repeat for 2 quadrata
sm_dens2=sm_dens2[order(sm_dens2$strata),]
lg_dens2=lg_dens2[order(lg_dens2$strata),]
tot_dens2=tot_dens2[order(tot_dens2$strata),]

### Plot Density v Total Mass

### 4 Quadrats

par(mfrow=c(2,3))
plot(sm_dens$density, sm_dens$total_kg, ylab="Small Cultch Mass (kg) 5-10cm", 
     xlab="", xlim=c(0,500), ylim=c(0,30),
     main="4 Quadrats Per Station",
     pch=c(1,2)[as.numeric(sm_dens$strata)])
legend("topright",legend=c("No Harvest","Harvest"),pch=c(1,2),title="Strata")

plot(lg_dens$density, lg_dens$total_kg, ylab="Large Cultch Mass (kg) >10cm", 
     xlim=c(0,500), ylim=c(0,30),
     xlab="Oyster Density (per sq m)",pch=c(1,2)[as.numeric(sm_dens$strata)])

plot(tot_dens$density, tot_dens$total_kg, ylab="Total Cultch Mass (kg) >5cm", 
     xlim=c(0,500), ylim=c(0,30),
     xlab="",pch=c(1,2)[as.numeric(sm_dens$strata)])

### 2 Quadrats

plot(sm_dens2$density, sm_dens2$total_kg, ylab="Small Cultch Mass (kg) 5-10cm", 
     xlab="", xlim=c(0,500), ylim=c(0,30),
     main="2 Quadrats Per Station",
     pch=c(1,2)[as.numeric(sm_dens$strata)])

plot(lg_dens2$density, lg_dens2$total_kg, ylab="Large Cultch Mass (kg) >10cm", 
     xlim=c(0,500), ylim=c(0,30),
     xlab="Oyster Density (per 0.5 sq m)",pch=c(1,2)[as.numeric(sm_dens$strata)])

plot(tot_dens2$density, tot_dens2$total_kg, ylab="Total Cultch Mass (kg) >5cm", 
     xlim=c(0,500), ylim=c(0,30),
     xlab="",pch=c(1,2)[as.numeric(sm_dens$strata)])

dev.copy2pdf(file="oyster/cultchmass/fig/CultchMass_v_Density_sum_strata.pdf")

### Boxplots:

par(mfrow=c(2,3))
boxplot(sm_dens$total_kg~sm_dens$strata,data=sm_dens,xlab="Small (5-10cm)",ylab="Cultch Mass (kg)",ylim=c(0,30),main="4 Quadrats per Station" )
boxplot(lg_dens$total_kg~lg_dens$strata,data=lg_dens,xlab="Large (>10cm)",ylim=c(0,30))
boxplot(tot_dens$total_kg~tot_dens$strata,data=tot_dens,xlab="Total (>5cm)",ylim=c(0,30))
boxplot(sm_dens2$total_kg~sm_dens2$strata,data=sm_dens2,xlab="Small (5-10cm)",ylab="Cultch Mass (kg)",ylim=c(0,30),main="2 Quadrats per Station" )
boxplot(lg_dens2$total_kg~lg_dens2$strata,data=lg_dens2,xlab="Large (>10cm)",ylim=c(0,30))
boxplot(tot_dens2$total_kg~tot_dens2$strata,data=tot_dens2,xlab="Total (>5cm)",ylim=c(0,30))

dev.copy2pdf(file="oyster/cultchmass/fig/CultchMass_box_strata.pdf")


####Total Reef appears different between harvest strata

shapiro.test(tot_dens$total_kg)  # therefore normal 

t.test(tot_dens$total_kg~tot_dens$strata)  #  P<0.05


par(mfrow=c(1,1),pty="s")
plot(cultch_dens$sm, cultch_dens$lg, xlab="Small Cultch Mass (kg) 5-10cm", 
     ylab="Large Cultch Mass (kg) >10cm", ylim=c(0,10), xlim=c(0,10),
     pch=c(1,2)[as.numeric(cultch_dens$strata)])
legend("topright",legend=c("No Harvest","Harvest"),pch=c(1,2),title="Strata")

dev.copy2pdf(file="oyster/cultchmass/fig/Sm_v_Lg_cultchmass_strata.pdf")
