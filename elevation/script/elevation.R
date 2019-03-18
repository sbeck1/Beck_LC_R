####import precon bathymetric survey data exported from ArcMap
bathy=read.csv("elevation/data/development/contractor/precon_bathy_survey/precon_bathy_utm_exp.csv")
#points within elements and labeled
bathy_clip=read.csv("elevation/data/development/contractor/precon_bathy_survey/precon_bathy_utm_clip_exp.csv")
#combine
bathy=merge(bathy_clip, bathy, by=c("point"), all=TRUE)

#clean/format
bathy$locality="LC"
bathy$site="O"
bathy$elev_m=bathy$depth_ft*0.3048
bathy=subset(bathy,select=-c(FID.x,FID.y,depth_ft))
bathy=bathy[with(bathy,order(point)),]
bathy=rename(bathy, c("point"="field_point","POINT_X"="easting","POINT_Y"="northing"))
bathy=bathy[c("field_point","easting","northing","elev_m","date","locality","site","bar","station")]
write.csv(bathy,"elevation/data/development/contractor/precon_bathy_survey/precon_bathy_format.csv")
#manually added to "elevation_prod.csv"



####import epoch 1 elevation data, manual cleaning:  un-needed columns removed, remaining columns renamed
e1=read.csv("elevation/data/development/uf/epoch1/epoch1_elevation_pf.csv")

#remove stations with no elevation data
e1[e1==""]=NA
e1_clean=e1[complete.cases(e1),]

summary(e1_clean$station)

#rename stations
e1_clean$station=as.character(e1_clean$station)
e1_clean$station[e1_clean$station=='LCN3']<-'LCN6'
e1_clean$station[e1_clean$station=='LCN4']<-'LCN3' #no elevation data
e1_clean$station[e1_clean$station=='LCN5']<-'LCN9'
e1_clean$station[e1_clean$station=='LCO1']<-'LCN9B' #no elevation data
e1_clean$station[e1_clean$station=='LCO2']<-'LCN8A' #no elevation data

e1_clean$station=as.factor(e1_clean$station)

write.csv(e1_clean, "elevation/data/development/uf/epoch1/epoch1_elevation_clean.csv")
#manual changes:  rename bars, remove duplicate elevation data from multiple oyster sampling trips (differing end lengths and repeat elevations)
#notes from manual cleanup: no 2.5m segment for CKO3, CRN1 data entered twice, deleted HBO1 due to differing elevations at same tran_length between dates/no way to choose which is correct

#final epoch 1 data:  epoch1_elevation_prod.csv


###prep table for Arc 
e_prod=read.csv("elevation/data/development/elevation_production.csv")

###heat map
#elevation size bins/no negative values allowed
e_neg=e_prod[which(e_prod$elev_m<=0),]
e_neg$neg_e_abs=abs(e_neg$elev_m)
#just postcon elevations

summary(e_neg$neg_e_abs)

library(FSA)
library(magrittr)
library(dplyr)
library(plotrix)
library(Matching)

e_neg %<>% mutate(elev_cat=lencat(neg_e_abs,w=0.1))
e_neg$elev_cat=-(e_neg$elev_cat)
#just postcon elevations
e_postcon=e_neg[which(e_neg$year>2017),]
write.csv(e_postcon, "elevation/data/development/e_postcon.csv")
#just epoch1 elevations
e_epoch1=e_neg[which(e_neg$year<2011),]
write.csv(e_epoch1, "elevation/data/development/e_epoch1.csv")




###Exploratory plots
library(ggplot2)
library(plotly)

#epoch1
e_2010=e_prod[which(e_prod$year==2010),]

#Entire dataset 
ggplot(e_2010,aes(x=elev_m))+
  labs(title="Frequency Histogram of Epoch 1 Reef Elevation")+
  geom_histogram(bins=30)+
  geom_vline(xintercept=0)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/epoch1_elev_hist.pdf")

ggplot(e_2010,aes(x=elev_m))+
  labs(title="Probability Density Function: Epoch 1 Reef Elevation")+
  geom_density(alpha=0.4)+
  geom_vline(xintercept=0)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/epoch1_elev_pdf.pdf")

#By site
ggplot(e_2010,aes(x=elev_m))+
  labs(title="Frequency Histogram of Epoch 1 Reef Elevation by Site")+
  geom_histogram(bins=30)+
  geom_vline(xintercept=0)+
  facet_wrap(~site)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/epoch1_elev_hist_site.pdf")

ggplot(e_2010,aes(x=elev_m, fill=site))+
  labs(title="Probability Density Function: Epoch 1 Reef Elevation by Site")+
  geom_density(alpha=0.4)+
  geom_vline(xintercept=0)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/epoch1_elev_pdf_site.pdf")

ggplot(e_2010,aes(x=tran_length,y=elev_m,color=site))+
  geom_point()+
  geom_smooth()+
  ylab("Elevation(m)")+
  xlab("Transect length (m)")
dev.copy2pdf(file="elevation/fig/epoch1_elev_plot_site.pdf")

#by locality*site

ggplot(e_2010,aes(x=elev_m, fill=site))+
  labs(title="Probability Density Function: Epoch 1 Reef Elevation by Site and Locality")+
  geom_density(alpha=0.4)+
  geom_vline(xintercept=0)+
  facet_wrap(~locality)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/epoch1_elev_pdf_site_local.pdf")

ggplot(e_2010,aes(x=tran_length,y=elev_m,color=site))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~locality)+
  ylab("Elevation(m)")+
  xlab("Transect length (m)")
dev.copy2pdf(file="elevation/fig/epoch1_elev_plot_site_local.pdf")

#by Locality*Site*Bar
ggplot(e_2010,aes(x=tran_length,y=elev_m,color=bar))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~locality)+
  ylab("Elevation(m)")+
  xlab("Transect length (m)")
dev.copy2pdf(file="elevation/fig/epoch1_elev_plot_local_site_bar.pdf")



#pre vs post construction
e_pp=e_prod[which(e_prod$collector=="YOUNG"|e_prod$collector=="DANIEL_GORE"),]
e_pp1=e_pp[-which(e_pp$bar=="-999"|e_pp$bar=="N_A"),]
e_pp2=e_pp1[-which(e_pp1$type=="BENCHMARK"),]
levels(e_pp$collector)[levels(e_pp$collector)=="YOUNG"]="precon"
levels(e_pp$collector)[levels(e_pp$collector)=="DANIEL_GORE"]="postcon"

levels(e_pp2$type)

ggplot(e_pp1,aes(x=elev_m, fill=collector))+
  labs(title="Probability Density Function: Lone Cabbage Reef Elevation Pre vs Post Construction")+
  geom_density(alpha=0.4)+
  geom_vline(xintercept=0)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/elev_pdf_lc_pre_post.pdf")


ggplot(e_pp2,aes(x=bar,y=elev_m,color=collector))+
  geom_point()+
  geom_smooth()+
  ylab("Elevation(m)")+
  xlab("Element")
dev.copy2pdf(file="elevation/fig/elev_plot_lc_pre_post.pdf")


#epoch3
#entire dataset
e_3=e_prod[which(e_prod$year>2017),]
e_3=e_3[-which(e_3$bar=="-999"|e_3$bar=="N_A"),]

ggplot(e_3,aes(x=elev_m))+
  labs(title="Frequency Histogram of Epoch 3 Reef Elevation")+
  geom_histogram(bins=30)+
  geom_vline(xintercept=0)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/epoch3_elev_hist.pdf")

ggplot(e_3,aes(x=elev_m))+
  labs(title="Probability Density Function: Epoch 3 Reef Elevation")+
  geom_density(alpha=0.4)+
  geom_vline(xintercept=0)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/epoch3_elev_pdf.pdf")

#by Site
ggplot(e_3,aes(x=elev_m))+
  labs(title="Frequency Histogram of Epoch 3 Reef Elevation by Site")+
  geom_histogram(bins=30)+
  geom_vline(xintercept=0)+
  facet_wrap(~site)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/epoch3_elev_hist_site.pdf")

ggplot(e_3,aes(x=elev_m, fill=site))+
  labs(title="Probability Density Function: Epoch 3 Reef Elevation by Site")+
  geom_density(alpha=0.4)+
  geom_vline(xintercept=0)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/epoch3_elev_pdf_site.pdf")


#by locality*site

ggplot(e_3,aes(x=elev_m, fill=site))+
  labs(title="Probability Density Function: Epoch 3 Reef Elevation by Site and Locality")+
  geom_density(alpha=0.4)+
  geom_vline(xintercept=0)+
  facet_wrap(~locality)+
  ylab("Frequency")+
  xlab("Elevation (m)")
dev.copy2pdf(file="elevation/fig/epoch3_elev_pdf_site_local.pdf")


#Epoch 3 UF stations only

#locality*Site*bar
e3_uf=e_3[which(e_3$collector=="UF"&e_3$type=="REEF"),]

ggplot(e3_uf,aes(x=tran_length,y=elev_m,color=bar))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~locality)+
  ylab("Elevation(m)")+
  xlab("Transect length (m)")
dev.copy2pdf(file="elevation/fig/epoch3_elev_plot_local_site_bar.pdf")
