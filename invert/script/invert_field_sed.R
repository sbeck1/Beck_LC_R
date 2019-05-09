#### invert field measurement/sediment summaries ####

sed=read.csv("invert/data/production/20190502_lc_invert_field_sed_recon.csv") 

library(ggplot2)
library(dplyr)
library(plotrix) #for stderr

###subtract tray weights
sed$sed_dry=sed$sed_samp_tot_drywt_g-sed$sed_samp_tray_wt_g
sed$sed_ash=sed$sed_samp_tot_ashwt_g-sed$sed_samp_tray_wt_g
sed$sed_org_c=sed$sed_dry-sed$sed_ash
sed$coarse=sed$org_samp_tot_drywt_g-sed$org_samp_tray_wt_g

###organic carbon totals
sed_org_c=sed[c("station","sample","sed_org_c")] %>%
group_by(station)%>%
  summarise_all(funs(mean,sd,std.error))

ggplot(sed_org_c, aes(x=station,y=sed_org_c_mean))+
  geom_bar(stat="identity",colour="black")+
  labs(title="Mean Organic Carbon Per Station",x="Station",y="Mean (g) +/- SE")+
  scale_x_discrete(limits=c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14"))+
  geom_errorbar(aes(ymin=sed_org_c_mean-sed_org_c_std.error,ymax=sed_org_c_mean+sed_org_c_std.error),width=.2)
dev.copy2pdf(file="invert/fig/invert_org_carbon_mean.pdf")

###coarse sediment weights (leftover material in organism samples)
sed_coarse=sed[c("station","sample","coarse")] %>%
  group_by(station)%>%
  summarise_all(funs(mean,sd,std.error))

ggplot(sed_coarse, aes(x=station,y=coarse_mean))+
  geom_bar(stat="identity",colour="black")+
  labs(title="Mean Coarse Sediment (>1mm) Per Station",x="Station",y="Mean (g) +/- SE")+
  scale_x_discrete(limits=c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14"))+
  geom_errorbar(aes(ymin=coarse_mean-coarse_std.error,ymax=coarse_mean+coarse_std.error),width=.2)
dev.copy2pdf(file="invert/fig/invert_coarse_sed_mean.pdf")

###merge with station coordinates and create CSV for Arc
sed_org_c=merge(invert_stn,sed_org_c)
write.csv(sed_org_c,"invert/fig/invert_org_carbon_mean.csv")

sed_course=merge(invert_stn,sed_coarse)
write.csv(sed_coarse,"invert/fig/invert_coarse_sed_mean.csv")

sp_sed=merge(comm_sp_abund_stn,sed)

ggplot(sp_sed, aes(x=coarse, y=total))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,color="black")+
  labs(title="Coarse Sediment (>1mm, g) vs Common Sp Abundance",x="Coarse Sediment (g)",y="Number of Organisms")+
  facet_wrap(~fam_sp)
dev.copy2pdf(file="invert/fig/comm_sp_v_coarse.pdf")

ggplot(sp_sed, aes(x=sed_org_c, y=total))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,color="black")+
  labs(title="Mean Organic Carbon vs Common Sp Abundance",x="Organic Carbon (g)",y="Number of Organisms")+
  facet_wrap(~fam_sp)
dev.copy2pdf(file="invert/fig/comm_sp_v_org_c.pdf")
