
weight=read.csv("Oyster/Biomass/2Feb2018/20180202_oyster_biomass.csv")

#weight corrections to subtract tray weight
weight$Whole_wet_wt_corr=(weight$Whole_wet_wt_g-weight$Tray_wt_g)
weight$Wet_shell_wt_corr=(weight$Wet_shell_wt_g-weight$Tray_wt_g)
weight$wet_mt_wt_corr=(weight$Wet_mt_weight_g-weight$Tray_wt_g)
weight$dry_mt_wt_24corr=(weight$Dry_wt_24-weight$Tray_wt_g)
weight$dry_mt_wt_48corr=(weight$Dry_wt_48-weight$Tray_wt_g)
weight$dry_mt_wt_72corr=(weight$Dry_wt_72-weight$Tray_wt_g)
weight$dry_mt_wt_96corr=(weight$Dry_wt_96-weight$Tray_wt_g)
weight$dry_mt_wt_120corr=(weight$Dry_wt_120-weight$Tray_wt_g)
weight$dry_mt_wt_144corr=(weight$Dry_wt_144-weight$Tray_wt_g)

#interval weight differences
weight$diff_0_24=(weight$dry_mt_wt_24corr-weight$wet_mt_wt_corr)
summary(weight$diff_0_24)
weight$diff_24_48=(weight$dry_mt_wt_48corr-weight$dry_mt_wt_24corr)
summary(weight$diff_24_48)
weight$diff_48_72=(weight$dry_mt_wt_72corr-weight$dry_mt_wt_48corr)
summary(weight$diff_48_72)
weight$diff_72_96=(weight$dry_mt_wt_96corr-weight$dry_mt_wt_72corr)
summary(weight$diff_72_96)
weight$diff_96_120=(weight$dry_mt_wt_120corr-weight$dry_mt_wt_96corr)
summary(weight$diff_96_120)
weight$diff_120_144=(weight$dry_mt_wt_144corr-weight$dry_mt_wt_120corr)
summary(weight$diff_120_14)

#interval weight loss percentages compared to wet weight
weight$perc_0_24=((weight$diff_0_24/weight$wet_mt_wt_corr)*100)
summary(weight$perc_0_24)
weight$perc_24_48=((weight$diff_24_48/weight$wet_mt_wt_corr)*100)
summary(weight$perc_24_48)
weight$perc_48_72=((weight$diff_48_72/weight$wet_mt_wt_corr)*100)
summary(weight$perc_48_72)
weight$perc_72_96=((weight$diff_72_96/weight$wet_mt_wt_corr)*100)
summary(weight$perc_72_96)
weight$perc_96_120=((weight$diff_96_120/weight$wet_mt_wt_corr)*100)
summary(weight$perc_96_120)
weight$perc_120_144=((weight$diff_120_144/weight$wet_mt_wt_corr)*100)
summary(weight$perc_120_144)

#percentage total weight lost allocated to intervals (assuming 144 hr wt final)
weight$weight_end=weight$wet_mt_wt_corr-weight$dry_mt_wt_144corr
weight$weight_prop24=((weight$diff_0_24/weight$weight_end)*100)
summary(weight$weight_prop24)
weight$weight_prop48=((weight$diff_24_48/weight$weight_end)*100)
summary(weight$weight_prop48)
weight$weight_prop72=((weight$diff_48_72/weight$weight_end)*100)
summary(weight$weight_prop72)
weight$weight_prop96=((weight$diff_72_96/weight$weight_end)*100)
summary(weight$weight_prop96)
weight$weight_prop120=((weight$diff_96_120/weight$weight_end)*100)
summary(weight$weight_prop120)
weight$weight_prop144=((weight$diff_120_144/weight$weight_end)*100)
summary(weight$weight_prop144)

#cumulative weight loss
weight$cumm_wt_loss24=weight$weight_prop24
weight$cumm_wt_loss48=weight$cumm_wt_loss24+weight$weight_prop48
weight$cumm_wt_loss72=weight$cumm_wt_loss48+weight$weight_prop72
weight$cumm_wt_loss96=weight$cumm_wt_loss72+weight$weight_prop96
weight$cumm_wt_loss120=weight$cumm_wt_loss96+weight$weight_prop120
weight$cumm_wt_loss144=weight$cumm_wt_loss120+weight$weight_prop144

#% wet weight lost
weight$prop_wet_24=((weight$dry_mt_wt_24corr/weight$wet_mt_wt_corr)*100)
summary(weight$prop_wet_24)
weight$prop_wet_48=((weight$dry_mt_wt_48corr/weight$wet_mt_wt_corr)*100)
summary(weight$prop_wet_48)
weight$prop_wet_72=((weight$dry_mt_wt_72corr/weight$wet_mt_wt_corr)*100)
summary(weight$prop_wet_72)
weight$prop_wet_96=((weight$dry_mt_wt_96corr/weight$wet_mt_wt_corr)*100)
summary(weight$prop_wet_96)
weight$prop_wet_120=((weight$dry_mt_wt_120corr/weight$wet_mt_wt_corr)*100)
summary(weight$prop_wet_120)
weight$prop_wet_144=((weight$dry_mt_wt_144corr/weight$wet_mt_wt_corr)*100)
summary(weight$prop_wet_144)

#wet weight:shell conversion
weight$sh_mt_rat=((weight$Whole_wet_wt_corr-weight$wet_mt_wt_corr)/weight$wet_mt_wt_corr)
weight$sh_mt_rat2=weight$Wet_shell_wt_corr/weight$wet_mt_wt_corr
conver_sack=weight[which(weight$Height_mm>=75),]
summary(conver_sack$sh_mt_rat)
summary(conver_sack$sh_mt_rat2)

#add, wet:dry ratio, residuals, wet weight:shell conversion condition index section

#PLOTS
#plotting multiple plots per page
par(mfrow=c(3,6))
#interval scatterplots with linear regression
m24=lm(weight$dry_mt_wt_24corr~weight$wet_mt_wt_corr)
plot(weight$wet_mt_wt_corr,weight$dry_mt_wt_24corr,main="Wet Wt vs 24hr Dry Wt",xlab="", ylab="Dry Weight (g)", ylim=c(0,8))
abline(m24)
m48=lm(weight$dry_mt_wt_48corr~weight$wet_mt_wt_corr)
plot(weight$wet_mt_wt_corr,weight$dry_mt_wt_48corr,main="48hr Dry Wt",xlab="", ylab="", ylim=c(0,8))
abline(m48)
m72=lm(weight$dry_mt_wt_72corr~weight$wet_mt_wt_corr)
plot(weight$wet_mt_wt_corr,weight$dry_mt_wt_72corr,main="72hr Dry Wt",xlab="", ylab="", ylim=c(0,8))
abline(m72)
m96=lm(weight$dry_mt_wt_96corr~weight$wet_mt_wt_corr)
plot(weight$wet_mt_wt_corr,weight$dry_mt_wt_96corr,main="96hr Dry Wt",xlab="", ylab="", ylim=c(0,8))
abline(m96)
m120=lm(weight$dry_mt_wt_120corr~weight$wet_mt_wt_corr)
plot(weight$wet_mt_wt_corr,weight$dry_mt_wt_120corr,main="120hr Dry Wt",xlab="", ylab="", ylim=c(0,8))
abline(m120)
m144=lm(weight$dry_mt_wt_144corr~weight$wet_mt_wt_corr)
plot(weight$wet_mt_wt_corr,weight$dry_mt_wt_144corr,main="144hr Dry Wt",xlab="", ylab="", ylim=c(0,8))
abline(m144)
#interval weight difference plots
plot(weight$wet_mt_wt_corr,weight$diff_0_24,main="0-24hr Wt Diff",xlab="", ylab="Difference (g)", ylim=c(-40,0))
plot(weight$wet_mt_wt_corr,weight$diff_24_48,main="24-48hr Wt Diff",xlab="", ylab="", ylim=c(-5,0))
plot(weight$wet_mt_wt_corr,weight$diff_48_72,main="48-72hr Wt Diff",xlab="", ylab="", ylim=c(-0.2,0))
plot(weight$wet_mt_wt_corr,weight$diff_72_96,main="72-96hr Wt Diff",xlab="", ylab="", ylim=c(-0.2,0))
plot(weight$wet_mt_wt_corr,weight$diff_96_120,main="96-120hr Wt Diff",xlab="", ylab="", ylim=c(-0.2,0))
plot(weight$wet_mt_wt_corr,weight$diff_120_144,main="120-144hr Wt Diff",xlab="", ylab="", ylim=c(-0.2,0))
#interval percentage wet weight lost plots
plot(weight$wet_mt_wt_corr,weight$perc_0_24,main="Wet Wt Lost: 0-24hr",xlab="Wet Weight (g)", ylab="Percent", ylim=c(-100,0))
plot(weight$wet_mt_wt_corr,weight$perc_24_48,main="24-48hr",xlab="Wet Weight (g)", ylab="", ylim=c(-5,0))
plot(weight$wet_mt_wt_corr,weight$perc_48_72,main="48-72hr",xlab="Wet Weight (g)", ylab="", ylim=c(-0.5,0))
plot(weight$wet_mt_wt_corr,weight$perc_72_96,main="72-96hr",xlab="Wet Weight (g)", ylab="", ylim=c(-0.5,0))
plot(weight$wet_mt_wt_corr,weight$perc_96_120,main="96-120hr",xlab="Wet Weight (g)", ylab="", ylim=c(-0.5,0))
plot(weight$wet_mt_wt_corr,weight$perc_120_144,main="120-144hr",xlab="Wet Weight (g)", ylab="", ylim=c(-0.5,0))
