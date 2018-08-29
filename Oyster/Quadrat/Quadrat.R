rm(list=ls())

jan30=read.csv("Oyster/Quadrat/OysterData_30Jan2018_Quadrat_final.csv")
feb16=read.csv("Oyster/Quadrat/OysterData_16Feb2018_Quadrat_final.csv")
mar2=read.csv("20180302_oyster_quadrat_SB.csv")
quad=rbind(jan30,feb16,mar2)
quad$Bar2=paste(quad$Locality,quad$Bar)

quad_l=quad[which(quad$Live_Dead=='L'),]
quad_d=quad[which(quad$Live_Dead=='D'),]

quad_h=quad[which(quad$Height>0),]
quad_h_l=quad_h[which(quad$Live_Dead=='L'),]
quad_h_d=quad_h[which(quad$Live_Dead=='D'),]




#HEIGHT ANALYSIS

#SUMMARY: LIVE OYSTERS
library(data.table)
quad_h_l=data.table(quad_h_l)
quad_h_summary=quad_h_l[,.('mean'=mean(Height),'SE'=std.error(Height)),by=c('Locality')]
write.csv(quad_h_summary,'QuadHeight_summary.csv')

#NORMALITY TEST
shapiro.test(quad_h_l$Height)

#ANOVA + TUKEY POST HOC
h_lm=lm(Height~Locality,data=quad_h_l)
h_aov=aov(h_lm)
summary(h_aov)
tukey.test=TukeyHSD(h_aov)
print(tukey.test)
plot(tukey.test)

library('agricolae')
tukey.test2=HSD.test(h_aov,trt='Locality')
print(tukey.test2)

#NON-PARA ANOVA + TUKEY POST HOC
library('PMCMR')
kruskal.test(Height~Locality,data=quad_h_l)
posthoc.kruskal.nemenyi.test(x=quad_h_l$Height, g=quad_h_l$Locality, method="Tukey")

library('FSA')
library('magrittr')
library('dplyr')
library('plotrix')
library('Matching')

quad_h_l %<>% mutate(hcat5=lencat(Height,w=5))  #ALLOCATING OYSTERS INTO 5MM SIZE GROUPS

#HEIGHT SIZE FREQ HISTOGRAMS

par(mfrow=c(1,6))

hist(~Height,data=filter(quad_h_l,Locality=='NLC'),breaks=seq(0,120,5), 
     main="N Lone Cabbage",xlab="Height(mm)", ylab="Live Oyster Size Frequency", ylim=c(0,40))
hist(~Height,data=filter(quad_h_l,Locality=='BT'),breaks=seq(0,120,5), 
     main="Big Trout Creek",xlab="Height(mm)", ylab="", ylim=c(0,40))
hist(~Height,data=filter(quad_h_l,Locality=='LT'),breaks=seq(0,120,5), 
     main="Little Trout Creek",xlab="Height(mm)", ylab="", ylim=c(0,40))
hist(~Height,data=filter(quad_h_l,Locality=='NN'),breaks=seq(0,120,5), 
     main="No-Name Creek",xlab="Height(mm)", ylab="", ylim=c(0,40))
hist(~Height,data=filter(quad_h_l,Locality=='GC'),breaks=seq(0,120,5), 
     main="Giger Creek",xlab="Height(mm)", ylab="", ylim=c(0,40))
hist(~Height,data=filter(quad_h_l,Locality=='LC'),breaks=seq(0,120,5), 
     main="Lone Cabbage Inshore",xlab="Height(mm)", ylab="", ylim=c(0,40))

#HEIGHT SIZE FREQ PERCENT

hfreq=xtabs(~hcat5,data=filter(quad_h_l,Locality=='NLC')) 
write.csv(prop.table(hfreq)*100,'NLC_freq.csv')
NLC_freq=read.csv('NLC_freq.csv')
NLC_freq$Locality='NLC'

hfreq=xtabs(~hcat5,data=filter(quad_h_l,Locality=='BT')) 
write.csv(prop.table(hfreq)*100,'BT_freq.csv')
BT_freq=read.csv('BT_freq.csv')
BT_freq$Locality='BT'

hfreq=xtabs(~hcat5,data=filter(quad_h_l,Locality=='LT')) 
write.csv(prop.table(hfreq)*100,'LT_freq.csv')
LT_freq=read.csv('LT_freq.csv')
LT_freq$Locality='LT'

hfreq=xtabs(~hcat5,data=filter(quad_h_l,Locality=='NN')) 
write.csv(prop.table(hfreq)*100,'NN_freq.csv')
NN_freq=read.csv('NN_freq.csv')
NN_freq$Locality='NN'

hfreq=xtabs(~hcat5,data=filter(quad_h_l,Locality=='GC')) 
write.csv(prop.table(hfreq)*100,'GC_freq.csv')
GC_freq=read.csv('GC_freq.csv')
GC_freq$Locality='GC'

hfreq=xtabs(~hcat5,data=filter(quad_h_l,Locality=='LC')) 
write.csv(prop.table(hfreq)*100,'LC_freq.csv')
LC_freq=read.csv('LC_freq.csv')
LC_freq$Locality='LC'

percfreq=rbind(NLC_freq,BT_freq,LT_freq,NN_freq,BC_freq,LC_freq)

plot(NLC_freq$hcat5,NLC_freq$Freq,
     main="N Lone Cabbage",xlab="Height(mm)", ylab="Live Oyster Size Frequency (%)", xlim=c(0,110), ylim=c(0,25))
plot(BT_freq$hcat5,BT_freq$Freq, 
     main="Big Trout Creek",xlab="Height(mm)", ylab="", xlim=c(0,110), ylim=c(0,25))
plot(LT_freq$hcat5,LT_freq$Freq, 
     main="Little Trout Creek",xlab="Height(mm)", ylab="", xlim=c(0,110), ylim=c(0,25))
plot(NN_freq$hcat5,NN_freq$Freq, 
     main="No-Name Creek",xlab="Height(mm)", ylab="", xlim=c(0,110),ylim=c(0,25))
plot(GC_freq$hcat5,GC_freq$Freq, 
     main="Giger Creek",xlab="Height(mm)", ylab="", xlim=c(0,110), ylim=c(0,25))
plot(LC_freq$hcat5,LC_freq$Freq, 
     main="Lone Cabbage Inshore",xlab="Height(mm)", ylab="", xlim=c(0,110), ylim=c(0,25))




#HEIGHT ANALYSIS

#SUMMARY: DEAD OYSTERS
library(data.table)
quad_h_d=data.table(quad_h_d)
quad_h_summary_d=quad_h_d[,.('mean'=mean(Height),'SE'=std.error(Height)),by=c('Locality')]
write.csv(quad_h_summary_d,'QuadHeight_summary_d.csv')

#NORMALITY TEST
shapiro.test(quad_h_d$Height)

#ANOVA + TUKEY POST HOC
h_lm=lm(Height~Locality,data=quad_h_d)
h_aov=aov(h_lm)
summary(h_aov)
tukey.test=TukeyHSD(h_aov)
print(tukey.test)
plot(tukey.test)

library('agricolae')
tukey.test2=HSD.test(h_aov,trt='Locality')
print(tukey.test2)

#NON-PARA ANOVA + TUKEY POST HOC
library('PMCMR')
kruskal.test(Height~Locality,data=quad_h_d)
posthoc.kruskal.nemenyi.test(x=quad_h_d$Height, g=quad_h_d$Locality, method="Tukey")

library('FSA')
library('magrittr')
library('dplyr')
library('plotrix')
library('Matching')

quad_h_d %<>% mutate(hcat5=lencat(Height,w=5))  #ALLOCATING OYSTERS INTO 5MM SIZE GROUPS

#HEIGHT SIZE FREQ HISTOGRAMS

par(mfrow=c(1,6))

hist(~Height,data=filter(quad_h_d,Locality=='NLC'),breaks=seq(0,120,5), 
     main="N Lone Cabbage",xlab="Height(mm)", ylab="Dead Oyster Size Frequency", ylim=c(0,10))
hist(~Height,data=filter(quad_h_d,Locality=='BT'),breaks=seq(0,120,5), 
     main="Big Trout Creek",xlab="Height(mm)", ylab="", ylim=c(0,10))
hist(~Height,data=filter(quad_h_d,Locality=='LT'),breaks=seq(0,120,5), 
     main="Little Trout Creek",xlab="Height(mm)", ylab="", ylim=c(0,10))
hist(~Height,data=filter(quad_h_d,Locality=='NN'),breaks=seq(0,120,5), 
     main="No-Name Creek",xlab="Height(mm)", ylab="", ylim=c(0,10))
hist(~Height,data=filter(quad_h_d,Locality=='GC'),breaks=seq(0,120,5), 
     main="Giger Creek",xlab="Height(mm)", ylab="", ylim=c(0,10))
hist(~Height,data=filter(quad_h_d,Locality=='LC'),breaks=seq(0,120,5), 
     main="Lone Cabbage Inshore",xlab="Height(mm)", ylab="", ylim=c(0,10))

#HEIGHT SIZE FREQ PERCENT

hfreq=xtabs(~hcat5,data=filter(quad_h_d,Locality=='NLC')) 
write.csv(prop.table(hfreq)*100,'NLC_freq_d.csv')
NLC_freq_d=read.csv('NLC_freq_d.csv')
NLC_freq_d$Locality='NLC'

hfreq=xtabs(~hcat5,data=filter(quad_h_d,Locality=='BT')) 
write.csv(prop.table(hfreq)*100,'BT_freq_d.csv')
BT_freq_d=read.csv('BT_freq_d.csv')

hfreq=xtabs(~hcat5,data=filter(quad_h_d,Locality=='LT')) 
write.csv(prop.table(hfreq)*100,'LT_freq_d.csv')
LT_freq_d=read.csv('LT_freq_d.csv')

hfreq=xtabs(~hcat5,data=filter(quad_h_d,Locality=='NN')) 
write.csv(prop.table(hfreq)*100,'NN_freq_d.csv')
NN_freq_d=read.csv('NN_freq_d.csv')

hfreq=xtabs(~hcat5,data=filter(quad_h_d,Locality=='GC')) 
write.csv(prop.table(hfreq)*100,'GC_freq_d.csv')
GC_freq_d=read.csv('GC_freq_d.csv')

hfreq=xtabs(~hcat5,data=filter(quad_h_d,Locality=='LC')) 
write.csv(prop.table(hfreq)*100,'LC_freq_d.csv')
LC_freq_d=read.csv('LC_freq_d.csv')

plot(NLC_freq_d$hcat5,NLC_freq_d$Freq,
     main="N Lone Cabbage",xlab="Height(mm)", ylab="Dead Oyster Size Frequency (%)", xlim=c(0,110), ylim=c(0,25))
plot(BT_freq_d$hcat5,BT_freq_d$Freq, 
     main="Big Trout Creek",xlab="Height(mm)", ylab="", xlim=c(0,110), ylim=c(0,25))
plot(LT_freq_d$hcat5,LT_freq_d$Freq, 
     main="Little Trout Creek",xlab="Height(mm)", ylab="", xlim=c(0,110), ylim=c(0,25))
plot(NN_freq_d$hcat5,NN_freq_d$Freq, 
     main="No-Name Creek",xlab="Height(mm)", ylab="", xlim=c(0,110),ylim=c(0,25))
plot(GC_freq_d$hcat5,GC_freq_d$Freq, 
     main="Giger Creek",xlab="Height(mm)", ylab="", xlim=c(0,110), ylim=c(0,25))
plot(LC_freq_d$hcat5,LC_freq_d$Freq, 
     main="Lone Cabbage Inshore",xlab="Height(mm)", ylab="", xlim=c(0,110), ylim=c(0,25))






#DENSITY

#SUMMARY
quad_l=data.table(quad_l)
quad_cpue=quad_l[,.('cpue'=sum(Count)),by=c('Station','Bar2','Locality')]
quad_cpue$cpue_m2=((quad_cpue$cpue)*16)
write.csv(quad_cpue,'QuadCPUE.csv')

quad_cpue_summary=quad_cpue[,.('mean'=mean(cpue_m2),'SE'=std.error(cpue_m2)),by=c('Locality')]
write.csv(quad_cpue_summary,'QuadCPUE_summary.csv')

quad_cpue_summary_bar=quad_cpue[,.('mean'=mean(cpue_m2),'SE'=std.error(cpue_m2)),by=c('Bar2')]
write.csv(quad_cpue_summary_bar,'QuadCPUE_summary_bar.csv')

#NORMALITY TEST
shapiro.test(quad_cpue$cpue_m2)

kruskal.test(cpue_m2~Locality,data=quad_cpue)
posthoc.kruskal.nemenyi.test(x=quad_cpue$cpue_m2, g=quad_cpue$Locality, method="Tukey")

#SIZE CLASSES
quad_spat=quad_l[which(quad_l$Height<25),]
quad_seed=quad_l[which(quad_l$Height>=25),]
quad_seed2=quad_seed[which(quad_seed$Height<75),]
quad_sack=quad_l[which(quad_l$Height>=75),]

#SPAT
quad_spat_cpue=quad_spat[,.('cpue'=sum(Count)),by=c('Station','Bar2','Locality')]
quad_spat_cpue$cpue_m2=((quad_spat_cpue$cpue)*16)
write.csv(quad_spat_cpue,'QuadCPUE_spat.csv')

quad_spat_cpue_summary=quad_spat_cpue[,.('mean'=mean(cpue_m2),'SE'=std.error(cpue_m2)),by=c('Locality')]
quad_spat_cpue_summary$size="spat"
write.csv(quad_spat_cpue_summary,'QuadCPUE_summary_spat.csv')

quad_spat_cpue_summary_bar=quad_spat_cpue[,.('mean'=mean(cpue_m2),'SE'=std.error(cpue_m2)),by=c('Bar2')]
write.csv(quad_spat_cpue_summary_bar,'QuadCPUE_summary_bar_spat.csv')

shapiro.test(quad_spat_cpue$cpue_m2)

kruskal.test(cpue_m2~Locality,data=quad_spat_cpue)
posthoc.kruskal.nemenyi.test(x=quad_spat_cpue$cpue_m2, g=quad_spat_cpue$Locality, method="Tukey")

#SEED
quad_seed_cpue=quad_seed2[,.('cpue'=sum(Count)),by=c('Station','Bar2','Locality')]
quad_seed_cpue$cpue_m2=((quad_seed_cpue$cpue)*16)
write.csv(quad_seed_cpue,'QuadCPUE_seed.csv')

quad_seed_cpue_summary=quad_seed_cpue[,.('mean'=mean(cpue_m2),'SE'=std.error(cpue_m2)),by=c('Locality')]
quad_seed_cpue_summary$size="seed"
write.csv(quad_seed_cpue_summary,'QuadCPUE_summary_seed.csv')

quad_seed_cpue_summary_bar=quad_seed_cpue[,.('mean'=mean(cpue_m2),'SE'=std.error(cpue_m2)),by=c('Bar2')]
write.csv(quad_seed_cpue_summary_bar,'QuadCPUE_summary_bar_seed.csv')

shapiro.test(quad_seed_cpue$cpue_m2)

kruskal.test(cpue_m2~Locality,data=quad_seed_cpue)
posthoc.kruskal.nemenyi.test(x=quad_seed_cpue$cpue_m2, g=quad_seed_cpue$Locality, method="Tukey")

#SACK
quad_sack_cpue=quad_sack[,.('cpue'=sum(Count)),by=c('Station','Bar2','Locality')]
quad_sack_cpue$cpue_m2=((quad_sack_cpue$cpue)*16)
write.csv(quad_sack_cpue,'QuadCPUE_sack.csv')

quad_sack_cpue_summary=quad_sack_cpue[,.('mean'=mean(cpue_m2),'SE'=std.error(cpue_m2)),by=c('Locality')]
quad_sack_cpue_summary$size="sack"
write.csv(quad_sack_cpue_summary,'QuadCPUE_summary_sack.csv')

quad_sack_cpue_summary_bar=quad_sack_cpue[,.('mean'=mean(cpue_m2),'SE'=std.error(cpue_m2)),by=c('Bar2')]
write.csv(quad_sack_cpue_summary_bar,'QuadCPUE_summary_bar_sack.csv')

shapiro.test(quad_sack_cpue$cpue_m2)

kruskal.test(cpue_m2~Locality,data=quad_sack_cpue)
posthoc.kruskal.nemenyi.test(x=quad_sack_cpue$cpue_m2, g=quad_sack_cpue$Locality, method="Tukey")

#Graph CPUE BY SIZE CLASS
LocalityCPUE=rbind(quad_spat_cpue_summary,quad_seed_cpue_summary,quad_sack_cpue_summary)
library('ggplot2')
ggplot(LocalityCPUE,aes(x=Locality,y=mean,fill=size))+
  geom_bar(position=position_dodge(),stat="identity")+
  geom_errorbar(aes(ymin=mean-SE,ymax=mean+SE),
                width=.2,position = position_dodge(.9))
  

