
####Need 2019-2020 densities by bar for cultch mass draw.  Copied this code from 20181002_summ_stats_plots.R script from project transect repository

library("dplyr")
library("ggplot2")
library("lubridate")
library("viridis")

#read in data
transect<- read.csv("oyster/transect/data/2019_2020_trans_data.csv", header= T)

#create season column
transect$Season<-ifelse(transect$month ==1 | transect$month == 10 | transect$month == 11 | transect$month == 12, "Winter", "Summer")

#Not all of the observations have time, so time would not parse all of the values
transect$date<- ymd(with(transect, paste(year,month,day, sep="-")))

#### SB:  remove -999 values for flooded transect segments

transect2=transect[which(transect$count_live>=0),]
transect2=transect2[which(transect2$tran_length>=0),]


########################
## Summary Statistics ##
########################

#aggregate counts data for each bar 
#on a day at a bar, this is all the live oysters counted
dta=aggregate(count_live~day+month+year+Season+treatment+locality+site+bar+station,data=transect2,sum)
#any transects with all 0s - yes, 6 of 256! (2%)

#aggregate data to count live per transect on each bar 

#dta1 has replicate (aka transect), so you will see the individual transects #on a bar
#this is the summary that field crews remember, how many transects that were #done
dta1=aggregate(count_live~day+month+year+Season+treatment+locality+site+transect,data=transect2,sum)

#this is summing the number of oysters at a bar. So if 3 transects done on 1 bar, 
#the number of oysters from those 3 transects is summed
#dta1.1 sums the individual transects on the bar
#dta1.1=aggregate(count_live~day+month+year+Season+treatment+locality+site,data=transect2,sum)

#aggregate data for transect length
#this is summing the length of transects at a bar. 
#So if 3 transects done on 1 bar, 
#the lengths of those 3 transects is summed

#note this is taking the max of the transect lengths, 
#but need to make sure this is doing 
#this on a per transect basis.  
#So if you have 3 replicate transects, tran 1 = 20 total, tran 2=
#18, tran 3= 22 these will eventually need to summed

# max has to be used for each transect because transect length 
#records the "segments" 2.5, 5, up to the max length
# so if you sum this then you end up with the wrong total length of transect 

dta2=aggregate(tran_length~day+month+year+Season+treatment+locality+site+bar+station+transect,data=transect2,max)

#so by including transect in dta2 it determines maximum length of each #transect when multiple transects done on a bar.

#### SB:  need to substract flooded segments from max transect lengths
#extract flooded segments
#flood_seg=transect[which(transect$count_live==-999),]
#sum flooded lengths for each transect (not complete, need to figure this out)


#really big decision, note that everything is being summed or counted by day, #month, year, season.  so if you do a station over two days this is summing
#it as what you did on day 1 and then what you did on day 2.  
#really should figure out what we are wanting to show,
#maybe this is just "winter year 1" and need to create dataframe with that #column

#now need to sum those max transects, this gives you the total length of all 
#oyster bar measured on a day when multiple transects were done on a bar
dta2.2=aggregate(tran_length~day+month+year+Season+treatment+locality+site+bar+station,data=dta2,sum)

#really big decision, note that everything is being summed or counted by day, #month, year, season.  so if you do a station over two days this is summing
#it as what you did on day 1 and then what you did on day 2.  
#really should figure out what we are wanting to show,
#maybe this is just "winter year 1" and need to create dataframe with that #column


#merge data frames
#so this merges the summed oyster counts by transect with the summed transect lengths  

#I've checked these by hand (January 2019) in terms of making sure the correct
#total transect length is calculated and it looks good

dta3=merge(dta,dta2.2,by=c("day","month","year","Season","treatment","locality","site","bar","station"))

sort.dta3<-dta3[order(dta3$year, dta3$month, dta3$station),]

#calculate density
dta3$area = dta3$tran_length*.154
dta3$density = dta3$count_live/dta3$area

####Jan 7 2020: just need harvest/no rock strata (Y_NA) behind deer island for cultch mass sampling. No flooded transects to affect densities.
mass_reefs=dta3[which(dta3$locality=="LC"&dta3$site!="O"),]
mass_reefs2=mass_reefs[-which(mass_reefs$station=="LCI23"|mass_reefs$station=="LCI24"),]
write.csv(mass_reefs2,"oyster/cultchmass/data/development/trans_dens_2019_2020.csv")

