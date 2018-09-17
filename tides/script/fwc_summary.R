setwd("C:/Users/stevenbeck/Desktop/Reef_Project/R/Tides/NOAARaw_8727520")
D12=read.csv("Dec2012.csv")
J13=read.csv("Jan2013.csv")
D13=read.csv("Dec2013.csv")
J14=read.csv("Jan2014.csv")
D14=read.csv("Dec2014.csv")
J15=read.csv("Jan2015.csv")
D15=read.csv("Dec2015.csv")
J16=read.csv("Jan2016.csv")
D16=read.csv("Dec2016.csv")
J17=read.csv("Jan2017.csv")
D17=read.csv("Dec2017.csv")
J18=read.csv("Jan2018.csv")

DecJan6yr=rbind(D12,J13,D13,J14,D14,J15,D15,J16,D16,J17,D17,J18)
DecJan6yrLow=DecJan6yr[which(DecJan6yr$Water.Level<0),]
summary(DecJan6yrLow$Water.Level)
DecJan6yr_neg1=DecJan6yrLow[which(DecJan6yrLow$Water.Level< -1),]
DecJan6yr_neg2=DecJan6yr_neg1[which(DecJan6yr_neg1$Water.Level< -2),]

write.csv(DecJan6yrLow,"C:/Users/stevenbeck/Desktop/Reef_Project/R/Tides/DecJan6yrLow.csv")
write.csv(DecJan6yr_neg1,"C:/Users/stevenbeck/Desktop/Reef_Project/R/Tides/DecJan6yr_neg1.csv")
write.csv(DecJan6yr_neg2,"C:/Users/stevenbeck/Desktop/Reef_Project/R/Tides/DecJan6yr_neg2.csv")
