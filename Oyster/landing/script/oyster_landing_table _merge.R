#This code combines the two CSVs into a uniform format

install.packages("dplyr")
library(dplyr)

pre_1983 <- read.csv("CSV/Florida Commercial Marine Fish Landings.csv", header = TRUE)
post_1986 <- read.csv("CSV/post1986bycounty.csv", header = TRUE)


#################################################################
#for 1950 - 1983

#fix the dataframe dimensions
pre_1983 <- pre_1983[-c(35,36),] 


#start buildling the framework for the dataframe using different vectors
#year column
Year <- c(1950:1983)

#sum of Gulf and Franklin counties by year
Landings_apa <- rowSums(pre_1983[,c("Gulf", "Franklin")], na.rm =TRUE)
Landings_apa

#sum of Dixie and Levy counties by year
Landings_suw <- rowSums(pre_1983[,c("Dixie","Levy")], na.rm = TRUE)
Landings_suw

#sum of all FL counties
Landings_state <- pre_1983$Total
Landings_state

#now merge into one dataframe
landings_50to83 <- data.frame(Year,Landings_apa,Landings_suw,Landings_state)

landings_50to83[landings_50to83==0] <- NA

#add the 1984 and 1985 years and fill with NA

landings_50to83[nrow(landings_50to83) +1,] = list(1984, NA, NA,NA)
landings_50to83[nrow(landings_50to83) +1,] = list(1985, NA, NA,NA)

#######################################################################
#######################################################################
#for 1986 - 2018

post_1986 <- select(post_1986, Year, County.Landed, Pounds)

Dixie <- filter(post_1986, County.Landed == "DIXIE")
Gulf <- filter(post_1986, County.Landed == "GULF")
Franklin <- filter(post_1986, County.Landed == "FRANKLIN")
Levy <- filter(post_1986, County.Landed == "LEVY")



#export dataframes to add in NAs for missing years, then put them in the CSV folder and re-upload
write.csv(Dixie, file = "dixie.csv")
write.csv(Gulf, file = "gulf.csv")

Dixie <- read.csv("CSV/dixie.csv", header = TRUE)
Gulf <- read.csv("CSV/gulf.csv", header = TRUE)

Dixie <- select(Dixie, Year, Dixie.Pounds)
Gulf <- select(Gulf,Pounds)
Franklin <- select(Franklin, Pounds)
Levy <- select(Levy, Pounds)

colnames(Dixie) <- c("Year", "Dixie.Pounds") 
colnames(Gulf) <- c("Gulf.Pounds")
colnames(Franklin) <- c("Franklin.Pounds")
colnames(Levy) <- c("Levy.Pounds")


landings_86to18 <- cbind(Dixie, Gulf, Franklin, Levy)

#now sum the pounds for Dixie and Levy, Franklin and Gulf so that we can merge 
#them into the landings data
landings_86to18$Landings_suw <- rowSums(landings_86to18[,c("Dixie.Pounds","Levy.Pounds")], na.rm = TRUE)
landings_86to18$Landings_apa <- rowSums(landings_86to18[,c("Franklin.Pounds","Gulf.Pounds")], na.rm = TRUE)

#Now eliminate the old unnecessary columns to keep only the regional and state totals
landings_86to18 <- select(landings_86to18, Year, Landings_apa, Landings_suw)


########################################################
#now sum all of the counties by year using dplyr
Landings_state86to18 <- post_1986 %>%
  group_by(Year) %>%
  summarize(Landings_state = sum(Pounds))

#create the new column data.frame so we can merge it into landings_86to18 
Landings_state86to18 <- select(Landings_state86to18, Landings_state)

#crete the final form of the dataframe before merging 50to83 to 86to18
landings_86to18 <- cbind(landings_86to18, Landings_state86to18)


#############################
#merge the 1950-1983 and 1986-2018 dataframes into one 
landings_50to18 <- rbind(landings_50to83, landings_86to18)

rownames(landings_50to18) <- NULL

plot(landings_50to18$Year, landings_50to18$Landings_apa, type = "l")
plot(landings_50to18$Year, landings_50to18$Landings_suw, type = "l")
plot(landings_50to18$Year, landings_50to18$Landings_state, type = "l")



################################################################################
write.csv(landings_50to18, file = "1950to2018_historical_landings.csv")
################################################################################

rm(Dixie, Franklin, Gulf, Levy, post_1986,pre_1983, Landings_state86to18)


