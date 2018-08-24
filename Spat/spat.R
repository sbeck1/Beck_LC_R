setwd("C:/Users/stevenbeck/Desktop/Reef_Project/R/Spat")
shell_spat=read.csv("shell_spat_collectors.csv")
shell_spat_apr=shell_spat[which(shell_spat$month=="April"),]

library (dplyr)


shell_spat_apr$count=shell_spat_apr$int_count+shell_spat_apr$ext_count

shell_spat_apr[!is.na(shell_spat_apr$count),]

apr_spat_total = shell_spat_apr %>%
  group_by(station)%>%
  summarise(total_spat = sum(count))

apr_spat_mean = shell_spat_apr %>%
  group_by(station)%>%
  summarise(mean_spat = mean(count))

apr_spat_mean_max = shell_spat_apr %>%
  group_by(station)%>%
  summarise(mean_max_ht = mean(max_spat_ht_mm))
