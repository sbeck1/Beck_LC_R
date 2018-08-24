#code to read FWC SAS files with water quality data, and convert to csv

#setwd("C:/Users/billpine/Google Drive/GEBF Lone Cabbage Oyster/WQ/FWC_FIM")

#Note from Caleb
#I have attached a file that shows the structure of our water quality data.  
#I also attached our physical data, since you will need to merge the two together by Reference.  
#The physical data will provide location (GPS and Zone B), date, and time.

library(sas7bdat)

# read in water quality data (hydro) and physical data (physical)
hydro = read.sas7bdat("Cooperator_WQ/ckm_hydrolab.sas7bdat")
physical = read.sas7bdat("Cooperator_WQ/ckm_physical.sas7bdat")

# write to csv


write.csv(hydro,"Cooperator_WQ/FWC_waterquality.csv",row.names=F,na='',quote=F)
write.csv(physical,"Cooperator_WQ/FWC_physicaltable.csv",row.names=F,na='',quote=F)
