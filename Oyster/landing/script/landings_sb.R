##########################

#Simple Landings Plot

##########################

library(plyr)
library(ggplot2)

landings = read.csv("CSV/1950to2018_oys_ldg.csv")
landings$Landings_mil=landings$Landings/1000000


ggplot(landings, aes(x=Year, y=Landings_mil, colour=Area)) +
  geom_line(size=1.5)+
theme_grey()+
  scale_x_continuous(breaks=c(1950,1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990,
                              1995,2000, 2005, 2010,2015,2020)) +
  theme(axis.text=element_text(size= 10),axis.title=element_text(size=10,
                                                                 face="bold"),
        plot.title =element_text(size=12, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Florida Oyster Landings by Major Area", y = "Landings (million lbs meat)")

  