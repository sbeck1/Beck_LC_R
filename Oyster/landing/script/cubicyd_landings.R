#Converting landings (pounds of meat) into shell removed (cubic yards)
#to be used in conjuction with the environment "50to18_landings.RData"

library(dplyr)
library(ggplot2)

######################################################
#shell pounds for apaalachicola
shell_pounds_apa <- landings_50to18 %>% 
  group_by(Year) %>%
  summarize(shell_pounds_apa = 3.578*(Landings_apa))

#shell pounds for suwannee
shell_pounds_suw <- landings_50to18 %>%
  group_by(Year) %>%
  summarize(shell_pounds_suw = 3.578*(Landings_suw))

#shell_pounds_state
shell_pounds_state <- landings_50to18 %>%
  group_by(Year) %>%
  summarize(shell_pounds_state = 3.578*(Landings_state))


#######################################################

#shell pounds converted to oyster shell cubic feet cubic yards
shell_cubicft_apa <- shell_pounds_apa %>% 
  group_by(Year) %>%
  summarize(shell_cubicft_apa = (shell_pounds_apa)/35)


shell_cubicft_suw <- shell_pounds_suw %>%
  group_by(Year) %>%
  summarize(shell_cubicft_suw = (shell_pounds_suw)/35)


shell_cubicft_state <- shell_pounds_state %>%
  group_by(Year) %>%
  summarize(shell_cubicft_state = (shell_pounds_state)/35)

######################################################

#shell cubic feet converted to cubic yards
shell_cubicyd_apa <- shell_cubicft_apa %>% 
  group_by(Year) %>%
  summarize(shell_cubicyd_apa = (shell_cubicft_apa)/27)


shell_cubicyd_suw <- shell_cubicft_suw %>%
  group_by(Year) %>%
  summarize(shell_cubicyd_suw = (shell_cubicft_suw)/27)


shell_cubicyd_state <- shell_cubicft_state %>%
  group_by(Year) %>%
  summarize(shell_cubicyd_state = (shell_cubicft_state)/27)

######################################################
#bind together in long form for easier plotting
Apalachicola <- select(shell_cubicyd_apa, Year, shell_cubicyd_apa)
Suwannee <- select(shell_cubicyd_suw, Year, shell_cubicyd_suw)
Florida <- select(shell_cubicyd_state, Year, shell_cubicyd_state)

Apalachicola$Region <- rep("Apalachicola", length(Apalachicola$Year))
colnames(Apalachicola) <- c('Year', 'Cubic_yards', 'Region')

Suwannee$Region <- rep("Suwannee", length(Suwannee$Year))
colnames(Suwannee) <- c('Year', 'Cubic_yards', 'Region')

Florida$Region <- rep("Florida", length(Florida$Year))
colnames(Florida) <- c('Year', 'Cubic_yards', 'Region')

landings_cubicyd_long <- rbind(Apalachicola,Suwannee,Florida)

#split up into pre and post
landings_pre <- subset(landings_cubicyd_long, landings_cubicyd_long$Year<1986)
landings_post <- subset(landings_cubicyd_long, landings_cubicyd_long$Year>1986)
################################################################################
#now plot


ggplot() +
  labs(title = "Historical shell material removed in cubic yards 
      (before & after state required reporting)") +
  geom_point(data = landings_pre, aes(x = Year, y = Cubic_yards, 
                                      colour = Region)) +
  geom_point(data = landings_post, aes(x = Year, y = Cubic_yards, 
                                       colour = Region)) +
  geom_vline(xintercept = 1986, colour = "grey") +
  theme_classic()+
  scale_x_continuous(breaks=c(1950,1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990,
                              1995,2000, 2005, 2010,2015,2020)) +
  theme(axis.text=element_text(size=07),axis.title=element_text(size=07,
                                                                face="bold"),
        plot.title =element_text(size=12, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid"))
  
################################################################################
#Suwannee
ggplot(data = shell_cubicyd_suw, aes(x = Year, y = shell_cubicyd_suw)) +

  geom_vline(xintercept = 1986, colour = "grey") +
  geom_line(colour ="#CC79A7", size = 1) +
  theme_classic()+
  scale_x_continuous(breaks=c(1950,1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990,
                              1995,2000, 2005, 2010,2015,2020)) +
  theme(axis.text=element_text(size= 10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=12, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell material removed from Suwannee region 
(before and after mandatory reporting)", y = "Cubic Yards")
  

#####################
#Apalachicola
ggplot(data = shell_cubicyd_apa, aes(x = Year, y = shell_cubicyd_apa)) +
  
  geom_point(colour ="#009E73") +
  geom_vline(xintercept = 1986, colour = "grey") +
  theme_classic()+
  scale_x_continuous(breaks=c(1950,1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990,
                              1995,2000, 2005, 2010,2015,2020)) +
  theme(axis.text=element_text(size=07),axis.title=element_text(size=07,
                                                                face="bold"),
        plot.title =element_text(size=12, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell material removed from Apalachicola region 
       (before and after mandatory reporting)",
       y = "Cubic Yards")
#########################################################################
#Statewide
ggplot(data = shell_cubicyd_state, aes(x = Year, y = shell_cubicyd_state)) +
  
  geom_point(colour ="tomato") +
  geom_vline(xintercept = 1986, colour = "grey") +
  theme_classic()+
  scale_x_continuous(breaks=c(1950,1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990,
                              1995,2000, 2005, 2010,2015,2020)) +
  theme(axis.text=element_text(size=07),axis.title=element_text(size=07,
                                                                face="bold"),
        plot.title =element_text(size=12, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell material removed from all of Florida 
       (before and after mandatory reporting)",
       y = "Cubic Yards")



#comparison of total cubic yards harvested to total cubic yards of material being added
sum(Suwannee$Cubic_yards, na.rm = TRUE)


((17000 + 1000)/31674.47) 


#find out how many years we are restoring 
shell_cubicyd_suw %>%
  filter(Year > 1988) %>%
  summarise(sum(shell_cubicyd_suw))

#We are restoring the amount that has been removed since 1989  

            