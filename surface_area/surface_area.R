library (dplyr)
library(tidyr)

con_guide=read.csv("surface_area/con_guide.csv")

con_guide %>%
  group_by(Value) %>%
  summarize (total= sum(sqft))
