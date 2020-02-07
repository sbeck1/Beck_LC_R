grid=read.csv("oyster/reef_substrate_type/substrate.csv")

library(plyr)
grid_draw=ddply(grid,.(STATION),function(x) x[sample(nrow(x),3),])

grid_draw$quadrat=seq(1,3)

grid_draw$label=with(grid_draw,paste(STATION,quadrat),sep="-")

write.csv(grid_draw,"oyster/reef_substrate_type/substrate_draw.csv")
