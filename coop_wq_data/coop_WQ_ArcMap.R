#Need seperate year/month files to generate year/month heat maps in Arc

coopwq=read.csv("Cooperator_WQ/CooperatorWaterQuality.csv")

#attempting to create all month/year files at once (not working)
coopwq_y=split(coopwq,coopwq$Year)
list2env(coopwq_y,envir=.GlobalEnv)
Jan2017=2017[which(2017$Month==1),]

#creating files the slow way

#flood year = 1998 
wq1998=coopwq[which(coopwq$Year==1998),]

Jan1998=wq1998[which(wq1998$Month==1),]
write.csv(Jan1998,"Cooperator_WQ/ArcHeatMap/Jan1998.csv")

Feb1998=wq1998[which(wq1998$Month==2),]
write.csv(Feb1998,"Cooperator_WQ/ArcHeatMap/Feb1998.csv")

Mar1998=wq1998[which(wq1998$Month==3),]
write.csv(Mar1998,"Cooperator_WQ/ArcHeatMap/Mar1998.csv")

Apr1998=wq1998[which(wq1998$Month==4),]
write.csv(Apr1998,"Cooperator_WQ/ArcHeatMap/Apr1998.csv")

May1998=wq1998[which(wq1998$Month==5),]
write.csv(May1998,"Cooperator_WQ/ArcHeatMap/May1998.csv")

Jun1998=wq1998[which(wq1998$Month==6),]
write.csv(Jun1998,"Cooperator_WQ/ArcHeatMap/Jun1998.csv")

Jul1998=wq1998[which(wq1998$Month==7),]
write.csv(Jul1998,"Cooperator_WQ/ArcHeatMap/Jul1998.csv")

Aug1998=wq1998[which(wq1998$Month==8),]
write.csv(Aug1998,"Cooperator_WQ/ArcHeatMap/Aug1998.csv")

Sept1998=wq1998[which(wq1998$Month==9),]
write.csv(Sept1998,"Cooperator_WQ/ArcHeatMap/Sept1998.csv")

Oct1998=wq1998[which(wq1998$Month==10),]
write.csv(Oct1998,"Cooperator_WQ/ArcHeatMap/Oct1998.csv")

Nov1998=wq1998[which(wq1998$Month==11),]
write.csv(May1998,"Cooperator_WQ/ArcHeatMap/Nov1998.csv")

Dec1998=wq1998[which(wq1998$Month==12),]
write.csv(Dec1998,"Cooperator_WQ/ArcHeatMap/Dec1998.csv")

#drought year = 2012 
wq2012=coopwq[which(coopwq$Year==2012),]

Jan2012=wq2012[which(wq2012$Month==1),]
write.csv(Jan2012,"Cooperator_WQ/ArcHeatMap/Jan2012.csv")

Feb2012=wq2012[which(wq2012$Month==2),]
write.csv(Feb2012,"Cooperator_WQ/ArcHeatMap/Feb2012.csv")

Mar2012=wq2012[which(wq2012$Month==3),]
write.csv(Mar2012,"Cooperator_WQ/ArcHeatMap/Mar2012.csv")

Apr2012=wq2012[which(wq2012$Month==4),]
write.csv(Apr2012,"Cooperator_WQ/ArcHeatMap/Apr2012.csv")

May2012=wq2012[which(wq2012$Month==5),]
write.csv(May2012,"Cooperator_WQ/ArcHeatMap/May2012.csv")

Jun2012=wq2012[which(wq2012$Month==6),]
write.csv(Jun2012,"Cooperator_WQ/ArcHeatMap/Jun2012.csv")

Jul2012=wq2012[which(wq2012$Month==7),]
write.csv(Jul2012,"Cooperator_WQ/ArcHeatMap/Jul2012.csv")

Aug2012=wq2012[which(wq2012$Month==8),]
write.csv(Aug2012,"Cooperator_WQ/ArcHeatMap/Aug2012.csv")

Sept2012=wq2012[which(wq2012$Month==9),]
write.csv(Sept2012,"Cooperator_WQ/ArcHeatMap/Sept2012.csv")

Oct2012=wq1998[which(wq2012$Month==10),]
write.csv(Oct2012,"Cooperator_WQ/ArcHeatMap/Oct2012.csv")

Nov2012=wq2012[which(wq2012$Month==11),]
write.csv(May2012,"Cooperator_WQ/ArcHeatMap/Nov2012.csv")

Dec2012=wq2012[which(wq2012$Month==12),]
write.csv(Dec2012,"Cooperator_WQ/ArcHeatMap/Dec2012.csv")

#2017
wq2017=coopwq[which(coopwq$Year==2017),]

Dec2017=wq2017[which(wq2017$Month==12),]
write.csv(Dec2017,"Cooperator_WQ/ArcHeatMap/Dec2017.csv")


