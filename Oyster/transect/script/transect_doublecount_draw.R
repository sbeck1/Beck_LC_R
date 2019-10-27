#### random draw for which transect to doublecount ####

trans_double=read.csv("oyster/transect/data/transect_draw_final_2019.csv")
trans_double=trans_double[which(trans_double$TYPE=="TRAN_ST"),]
trans_double=trans_double[which(trans_double$QUESTION==2),]

trans_double_rand=sample(nrow(trans_double))
trans_double2=trans_double[trans_double_rand,1:11]
91*.2 # 20% of transects doublecount per 2018-2019 = 18
write.csv(trans_double2, file="oyster/transect/data/trans_dblcnt_2019.csv")

#### random draw for which segment of each transect to measure oyster heights  ####

rand_seg=read.csv("oyster/transect/data/rand_seg.csv")
rand_seg2=sample(rand_seg$segment,150,replace=T)
as.data.frame(rand_seg2)
