trans_double=read.csv("oyster/transect/data/transect_draw_final_2019.csv")
trans_double=trans_double[which(trans_double$TYPE=="TRAN_ST"),]
trans_double=trans_double[which(trans_double$QUESTION==2),]

trans_double_rand=sample(nrow(trans_double))
trans_double2=trans_double[trans_double_rand,1:11]
91*.2 # 20% of transects doublecount per 2018-2019 = 18
write.csv(trans_double2, file="oyster/transect/data/trans_dblcnt_2019.csv")
