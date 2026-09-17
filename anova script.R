data <- read.csv("data.csv") # header=3, skip = 19
colnames(data) <- c("condition","code", "att1", "att2", "att3", "att4", "att5", 
                    "att6", "att7", "att8", "social1","social2","social3","social4",
                    "social5","iden1","iden2", "anx1_1", "anx1_2", "anx1_3", "anx1_4", "anx1_5", "anx1_6", "anx1_7", "anx1_8", "anx1_9"
                    , "anx1_10", "anx1_11", "anx1_12", "anx1_13", "anx1_14", "anx1_15"
                    , "anx1_16", "anx1_17", "anx1_18", "anx1_19", "anx1_20", "anx2_1"
                    , "anx2_2", "anx2_3", "anx2_4", "anx2_5", "anx2_6", "anx2_7", "anx2_8"
                    , "anx2_9", "anx2_10", "anx2_11", "anx2_12", "anx2_13", "anx2_14"
                    , "anx2_15", "anx2_16", "anx2_17", "anx2_18", "anx2_19", "anx2_20")
data$att2 <- 6-as.numeric(data$att2)
data$att4 <- 6-as.numeric(data$att4)
data$att6 <- 6-as.numeric(data$att6)
att.sum <- data.frame()
for (i in unique(data$code)){
att.sum <- rbind(att.sum,c(sum(data$att1[data$code==i],data$att2[data$code==i],
                               data$att3[data$code==i],data$att4[data$code==i],
                               data$att5[data$code==i],data$att6[data$code==i],
                               data$att7[data$code==i],data$att8[data$code==i])))
}

social.sum <- data.frame()
for (i in unique(data$code)){
  social.sum <- rbind(social.sum,c(sum(data$social1[data$code==i],data$social2[data$code==i],
                                 data$social3[data$code==i],data$social4[data$code==i],
                                 data$social5[data$code==i])))
}

iden.sum <- data.frame()
for (i in unique(data$code)){
  iden.sum <- rbind(iden.sum,c(sum(data$iden1[data$code==i],data$iden2[data$code==i])))
}

anx1.sum <- data.frame()
for (i in unique(data$code)){
  anx1.sum <- rbind(anx1.sum,c(sum(data$anx1_1[data$code==i],data$anx1_2[data$code==i],
                                   data$anx1_3[data$code==i],data$anx1_4[data$code==i],
                                   data$anx1_5[data$code==i],data$anx1_6[data$code==i],
                                   data$anx1_7[data$code==i],data$anx1_8[data$code==i],
                                   data$anx1_9[data$code==i],data$anx1_10[data$code==i],
                                   data$anx1_11[data$code==i],data$anx1_12[data$code==i],
                                   data$anx1_13[data$code==i],data$anx1_14[data$code==i],
                                   data$anx1_15[data$code==i],data$anx1_16[data$code==i],
                                   data$anx1_17[data$code==i],data$anx1_18[data$code==i],
                                   data$anx1_19[data$code==i],data$anx1_20[data$code==i])))
}

anx2.sum <- data.frame()
for (i in unique(data$code)){
  anx2.sum <- rbind(anx2.sum,c(sum(data$anx2_1[data$code==i],data$anx2_2[data$code==i],
                                   data$anx2_3[data$code==i],data$anx2_4[data$code==i],
                                   data$anx2_5[data$code==i],data$anx2_6[data$code==i],
                                   data$anx2_7[data$code==i],data$anx2_8[data$code==i],
                                   data$anx2_9[data$code==i],data$anx2_10[data$code==i],
                                   data$anx2_11[data$code==i],data$anx2_12[data$code==i],
                                   data$anx2_13[data$code==i],data$anx2_14[data$code==i],
                                   data$anx2_15[data$code==i],data$anx2_16[data$code==i],
                                   data$anx2_17[data$code==i],data$anx2_18[data$code==i],
                                   data$anx2_19[data$code==i],data$anx2_20[data$code==i])))
}
datasum <- data.frame()
datasum <- cbind(data$condition,att.sum, social.sum, iden.sum, anx1.sum, anx2.sum)
colnames(datasum) <- c("condition","att","social","iden","anx1","anx2")
anovadata <- datasum[order(datasum$condition), ]

anovadata$condition1 <- ifelse(anovadata$condition == "1"| anovadata$condition == "2", "a1", 
                               ifelse(anovadata$condition == "3"|anovadata$condition == "4", "a2",NA))
anovadata$condition2 <- ifelse(anovadata$condition == "1"| anovadata$condition == "3", "b1", 
                               ifelse(anovadata$condition == "2"|anovadata$condition == "4", "b2",NA))

anovadata_att <- data.frame(cbind(anovadata$condition1, anovadata$condition2, anovadata$att))
anovadata_social <- data.frame(cbind(anovadata$condition1, anovadata$condition2, anovadata$social))
anovadata_iden <- data.frame(cbind(anovadata$condition1, anovadata$condition2, anovadata$iden))
source("anovakun_487.txt")
anovakun(anovadata_att, "ABs", 2,2, peta=T)
anovakun(anovadata_social, "ABs", 2,2, peta=T)
anovakun(anovadata_iden, "ABs", 2,2, peta=T)

