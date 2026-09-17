# exp1

library(dplyr)
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

social_dat <- dplyr::select(data, condition, starts_with('social'))
