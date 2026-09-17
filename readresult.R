data <- read.csv("data.csv") # header=3, skip = 19


colnames(data) <- c("code","doui", "age", 
                    "r_cv_1", "r_cv_2", "r_ci_1", "r_ci_2", 
                    "r_nv_1", "r_nv_2", "r_ni_1", "r_ni_2",
                    "h_cv_1", "h_cv_2", "h_ci_1", "h_ci_2", 
                    "h_nv_1", "h_nv_2", "h_ni_1", "h_ni_2",
                    "att1", "att2", "att3", "att4", "att5", "att6", "att7", "att8", "dqs", 
                    "social1", "social2", "social3", "social4", "social5", 
                    "iden1","iden2", "not")
# ?t?]???Ú‚ÌŒv?Z
data$att2 <- 6 - as.numeric(data$att2)
data$att4 <- 6 - as.numeric(data$att4)
data$att6 <- 6 - as.numeric(data$att6)

# att.sum ?Ìì¬
att.sum <- data.frame(code = unique(data$code), 
                      att_sum = sapply(unique(data$code), function(i) {
                        sum(data$att1[data$code == i], 
                            data$att2[data$code == i],
                            data$att3[data$code == i], 
                            data$att4[data$code == i],
                            data$att5[data$code == i], 
                            data$att6[data$code == i],
                            data$att7[data$code == i], 
                            data$att8[data$code == i])
                      }))

# social.sum ?Ìì¬
social.sum <- data.frame(code = unique(data$code),
                         social_sum = sapply(unique(data$code), function(i) {
                           sum(data$social1[data$code == i], 
                               data$social2[data$code == i],
                               data$social3[data$code == i], 
                               data$social4[data$code == i],
                               data$social5[data$code == i])
                         }))

# iden.sum ?Ìì¬
iden.sum <- data.frame(code = unique(data$code), 
                       iden_sum = sapply(unique(data$code), function(i) {
                         sum(data$iden1[data$code == i], 
                             data$iden2[data$code == i])
                       }))

# datasum ?Ìì¬
datasum <- data.frame(
  code = data$code, doui = data$doui, age = data$age, 
  r_cv_1 = data$r_cv_1, r_cv_2 = data$r_cv_2, r_ci_1 = data$r_ci_1, r_ci_2 = data$r_ci_2, 
  r_nv_1 = data$r_nv_1, r_nv_2 = data$r_nv_2, r_ni_1 = data$r_ni_1, r_ni_2 = data$r_ni_2, 
  h_cv_1 = data$h_cv_1, h_cv_2 = data$h_cv_2, h_ci_1 = data$h_ci_1, h_ci_2 = data$h_ci_2, 
  h_nv_1 = data$h_nv_1, h_nv_2 = data$h_nv_2, h_ni_1 = data$h_ni_1, h_ni_2 = data$h_ni_2, 
  att_sum = att.sum$att_sum, social_sum = social.sum$social_sum, 
  iden_sum = iden.sum$iden_sum, dqs = data$dqs, not = data$not
)

# ?????I?ÑŠÔˆá‚¢?Ìl?Æ“??Ó‚??È‚??Á‚??l?ğœ‚?
outdf <- subset(datasum, 
                (r_cv_1 == 1 & r_cv_2 == 4) | 
                  (r_ci_1 == 1 & r_ci_2 == 4) |
                  (r_nv_1 == 1 & r_nv_2 == 4) | 
                  (r_ni_1 == 1 & r_ni_2 == 4) |
                  (h_cv_1 == 1 & h_cv_2 == 4) | 
                  (h_ci_1 == 1 & h_ci_2 == 4) |
                  (h_nv_1 == 1 & h_nv_2 == 4) | 
                  (h_ni_1 == 1 & h_ni_2 == 4))

# DQS???Ú‚?5?ÈŠO?Ìs?ğœŠO
newdf <- subset(outdf, dqs == 5)

# ?ğŒ‚Ìƒy?A?ğƒŠƒX?g?Æ‚??Ä’??`
condition_pairs <- list(
  c("r_cv_1", "r_cv_2"),
  c("r_ci_1", "r_ci_2"),
  c("r_nv_1", "r_nv_2"),
  c("r_ni_1", "r_ni_2"),
  c("h_cv_1", "h_cv_2"),
  c("h_ci_1", "h_ci_2"),
  c("h_nv_1", "h_nv_2"),
  c("h_ni_1", "h_ni_2")
)

# ???Ê‚??Û‘????éƒŠ?X?g
result_list <- list()

# ?e?ğŒƒy?A?É‚Â‚??Äƒf?[?^?ğ’Šo
for (pair in condition_pairs) {
  col1 <- pair[1]  # ?y?A??1????
  col2 <- pair[2]  # ?y?A??2????
  
  # ?ğŒ‚ğ–‚????s?ğ’Šo???A?w?????Ì‚İ‘I??
  filtered_data <- newdf[!is.na(newdf[[col1]]) & !is.na(newdf[[col2]]), 
                         c("code", "att_sum", "social_sum", "iden_sum")]
  
  # ?f?[?^?????Å‚È‚??ê‡?Ì‚İ????ğ‘±‚???
  if (nrow(filtered_data) > 0) {
    # ?ğŒ–??????Æ‚??Ä’Ç‰?
    filtered_data$condition <- paste(col1, col2, sep = "_and_")
    
    # ???Ê‚ğƒŠƒX?g?É•Û‘?
    result_list[[paste(col1, col2, sep = "_and_")]] <- filtered_data
  }
}

# ?S?ğŒ‚ÌŒ??Ê‚?1?Â‚Ìƒf?[?^?t???[???ÉŒ????i?s???ğƒŠƒZ?b?g?j
combined_result <- do.call(rbind, result_list)

# ?s???ğƒŠƒZ?b?g
rownames(combined_result) <- NULL

# ???Ê‚??m?F
print(combined_result)

# csv?t?@?C???É????o??
write.csv(combined_result, "output.csv")
