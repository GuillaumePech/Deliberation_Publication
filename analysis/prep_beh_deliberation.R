library(funtimes)
library(plyr)

# .vsc.attach() in case workspace do not display variable
#new outliers

# # clean the Environment
# rm(list = ls())

outlier <- function(dat) {
  stop <- 0
  while (stop!=1){
  outlierp <- median(dat,na.rm =T) + (3 * mad(dat,na.rm =T))
  outlierm <- median(dat,na.rm =T) - (3 * mad(dat,na.rm =T))
  idx <- which(dat <= outlierm | dat >= outlierp)
  
  # outlierp <- quantile(dat,.75,na.rm =T) + (1.5 * IQR(dat,na.rm =T))
  # outlierm <- quantile(dat,.25,na.rm =T) - (1.5 * IQR(dat,na.rm =T))
  # idx <- c(idx,which(dat <= outlierm | dat >= outlierp))
  
  # outlierp <- mean(dat,na.rm =T) + (2.5 * sd(dat,na.rm =T))
  # outlierm <- mean(dat,na.rm =T) - (2.5 * sd(dat,na.rm =T))
  # idx <- c(idx,which(dat <= outlierm | dat >= outlierp))
  

  # idx <- unique(idx[idx %in% unique(idx)[table(idx)>1]])
#   idx <- unique(idx[duplicated(idx)])

  if (length(idx)>0){
  dat <- replace(dat, idx, NA)
  stop <- 1
  }
  else{
      stop<-1
  }
  }
  # dat <- dat[!is.na(dat)]
  return(dat)
}

setwd(dir= "C:/Users/mfbpe/Desktop/DATA/2022_deliberation/beh/") # nolint # nolint
df <- data.frame()
df3 <- data.frame()
df4 <- data.frame()
df_qst <- data.frame()
df_bad_rt <- data.frame()


#select the path of all the files with a specific name (here Pilot_Deliberation_Participant_*.csv, where * can be replace by everything)
files_path <- Sys.glob("Pilot_Deliberation_Participant_*.csv")
files_path2 <- Sys.glob("Pilot_Deliberation_Questionaire_*.csv")

for (i in seq_len(length(files_path))){
df1 <- read.csv(files_path[i], sep=";", header=TRUE)
df2 <- df1[1, 1:2]



original_data <- sum(!is.na(df1$RT_Answer))


idx <- which(df1$RT_Answer<1.2)

df1$RT <- df1$RT_Answer
df1$Will <- df1$Strength_Squeeze
df1$SoA <- df1$Tps_Esti

df1$RT_Answer <- outlier(df1$RT_Answer)
# # df1$Strength_Squeeze[which(df1$Hand_0left_1right==0)] <- outlier(df1$Strength_Squeeze[which(df1$Hand_0left_1right==0)])
# # df1$Strength_Squeeze[which(df1$Hand_0left_1right==1)] <- outlier(df1$Strength_Squeeze[which(df1$Hand_0left_1right==1)])
df1$Strength_Squeeze <- outlier(df1$Strength_Squeeze)
# df1$Tps_Esti <- outlier(df1$Tps_Esti)
df1$Tps_Esti[which(df1$Tps_Real==0)] <- outlier(df1$Tps_Esti[which(df1$Tps_Real==0)])
df1$Tps_Esti[which(df1$Tps_Real==300)] <- outlier(df1$Tps_Esti[which(df1$Tps_Real==300)])
df1$Tps_Esti[which(df1$Tps_Real==600)] <- outlier(df1$Tps_Esti[which(df1$Tps_Real==600)])

df1[idx,c(6,7,9)] <- NA

reject_short <- (length(idx)/original_data)*100
# reject_short <- 0
clean_rt <- sum(!is.na(df1$RT_Answer))
reject_rt <- -reject_short+100-(clean_rt / original_data)*100

clean_strength <- sum(!is.na(df1$Strength_Squeeze))
reject_strength <- -reject_short + 100- ( clean_strength / original_data)*100

clean_tps_esti <- sum(!is.na(df1$Tps_Esti))
reject_tps_esti <- -reject_short + 100-(clean_tps_esti / original_data)*100

#check for linear trend
df1$Tps_Real <- as.factor(df1$Tps_Real) #transform into factor to do a contrast
c1 <- c(-1,0,1) #to compare the three level of time
contrasts(df1$Tps_Real) <- c1 #attribue the contrast to real time
model1 <- aov(Tps_Esti ~ Tps_Real, data = df1) #do a anova with Estimated Times as outcome and Real time as predictor 
contrast_analysis <- coef(summary.lm(model1)) #store the results of the contrasts calculation
if(contrast_analysis[2,1] >0 ){
  p_value_linear_trend <- contrast_analysis[2,4] #store the p_value of the linear contrast
}else{
  p_value_linear_trend <- 1
}
df2$Version <- ifelse(i%%2!=0, 1,2)
df2$Linear_trend <- round(p_value_linear_trend,3)
df1$Linear_trend <-round(p_value_linear_trend,3)
df1$Version <- ifelse(i%%2!=0, 1,2)

# library(MASS)
# b <- boxcox(lm(df1$RT_Answer[df1$Condition==2]~1))
# lambda <- b$x[which.max(b$y)]
# lambda
# new_x_exact <- (df1$RT_Answer[df1$Condition==2] ^ lambda - 1) / lambda

# shapiro.test(df1$Tps_Esti)
# shapiro.test(new_x_exact)
# shapiro.test(df1$Strength_Squeeze)
# library(ggpubr)
# ggdensity(new_x_exact)

# df1$Tps_Esti <- df1$Tps_Esti/max(df1$Tps_Esti,na.rm=T)
# df1$RT_Answer <- df1$RT_Answer/max(df1$RT_Answer,na.rm=T)
# df1$Strength_Squeeze <- df1$Strength_Squeeze/max(df1$Strength_Squeeze,na.rm=T)

# df1$Tps_Esti <- scale(df1$Tps_Esti)
# df1$RT_Answer <- scale(df1$RT_Answer)
# df1$Strength_Squeeze <- scale(df1$Strength_Squeeze)
# df1$Tps_Esti <- log(df1$Tps_Esti)
# df1$RT_Answer <- log(df1$RT_Answer)
# df1$Strength_Squeeze <- log(df1$Strength_Squeeze)
# df1$Strength_Squeeze[which(df1$Hand_0left_1right==0)] <- scale(df1$Strength_Squeeze[which(df1$Hand_0left_1right==0)])
# df1$Strength_Squeeze[which(df1$Hand_0left_1right==1)] <- scale(df1$Strength_Squeeze[which(df1$Hand_0left_1right==1)])

df1$Ballon_Pos <- (df1$real_value_baloon_selected / (df1$real_value_baloon_selected+ df1$real_value_baloon_not_selected))*100
df1$Token_Pos <- (df1$real_value_token_selected / (df1$real_value_token_selected+ df1$real_value_token_not_selected))*100
df1$All_Pos <- ( (df1$real_value_baloon_selected +df1$real_value_token_selected) / (df1$real_value_baloon_selected +df1$real_value_token_selected +  df1$real_value_baloon_not_selected+ df1$real_value_token_not_selected))*100

df4 <- rbind(df4, df1)


df2$Soa_Arbi_200 <- if (length(df1$Tps_Esti[df1$Condition==0 & df1$Tps_Real==0])>5){ round(mean(df1$Tps_Esti[df1$Condition==0 & df1$Tps_Real==0],na.rm = T),3)}
df2$Soa_Edeli_200 <- if (length(df1$Tps_Esti[df1$Condition==1 & df1$Tps_Real==0])>5){round(mean(df1$Tps_Esti[df1$Condition==1 & df1$Tps_Real==0],na.rm = T),3)}
df2$Soa_Hdeli_200 <- if (length(df1$Tps_Esti[df1$Condition==2 & df1$Tps_Real==0])>5){round(mean(df1$Tps_Esti[df1$Condition==2 & df1$Tps_Real==0],na.rm = T),3)}
df2$Soa_Arbi_500 <- if (length(df1$Tps_Esti[df1$Condition==0 & df1$Tps_Real==300])>5){ round(mean(df1$Tps_Esti[df1$Condition==0 & df1$Tps_Real==300],na.rm = T),3)}
df2$Soa_Edeli_500 <- if (length(df1$Tps_Esti[df1$Condition==1 & df1$Tps_Real==300])>5){round(mean(df1$Tps_Esti[df1$Condition==1 & df1$Tps_Real==300],na.rm = T),3)}
df2$Soa_Hdeli_500 <- if (length(df1$Tps_Esti[df1$Condition==2 & df1$Tps_Real==300])>5){round(mean(df1$Tps_Esti[df1$Condition==2 & df1$Tps_Real==300],na.rm = T),3)}
df2$Soa_Arbi_800 <- if (length(df1$Tps_Esti[df1$Condition==0 & df1$Tps_Real==600])>5){ round(mean(df1$Tps_Esti[df1$Condition==0 & df1$Tps_Real==600],na.rm = T),3)}
df2$Soa_Edeli_800 <- if (length(df1$Tps_Esti[df1$Condition==1 & df1$Tps_Real==600])>5){round(mean(df1$Tps_Esti[df1$Condition==1 & df1$Tps_Real==600],na.rm = T),3)}
df2$Soa_Hdeli_800 <- if (length(df1$Tps_Esti[df1$Condition==2 & df1$Tps_Real==600])>5){round(mean(df1$Tps_Esti[df1$Condition==2 & df1$Tps_Real==600],na.rm = T),3)}
df2$Soa_Arbi <- if (length(df1$Tps_Esti[df1$Condition==0])>5){ round(mean(df1$Tps_Esti[df1$Condition==0],na.rm = T),3)}
df2$Soa_Edeli <- if (length(df1$Tps_Esti[df1$Condition==1])>5){round(mean(df1$Tps_Esti[df1$Condition==1],na.rm = T),3)}
df2$Soa_Hdeli <- if (length(df1$Tps_Esti[df1$Condition==2])>5){round(mean(df1$Tps_Esti[df1$Condition==2],na.rm = T),3)}
df2$RT_Arbi <-if (length(df1$RT_Answer[df1$Condition==0])>5){ round(mean(df1$RT_Answer[df1$Condition==0],na.rm = T),3)}
df2$RT_Edeli <- if (length(df1$RT_Answer[df1$Condition==1])>5){round(mean(df1$RT_Answer[df1$Condition==1],na.rm = T),3)}
df2$RT_Hdeli <- if (length(df1$RT_Answer[df1$Condition==2])>5){round(mean(df1$RT_Answer[df1$Condition==2],na.rm = T),3)}
df2$Strength_Squeeze_Arbi <- if (length(df1$Strength_Squeeze[df1$Condition==0])>5){round(mean(df1$Strength_Squeeze[df1$Condition==0],na.rm = T),3)}
df2$Strength_Squeeze_Edeli <- if (length(df1$Strength_Squeeze[df1$Condition==1])>5){round(mean(df1$Strength_Squeeze[df1$Condition==1],na.rm = T),3)}
df2$Strength_Squeeze_Hdeli <- if (length(df1$Strength_Squeeze[df1$Condition==2])>5){round(mean(df1$Strength_Squeeze[df1$Condition==2],na.rm = T),3)}
df2$Rejection_Short <- reject_short
df2$Rejection_RT <- reject_rt
df2$Rejection_Strength <- reject_strength
df2$Rejection_Tps_esti <- reject_tps_esti
df2$Score_baloon_Arbi <- mean(df1$Ballon_Pos[df1$Condition==0], na.rm=T)
df2$Score_baloon_Edeli <- mean(df1$Ballon_Pos[df1$Condition==1], na.rm=T)
df2$Score_baloon_Hdeli <- mean(df1$Ballon_Pos[df1$Condition==2], na.rm=T)
df2$Score_token_Arbi <- mean(df1$Token_Pos[df1$Condition==0], na.rm=T)
df2$Score_token_Edeli <- mean(df1$Token_Pos[df1$Condition==1], na.rm=T)
df2$Score_token_Hdeli <- mean(df1$Token_Pos[df1$Condition==2], na.rm=T)
df2$Score_both_Arbi <- mean(df1$All_Pos[df1$Condition==0], na.rm=T)
df2$Score_both_Edeli <- mean(df1$All_Pos[df1$Condition==1], na.rm=T)
df2$Score_both_Hdeli <- mean(df1$All_Pos[df1$Condition==2], na.rm=T)

# df_bad_rt <- rbind.fill(df_bad_rt,  data.frame(t(idx)))

df3 <- rbind(df3, cbind(df1$Condition, df1$RT_Answer, df1$Strength_Squeeze))

df1 <- tryCatch(read.csv(files_path2[i], sep = ";", header = TRUE),
 error = function(e) {
 0})
if (length(df1) > 1) {
df_qst <- rbind(df_qst,df1)
df2$Perf_Arbi <- round(mean(df1$Percentage_Perf[df1$Condition==0], na.rm=T),3)
df2$Perf_Edeli <- round(mean(df1$Percentage_Perf[df1$Condition==1], na.rm=T),3)
df2$Perf_Hdeli <- round(mean(df1$Percentage_Perf[df1$Condition==2], na.rm=T),3)

df2$Q1_Arbi <- round(mean((df1$Q1[df1$Condition==0]), na.rm=T),3)
df2$Q1_Edeli <- round(mean((df1$Q1[df1$Condition==1]), na.rm=T),3)
df2$Q1_Hdeli <- round(mean((df1$Q1[df1$Condition==2]), na.rm=T),3)
df2$Q2_Arbi <- round(mean((df1$Q2[df1$Condition==0]), na.rm=T),3)
df2$Q2_Edeli <- round(mean((df1$Q2[df1$Condition==1]), na.rm=T),3)
df2$Q2_Hdeli <- round(mean((df1$Q2[df1$Condition==2]), na.rm=T),3)
df2$Q3_Arbi <- round(mean((df1$Q3[df1$Condition==0]), na.rm=T),3)
df2$Q3_Edeli <- round(mean((df1$Q3[df1$Condition==1]), na.rm=T),3)
df2$Q3_Hdeli <- round(mean((df1$Q3[df1$Condition==2]), na.rm=T),3)
df2$Q4_Arbi <- round(mean((df1$Q4[df1$Condition==0]), na.rm=T),3)
df2$Q4_Edeli <- round(mean((df1$Q4[df1$Condition==1]), na.rm=T),3)
df2$Q4_Hdeli <- round(mean((df1$Q4[df1$Condition==2]), na.rm=T),3)
df2$Q5_Arbi <- round(mean((df1$Q5[df1$Condition==0]), na.rm=T),3)
df2$Q5_Edeli <- round(mean((df1$Q5[df1$Condition==1]), na.rm=T),3)
df2$Q5_Hdeli <- round(mean((df1$Q5[df1$Condition==2]), na.rm=T),3)
df2$Q6_Arbi <- round(mean((df1$Q6[df1$Condition==0]), na.rm=T),3)
df2$Q6_Edeli <- round(mean((df1$Q6[df1$Condition==1]), na.rm=T),3)
df2$Q6_Hdeli <- round(mean((df1$Q6[df1$Condition==2]), na.rm=T),3)
df2$Q7_Arbi <- round(mean((df1$Q7[df1$Condition==0]), na.rm=T),3)
df2$Q7_Edeli <- round(mean((df1$Q7[df1$Condition==1]), na.rm=T),3)
df2$Q7_Hdeli <- round(mean((df1$Q7[df1$Condition==2]), na.rm=T),3)
df2$Q8_Arbi <- round(mean((df1$Q8[df1$Condition==0]), na.rm=T),3)
df2$Q8_Edeli <- round(mean((df1$Q8[df1$Condition==1]), na.rm=T),3)
df2$Q8_Hdeli <- round(mean((df1$Q8[df1$Condition==2]), na.rm=T),3)
df2$Q9_Arbi <- round(mean((df1$Q9[df1$Condition==0]), na.rm=T),3)
df2$Q9_Edeli <- round(mean((df1$Q9[df1$Condition==1]), na.rm=T),3)
df2$Q9_Hdeli <- round(mean((df1$Q9[df1$Condition==2]), na.rm=T),3)
df2$Q10_Arbi <- round(mean((df1$Q10[df1$Condition==0]), na.rm=T),3)
df2$Q10_Edeli <- round(mean((df1$Q10[df1$Condition == 1]), na.rm=T),3)
df2$Q10_Hdeli <- round(mean((df1$Q10[df1$Condition == 2]), na.rm=T),3)
df2$Q11_Arbi <- round(mean((df1$Q11[df1$Condition == 0]), na.rm=T),3)
df2$Q11_Edeli <- round(mean((df1$Q11[df1$Condition == 1]), na.rm=T),3)
df2$Q11_Hdeli <- round(mean((df1$Q11[df1$Condition == 2]), na.rm=T),3)
}
df <- rbind.fill(df, df2)
}
df <- df[order(df$Participant),]
df4 <- df4[order(df4$Participant),]
df_qst <- df_qst[order(df_qst$Participant),]
# df_bad_rt <- df_bad_rt[order(df$Participant),]

# for (i in 4:ncol(df)){
#   df[,i] <- outlier(df[,i])
# }
write.table(df, file = paste(c("Extract_beh_deliberation_raw.csv"), collapse = ""), sep=";", dec = ".", row.names = FALSE)
write.table(df4, file = paste(c("Extract_beh_deliberation_lmer.csv"), collapse = ""), sep=";", dec = ".", row.names = FALSE)
write.table(df_qst, file = paste(c("Extract_beh_deliberation_qstr.csv"), collapse = ""), sep=";", dec = ".", row.names = FALSE)
# write.table(df_bad_rt, file = paste(c("final_bad_trials_rt.csv"), collapse = ""), sep=";", dec = ".", row.names = FALSE)

# library(xlsx)
# write.xlsx(df, file = paste(c("final_deliberationV2.xlsx"), collapse = ""), sheet='1')





