library(brms); library(emmeans); library(lmerTest); library(bayestestR); library(dplyr); library(ggplot2)

setwd(dir = "C:/Users/mfbpe/Desktop/DATA/2022_deliberation//results/") # nolint # nolint
.vsc.attach()

cbind.fill <- function(...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) {
    rbind(x, matrix(, n - nrow(x), ncol(x)))
  }))
}

outlier <- function(dat) {

    outlierp <- median(dat, na.rm = T) + (3 * mad(dat, na.rm = T))
    outlierm <- median(dat, na.rm = T) - (3 * mad(dat, na.rm = T))
    idx <- which(dat <= outlierm | dat >= outlierp)

    if (length(idx) > 0) {
      dat <- replace(dat, idx, NA)
    } 
  }
  return(dat)
}

normalize <- function(x,na=F) {
    return((x- min(x,na.rm=na)) /(max(x,na.rm=na)-min(x,na.rm=na)))
}

df1 <- read.csv("Extract_beh_deliberation_lmer.csv", sep = ";", header = TRUE)
df2 <- read.csv("data_RP.csv", sep = ";", header = TRUE)
dfqst <- read.csv("Extract_beh_deliberation_qstr.csv", sep = ";", header = TRUE)
df_deli <- read.csv("Delib_report.csv", sep = ";", header = TRUE)

df1 <- df1[order(df1$Participant),]
df2 <- df2[order(df2$Participant),]

df3 <- cbind(df1[0,],df2[0,])

for (i in unique(df1$Participant)){
  part_df1 <- df1[df1$Participant==i,]
  part_df2 <- df2[df2$Participant==i,]
  if(i==18){
    ifelse(nrow(part_df1)>=nrow(part_df2),part_df2 <- part_df2[1:nrow(part_df1),],part_df1 <- part_df1[1:nrow(part_df2),])

    index_list<- which(abs(part_df2$RT_RP - part_df1$RT)>.02)
    part_df1 <- part_df1[-c(index_list[1]:(index_list[1]+4)),]

    ifelse(nrow(part_df1)>=nrow(part_df2),part_df2 <- part_df2[1:nrow(part_df1),],part_df1 <- part_df1[1:nrow(part_df2),])

  }else{
    if(nrow(df2[df2$Participant==i,])<216){
      a <-  216 - nrow(df2[df2$Participant==i,]) 
      part_df1 <- part_df1[part_df1$Participant==i,][-seq(1,a),]
    }else if (nrow(df2[df2$Participant==i,])>216){
      a <-  nrow(df2[df2$Participant==i,]) - 216
      part_df2 <- part_df2[part_df2$Participant==i,][-seq(1,a),]
    }

    ifelse(nrow(part_df1)>=nrow(part_df2),part_df2 <- part_df2[1:nrow(part_df1),],part_df1 <- part_df1[1:nrow(part_df2),])

   }
  df3 <- rbind(df3, cbind(part_df2,part_df1 ))
}
df3 <- df3[!duplicated(as.list(df3))]

df4<-df3
df3<-df4

col_name_df3 <- c("Participant" ,"outlier_RP" ,"mean_RP_early",  "slope_RP_early"  ,"mean_RP_late", "slope_RP_late",
 "mean_RP", "slope_RP", "Age" , "Block", "Condition", "Trial",  "Tps_Real", "RT", "Will", "SoA",  "Linear_trend" )


df3 <- df3[,col_name_df3]
df3<-df3[complete.cases(df3),]

df3$RT[df3$RT<1.2] <- NA
rejection <- c()
for (i in unique(df3$Participant)){
    rejection[i] <- sum(is.na(df3$RT[df3$Participant==i]))/nrow(df3[df3$Participant==i,])
}
mean(rejection,na.rm=T)
sd(rejection,na.rm=T)

df3<-df3[complete.cases(df3),]


df3$slope_RP_late[df3$outlier_RP==0]=NA; df3$mean_RP_late[df3$outlier_RP==0]=NA
df3$slope_RP_early[df3$outlier_RP==0]=NA;df3$mean_RP_early[df3$outlier_RP==0]=NA
df3$slope_RP[df3$outlier_RP==0]=NA;df3$mean_RP[df3$outlier_RP==0]=NA

rejection_Will <- c()
rejection_RT <- c()
rejection_SoA <- c()
rejection_meanRP <- c()
rejection_slopeRP <- c()

for (i in unique(df3$Participant)){
 
  df3$Will[df3$Participant==i & !is.na(df3$Participant==i)] <- normalize(df3$Will[df3$Participant==i & !is.na(df3$Participant==i)])*100 
  df3$Will[df3$Participant==i & !is.na(df3$Participant==i)] <- outlier(df3$Will[df3$Participant==i& !is.na(df3$Participant==i)])
  df3$RT[df3$Participant==i & !is.na(df3$Participant==i)] <- outlier(df3$RT[df3$Participant==i& !is.na(df3$Participant==i)])
  df3$SoA[df3$Participant==i & !is.na(df3$Participant==i) & df3$Tps_Real==0] <- outlier(df3$SoA[df3$Participant==i& !is.na(df3$Participant==i) & df3$Tps_Real==0])
  df3$SoA[df3$Participant==i & !is.na(df3$Participant==i) & df3$Tps_Real==300] <- outlier(df3$SoA[df3$Participant==i& !is.na(df3$Participant==i) & df3$Tps_Real==300])
  df3$SoA[df3$Participant==i & !is.na(df3$Participant==i) & df3$Tps_Real==600] <- outlier(df3$SoA[df3$Participant==i& !is.na(df3$Participant==i) & df3$Tps_Real==600])

  rejection_Will[i] <- sum(is.na(df3$Will[df3$Participant==i])) / length(df3$Participant[df3$Participant==i])
  rejection_RT[i] <- sum(is.na(df3$RT[df3$Participant==i])) / length(df3$Participant[df3$Participant==i])
  rejection_SoA[i] <- sum(is.na(df3$SoA[df3$Participant==i])) / length(df3$Participant[df3$Participant==i])
  rejection_meanRP[i] <- sum(is.na(df3$mean_RP[df3$Participant==i])) / length(df3$Participant[df3$Participant==i])
  rejection_slopeRP[i] <- sum(is.na(df3$slope_RP[df3$Participant==i])) / length(df3$Participant[df3$Participant==i])

  if (i!=29){
    for (j in unique(df3$Block)){      
      df3$planned[df3$Participant==i & df3$Condition==0 & df3$Block==j] <- dfqst$Q1[dfqst$Participant==i & dfqst$Block==j]
      df3$cause[df3$Participant==i & df3$Condition==0 & df3$Block==j] <- dfqst$Q2[dfqst$Participant==i &dfqst$Block==j]
      df3$control[df3$Participant==i & df3$Condition==0 & df3$Block==j] <- dfqst$Q3[dfqst$Participant==i& dfqst$Block==j]
      df3$predictable[df3$Participant==i & df3$Condition==0 & df3$Block==j] <- 100-dfqst$Q4[dfqst$Participant==i& dfqst$Block==j]
      df3$avoidable[df3$Participant==i & df3$Condition==0& df3$Block == j] <- 100-dfqst$Q5[dfqst$Participant==i & dfqst$Block == j]
      df3$desired[df3$Participant==i & df3$Condition==0& df3$Block == j] <- dfqst$Q6[dfqst$Participant==i & dfqst$Block == j]
      df3$ownwill[df3$Participant==i & df3$Condition==0& df3$Block == j] <- dfqst$Q7[dfqst$Participant==i & dfqst$Block == j]
      df3$voluntary[df3$Participant==i & df3$Condition==0& df3$Block == j] <- 100-dfqst$Q8[dfqst$Participant==i & dfqst$Block == j]
      df3$effortfull[df3$Participant==i & df3$Condition==0& df3$Block == j] <- 100-dfqst$Q9[dfqst$Participant==i & dfqst$Block == j]
      df3$generated[df3$Participant==i & df3$Condition==0& df3$Block == j] <- dfqst$Q10[dfqst$Participant==i & dfqst$Block == j]
      df3$intentional[df3$Participant==i & df3$Condition==0& df3$Block == j] <- 100-dfqst$Q11[dfqst$Participant==i & dfqst$Block == j]
      df3$planned[df3$Participant==i & df3$Condition==1& df3$Block == j] <- dfqst$Q1[dfqst$Participant==i & dfqst$Block == j]
      df3$cause[df3$Participant==i & df3$Condition==1& df3$Block == j] <- dfqst$Q2[dfqst$Participant==i & dfqst$Block == j]
      df3$control[df3$Participant==i & df3$Condition==1& df3$Block == j] <- dfqst$Q3[dfqst$Participant==i & dfqst$Block == j]
      df3$predictable[df3$Participant==i & df3$Condition==1& df3$Block == j] <-100- dfqst$Q4[dfqst$Participant==i & dfqst$Block == j]
      df3$avoidable[df3$Participant==i & df3$Condition==1& df3$Block == j] <- 100-dfqst$Q5[dfqst$Participant==i & dfqst$Block == j]
      df3$desired[df3$Participant==i & df3$Condition==1& df3$Block == j] <- dfqst$Q6[dfqst$Participant==i & dfqst$Block == j]
      df3$ownwill[df3$Participant==i & df3$Condition==1& df3$Block == j] <- dfqst$Q7[dfqst$Participant==i & dfqst$Block == j]
      df3$voluntary[df3$Participant==i & df3$Condition==1& df3$Block == j] <- 100-dfqst$Q8[dfqst$Participant==i & dfqst$Block == j]
      df3$effortfull[df3$Participant==i & df3$Condition==1& df3$Block == j] <- 100-dfqst$Q9[dfqst$Participant==i & dfqst$Block == j]
      df3$generated[df3$Participant==i & df3$Condition==1& df3$Block == j] <- dfqst$Q10[dfqst$Participant==i & dfqst$Block == j]
      df3$intentional[df3$Participant==i & df3$Condition==1& df3$Block == j] <- 100-dfqst$Q11[dfqst$Participant==i & dfqst$Block == j]
      df3$planned[df3$Participant==i & df3$Condition==2& df3$Block == j] <- dfqst$Q1[dfqst$Participant==i & dfqst$Block == j]
      df3$cause[df3$Participant==i & df3$Condition==2& df3$Block == j] <- dfqst$Q2[dfqst$Participant==i & dfqst$Block == j]
      df3$control[df3$Participant==i & df3$Condition==2& df3$Block == j] <- dfqst$Q3[dfqst$Participant==i & dfqst$Block == j]
      df3$predictable[df3$Participant==i & df3$Condition==2& df3$Block == j] <-100- dfqst$Q4[dfqst$Participant==i & dfqst$Block == j]
      df3$avoidable[df3$Participant==i & df3$Condition==2& df3$Block == j] <- 100-dfqst$Q5[dfqst$Participant==i & dfqst$Block == j]
      df3$desired[df3$Participant==i & df3$Condition==2& df3$Block == j] <- dfqst$Q6[dfqst$Participant==i & dfqst$Block == j]
      df3$ownwill[df3$Participant==i & df3$Condition==2& df3$Block == j] <- dfqst$Q7[dfqst$Participant==i & dfqst$Block == j]
      df3$voluntary[df3$Participant==i & df3$Condition==2& df3$Block == j] <-100- dfqst$Q8[dfqst$Participant==i & dfqst$Block == j]
      df3$effortfull[df3$Participant==i & df3$Condition==2& df3$Block == j] <- 100-dfqst$Q9[dfqst$Participant==i & dfqst$Block == j]
      df3$generated[df3$Participant==i & df3$Condition==2& df3$Block == j] <- dfqst$Q10[dfqst$Participant==i & dfqst$Block == j]
      df3$intentional[df3$Participant==i & df3$Condition==2& df3$Block == j] <- 100-dfqst$Q11[dfqst$Participant==i & dfqst$Block == j]
    }
  }
 
}

mean(rejection_Will); sd(rejection_Will)
mean(rejection_RT); sd(rejection_RT)
mean(rejection_SoA); sd(rejection_SoA)
mean(rejection_meanRP); sd(rejection_meanRP)
mean(rejection_slopeRP); sd(rejection_slopeRP)


qst_columns <- c("planned", "cause", "control" , "predictable", "avoidable", "desired" ,  "ownwill",   "voluntary" ,"effortfull" ,"generated", "intentional")
df3$SoV <-apply(df3[,qst_columns],1,FUN=mean)

write.table(df3, file = paste(c("all_data_lmer.csv"), collapse = ""), sep=";", dec = ".", row.names = FALSE)

df3 <- read.csv("all_data_lmer.csv", sep = ";", header = TRUE)

str(df3)

df3$Participant<-factor(df3$Participant)
df3$Condition<-factor(df3$Condition, labels = c("Arbi","Edeli", "Hdeli"))
df3$Tps_Real <- factor(df3$Tps_Real, labels = c("0", "300", "600"))
contrasts(df3$Condition) <- MASS::contr.sdif(3)
contrasts(df3$Tps_Real) <- MASS::contr.sdif(3)


df3$Trial<-scale(df3$Trial)


'''
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
###########################                        #####################################
###########################      BAYESIAN          #####################################
###########################                        #####################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
'''

setwd(dir = "C:/Users/mfbpe/Desktop/DATA/2022_deliberation//results/models/") # nolint # nolint

# ------------- SENSE OF AGENCY ----------------------------------

df5<-df3
df3<-df5
df3 <- df3[df3$Linear_trend<.05,]


formula_SoA <- bf(SoA ~  Condition + Tps_Real + scale(RT) + Trial + (Condition| Participant))
get_prior(formula_SoA, data = df3)


prior_Soa <- prior("normal(0,30)", class="b") + 
  prior("normal(0,150)", class="b", coef="Tps_Real2M1") + 
  prior("normal(0,150)", class="b", coef="Tps_Real3M2") + 
  prior("normal(500, 100)", class="Intercept") + 
  prior("normal(0,200)", class="sd") +
  prior("normal(0,30)", class="sd", coef="Condition2M1", group="Participant") +
  prior("normal(0,30)", class="sd", coef="Condition3M2", group="Participant") +
  prior("lkj(1)", class="cor") +
  prior("normal(0,250)", class="sigma", lb=0) 

mSoA_bayes <- brm(formula_SoA, data = df3, prior=prior_Soa, iter=2000, warmup = 1000, chains=1, backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())

save(mSoA_bayes, file = 'mSoA_bayes_28_05_24.Rdata')

#------------------------------------------------
#-----------------------WILL POWER-------------------------
#------------------------------------------------
df5<-df3
df3<-df5


formula_Will <- bf(Will ~  Condition + Trial + (Condition| Participant))


prior_Will <- prior("normal(0,10)", class="b") + 
  prior("normal(50, 20)", class="Intercept") + 
  prior("normal(0,20)", class="sd") +
  prior("normal(0,10)", class="sd", coef="Condition2M1", group="Participant") +
  prior("normal(0,10)", class="sd", coef="Condition3M2", group="Participant") +
  prior("lkj(1)", class="cor") +
  prior("normal(50,20)", class="sigma", lb=0, ub=100) 

mWill_bayes <- brm(formula_Will, data = df3, prior=prior_Will, sample_prior = "yes", iter=2000, warmup = 1000, chains=1, backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())

save(mWill_bayes, file = 'mWill_bayes.Rdata')



#------------------------------------------------------------
#-----------------------Reaction Times-------------------------
#------------------------------------------------------------
df3<-df5



formula_RT <- bf(RT ~  Condition + Trial + (Condition| Participant))

prior_RT <- prior("normal(0,1)", class="b") + 
  prior("cauchy(2,.5)", class="Intercept") + 
  prior("cauchy(0,.5)", class="sd") +
  prior("lkj(1)", class="cor") +
  prior("exponential(1)", class="sigma")

mRT_bayes <- brm(formula_RT, data = df3, prior=prior_RT, sample_prior = "yes",iter=12000, warmup = 2000, chains=4, backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())

save(mRT_bayes, file = 'mRT_bayes.Rdata')


#---------------------------------------------------------------------
#-----------------------READINESS POTENTIAL -------------------------
#---------------------------------------------------------------------
df5<-df3
df3<-df5

formula_mean_RP <- bf(mean_RP ~  Condition + scale(RT) + Trial + (Condition| Participant))

prior_mean_RP <- prior("normal(0,1)", class="b") + 
  prior("cauchy(0, 2)", class="Intercept") + 
  prior("cauchy(0, 2)", class="sd") +
  prior("lkj(1)", class="cor") +
  prior("exponential(1)", class="sigma")

mmean_RP_bayes <- brm(formula_mean_RP, data = df3, prior=prior_mean_RP, sample_prior = "yes",iter=12000, warmup = 2000, chains=4, backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())

save(mmean_RP_bayes, file = 'mmean_RP_bayes.Rdata')

formula_slope_RP <- bf(slope_RP ~  Condition + scale(RT) + Trial + (Condition| Participant))

prior_slope_RP <- prior("normal(0,1)", class="b") + 
  prior("cauchy(0, 2)", class="Intercept") + 
  prior("cauchy(0, 2)", class="sd") +
  prior("lkj(1)", class="cor") +
  prior("exponential(1)", class="sigma")

mslope_RP_bayes <- brm(formula_slope_RP, data = df3, prior=prior_slope_RP, sample_prior = "yes",iter=12000, warmup = 2000, chains=4, backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())

save(mslope_RP_bayes, file = 'mslope_RP_bayes.Rdata')


#------------------------------------------------------------
#-----------------------SENSE OF VOLITION--------------------
#------------------------------------------------------------
df5<-df3
df3<-df5
library(dplyr); library(ltm)
df3 <- df3%>%
  group_by(Participant, Block, Condition)%>%
  summarize(SoV=mean(SoV), planned = mean(planned),        cause = mean(cause),          control = mean(control),        predictable = mean(predictable),    avoidable = mean(avoidable),      desired = mean(desired), ownwill = mean(ownwill),        voluntary = mean(voluntary),      effortfull = mean(effortfull),     generated = mean(generated),      intentional = mean(intentional)  )
df3$Block <- scale(df3$Block)

cronbach.alpha(df3[,5:15], CI = T, probs = c(0.055, 0.945), B = 1000, na.rm=T )


formula_SoV <- bf(SoV ~  Condition + Block + (Condition| Participant))

prior_SoV <- prior("normal(0,25)", class="b") + 
  prior("cauchy(50, 25)", class="Intercept") + 
  prior("cauchy(0, 25)", class="sd") +
  prior("lkj(1)", class="cor") +
  prior("exponential(1)", class="sigma")

mSoV_bayes <- brm(formula_SoV, data = df3, prior=prior_SoV, sample_prior = "yes",iter=12000, warmup = 2000, chains=4, backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())

save(mSoV_bayes, file = 'mSoV_bayes.Rdata')



# -------------------------------------------------------------------------
# ------------------------ DELIBERATION -----------------------------------
# -------------------------------------------------------------------------

df5<-df3
df3<-df5

df3 <- df3%>%
group_by(Participant,Condition)%>%
summarize(planned=mean(planned,na.rm=T),cause=mean(cause,na.rm=T),control=mean(control,na.rm=T),avoidable=mean(avoidable,na.rm=T),
desired=mean(desired,na.rm=T),ownwill=mean(ownwill,na.rm=T),voluntary=mean(voluntary,na.rm=T), SoV= mean(SoV,na.rm=T),
predictable=mean(predictable,na.rm=T),effortfull=mean(effortfull,na.rm=T),generated=mean(generated,na.rm=T), intentional=mean(intentional,na.rm=T),
Will=mean(Will,na.rm=T),SoA=mean(SoA,na.rm=T),slope_RP_early=mean(slope_RP_early,na.rm=T),mean_RP_early=mean(mean_RP_early,na.rm=T),
,slope_RP_late=mean(slope_RP_late,na.rm=T),mean_RP_late=mean(mean_RP_late,na.rm=T), RT=mean(RT,na.rm=T))

df_delib <- data.frame(Participant=rep(df_deli$Participant,3),Deliberation=c(df_deli$Deliberation_Arbi,df_deli$Deliberation_Edeli,df_deli$Deliberation_Hdeli),
Condition=c(rep("0",50),rep("1",50),rep("2",50)))

df_delib <- df_delib[order(df_delib$Participant),]
df3 <- data.frame(cbind(df3,Deliberation=df_delib$Deliberation))

formula_Deliberation <- bf(Deliberation ~  Condition + scale(RT) + (Condition| Participant))

prior_Deliberation <- prior("normal(0,25)", class="b") + 
  prior("cauchy(50, 25)", class="Intercept") + 
  prior("cauchy(0, 25)", class="sd") +
  prior("lkj(1)", class="cor") +
  prior("exponential(1)", class="sigma")

mDeliberation_bayes <- brm(formula_Deliberation, data = df3, prior=prior_Deliberation, sample_prior = "yes",iter=5000, warmup = 2000, chains=1, backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())

save(mDeliberation_bayes, file = 'mDeliberation_bayes.Rdata')
