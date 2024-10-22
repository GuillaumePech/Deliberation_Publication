library(brms); library(emmeans); library(lmerTest); library(bayestestR)
setwd(dir = "C:/Users/mfbpe/Desktop/DATA/2022_deliberation//results/models/") 

outlier <- function(dat) {
  stop <- 0

  outlierp <- median(dat, na.rm = T) + (3 * mad(dat, na.rm = T))
  outlierm <- median(dat, na.rm = T) - (3 * mad(dat, na.rm = T))
  idx <- which(dat <= outlierm | dat >= outlierp)

  if (length(idx) > 0) {
    dat <- replace(dat, idx, NA)
  }
  }
  return(dat)
}
"""
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
##############################                                                           ##########################################
##############################                      Deliberation                         ##########################################
##############################                                                           ##########################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
"""

load('mslope_RP_bayes.Rdata')
load('mmean_RP_bayes_02_03_24.Rdata')
load('mWill_bayes.Rdata')
load('mWill_bayes.Rdata')
load('mSoA_bayes.Rdata')
load('mRT_bayes.Rdata')
load('mSoV_bayes.Rdata')
load('mDeliberation_bayes.Rdata')


# You have to change mWill etc if you want to test other models
Edeli_Arbi <-ranef(mmean_RP_bayes, pars= "Condition2M1",groups='Participant')
Edeli_Hdeli <-ranef(mmean_RP_bayes, pars= "Condition3M2",groups='Participant')

a <- data.frame(Edeli_Arbi)
b <- data.frame(Edeli_Hdeli)
c <- a+b

outlier(a$Participant.Estimate.Condition2M1)
outlier(b$Participant.Estimate.Condition3M2)
outlier(c$Participant.Estimate.Condition2M1)

a[which(is.na(outlier(a$Participant.Estimate.Condition2M1))),]
b[which(is.na(outlier(b$Participant.Estimate.Condition3M2))),]
c[which(is.na(outlier(c$Participant.Estimate.Condition2M1))),]

mslope_RP_bayes_clean <- update(mslope_RP_bayes, newdata = mslope_RP_bayes$data[!(mslope_RP_bayes$data$Participant %in% c(31, 32)),] ,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")
save(mslope_RP_bayes_clean, file = 'mslope_RP_bayes_clean.Rdata')


mmean_RP_bayes_clean <- update(mmean_RP_bayes, newdata = mmean_RP_bayes$data[!(mmean_RP_bayes$data$Participant %in% c(31,32)),] ,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")
save(mmean_RP_bayes_clean, file = 'mmean_RP_bayes_clean.Rdata')

mWill_bayes_clean <- update(mWill_bayes, newdata = mWill_bayes$data[!(mWill_bayes$data$Participant %in% c(36)),] ,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")
save(mWill_bayes_clean, file = 'mWill_bayes_clean.Rdata')

mSoA_bayes_clean <- update(mSoA_bayes,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")
save(mSoA_bayes_clean, file = 'mSoA_bayes_clean.Rdata')

mRT_bayes_clean <- update(mRT_bayes, newdata = mRT_bayes$data[!(mRT_bayes$data$Participant %in% c(20, 23 , 24, 30, 45, 50)),] ,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")
save(mRT_bayes_clean, file = 'mRT_bayes_clean.Rdata')

mDeliberation_bayes_clean <- update(mDeliberation_bayes, newdata = mDeliberation_bayes$data[!(mDeliberation_bayes$data$Participant %in% c(32, 42, 45, 48)),] ,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")
save(mDeliberation_bayes_clean, file = 'mDeliberation_bayes_clean.Rdata')

mSoV_bayes_clean <- update(mSoV_bayes, newdata = mSoV_bayes$data[!(mSoV_bayes$data$Participant %in% c(1, 39)),] ,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")
save(mSoV_bayes_clean, file = 'mSoV_bayes_clean.Rdata')

