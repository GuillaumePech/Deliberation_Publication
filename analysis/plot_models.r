library(brms); library(emmeans); library(bayestestR); library(ggplot2); library(tidybayes); library(dplyr)
setwd(dir = "C:/Users/mfbpe/Desktop/DATA/2022_deliberation//results/models/") 

windows()
load('mWill_bayes_clean.Rdata')
load('mslope_RP_bayes_clean.Rdata')
load('mmean_RP_bayes_clean.Rdata')
load('mSoA_bayes_clean.Rdata')
load('mRT_bayes_clean.Rdata')
load('mSoV_bayes_clean.Rdata')
load('mDeliberation_bayes_clean.Rdata')



b <- mSoV_bayes_clean$data
b <- mSoV_bayes_clean$data %>%
    group_by(Participant,Condition)%>%
    summarise(SoV = mean(SoV,na.rm=T))

b <- mSoA_bayes_clean$data %>%
    group_by(Participant,Condition)%>%
    summarise(SoA = mean(SoA,na.rm=T))

b <- mWill_bayes_clean$data
b <- mWill_bayes_clean$data %>%
    group_by(Participant,Condition)%>%
    summarise(Will = mean(Will,na.rm=T))


b <- mslope_RP_bayes_clean$data
b <- mslope_RP_bayes_clean$data %>%
    group_by(Participant,Condition)%>%
    summarise(slope_RP = mean(slope_RP,na.rm=T))


b <- mmean_RP_bayes_clean$data
b <- mmean_RP_bayes_clean$data %>%
    group_by(Participant,Condition)%>%
    summarise(mean_RP = mean(mean_RP,na.rm=T))


b <- mRT_bayes_clean$data
b <- mRT_bayes_clean$data %>%
    group_by(Participant,Condition)%>%
    summarise(RT = mean(RT,na.rm=T))


b <- mDeliberation_bayes_clean$data
b <- mDeliberation_bayes_clean$data %>%
    group_by(Participant,Condition)%>%
    summarise(Deliberation = mean(Deliberation,na.rm=T))

ggplot(b, aes(Condition, Deliberation, fill=Condition))+
  geom_line(aes(Condition, Deliberation,group=Participant),color='#3e3e3e',lwd=1, show.legend=F, inherit.aes = F, alpha=.8) +
  geom_jitter(aes(color=Condition),width=.2, size=2)+
  geom_boxplot(
    alpha=.8,
    width = .9,
    size = 1.5, outlier.shape = NA,
    position=position_dodge(),
  )  +
  scale_y_continuous(lim= c(0,110),breaks=seq(0, 100, 25))+   
  scale_x_discrete(breaks=NULL)+
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(labels = c("Arbitrary", "Easy-Deliberate", "Hard-Deliberate"), values=c("#f26d41","#40b2d8", "#ac4bd8"))+
  scale_color_manual(labels = c("Arbitrary", "Easy-Deliberate", "Hard-Deliberate"), values=c("#f26d41","#40b2d8", "#ac4bd8"))+
  stat_slab(aes(y=Deliberation, Fill=Condition), 
  inherit.aes = F, adjust = 1, width = 3, linewidth = 1.5, alpha=.7, color=rep(c("#f26d41","#40b2d8", "#ac4bd8"),each=501), justification=-1.5, fill=rep(c("#f26d41","#40b2d8", "#ac4bd8"),each=501)) +
  theme(
   axis.text=element_text(size=30,face='bold'),
   legend.title = element_blank(),
   legend.text = element_text(size=20,face='bold'),
   legend.background = element_rect(colour = NA, fill = NA),
   legend.key = element_rect(colour = NA, fill = NA),
   legend.key.size = unit(2, 'cm'),
   legend.position = c(.85,.5)
  )
  

ggplot(b, aes(SoA , fill=Condition))+
    stat_slab(color="black",alpha = .8,  justification=-.67,linewidth=1.5) +
  geom_boxplot(alpha = .7,linewidth=1,width=.7, position=position_dodge(1),outliers=F)+
   labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(values=c("#f26d41","#40b2d8", "#ac4bd8"))+
  scale_x_continuous(breaks=seq(200,800,100))+   
  scale_y_continuous( breaks=seq(.6,1.4,.2), labels=seq(0,.8,.2))+
  theme(
   text=element_text(size=30,face='bold'),
   legend.background = element_rect(colour = NA, fill = NA),
   legend.key = element_rect(colour = NA, fill = NA),
   legend.key.size = unit(2, 'cm'),
   legend.position = c(.9,.8))
