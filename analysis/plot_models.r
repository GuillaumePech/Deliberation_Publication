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
    group_by(Participant,Condition, Tps_Real)%>%
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

ggplot(b, aes(Condition, SoA, fill=Condition))+
  geom_line(aes(Condition, SoA,group=Participant),color='#3e3e3e',lwd=1, show.legend=F, inherit.aes = F, alpha=.8) +
  geom_jitter(aes(color=Condition),width=.2, size=2)+
  geom_boxplot(
    alpha=.8,
    width = .9,
    size = 1.5, outlier.shape = NA,
    position=position_dodge(),
  )  +
  scale_y_continuous(lim= c(200,800),breaks=seq(200, 800, 100))+   
  scale_x_discrete(breaks=NULL)+
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(labels = c("Arbitrary", "Easy-Deliberate", "Hard-Deliberate"), values=c("#f26d41","#40b2d8", "#ac4bd8"))+
  scale_color_manual(labels = c("Arbitrary", "Easy-Deliberate", "Hard-Deliberate"), values=c("#f26d41","#40b2d8", "#ac4bd8"))+
  stat_slab(aes(y=SoA, Fill=Condition), 
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








###SOA 






ggplot(b[b$Tps_Real=="0",], aes(Condition, SoA, fill=Condition))+ #change the delay 0 for 200ms, 300 for 500ms and 600 for 800ms
  geom_line(aes(Condition, SoA,group=Participant),color='#3e3e3e',lwd=1, show.legend=F, inherit.aes = F, alpha=.8) +
  geom_jitter(aes(color=Condition),width=.2, size=2)+
  geom_boxplot(
    alpha=.8,
    width = .4,
    size = 1.5, outlier.shape = NA,
    position=position_dodge(),
  )  +
  scale_y_continuous(lim= c(240,1000),breaks=seq(0, 1000, 100))+   
  scale_x_discrete(breaks=NULL)+
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(labels = c("Arbitrary", "Easy-Deliberation", "Hard-Deliberation"), values=c("#f26d41","#40b2d8", "#ac4bd8"))+
  scale_color_manual(labels = c("Arbitrary", "Easy-Deliberation", "Hard-Deliberation"), values=c("#f26d41","#40b2d8", "#ac4bd8"))+
  guides(fill = guide_legend(nrow = 1))+
  theme(
   axis.text=element_text(size=30,face='bold'),
   legend.title = element_blank(),
   legend.text = element_text(size=20,face='bold'),
   legend.background = element_rect(colour = NA, fill = NA),
   legend.key = element_rect(colour = NA, fill = NA),
   legend.key.size = unit(2, 'cm'),
   legend.position = c(.5,95)
     )



estimated_means1 <- emmeans(mSoA_bayes_clean, ~ Condition*Tps_Real)


min_range_density = -40
max_range_density = 50 
density_prior <- data.frame(density(rnorm(40000, 0, 30), from = min_range_density, to = max_range_density, n = 1001))
density_prior$y <- density_prior$y / sum(density_prior$y) # normalize to sum to 1

#for 200ms
arbi_easy_200 <- contrast(estimated_means1,  list(c1 = c(1,-1,0, 0,0,0 ,0,0,0))) 
arbi_hard_200 <- contrast(estimated_means1,  list(c1 = c(1,0,-1, 0,0,0 ,0,0,0))) 
easy_hard_200 <- contrast(estimated_means1,  list(c1 = c(0,1,-1, 0,0,0 ,0,0,0))) 

arbi_easy_200_draws <-   data.frame(density(gather_emmeans_draws(arbi_easy_200)$.value, from = min_range_density, to = max_range_density, n = 1001))
arbi_hard_200_draws <-   data.frame(density(gather_emmeans_draws(arbi_hard_200)$.value, from = min_range_density, to = max_range_density, n = 1001))
easy_hard_200_draws <-   data.frame(density(gather_emmeans_draws(easy_hard_200)$.value, from = min_range_density, to = max_range_density, n = 1001))

arbi_easy_200_draws$y <- arbi_easy_200_draws$y / sum(arbi_easy_200_draws$y) # normalize to sum to 1
arbi_hard_200_draws$y <- arbi_hard_200_draws$y / sum(arbi_hard_200_draws$y) # normalize to sum to 1
easy_hard_200_draws$y <- easy_hard_200_draws$y / sum(easy_hard_200_draws$y) # normalize to sum to 1

dim(easy_hard_200_draws)

df <- data.frame(cond=cbind(c(arbi_easy_200_draws, arbi_hard_200_draws, easy_hard_200_draws)),group=c(rep(1,40000),rep(2,40000),rep(3,40000)))
str(df)
ggplot(df, aes(x = cond, y = 0, fill=stat(quantile))) + 
  ggridges::geom_density_ridges_gradient(quantile_lines = TRUE,calc_ecdf=T, quantiles=c(.05,.95), vline_linetype = 1,size=1) +
  facet_grid(df$group) +
  scale_fill_manual(values = c( "#7daed679","#008cffe6", "#7daed679"), guide = "none")+
  geom_vline(xintercept =rep(0,120000), linetype="dashed",size=.6)+
  scale_x_continuous(lim= c(-50,50),breaks=seq(-40, 50, 15))+   
  scale_y_continuous(lim= c(0,.08),breaks=seq(0, .08, .08))+
  labs(
    x = NULL,
    y = NULL,
  )+
  theme(
    axis.text=element_text(size=30,face='bold'),
    panel.grid.major.x = element_blank(),      # Remove major vertical grid lines
    panel.grid.minor.x = element_blank(),       # Remove minor vertical grid lines
    panel.grid.major.y = element_line(color = "grey80", size = 0.3),  # Grey horizontal lines
    panel.grid.minor.y = element_line(color = "grey90", size = 0.2),  # Lighter minor horizontal lines
    panel.background = element_rect(fill = "white", color = NA)       # Ensure white background
  )

#for 500ms
arbi_easy_500 <- contrast(estimated_means1,  list(c1 = c(0,0,0, 1,-1,0 ,0,0,0))) 
arbi_hard_500 <- contrast(estimated_means1,  list(c1 = c(0,0,0, 1,0,-1 ,0,0,0))) 
easy_hard_500 <- contrast(estimated_means1,  list(c1 = c(0,0,0, 0,1,-1 ,0,0,0))) 

arbi_easy_500_draws <-   gather_emmeans_draws(arbi_easy_500)$.value
arbi_hard_500_draws <-   gather_emmeans_draws(arbi_hard_500)$.value
easy_hard_500_draws <-   gather_emmeans_draws(easy_hard_500)$.value

df <- data.frame(cond=cbind(c(arbi_easy_500_draws, arbi_hard_500_draws, easy_hard_500_draws)),group=c(rep(1,40000),rep(2,40000),rep(3,40000)))


ggplot(df, aes(x = cond, y = 0, fill = stat(quantile))) + 
  ggridges::geom_density_ridges_gradient(quantile_lines = TRUE,calc_ecdf=T, quantiles=c(.05,.95), vline_linetype = 1,size=1) +
  facet_grid(df$group) +
  scale_fill_manual(values = c( "#7daed679","#008cffe6", "#7daed679"), guide = "none")+
  geom_vline(xintercept =rep(0,120000), linetype="dashed",size=.6)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#for 800ms
arbi_easy_800 <- contrast(estimated_means1,  list(c1 = c(0,0,0 ,0,0,0, 1,-1,0))) 
arbi_hard_800 <- contrast(estimated_means1,  list(c1 = c(0,0,0 ,0,0,0, 1,0,-1))) 
easy_hard_800 <- contrast(estimated_means1,  list(c1 = c(0,0,0 ,0,0,0, 0,1,-1))) 


arbi_easy_800_draws <-   gather_emmeans_draws(arbi_easy_800)$.value
arbi_hard_800_draws <-   gather_emmeans_draws(arbi_hard_800)$.value
easy_hard_800_draws <-   gather_emmeans_draws(easy_hard_800)$.value

df <- data.frame(cond=cbind(c(arbi_easy_800_draws, arbi_hard_800_draws, easy_hard_800_draws)),group=c(rep(1,40000),rep(2,40000),rep(3,40000)))


ggplot(df, aes(x = cond, y = 0, fill = stat(quantile))) + 
  ggridges::geom_density_ridges_gradient(quantile_lines = TRUE,calc_ecdf=T, quantiles=c(.05,.95), vline_linetype = 1,size=1) +
  facet_grid(df$group) +
  scale_fill_manual(values = c( "#7daed679","#008cffe6", "#7daed679"), guide = "none")+
  geom_vline(xintercept =rep(0,120000), linetype="dashed",size=.6)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
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
  















library(ggplot2)
library(dplyr)

# Generate two example densities for two groups
set.seed(123)


min_range_density = -60
max_range_density = 60 
density_prior <- data.frame(density(rnorm(40000, 0, 30), from = min_range_density, to = max_range_density, n = 1001))
density_prior$y <- density_prior$y / sum(density_prior$y) # normalize to sum to 1

#for 200ms
arbi_easy_200 <- contrast(estimated_means1,  list(c1 = c(1,-1,0, 0,0,0 ,0,0,0))) 
arbi_hard_200 <- contrast(estimated_means1,  list(c1 = c(1,0,-1, 0,0,0 ,0,0,0))) 
easy_hard_200 <- contrast(estimated_means1,  list(c1 = c(0,1,-1, 0,0,0 ,0,0,0))) 

arbi_easy_200_draws <-   data.frame(density(gather_emmeans_draws(arbi_easy_800)$.value, from = min_range_density, to = max_range_density, n = 1001))
arbi_hard_200_draws <-   data.frame(density(gather_emmeans_draws(arbi_hard_800)$.value, from = min_range_density, to = max_range_density, n = 1001))
easy_hard_200_draws <-   data.frame(density(gather_emmeans_draws(easy_hard_800)$.value, from = min_range_density, to = max_range_density, n = 1001))

arbi_easy_200_draws$y <- arbi_easy_200_draws$y / sum(arbi_easy_200_draws$y) # normalize to sum to 1
arbi_hard_200_draws$y <- arbi_hard_200_draws$y / sum(arbi_hard_200_draws$y) # normalize to sum to 1
easy_hard_200_draws$y <- easy_hard_200_draws$y / sum(easy_hard_200_draws$y) # normalize to sum to 1

arbi_easy_200_draws <- arbi_easy_200_draws %>%
  mutate(group = "Group 1")
arbi_hard_200_draws <- arbi_hard_200_draws %>%
  mutate(group = "Group 2")  
easy_hard_200_draws <- easy_hard_200_draws %>%
  mutate(group = "Group 3")  

# Combine densities into a single data frame
all_densities <- bind_rows(arbi_easy_200_draws, arbi_hard_200_draws, easy_hard_200_draws)


# Calculate quantiles and add classification for each group
all_densities <- all_densities %>%
  group_by(group) %>%
  mutate(cumy = cumsum(y))
# Calculate quantiles and add classification for each group
all_densities <- all_densities %>%
  group_by(group) %>%
  mutate(
    quantile = case_when(
      cumy <= .055 ~ "lower",
      cumy  > .055 & cumy <= .945 ~ "middle",
      cumy > .945 ~ "upper"
    )
  )





# Define colors for each quantile region by group
quantile_colors <- c("Group 1_lower" = "#7daed679", "Group 1_middle" = "#008cffe6", "Group 1_upper" = "#7daed679",
                     "Group 2_lower" = "#7daed679", "Group 2_middle" = "#008cffe6", "Group 2_upper" = "#7daed679",
                     "Group 3_lower" = "#7daed679", "Group 3_middle" = "#008cffe6", "Group 3_upper" = "#7daed679")

# Map the `quantile` and `group` to specific colors for each region
all_densities <- all_densities %>%
  mutate(
    fill_color = paste(group, quantile, sep = "_")
  )


density_prior <- rbind(density_prior,density_prior,density_prior)
# Plot the densities with quantile coloring and grouping
ggplot(all_densities, aes(x = x, y = y, fill = fill_color)) +
  geom_area(alpha = 1, position = "identity") +  
  geom_line(color = "black", size = 0.1) +
  scale_fill_manual(values = quantile_colors, guide = "none") +
  geom_area(data=density_prior, aes(x=x,y=y), alpha = 0.5, fill="#f0b56d", position = "identity", inherit.aes = F) + 
  geom_line(data=density_prior, aes(x=x,y=y),color = "black", size = .1, inherit.aes = F) +
   # Separate densities with transparency
  # Adjust y-axis to display top and bottom densities+ 
  scale_x_continuous(limits = c(-40, 40), breaks = seq(-40, 60, 10)) +
  scale_y_continuous(limits = c(0.0, 0.006), breaks = seq(0.0, 0.006, 0.006)) +
  facet_grid(factor(all_densities$group)) +
  geom_vline(xintercept =rep(0,3003), linetype="dashed",size=.6)+
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text = element_text(size = 15, face = 'bold'),
    panel.grid.major.x = element_blank(),      # Remove major vertical grid lines
    panel.grid.minor.x = element_blank(),       # Remove minor vertical grid lines
    panel.grid.major.y = element_line(color = "grey80", size = 0.3),  # Grey horizontal lines
    panel.grid.minor.y = element_line(color = "grey90", size = 0.2),  # Lighter minor horizontal lines
    panel.background = element_rect(fill = "white", color = NA)       # Ensure white background
  )
