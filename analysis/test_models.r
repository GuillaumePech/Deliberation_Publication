library(brms); library(emmeans); library(bayestestR)
setwd(dir = "C:/Users/mfbpe/Desktop/DATA/2022_deliberation/results/models/") 

load('mslope_RP_bayes_clean.Rdata')
load('mmean_RP_bayes_clean.Rdata')
load('mWill_bayes_clean.Rdata')
load('mSoA_bayes_clean.Rdata')
load('mRT_bayes_clean.Rdata')
load('mSoV_bayes_clean.Rdata')
load('mDeliberation_bayes_clean.Rdata')



test_list <- list(mslope_RP_bayes_clean, mmean_RP_bayes_clean, mWill_bayes_clean,
   mRT_bayes_clean, mSoV_bayes_clean, mDeliberation_bayes_clean)
name_test_list <- c("mslope_RP_bayes_clean", "mmean_RP_bayes_clean", "mWill_bayes_clean", "mRT_bayes_clean", "mSoV_bayes_clean", "mDeliberation_bayes_clean")
   
sd_list <- c(1, 1, 10, 1, 25, 25)
emm_options(opt.digits = FALSE)

test_list <- list(mslope_RP_bayes_clean)
name_test_list <- c("mSoA_bayes_clean")
sd_list <- c(2)



for (test_i in 1:length(test_list)){
  actual_sd <- sd_list[test_i]
  test_on <- test_list[[test_i]]
  print(c("test on:", name_test_list[test_i]))
  print(hpd.summary(emmeans(test_on, ~Condition), .89))

  posterior_extraction_easy_arbitrary <- as_draws_array(test_on, variable="b_Condition2M1")
  posterior_extraction_hard_easy <-as_draws_array(test_on, variable="b_Condition3M2")
  posterior_extraction_hard_arbitrary <- posterior_extraction_easy_arbitrary + posterior_extraction_hard_easy

  conditions <- list(posterior_extraction_easy_arbitrary, posterior_extraction_hard_easy, posterior_extraction_hard_arbitrary)
  name_conditions <- c("easy_arbitrary", "hard_easy", "hard_arbitrary")

  for (condition_i in 1:length(conditions)){
    
    print(c("Comparison of  :",name_conditions[condition_i]))
    print( c("median: ", median(conditions[[condition_i]])))
    print (  pd(conditions[[condition_i]])[2])
    print ( hdi(conditions[[condition_i]], .89)[3:4])

    
    max_range_density <- round(ifelse(actual_sd*2 > max(abs(conditions[[condition_i]])), actual_sd*4,  max(abs(conditions[[condition_i]]))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
    min_range_density <- - max_range_density
    
    density_posterior_extraction <-density(conditions[[condition_i]], from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
    density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
    probabilty_H0_posterior <- density_posterior_extraction$y[density_posterior_extraction$x==0] # find the value to 0

    density_prior <- density(rnorm(4e4,0, actual_sd), from = min_range_density, to = max_range_density, n = 1001)
    density_prior$y <- density_prior$y / sum(density_prior$y)
    probabilty_H0_prior <- density_prior$y[density_prior$x==0] # find the value to 0

    ratio_H01 <- probabilty_H0_prior / probabilty_H0_posterior 
    print(c("Ratio H01 :", ratio_H01))
    print(c("Actual_sd :", actual_sd))
    favorite_hypothesis <- ifelse(ratio_H01 > 1/3, ifelse(ratio_H01 < 3, "unconclusive","H1"), "H0")

    lower_range_list <- pracma::logseq(actual_sd, actual_sd/1000, 100)
    upper_range_list <- pracma::logseq(actual_sd, actual_sd*1000, 100)
    actual_hypothesis <- favorite_hypothesis
    original_hypothesis <- favorite_hypothesis
    range_check <- ifelse(actual_hypothesis=="unconclusive", 2, 2)
    upper_range_value <- NA; lower_range_value <- NA
    stop_H0 = F; stop_H1 = F
    for (ite in 1:length(lower_range_list)){

      if ((favorite_hypothesis != "H0") & stop_H0 == F){
        #increase sd for upper range to test range H1 or unconclusive 
        upper_range_value = upper_range_list[64]
        max_range_density <- round(ifelse(upper_range_value*2 > max(abs(conditions[[condition_i]])), upper_range_value*4,  max(abs(conditions[[condition_i]]))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
        min_range_density <- - max_range_density
        
        density_posterior_extraction <-density(conditions[[condition_i]], from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
        density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
        probabilty_H0_posterior <- density_posterior_extraction$y[density_posterior_extraction$x==0] # find the value to 0

        density_prior <- density(rnorm(4e4,0, upper_range_value), from = min_range_density, to = max_range_density, n = 1001)      
        density_prior$y <- density_prior$y / sum(density_prior$y)
        probabilty_H0_prior <- density_prior$y[density_prior$x==0] 
        ratio_H01 <- probabilty_H0_prior / probabilty_H0_posterior
        favorite_hypothesis <- ifelse(ratio_H01 > 1/3, ifelse(ratio_H01 < 3, "unconclusive","H1"), "H0")
        if (favorite_hypothesis != actual_hypothesis){range_check <- range_check-1; stop_H0 <- T; favorite_hypothesis <- "unconclusive"}
      }
      if (favorite_hypothesis != "H1" & stop_H1 ==F){
        #increase sd for upper range to test range H1 or unconclusive 
        lower_range_value = lower_range_list[ite]
        max_range_density <- round(ifelse(lower_range_value*2 > max(abs(conditions[[condition_i]])), lower_range_value*4,  max(abs(conditions[[condition_i]]))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
        min_range_density <- - max_range_density
        
        density_posterior_extraction <-density(conditions[[condition_i]], from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
        density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
        probabilty_H0_posterior <- density_posterior_extraction$y[density_posterior_extraction$x==0] # find the value to 0

        density_prior <- density(rnorm(4e4,0, lower_range_value), from = min_range_density, to = max_range_density, n = 1001)
        density_prior$y <- density_prior$y / sum(density_prior$y)
        probabilty_H0_prior <- density_prior$y[density_prior$x==0] 
        ratio_H01 <- probabilty_H0_prior / probabilty_H0_posterior
        favorite_hypothesis <- ifelse(ratio_H01 > 1/3, ifelse(ratio_H01 < 3, "unconclusive","H1"), "H0")
        if (favorite_hypothesis != actual_hypothesis){range_check <- range_check-1; stop_H1 <- T; favorite_hypothesis <- "unconclusive"}

      }
      if (range_check==0){break}
    }

      lower_range_value <- ifelse(original_hypothesis=="H1", actual_sd, lower_range_value)
      upper_range_value <- ifelse(original_hypothesis=="H0", actual_sd, upper_range_value)
      print(c(original_hypothesis, lower_range_value, upper_range_value))
    }

}

library(tidybayes)

inferential_test <- function(test_list, name_test_list, actual_sd){

for (test_i in 1:length(test_list)){

  test_on <- (gather_emmeans_draws(test_list[[test_i]])$.value)
  print(name_test_list[test_i], quote = FALSE)
  print( c("median: ", median(test_on)), quote = FALSE) 
  print ( bayestestR::pd(test_on), quote = FALSE)
  print ( HDInterval::hdi(test_on, .89)[1:2], quote = FALSE)

  max_range_density <- round(ifelse(actual_sd*2 > max(abs(test_on)), actual_sd*4,  max(abs(test_on))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
  
  min_range_density <- - max_range_density

  density_posterior_extraction <-density(test_on, from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
  density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
  probabilty_H0_posterior <- density_posterior_extraction$y[which.min(abs(density_posterior_extraction$x))] # find the value to 0

  density_prior <- density(rnorm(4e4,0, actual_sd), from = min_range_density, to = max_range_density, n = 1001)
  density_prior$y <- density_prior$y / sum(density_prior$y)
  probabilty_H0_prior <- density_prior$y[which.min(abs(density_prior$x))] # find the value to 0

  ratio_H10 <- probabilty_H0_prior / probabilty_H0_posterior 
  print(c("Ratio H10 :", ratio_H10), quote = FALSE)
  print(c("Actual_sd :", actual_sd), quote = FALSE)
  favorite_hypothesis <- ifelse(ratio_H10 > 1/3, ifelse(ratio_H10 < 3, "unconclusive","H1"), "H0")

  lower_range_list <- pracma::logseq(actual_sd, actual_sd/1000, 100)
  upper_range_list <- pracma::logseq(actual_sd, actual_sd*1000, 100)
  actual_hypothesis <- favorite_hypothesis
  original_hypothesis <- favorite_hypothesis
  range_check <- ifelse(actual_hypothesis=="unconclusive", 2, 2)
  upper_range_value <- NA; lower_range_value <- NA
  stop_H0 = F; stop_H1 = F
  for (ite in 1:length(lower_range_list)){

    if ((favorite_hypothesis != "H0") & stop_H0 == F){
      #increase sd for upper range to test range H1 or unconclusive 
      upper_range_value = upper_range_list[ite]
          
      max_range_density <- round(ifelse(upper_range_value*2 > max(abs(test_on)), upper_range_value*4,  max(abs(test_on))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
      min_range_density <- - max_range_density

      density_posterior_extraction <-density(test_on, from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
      density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
      probabilty_H0_posterior <- density_posterior_extraction$y[which.min(abs(density_posterior_extraction$x))] # find the value to 0

      density_prior <- density(rnorm(4e4,0, upper_range_value), from = min_range_density, to = max_range_density, n = 1001)      
      density_prior$y <- density_prior$y / sum(density_prior$y)
      probabilty_H0_prior <- density_prior$y[which.min(abs(density_prior$x))] 
      ratio_H10 <- probabilty_H0_prior / probabilty_H0_posterior
      favorite_hypothesis <- ifelse(ratio_H10 > 1/3, ifelse(ratio_H10 < 3, "unconclusive","H1"), "H0")
      if (favorite_hypothesis != actual_hypothesis){range_check <- range_check-1; stop_H0 <- T; favorite_hypothesis <- "unconclusive"}
    }
    if (favorite_hypothesis != "H1" & stop_H1 ==F){
      #increase sd for upper range to test range H1 or unconclusive 
      lower_range_value = lower_range_list[ite]
      
      max_range_density <- round(ifelse(lower_range_value*2 > max(abs(test_on)), lower_range_value*4,  max(abs(test_on))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
      min_range_density <- - max_range_density
      
      density_posterior_extraction <-density(test_on, from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
      density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
      probabilty_H0_posterior <- density_posterior_extraction$y[which.min(abs(density_posterior_extraction$x))] # find the value to 0

      density_prior <- density(rnorm(4e4,0, lower_range_value), from = min_range_density, to = max_range_density, n = 1001)
      density_prior$y <- density_prior$y / sum(density_prior$y)
      probabilty_H0_prior <- density_prior$y[which.min(abs(density_prior$x))] 
      ratio_H10 <- probabilty_H0_prior / probabilty_H0_posterior
      favorite_hypothesis <- ifelse(ratio_H10 > 1/3, ifelse(ratio_H10 < 3, "unconclusive","H1"), "H0")
      if (favorite_hypothesis != actual_hypothesis){range_check <- range_check-1; stop_H1 <- T; favorite_hypothesis <- "unconclusive"}

    }
    if (range_check==0){break}
  }

    lower_range_value <- ifelse(original_hypothesis=="H1", actual_sd, lower_range_value)
    upper_range_value <- ifelse(original_hypothesis=="H0", actual_sd, upper_range_value)
    print(c("Hypothesis favored originally is :", original_hypothesis), quote = FALSE)
    print(c("range giving the same results is :", lower_range_value, upper_range_value), quote = FALSE)
    cat("\n\n\n\n\n\n")
  }

}

estimated_means1 <- emmeans(mSoA_bayes_clean, ~ Condition*Tps_Real)

confint(estimated_means1,level=.89)
actual_sd <- 30


#for 200ms
arbi_easy_200 <- contrast(estimated_means1,  list(c1 = c(1,-1,0, 0,0,0 ,0,0,0))) 
arbi_hard_200 <- contrast(estimated_means1,  list(c1 = c(1,0,-1, 0,0,0 ,0,0,0))) 
easy_hard_200 <- contrast(estimated_means1,  list(c1 = c(0,1,-1, 0,0,0 ,0,0,0))) 
inferential_test(list(arbi_easy_200, arbi_hard_200, easy_hard_200), c("arbi_easy_200", "arbi_hard_200", "easy_hard_200"), actual_sd)

#for 500ms
arbi_easy_500 <- contrast(estimated_means1,  list(c1 = c(0,0,0, 1,-1,0 ,0,0,0))) 
arbi_hard_500 <- contrast(estimated_means1,  list(c1 = c(0,0,0, 1,0,-1 ,0,0,0))) 
easy_hard_500 <- contrast(estimated_means1,  list(c1 = c(0,0,0, 0,1,-1 ,0,0,0))) 
inferential_test(list(arbi_easy_500, arbi_hard_500, easy_hard_500), c("arbi_easy_500", "arbi_hard_500", "easy_hard_500"), actual_sd)

#for 800ms
arbi_easy_800 <- contrast(estimated_means1,  list(c1 = c(0,0,0 ,0,0,0, 1,-1,0))) 
arbi_hard_800 <- contrast(estimated_means1,  list(c1 = c(0,0,0 ,0,0,0, 1,0,-1))) 
easy_hard_800 <- contrast(estimated_means1,  list(c1 = c(0,0,0 ,0,0,0, 0,1,-1))) 
inferential_test(list(arbi_easy_800, arbi_hard_800, easy_hard_800), c("arbi_easy_800", "arbi_hard_800", "easy_hard_800"), actual_sd)
