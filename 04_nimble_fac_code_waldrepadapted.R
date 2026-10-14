######################## 04_facmodel_nimble.R ##################

# Code: NIMBLE FACE MODEL
# Date: 5 Oct 2026
# Author: Cassidy Waldrep

# Load packages and functions

library(tidyverse)
library(nimble)
library(nimbleHMC)
library(coda)
library(MCMCvis)

#-------------------------------------------------------------------------#
###### creating different season combinations for model #############
#-------------------------------------------------------------------------#

raw_data <- read_csv("data/fac_covariates_2022_sep2026.csv") %>%
  rename(bird_year = band_year)  %>%
  mutate(season_index = case_when(season == "winter" ~ 1, 
                             season == "late_winter" ~ 2, 
                             season == "spring_migration" ~ 3, 
                             season == "late_spring_migration" ~ 4, 
                             season == "early_breeding" ~ 5, 
                             season == "nesting" ~ 6, 
                             season == "brood_rearing" ~ 7))

season_summary <- raw_data %>%
  arrange(bird_year, season_index) %>%
  group_by(bird_year) %>%
  summarize(season_combo = paste(season_index, collapse = ", "))

input_data <- raw_data %>%
  left_join(season_summary) %>%
  mutate(mig_status = case_when((is.na(mig_status)) & str_detect(season_combo, "nesting") ~ "No",
                       TRUE ~ mig_status)) %>%
  # based on summary below
  filter(!season_combo == "1, 2, 5")


input_data_scale <- input_data %>%
  mutate(total_acc_fixes_corrected_season = as.numeric(total_acc_fixes_corrected_season),
         feed_fixes_corrected=as.numeric(feed_fixes_corrected_season),
         scaled_mean_precip = as.numeric(scale(mean_daily_precip_season)), 
         scaled_min_temp = as.numeric(scale(mean_min_daily_temp_season)), 
         scaled_freeze_events = as.numeric(scale(num_of_5_days_freeze_events)), 
         scaled_prop_forest = as.numeric(scale(forest)),
         scaled_prop_cropland = as.numeric(scale(cropland)),
         scaled_prop_marsh = as.numeric(scale(marsh)),
         scaled_prop_swamp = as.numeric(scale(swamp)),
         scaled_prop_urban = as.numeric(scale(urban)),
         scaled_prop_peatland = as.numeric(scale(peatland)),
         scaled_prop_open_water = as.numeric(scale(water)),
         scaled_first_day = as.numeric(scale(julian_arrival)),
         scaled_city=as.numeric(scale(city_center_proportion)),
         log_odba = log(mean_ODBA_season), 
         id = as.numeric(as.factor(bandnum)), 
         year_numeric = as.numeric(as.factor(year)), 
         migratory = as.numeric(factor(mig_status, levels = c("No", "Yes"))),
         age = as.numeric(factor(age, levels = c("SY", "ASY"))))

#-------------------------------------------------------------------------#
###### creating indexing #############
#-------------------------------------------------------------------------#
        
#### 1. a loop (nest propensity sub-model, individual/year index) #####
dat_annual <- input_data_scale %>% 
  filter(egg_laying_status == 1 | egg_laying_status == 0, 
         season_index == 5)

n_repro <- unique(dat_annual$id)

# look at all different combinations of sub-seasons by bird_year

sum <- dat_annual %>% group_by(season_combo) %>% count()         

sum %>% arrange(-n) %>% print(n = 29)

# 1. Complete group (migratory): "1, 2, 3, 4, 5", "1, 2, 3, 4, 5, 6", "1, 2, 3, 4, 5, 6, 7", 
# 2. migratory, missing winter: "2, 3, 4, 5", "2, 3, 4, 5, 6",  "2, 3, 4, 5, 6, 7", 
# 3. migratory, missing winter and late winter: "3, 4, 5", "3, 4, 5, 6", "3, 4, 5, 6, 7" 
# 4. migratory, missing late winter: "1, 3, 4, 5" , "1, 3, 4, 5, 6, 7"
# 5. migratory, missing winter - early migration: "4, 5", "4, 5, 6"
# 6. complete, non-migratory: "1, 5", "1, 5, 6", "1, 5, 6, 7" 
# 7, nonmigratory, missing winter: "5", "5, 6", "5, 6, 7"

# 1, 2, 5- one bird missing migration (removed up above)

# create season_combo column
dat_annual <- dat_annual %>%
  mutate(season_combo_index = case_when(
    season_combo == "1, 2, 3, 4, 5" |
      season_combo == "1, 2, 3, 4, 5, 6" |
      season_combo == "1, 2, 3, 4, 5, 6, 7" ~ 1, # migratory, complete
    season_combo == "2, 3, 4, 5" |
      season_combo == "2, 3, 4, 5, 6" |
      season_combo == "2, 3, 4, 5, 6, 7" ~ 2, # migratory, missing winter
    season_combo == "3, 4, 5" |
      season_combo == "3, 4, 5, 6" |
      season_combo == "3, 4, 5, 6, 7" ~ 3, # migratory, missing winter and late winter
    season_combo == "1, 3, 4, 5" |
      season_combo == "1, 3, 4, 5, 6, 7"   ~ 4, # migratory, missing late winter
    season_combo == "4, 5" |
      season_combo == "4, 5, 6" ~ 5, # migratory, missing winter - early migration
    season_combo == "1, 5" | 
      season_combo == "1, 5, 6" | 
      season_combo == "1, 5, 6, 7" ~ 6, # nonmigratory, complete
    season_combo == "5" | 
      season_combo == "5, 6" |
      season_combo == "5, 6, 7" ~ 7, # nonmigratory, missing winter
  )) %>%
  
  # select the columns you actually need
  
  dplyr::select(id, year_numeric, egg_laying_status, season_combo_index, season_index, log_odba, feed_fixes_corrected_season, total_acc_fixes_corrected_season, scaled_first_day, age) %>%
  
  # assign nonmigratory birds the mean (0) for arrival date
  
  mutate(scaled_arrival_day=ifelse(is.na(scaled_first_day) &
                                     season_combo_index == 6 | season_combo_index == 7,
                                   0, scaled_first_day)) %>%
  dplyr::select(-scaled_first_day)

# NO NAS!!!
# na <- dat_annual %>%
#   # keep only rows that have no NAs
#   drop_na() 

unique(dat_annual$season_combo_index)

# count number of birds with more than one year of data

multi_years_NP <- dat_annual %>% 
  group_by(id) %>%
  summarize(n=n()) %>% # getting number of years for bird
  group_by(n) %>%
  summarize(n_birds=n()) # summarizing to number of birds with each number of years

##### 2. b loop (odba loop, individual/year/sub-season index) #####

dat_odba <- input_data_scale %>%
  filter(season_index == 1 | season_index == 2 | season_index == 3 | season_index == 4 | 
           season_index == 5)  %>%

    # select the columns you need
  
  dplyr::select(id, year_numeric, bird_year, season_index, log_odba,
                scaled_freeze_events, scaled_mean_precip, scaled_min_temp,
                scaled_prop_urban, scaled_prop_cropland,
                scaled_prop_swamp, scaled_prop_marsh, scaled_prop_open_water) 

# NO NAS!!!
# na <- dat_odba %>%
#   # keep only rows that have no NAs
#   drop_na()

n_odba_na_rem <- unique(dat_odba$id)

# count number of birds with more than one year of data

multi_years_odba <- dat_odba %>% 
  group_by(id, year_numeric) %>%
  slice(1) %>% # get one observation per year for each bird (currently have multiple rows per year for all the subseasons!)
  group_by(id) %>%
  summarize(n=n()) %>% # getting number of years for bird
  group_by(n) %>%
  summarize(n_years=n()) # summarizing to number of birds with each number of years

##### 3. c loop (feed loop, individual/year/sub-season index) #####

dat_feed <- input_data_scale  %>%
  filter(season_index == 1 | season_index == 2 | season_index == 3 | season_index == 4 | 
           season_index == 5) %>%
  
  # select the columns you need
  
  dplyr::select(id, year_numeric, season_index, bird_year, feed_fixes_corrected_season, total_acc_fixes_corrected_season,
                scaled_freeze_events, scaled_mean_precip, scaled_min_temp, 
                scaled_prop_urban, scaled_prop_cropland,
                scaled_prop_swamp, scaled_prop_marsh, scaled_prop_open_water) 

# NO NAS!!!
# na <- dat_feed %>%
#   # keep only rows that have no NAs
#   drop_na()

n_feed_na_rem <- unique(dat_feed$id)

# count number of birds with more than one year of data
multi_years_feed <- dat_feed %>% 
  group_by(id, year_numeric) %>%
  slice(1) %>% # get one observation per year for each bird (currently have multiple rows per year for all the subseasons!)
  group_by(id) %>%
  summarize(n=n()) %>% # getting number of years for bird
  group_by(n) %>%
  summarize(n_birds=n()) # summarizing to number of birds with each number of years

##### 4. d loop (nest success loop, individual/year) #####

dat_nest_success <- input_data_scale  %>% 
  filter(incubation_status == 1 | incubation_status == 0) %>%
  filter(season_index == 6) #%>%
#replace(is.na(.), 0)   #replace NAs with 0 so they don't mess up when indexing

n_NS <- unique(dat_nest_success$id)

# only keep individuals that are in dat_annual because otherwise p_nest doesn't work in NS model... 
dat_nest_success <- semi_join(dat_nest_success, dat_annual, by = c("id", "year_numeric"))

n_NS_only_dat_annual_birds <- unique(dat_nest_success$id)

# deal with NAs
dat_nest_success <- dat_nest_success %>%
  # select the columns you need
  dplyr::select(id, year_numeric, incubation_status, mig_status, nest_lat, nest_long,
                scaled_prop_swamp, scaled_prop_marsh, scaled_prop_cropland, scaled_prop_peatland) %>%
  # keep only rows that have no NAs
  drop_na() # removes 2 individuals currently... 

# NO NAS!!!
# na <- dat_nest_success %>%
#   # keep only rows that have no NAs
#   drop_na()

n_NS_na_rem <- unique(dat_nest_success$id)

dat_nest_success %>% group_by(mig_status) %>% count()

# count number of birds with more than one year of data
multi_years_NS <- dat_nest_success %>% group_by(id) %>%
  summarize(n=n()) %>% # getting number of years for bird
  group_by(n) %>%
  summarize(n_birds=n()) # summarizing to number of birds with each number of years

##### 5. e loop (brood success loop, individual/year) #####

dat_brood_success <- input_data_scale  %>% 
  filter(is.na(brood_status) == FALSE) %>%
  filter(season_index == 7)

n_BS <- unique(dat_brood_success$id)

# only keep individuals that are in dat_nest_success because otherwise p_hatch doesn't work in BS model... 
dat_brood_success <- semi_join(dat_brood_success, dat_nest_success, by = c("id", "year_numeric"))

n_BS_only_dat_annual_birds <- unique(dat_brood_success$id) # removes 4 individuals

# deal with NAs
dat_brood_success <- dat_brood_success %>%
  
  # select the columns you need
  
  dplyr::select(id, year_numeric, brood_status, 
                scaled_mean_precip, scaled_min_temp, 
                scaled_prop_swamp, scaled_prop_marsh, scaled_prop_open_water, 
                scaled_city) 

# # NO NAS!!!
# na <- dat_brood_success %>%
#   # keep only rows that have no NAs
#   drop_na()
# 

n_BS_na_rem <- unique(dat_brood_success$id)

# count number of birds with more than one year of data
multi_years_BS <- dat_brood_success %>% 
  group_by(id) %>%
  summarize(n=n()) %>% # getting number of years for bird
  group_by(n) %>%
  summarize(n_birds=n()) # summarizing to number of birds with each number of years


# for ODBA and feed observation prior loop
unique_birds_odba <- dat_odba[!duplicated(dat_odba$bird_year),]
unique_birds_feed <- dat_feed[!duplicated(dat_feed$bird_year),]



## indexing --------------------------------------------------------------------

# b loop 
nind <- length(unique(c(input_data_scale$id)))
nobs2 <- nrow(dat_odba)
ids2 <- dat_odba$id
years2 <- dat_odba$year_numeric
sub_seasons2 = dat_odba$season_index
nobs2_sd = length(unique(dat_odba$bird_year))
ids2_sd = unique_birds_odba$id
years2_sd = unique_birds_odba$year_numeric

# c loop 
nobs3 <- nrow(dat_feed)
ids3 <- dat_feed$id
years3 <- dat_feed$year_numeric
sub_seasons3 <- dat_feed$season_index


# specify GLMMs in BUGS language ----------------------------------------------

model_code <- nimbleCode({
  
  ## priors -------------------------------------------------------------------
  
  ### alpha  
  
  for(i in 1:nind){ # individual index
    alpha_ODBA[i] ~ dnorm(0, sd = 10)
  #  alpha_feed[i] ~ dnorm(0, sd = 10)

  }  
  
  # ODBA observation variation
  for(b in 1:nobs2_sd){ 
    sd_ODBA_obs[ids2_sd[b], years2_sd[b]] ~ dunif(0, 10)
  } 
  
  ### Beta coefficients  ------------------------------------------------------
  # Priors for ODBA betas across all 5 sub-seasons
  for(s in 1:5) {
    beta_ODBA_temp[s]         ~ dnorm(0, sd = 10)
    beta_ODBA_precip[s]       ~ dnorm(0, sd = 10)
    beta_ODBA_temp_precip[s]  ~ dnorm(0, sd = 10)
    beta_ODBA_urban[s]        ~ dnorm(0, sd = 10)
    beta_ODBA_cropland[s]     ~ dnorm(0, sd = 10)
    beta_ODBA_swamp[s]        ~ dnorm(0, sd = 10)
    beta_ODBA_marsh[s]        ~ dnorm(0, sd = 10)
    beta_ODBA_open_water[s]   ~ dnorm(0, sd = 10)
  }
  
  # Separate freeze prior for ODBA: season 1 gets normal, seasons 2-5 get tight uniform
  beta_ODBA_freeze[1] ~ dnorm(0, sd = 10)
  
  for(s in 2:5) {
    beta_ODBA_freeze[s] ~ dunif(-0.000001, 0.0000001)
  }
  
  
  # # Priors for PTF betas across all 5 sub-seasons
  # for(s in 1:5) {
  #   beta_feed_temp[s]         ~ dt(0, 0.40776, 7.763)
  #   beta_feed_precip[s]       ~ dt(0, 0.40776, 7.763)
  #   beta_feed_temp_precip[s]  ~ dt(0, 0.40776, 7.763)
  #   beta_feed_urban[s]        ~ dt(0, 0.40776, 7.763)
  #   beta_feed_cropland[s]     ~ dt(0, 0.40776, 7.763)
  #   beta_feed_swamp[s]        ~ dt(0, 0.40776, 7.763)
  #   beta_feed_marsh[s]        ~ dt(0, 0.40776, 7.763)
  #   beta_feed_open_water[s]   ~ dt(0, 0.40776, 7.763)
  # }
  # 
  # # Separate freeze prior for feed: season 1 gets normal, seasons 2-5 get tight uniform
  # beta_feed_freeze[1] ~ dt(0, 0.40776, 7.763)
  # for(s in 2:5) {
  #   beta_feed_freeze[s] ~ dunif(-0.000001, 0.0000001)
  # }

  ## likelihoods --------------------------------------------------------------


  ## effects of weather and habitat use on ODBA
  # observation variation
  
  for(b in 1:nobs2){ # individual/year/sub-season index
    log_ODBA[b] ~ dnorm(mu_log_ODBA[ids2[b], years2[b], sub_seasons2[b]], 
                        sd = sd_ODBA_obs[ids2[b],years2[b]])
    
    # link function
    mu_log_ODBA[ids2[b], years2[b], sub_seasons2[b]] <- 
      alpha_ODBA[ids2[b]] +
      beta_ODBA_precip[sub_seasons2[b]] * precip2[b] +
      beta_ODBA_temp[sub_seasons2[b]] * temp2[b] + 
      beta_ODBA_temp_precip[sub_seasons2[b]] * temp2[b] * precip2[b] +
      beta_ODBA_freeze[sub_seasons2[b]] * freeze2[b] + 
      beta_ODBA_urban[sub_seasons2[b]] * urban2[b] +
      beta_ODBA_cropland[sub_seasons2[b]] * cropland2[b] +
      beta_ODBA_swamp[sub_seasons2[b]] * swamp2[b] +
      beta_ODBA_marsh[sub_seasons2[b]] * marsh2[b] +
      beta_ODBA_open_water[sub_seasons2[b]] * open_water2[b] 
  } # b
  
  ## effects of weather and habitat use on PTF
  # observation variation
  # 
  # for(c in 1:nobs3){ # individual/year/sub-season index
  #   feed[c] ~ dbin(p_feed[ids3[c], years3[c], sub_seasons3[c]], n[c])
  #   
  #   # link function
  #   logit(p_feed[ids3[c], years3[c], sub_seasons3[c]]) <-
  #     alpha_feed[ids3[c]] +
  #     beta_feed_precip[sub_seasons3[c]] * precip3[c] +
  #     beta_feed_temp[sub_seasons3[c]] * temp3[c] +
  #     beta_feed_temp_precip[sub_seasons3[c]] * temp3[c] * precip3[c] + # ilsa had these coded as 2, i'm going to change to 3
  #     beta_feed_freeze[sub_seasons3[c]] * freeze3[c] +
  #     beta_feed_urban[sub_seasons3[c]] * urban3[c] +
  #     beta_feed_cropland[sub_seasons3[c]] * cropland3[c] +
  #     beta_feed_swamp[sub_seasons3[c]] * swamp3[c] +
  #     beta_feed_marsh[sub_seasons3[c]] * marsh3[c] +
  #     beta_feed_open_water[sub_seasons3[c]] * open_water3[c]
  # } # c

})


# set up NIMBLE model ---------------------------------------------------------

# initial values

na_fill_inits <- function(n_param, na_param, sd){
  t_inits <- rnorm(n_param, mean = 0, sd = sd)
  t_inits[na_param] <- NA
  t_inits

}

inits <- function() {
  list(
    alpha_ODBA = rnorm(nimble_constants$nind, 0, 1),
  #   alpha_feed = rnorm(nimble_constants$nind, 0, 1),
    
    # Initialize observation SDs (must be positive, matching your dunif(0, 10) prior)
    # Note: Adjust dimensions of this matrix to match max(ids2_sd) and max(years2_sd) if needed
    sd_ODBA_obs = array(runif(max(nimble_constants$ids2_sd) * max(nimble_constants$years2_sd), 0.5, 2), 
                        dim = c(max(nimble_constants$ids2_sd), max(nimble_constants$years2_sd))),
    
    # Betas across the 5 sub-seasons ODBA
    beta_ODBA_temp = rnorm(5, 0, 0.5),
    beta_ODBA_precip = rnorm(5, 0, 0.5),
    beta_ODBA_temp_precip = rnorm(5, 0, 0.5),
    beta_ODBA_freeze = na_fill_inits(5, c(2, 3, 4, 5), sd = 1), # this should mean only season 1 is looked at
    beta_ODBA_urban = rnorm(5, 0, 0.5),
    beta_ODBA_cropland = rnorm(5, 0, 0.5),
    beta_ODBA_swamp = rnorm(5, 0, 0.5),
    beta_ODBA_marsh = rnorm(5, 0, 0.5),
    beta_ODBA_open_water = rnorm(5, 0, 0.5) #, 
    
    # Betas across the 5 sub-seasons PTF
    
    # beta_feed_temp = rnorm(5, 0, 0.5),
    # beta_feed_precip = rnorm(5, 0, 0.5),
    # beta_feed_temp_precip = rnorm(5, 0, 0.5),
    # beta_feed_freeze = na_fill_inits(5, c(2, 3, 4, 5), sd = 1), # this should mean only season 1 is looked at
    # beta_feed_urban = rnorm(5, 0, 0.5),
    # beta_feed_cropland = rnorm(5, 0, 0.5),
    # beta_feed_swamp = rnorm(5, 0, 0.5),
    # beta_feed_marsh = rnorm(5, 0, 0.5),
    # beta_feed_open_water = rnorm(5, 0, 0.5)
  )
}

# constants (dimensions and indexing vectors that do not change during sampling)
nimble_constants <- list(
    nind = nind,
    ### b loop (weather and habitat on odba)
    nobs2 = nobs2,             
    nobs2_sd = length(ids2_sd),           
    ids2 = ids2,                          
    years2 = years2,                   
    sub_seasons2 = sub_seasons2,    
    ids2_sd = ids2_sd,                    
    years2_sd = years2_sd #,      
    ### c loop (weather and habitat on ptf)
    # nobs3 = nobs3, 
    # ids3 = dat_feed$id,
    # years3 = dat_feed$year_numeric,
    # sub_seasons3 = dat_feed$season_index
  )


# data

data <- list(
  
  # b loop (ODBA)
  log_ODBA = dat_odba$log_odba,                  # Response variable vector (length nobs2)
  precip2 = dat_odba$scaled_mean_precip,                    # Precipitation covariate
  temp2 = dat_odba$scaled_min_temp,                        # Temperature covariate
  freeze2 = dat_odba$scaled_freeze_events,                    # Freeze binary/covariate
  urban2 = dat_odba$scaled_prop_urban,                      # Urban habitat use
  cropland2 = dat_odba$scaled_prop_cropland,                # Cropland habitat use
  swamp2 = dat_odba$scaled_prop_swamp,                      # Swamp habitat use
  marsh2 = dat_odba$scaled_prop_marsh,                      # Marsh habitat use
  open_water2 = dat_odba$scaled_prop_open_water #,             # Open water habitat use
# 
#   # c loop (PTF)
#   precip3 = dat_feed$scaled_mean_precip, 
#   temp3 = dat_feed$scaled_min_temp, 
#   freeze3 = dat_feed$scaled_freeze_events, 
#   urban3 = dat_feed$scaled_prop_urban, 
#   cropland3 = dat_feed$scaled_prop_cropland, 
#   swamp3 = dat_feed$scaled_prop_swamp, 
#   marsh3 = dat_feed$scaled_prop_marsh, 
#   open_water3 = dat_feed$scaled_prop_open_water, 
#   feed = dat_feed$feed_fixes_corrected_season,
#   n = dat_feed$total_acc_fixes_corrected_season
)

# run NIMBLE model in series ---------------------------
nc <- 3

# build model
model <- nimbleModel(code = model_code, 
                     constants = nimble_constants,  
                     data =  data, 
                     buildDerivs = T)

parameters_to_save <- c( 
  "beta_ODBA_temp", 
  "beta_ODBA_precip", 
  "beta_ODBA_freeze",
  "beta_ODBA_temp_precip",
  "beta_ODBA_urban", 
  "beta_ODBA_cropland", 
  "beta_ODBA_swamp", 
  "beta_ODBA_marsh", 
  "beta_ODBA_open_water" #, 
  # "beta_feed_temp", 
  # "beta_feed_precip", 
  # "beta_feed_freeze",
  # "beta_feed_temp_precip",
  # "beta_feed_urban", 
  # "beta_feed_cropland", 
  # "beta_feed_swamp", 
  # "beta_feed_marsh", 
  # "beta_feed_open_water"
)


# configure MCMC
mcmc_Conf  <- configureHMC(model, print = F)

mcmc_Conf$resetMonitors()
mcmc_Conf$addMonitors(parameters_to_save)

# build MCMC
modelMCMC  <- buildMCMC(mcmc_Conf)

# compile model and MCMC
Cmodel     <- compileNimble(model)
CmodelMCMC <- compileNimble(modelMCMC, project = model)

# run model
samples <- runMCMC(CmodelMCMC,
                   niter = 300,
                   nburnin = 40,
                   thin = 10,
                   inits = inits,
                   nchains = nc,
                   samplesAsCodaMCMC = T)


# convert to MCMC list
samples <- list(chain1 = samples[[1]], 
                chain2 = samples[[2]], 
                chain3 = samples[[3]])

mcmc_list <- as.mcmc.list(lapply(samples, mcmc))

for(i in 1:3){
  mcmc_list[[i]] <- mcmc_list[[i]][ , which(is.na(colSums(mcmc_list[[i]])) == F)]
}

# assess convergence

rhat <- gelman.diag(mcmc_list, multivariate = F)
rhat <- unlist(rhat$psrf)
rhat[which(rhat[,1] > 1.1), ]

MCMCplot(samples)


# Calculate tail probabilities
# See proportion of samples above or below 0 (above for negative effects, below for positive effects)
prop_above_or_below <- function(x) {
  if(median(x) > 0) {return(mean(x < 0))}
  else {return(mean(x > 0))}
}

# calculate summary statistics
sum_stats <- MCMCsummary(mcmc_list, func = function(x) prop_above_or_below(x), func_name = "prop") %>%
  tibble::rownames_to_column(var = "parameter")


all_samples <- as.mcmc(do.call(rbind, samples))

p_tails <- sum_stats %>%
  dplyr::select(parameter, prop)

samples_long <- all_samples %>%
  as.data.frame() %>%
  pivot_longer(
    everything(), 
    names_to = "parameter", 
    values_to = "posterior"
  ) %>%
  left_join(p_tails, by = "parameter") 

odba_habitat <- samples_long %>%
  extract(
    parameter, 
    into = c("model", "habitat", "season"), 
    regex = "^beta_([^_]+)_(.*?)\\[([0-9]+)\\]$") %>%
  filter(habitat %in% c("cropland", "urban", "marsh", "swamp", "open_water"))

odba_weather <- samples_long %>%
  extract(
    parameter, 
    into = c("model", "weather", "season"), 
    regex = "^beta_([^_]+)_(.*?)\\[([0-9]+)\\]$") %>%
  filter(weather %in% c("freeze", "precip", "temp"), 
         model == "ODBA")



ggplot(odba_weather) + 
  stat_slab(aes(x = weather, y = posterior, color = season, fill = season),
            side = "both",
            position = position_dodge(width = 0.7),
            linewidth = 0.1) 
