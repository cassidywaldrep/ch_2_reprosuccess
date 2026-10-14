######################## 04_facmodel_nimble.R ##################

# Code: NIMBLE FACE MODEL
# Date: 5 Oct 2026
# Author: Cassidy Waldrep

# Load packages and functions

library(tidyverse)
library(nimble)

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

nobs2 <- nrow(dat_odba)
ids2 <- dat_odba$id
years2 <- dat_odba$year_numeric
sub_seasons2 = dat_odba$season_index

# specify GLMMs in BUGS language ----------------------------------------------

model_code <- nimbleCode({
  
  ## priors -------------------------------------------------------------------
  
  ### Beta coefficients  ------------------------------------------------------
  
  # betas for ODBA model
  
  # winter sub-season
  beta_ODBA_freeze[1] ~ dnorm(0, 0.01)
  beta_ODBA_temp[1] ~ dunif(-0.000001, 0.0000001)
  beta_ODBA_precip[1] ~ dnorm(0, 0.01)
  beta_ODBA_freeze_precip[1] ~ dnorm(0, 0.01)
  beta_ODBA_temp_precip[1] ~ dunif(-0.000001, 0.0000001)
  beta_ODBA_hfi[1] ~ dnorm(0, 0.01)
  beta_ODBA_forest[1] ~ dnorm(0, 0.01)
  beta_ODBA_peatland[1] ~ dnorm(0, 0.01)
  beta_ODBA_swamp[1] ~ dnorm(0, 0.01)
  beta_ODBA_marsh[1] ~ dnorm(0, 0.01)
  beta_ODBA_open_water[1] ~ dnorm(0, 0.01)
  
  
  # ### Random Effects ---------------------------------------------------------
  
  
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
      beta_ODBA_urban[sub_seasons2[b]] * forest2[b] +
      beta_ODBA_peatland[sub_seasons2[b]] * peatland2[b] +
      beta_ODBA_swamp[sub_seasons2[b]] * swamp2[b] +
      beta_ODBA_marsh[sub_seasons2[b]] * marsh2[b] +
      beta_ODBA_open_water[sub_seasons2[b]] * open_water2[b] 
  } # b
  
  
  ## effects of weather and habitat use on ODBA

})


# set up NIMBLE model ---------------------------------------------------------

# initial values

nimble_inits <- function(){
  bander_inits <- rnorm(nbander - 1, 0, 1)
  day_inits <- rnorm(nday - 1, 0, 1)
  age_inits <- rnorm(1, 0, 1)
  list(beta_bander_tmp = c(bander_inits, -sum(bander_inits)),
       beta_day_tmp = c(day_inits, -sum(day_inits)),
       beta_age = c(age_inits, -age_inits),
       beta_mean_mintemp_prev5 = rnorm(1, 0, 1),
       beta_weight = rnorm(1, 0, 1),
       sd_state = runif(1, 0.01, 0.4),
       alpha = rnorm(1, 5, 1))
}

# constants
nimble_constants <- list(
  nobs = nobs,
  # nyear = nyear,
  nday = nday,
  # nage = nage, 
  nbander = nbander,
  nstate = nstate,
  bander_index = data_model$bander_index_new, # Use new index where "other" = 54
  # year_index = data_model$year_index,
  day_index = data_model$day_index, 
  # age_index = data_model$age_index,
  age_cat = data_model$age_index,
  state_index = data_model$state_index,
  interval = data_model$interval,
  mean_mintemp_prev5 = as.numeric(data_model$mean_mintemp_prev5_scaled),
  #julian = as.numeric(data_model$julian_date_scaled),
  weight = as.numeric(data_model$PCA_weight_scaled)
)

# data
nimble_data <- list(
  survival = data_model$survived
)

# run NIMBLE model in series ---------------------------
nc <- 3

# nimble_inits <- inits_function() 

## quickly check collinearity 

model <- glmer(
  survived ~ 
    bander_index + 
    day_index + 
    mean_mintemp_prev5_scaled +
    PCA_weight_scaled +
    age +
    (1 | partner_agency) +
    offset(log(interval)),
  family = binomial(link = "cloglog"),
  data = data_model
)

vif(model)

# build model
model <- nimbleModel(code = model_code, 
                     constants = nimble_constants,  
                     data =  nimble_data, 
                     buildDerivs = T)

# configure MCMC
mcmc_Conf  <- configureHMC(model, print = F)
mcmc_Conf$setMonitors(c("alpha", "beta_age", "beta_bander", "beta_day", "beta_mean_mintemp_prev5", "beta_weight", "eps_state", "sd_state"))

# build MCMC
modelMCMC  <- buildMCMC(mcmc_Conf)

# compile model and MCMC
Cmodel     <- compileNimble(model)
CmodelMCMC <- compileNimble(modelMCMC, project = model)

# run model
samples <- runMCMC(CmodelMCMC,
                   niter = 10000,
                   nburnin = 2000,
                   thin = 10,
                   nchains = nc,
                   samplesAsCodaMCMC = T)
