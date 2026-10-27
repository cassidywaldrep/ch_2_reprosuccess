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

# summarizing data for jack
# 
# raw_data_jack <- raw_data %>%
#   filter(season == "nesting") %>%
#   dplyr::select(bandnum, year, bird_year, incubation_status, age, julian_arrival, mig_status)

# write.csv(raw_data_jack, "../../../OneDrive - University of Saskatchewan/Analyses/phd_dissertation/Chapter4_Competition/Jack/mall_incubation_data_20Oct2026.csv")

#######

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
         scaled_lat = as.numeric(scale(nest_lat)), 
         scaled_long = as.numeric(scale(nest_long)),
         log_odba = log(mean_ODBA_season), 
         year_numeric = as.numeric(as.factor(year)), 
         migratory = as.numeric(factor(mig_status, levels = c("No", "Yes"))),
         age = as.numeric(factor(age, levels = c("SY", "ASY"))))

master_birds <- unique(input_data_scale$bandnum)

# 2. Apply a unified sequential ID (1 to nind) to input_data_scale first
input_data_scale <- input_data_scale %>%
  mutate(id = match(bandnum, master_birds))

# 3. Also make sure your years are sequential integers starting at 1
input_data_scale <- input_data_scale %>%
  mutate(year_numeric = as.numeric(as.factor(year)))

nind <- length(master_birds)
nyears <- max(input_data_scale$year_numeric)

#-------------------------------------------------------------------------#
###### creating indexing #############
#-------------------------------------------------------------------------#
        
#### 1. a loop (nest propensity sub-model, individual/year index) #####
dat_annual <- input_data_scale %>% 
  filter(egg_laying_status == 1 | egg_laying_status == 0, 
         season_index == 5) %>%
  mutate(id_annual = as.numeric(as.factor(bandnum)))

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
  
  dplyr::select(id, year_numeric, egg_laying_status, season_combo_index, season_index, log_odba, feed_fixes_corrected_season, total_acc_fixes_corrected_season, scaled_first_day, age, id_annual, migratory) %>%
  
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

dat_annual_1 <- dat_annual %>% filter(season_combo_index == 1)
dat_annual_2 <- dat_annual %>% filter(season_combo_index == 2)
dat_annual_3 <- dat_annual %>% filter(season_combo_index == 3)
dat_annual_4 <- dat_annual %>% filter(season_combo_index == 4)
dat_annual_5 <- dat_annual %>% filter(season_combo_index == 5)
dat_annual_6 <- dat_annual %>% filter(season_combo_index == 6)
dat_annual_7 <- dat_annual %>% filter(season_combo_index == 7)


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
  dplyr::select(id, year_numeric, incubation_status, migratory, scaled_lat, scaled_long,
                scaled_prop_swamp, scaled_prop_marsh, scaled_prop_cropland, scaled_prop_peatland) %>%
  # keep only rows that have no NAs
  drop_na() # removes 2 individuals currently... 

# NO NAS!!!
# na <- dat_nest_success %>%
#   # keep only rows that have no NAs
#   drop_na()

n_NS_na_rem <- unique(dat_nest_success$id)

dat_nest_success %>% group_by(migratory) %>% count()

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

# a loop

nobs1 <- nrow(dat_annual)
ids1 <- dat_annual$id
years1 <- dat_annual$year_numeric

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
    alpha_feed[i] ~ dnorm(0, sd = 10)
    
  }  
  
  # ODBA observation variation
  for(b in 1:nyears){ 
    sd_ODBA_obs[b] ~ dunif(0, 10)
  } 
  
  ### Beta coefficients  ------------------------------------------------------
  # Priors for ODBA betas across all 5 sub-seasons
  for(s in 1:5) {
    beta_ODBA_temp[s]         ~ dnorm(0, sd = 5)
    beta_ODBA_precip[s]       ~ dnorm(0, sd = 5)
    beta_ODBA_temp_precip[s]  ~ dnorm(0, sd = 5)
    beta_ODBA_urban[s]        ~ dnorm(0, sd = 5)
    beta_ODBA_cropland[s]     ~ dnorm(0, sd = 5)
    beta_ODBA_swamp[s]        ~ dnorm(0, sd = 5)
    beta_ODBA_marsh[s]        ~ dnorm(0, sd = 5)
    beta_ODBA_open_water[s]   ~ dnorm(0, sd = 5)
  }
  
  # Separate freeze prior for ODBA: season 1 gets normal, seasons 2-5 get tight uniform
  beta_ODBA_freeze[1] ~ dnorm(0, sd = 5)
  
  for(s in 2:5) {
    beta_ODBA_freeze[s] ~ dunif(-0.000001, 0.0000001)
  }
  
  # # Priors for PTF betas across all 5 sub-seasons
  for(s in 1:5) {
    beta_feed_temp[s]         ~ dt(0, 0.40776, 7.763)
    beta_feed_precip[s]       ~ dt(0, 0.40776, 7.763)
    beta_feed_temp_precip[s]  ~ dt(0, 0.40776, 7.763)
    beta_feed_urban[s]        ~ dt(0, 0.40776, 7.763)
    beta_feed_cropland[s]     ~ dt(0, 0.40776, 7.763)
    beta_feed_swamp[s]        ~ dt(0, 0.40776, 7.763)
    beta_feed_marsh[s]        ~ dt(0, 0.40776, 7.763)
    beta_feed_open_water[s]   ~ dt(0, 0.40776, 7.763)
  }
  
  # Separate freeze prior for feed: season 1 gets normal, seasons 2-5 get tight uniform
  beta_feed_freeze[1] ~ dt(0, 0.40776, 7.763)
  for(s in 2:5) {
    beta_feed_freeze[s] ~ dunif(-0.000001, 0.0000001)
  }
  
  # priors for nesting propensity model
  
  alpha_global_NP ~ dlogis(0, 1)
  beta_arrival_breed ~ dt(0, 0.40776, 7.763)
  beta_age ~ dt(0, 0.40776, 7.763)
  # 
  # # priors for incubation model
  # 
  alpha_global_NS ~ dlogis(0, 1)
  beta_NP ~ dt(0, 0.40776, 7.763)
  beta_migratory ~ dt(0, 0.40776, 7.763)
  beta_nest_cropland ~ dt(0, 0.40776, 7.763)
  beta_nest_peatland ~ dt(0, 0.40776, 7.763)
  beta_lat ~ dt(0, 0.40776, 7.763)
  beta_long ~ dt(0, 0.40776, 7.763)
  beta_nest_swamp ~ dt(0, 0.40776, 7.763)
  beta_nest_marsh ~ dt(0, 0.40776, 7.763)
  # 
  # # priors for brood rearing
  # 
  alpha_global_BS ~ dlogis(0, 1)
  beta_NS ~ dt(0, 0.40776, 7.763)
  beta_brood_min_temp ~ dt(0, 0.40776, 7.763)
  beta_brood_precip ~ dt(0, 0.40776, 7.763)
  beta_brood_temp_precip ~ dt(0, 0.40776, 7.763)
  beta_brood_city ~ dt(0, 0.40776, 7.763)
  beta_brood_swamp ~ dt(0, 0.40776, 7.763)
  beta_brood_marsh ~ dt(0, 0.40776, 7.763)
  beta_brood_open_water ~ dt(0, 0.40776, 7.763)
  # 
  
  # mean betas
  mu_beta_ODBA ~ dnorm(0, sd = 5)
  mu_beta_feed ~ dnorm(0, sd = 5)
  
  for(s in 1:n_sub_seasons){ # sub-season index
    beta_ODBA[s] ~ dnorm(mu_beta_ODBA, sd = 5)
    beta_feed[s] ~ dnorm(mu_beta_feed, sd = 5)
  }
  
  
  # random year effect - propensity
  for(t in 1:nyears){ # year index
    eps_year_propensity[t] ~ dnorm(0, sd = 5)     
    eps_year_nest[t] ~ dnorm(0, sd = 5) 
    eps_year_brood[t] ~ dnorm(0, sd = 5) 
    
    
  }
  
  # 
  ## likelihoods --------------------------------------------------------------
  
  
  ## effects of weather and habitat use on ODBA
  # observation variation
  
  for (b in 1:nobs2) {
    
    log_ODBA[b] ~ dnorm(
      mu_log_ODBA[ids2[b], years2[b], sub_seasons2[b]],
      sd = sd_ODBA_obs[years2[b]]
    )
    
    mu_log_ODBA[ids2[b], years2[b], sub_seasons2[b]] <-
      alpha_ODBA[ids2[b]] +
      beta_ODBA_precip[sub_seasons2[b]] * precip2[b] +
      beta_ODBA_temp[sub_seasons2[b]] * temp2[b] +
      beta_ODBA_temp_precip[sub_seasons2[b]] *
      temp2[b] * precip2[b] +
      beta_ODBA_freeze[sub_seasons2[b]] * freeze2[b] +
      beta_ODBA_urban[sub_seasons2[b]] * urban2[b] +
      beta_ODBA_cropland[sub_seasons2[b]] * cropland2[b] +
      beta_ODBA_swamp[sub_seasons2[b]] * swamp2[b] +
      beta_ODBA_marsh[sub_seasons2[b]] * marsh2[b] +
      beta_ODBA_open_water[sub_seasons2[b]] * open_water2[b]
  }
  
  
  for (c in 1:nobs3) {
    
    feed[c] ~ dbin(
      p_feed[ids3[c], years3[c], sub_seasons3[c]],
      n[c]
    )
    
    logit(p_feed[ids3[c], years3[c], sub_seasons3[c]]) <-
      alpha_feed[ids3[c]] +
      beta_feed_precip[sub_seasons3[c]] * precip3[c] +
      beta_feed_temp[sub_seasons3[c]] * temp3[c] +
      beta_feed_temp_precip[sub_seasons3[c]] *
      temp3[c] * precip3[c] +
      beta_feed_freeze[sub_seasons3[c]] * freeze3[c] +
      beta_feed_urban[sub_seasons3[c]] * urban3[c] +
      beta_feed_cropland[sub_seasons3[c]] * cropland3[c] +
      beta_feed_swamp[sub_seasons3[c]] * swamp3[c] +
      beta_feed_marsh[sub_seasons3[c]] * marsh3[c] +
      beta_feed_open_water[sub_seasons3[c]] * open_water3[c]
  }
  
  for (a in 1:nobs1) {
    NP[a] ~ dbern(p_nest[ids1[a], years1[a]])
  }
  
  
  ## effects of behaviour on NP 1 subseason combo (migratory, complete)
  # this has to be coded differently than Ilsa's model because NIMBLE only allows nice integers in order for the "for" statement
  
  for(z in 1:n_obs_NP_1) {
    logit(p_nest[ids1[obs_NP_1[z]], years1[obs_NP_1[z]]]) <-
      alpha_global_NP +
      beta_ODBA[1] * mu_log_ODBA[ids1[obs_NP_1[z]], years1[obs_NP_1[z]], 1] +
      beta_ODBA[2] * mu_log_ODBA[ids1[obs_NP_1[z]], years1[obs_NP_1[z]], 2] +
      beta_ODBA[3] * mu_log_ODBA[ids1[obs_NP_1[z]], years1[obs_NP_1[z]], 3] +
      beta_ODBA[4] * mu_log_ODBA[ids1[obs_NP_1[z]], years1[obs_NP_1[z]], 4] +
      beta_ODBA[5] * mu_log_ODBA[ids1[obs_NP_1[z]], years1[obs_NP_1[z]], 5] +
      beta_feed[1] *  logit(p_feed[ids1[obs_NP_1[z]], years1[obs_NP_1[z]], 1]) +
      beta_feed[2] *  logit(p_feed[ids1[obs_NP_1[z]], years1[obs_NP_1[z]], 2]) +
      beta_feed[3] *  logit(p_feed[ids1[obs_NP_1[z]], years1[obs_NP_1[z]], 3]) +
      beta_feed[4] *  logit(p_feed[ids1[obs_NP_1[z]], years1[obs_NP_1[z]], 4]) +
      beta_feed[5] *  logit(p_feed[ids1[obs_NP_1[z]], years1[obs_NP_1[z]], 5]) +
      beta_arrival_breed * arrival_day_breed[obs_NP_1[z]] +
      beta_age * age[obs_NP_1[z]] +
      eps_year_propensity[years1[obs_NP_1[z]]]
  } 
  
  ## effects of behaviour on NP 2 subseason combo (migratory, missing winter)
  
  for(y in 1:n_obs_NP_2) {
    logit(p_nest[ids1[obs_NP_2[y]], years1[obs_NP_2[y]]]) <-
      alpha_global_NP +
      beta_ODBA[2] * mu_log_ODBA[ids1[obs_NP_2[y]], years1[obs_NP_2[y]], 2] +
      beta_ODBA[3] * mu_log_ODBA[ids1[obs_NP_2[y]], years1[obs_NP_2[y]], 3] +
      beta_ODBA[4] * mu_log_ODBA[ids1[obs_NP_2[y]], years1[obs_NP_2[y]], 4] +
      beta_ODBA[5] * mu_log_ODBA[ids1[obs_NP_2[y]], years1[obs_NP_2[y]], 5] +
      beta_feed[2] *  logit(p_feed[ids1[obs_NP_2[y]], years1[obs_NP_2[y]], 2]) +
      beta_feed[3] *  logit(p_feed[ids1[obs_NP_2[y]], years1[obs_NP_2[y]], 3]) +
      beta_feed[4] *  logit(p_feed[ids1[obs_NP_2[y]], years1[obs_NP_2[y]], 4]) +
      beta_feed[5] *  logit(p_feed[ids1[obs_NP_2[y]], years1[obs_NP_2[y]], 5]) +
      beta_arrival_breed * arrival_day_breed[obs_NP_2[y]] +
      beta_age * age[obs_NP_2[y]] +
      eps_year_propensity[years1[obs_NP_2[y]]]
  } 
  
  ## effects of behaviour on NP 3 subseason combo (migratory, missing winter and late winter)
  
  for(x in 1:n_obs_NP_3) {
    logit(p_nest[ids1[obs_NP_3[x]], years1[obs_NP_3[x]]]) <-
      alpha_global_NP +
      beta_ODBA[3] * mu_log_ODBA[ids1[obs_NP_3[x]], years1[obs_NP_3[x]], 3] +
      beta_ODBA[4] * mu_log_ODBA[ids1[obs_NP_3[x]], years1[obs_NP_3[x]], 4] +
      beta_ODBA[5] * mu_log_ODBA[ids1[obs_NP_3[x]], years1[obs_NP_3[x]], 5] +
      beta_feed[3] *  logit(p_feed[ids1[obs_NP_3[x]], years1[obs_NP_3[x]], 3]) +
      beta_feed[4] *  logit(p_feed[ids1[obs_NP_3[x]], years1[obs_NP_3[x]], 4]) +
      beta_feed[5] *  logit(p_feed[ids1[obs_NP_3[x]], years1[obs_NP_3[x]], 5]) +
      beta_arrival_breed * arrival_day_breed[obs_NP_3[x]] +
      beta_age * age[obs_NP_3[x]] +
      eps_year_propensity[years1[obs_NP_3[x]]]
  } 
  
  ## effects of behaviour on NP 4 subseason combo (migratory, missing late winter)
  
  for(w in 1:n_obs_NP_4) {
    logit(p_nest[ids1[obs_NP_4[w]], years1[obs_NP_4[w]]]) <-
      alpha_global_NP +
      beta_ODBA[1] * mu_log_ODBA[ids1[obs_NP_4[w]], years1[obs_NP_4[w]], 1] +
      beta_ODBA[3] * mu_log_ODBA[ids1[obs_NP_4[w]], years1[obs_NP_4[w]], 3] +
      beta_ODBA[4] * mu_log_ODBA[ids1[obs_NP_4[w]], years1[obs_NP_4[w]], 4] +
      beta_ODBA[5] * mu_log_ODBA[ids1[obs_NP_4[w]], years1[obs_NP_4[w]], 5] +
      beta_feed[1] *  logit(p_feed[ids1[obs_NP_4[w]], years1[obs_NP_4[w]], 1]) +
      beta_feed[3] *  logit(p_feed[ids1[obs_NP_4[w]], years1[obs_NP_4[w]], 3]) +
      beta_feed[4] *  logit(p_feed[ids1[obs_NP_4[w]], years1[obs_NP_4[w]], 4]) +
      beta_feed[5] *  logit(p_feed[ids1[obs_NP_4[w]], years1[obs_NP_4[w]], 5]) +
      beta_arrival_breed * arrival_day_breed[obs_NP_4[w]] +
      beta_age * age[obs_NP_4[w]] +
      eps_year_propensity[years1[obs_NP_4[w]]]
  } 
  
  ## effects of behaviour on NP 5 subseason combo (migratory, missing winter - early migration)
  
  for(v in 1:n_obs_NP_5) {
    logit(p_nest[ids1[obs_NP_5[v]], years1[obs_NP_5[v]]]) <-
      alpha_global_NP +
      beta_ODBA[4] * mu_log_ODBA[ids1[obs_NP_5[v]], years1[obs_NP_5[v]], 4] +
      beta_ODBA[5] * mu_log_ODBA[ids1[obs_NP_5[v]], years1[obs_NP_5[v]], 5] +
      beta_feed[4] *  logit(p_feed[ids1[obs_NP_5[v]], years1[obs_NP_5[v]], 4]) +
      beta_feed[5] *  logit(p_feed[ids1[obs_NP_5[v]], years1[obs_NP_5[v]], 5]) +
      beta_arrival_breed * arrival_day_breed[obs_NP_5[v]] +
      beta_age * age[obs_NP_5[v]] +
      eps_year_propensity[years1[obs_NP_5[v]]]
  } 
  
  # effects of behaviour on NP 6 subseason combo (nonmigratory, complete)
  
  for(u in 1:n_obs_NP_6) {
    logit(p_nest[ids1[obs_NP_6[u]], years1[obs_NP_6[u]]]) <-
      alpha_global_NP +
      beta_ODBA[1] * mu_log_ODBA[ids1[obs_NP_6[u]], years1[obs_NP_6[u]], 1] +
      beta_ODBA[5] * mu_log_ODBA[ids1[obs_NP_6[u]], years1[obs_NP_6[u]], 5] +
      beta_feed[1] *  logit(p_feed[ids1[obs_NP_6[u]], years1[obs_NP_6[u]], 1]) +
      beta_feed[5] *  logit(p_feed[ids1[obs_NP_6[u]], years1[obs_NP_6[u]], 5]) +
      beta_age * age[obs_NP_6[u]] +
      eps_year_propensity[years1[obs_NP_6[u]]]
  } 
  
  # effects of behaviour on NP 7 subseason combo (nonmigratory, complete)
  
  for(r in 1:n_obs_NP_7) {
    logit(p_nest[ids1[obs_NP_7[r]], years1[obs_NP_7[r]]]) <-
      alpha_global_NP +
      beta_ODBA[5] * mu_log_ODBA[ids1[obs_NP_7[r]], years1[obs_NP_7[r]], 5] +
      beta_feed[5] *  logit(p_feed[ids1[obs_NP_7[r]], years1[obs_NP_7[r]], 5]) +
      beta_age * age[obs_NP_7[r]] +
      eps_year_propensity[years1[obs_NP_7[r]]]
  }
  
  
  ## effects of habitat on nesting success
  
  for(d in 1:nobs4){ # individual/year index
    NS[d] ~ dbern(p_hatch[ids4[d], years4[d]])
    
    # link function
    logit(p_hatch[ids4[d], years4[d]]) <- 
      alpha_global_NS +
      beta_NP * logit(p_nest[ids4[d], years4[d]]) +
      beta_migratory * migratory[d] +
      beta_nest_cropland * nest_cropland[d] +
      beta_nest_peatland * nest_peatland[d] +
      beta_lat * nest_lat[d] +
      beta_long * nest_long[d] +
      beta_nest_swamp * nest_swamp[d] +
      beta_nest_marsh * nest_marsh[d] +
      eps_year_nest[years4[d]] 
    # d
    
  }
  
  ## effects of habitat amd weather on brood success
  for(e in 1:nobs5){ # individual/year index
    BS[e] ~ dbern(p_fledge[ids5[e], years5[e]])
    
    # link function
    logit(p_fledge[ids5[e], years5[e]]) <-
      alpha_global_BS +
      beta_NS * logit(p_hatch[ids5[e], years5[e]]) +
      beta_brood_min_temp * brood_min_temp[e] +
      beta_brood_precip * brood_precip[e] +
      beta_brood_temp_precip * brood_min_temp[e] * brood_precip[e] +
      beta_brood_city * brood_city[e] +
      beta_brood_swamp * brood_swamp[e] +
      beta_brood_marsh * brood_marsh[e] +
      beta_brood_open_water * brood_open_water[e] +
      eps_year_brood[years5[e]]
  } # e
  
  
})


# set up NIMBLE model ---------------------------------------------------------

# constants (dimensions and indexing vectors that do not change during sampling)
nimble_constants <- list(
  nind = nind,
  nyears = length(unique(dat_odba$year_numeric)),
  n_sub_seasons = length(unique(dat_odba$season_index)),
  
  
  ### b loop (weather and habitat on odba)
  nobs2 = nobs2,             
  ids2 = ids2,                          
  years2 = years2,                   
  sub_seasons2 = sub_seasons2,    
  
  ### c loop (weather and habitat on ptf)
  nobs3 = nobs3,
  ids3 = dat_feed$id,
  years3 = years3,                   
  sub_seasons3 = dat_feed$season_index, 
  
  ## a loop (nesting propensity)
  
  nobs1 = nrow(dat_annual),
  ids1 = dat_annual$id,
  years1 = dat_annual$year_numeric,
  
  ## d loop (fti)
  
  nobs4 = nrow(dat_nest_success),
  ids4 = dat_nest_success$id, 
  years4 = dat_nest_success$year_numeric, 
  
  ## e loop (brood rearing)
  nobs5 = nrow(dat_brood_success),
  ids5 = dat_brood_success$id, 
  years5 = dat_brood_success$year_numeric,
  
  n_obs_NP_1 = length(which(dat_annual$season_combo_index == 1)),
  obs_NP_1 = which(dat_annual$season_combo_index == 1),
  n_obs_NP_2 = length(which(dat_annual$season_combo_index==2)), 
  obs_NP_2 = which(dat_annual$season_combo_index==2), 
  n_obs_NP_3 = length(which(dat_annual$season_combo_index==3)), 
  obs_NP_3 = which(dat_annual$season_combo_index==3), 
  n_obs_NP_4 = length(which(dat_annual$season_combo_index==4)), 
  obs_NP_4 = which(dat_annual$season_combo_index==4), 
  n_obs_NP_5 = length(which(dat_annual$season_combo_index==5)),
  obs_NP_5 = which(dat_annual$season_combo_index==5), 
  n_obs_NP_6 = length(which(dat_annual$season_combo_index==6)), 
  obs_NP_6 = which(dat_annual$season_combo_index==6), 
  n_obs_NP_7 = length(which(dat_annual$season_combo_index==7)),
  obs_NP_7 = which(dat_annual$season_combo_index==7)
  
)

# initial values

inits <- function() {
  list(
    # Individual intercepts
    alpha_ODBA = rnorm(nimble_constants$nind, 0, 0.1),
    alpha_feed = rnorm(nimble_constants$nind, 0, 0.1),
    
    # ODBA coefficients
    beta_ODBA_temp = rnorm(5, 0, 0.5),
    beta_ODBA_precip = rnorm(5, 0, 0.5),
    beta_ODBA_temp_precip = rnorm(5, 0, 0.5),
    beta_feed_freeze = c(
      rnorm(1, 0, 0.5),
      runif(4, -0.000001, 0.0000001)
    ),
    
    beta_ODBA_freeze = c(
      rnorm(1, 0, 0.5),
      runif(4, -0.000001, 0.0000001)
    ),
    beta_ODBA_urban = rnorm(5, 0, 0.5),
    beta_ODBA_cropland = rnorm(5, 0, 0.5),
    beta_ODBA_swamp = rnorm(5, 0, 0.5),
    beta_ODBA_marsh = rnorm(5, 0, 0.5),
    beta_ODBA_open_water = rnorm(5, 0, 0.5),
    
    # Feeding coefficients
    beta_feed_temp = rnorm(5, 0, 0.5),
    beta_feed_precip = rnorm(5, 0, 0.5),
    beta_feed_temp_precip = rnorm(5, 0, 0.5),
    beta_feed_urban = rnorm(5, 0, 0.5),
    beta_feed_cropland = rnorm(5, 0, 0.5),
    beta_feed_swamp = rnorm(5, 0, 0.5),
    beta_feed_marsh = rnorm(5, 0, 0.5),
    beta_feed_open_water = rnorm(5, 0, 0.5),
    
    # Nesting propensity
    alpha_global_NP = rlogis(1, 0, 1),
    beta_arrival_breed = rnorm(1, 0, 0.5),
    beta_age = rnorm(1, 0, 0.5),
    eps_year_propensity = rnorm(nimble_constants$nyears, 0, 1),
    
    # Incubation
    alpha_global_NS = rlogis(1, 0, 1),
    beta_migratory = rnorm(1, 0, 0.5), 
    beta_nest_cropland =  rnorm(1, 0, 1),
    beta_nest_peatland =  rnorm(1, 0, 1),
    beta_nest_swamp =  rnorm(1, 0, 1),
    beta_nest_marsh =  rnorm(1, 0, 1),
    beta_lat = rnorm(1, 0, 1), 
    beta_long = rnorm(1, 0, 1), 
    beta_NP = rnorm(1, 0, 1),
    eps_year_nest = rnorm(nimble_constants$nyears, 0, 1),
    
    # Brood rearing
    alpha_global_BS = rlogis(1, 0, 1),
    beta_brood_min_temp =  rnorm(1, 0, 1),
    beta_brood_precip =  rnorm(1, 0, 1),
    beta_brood_temp_precip =  rnorm(1, 0, 1),
    beta_brood_city =  rnorm(1, 0, 1),
    beta_brood_marsh =  rnorm(1, 0, 1),
    beta_brood_open_water =  rnorm(1, 0, 1),
    beta_brood_swamp = rnorm(1, 0, 1),
    beta_NS = rnorm(1, 0, 1),
    eps_year_brood = rnorm(nimble_constants$nyears, 0, 1),
    
    # Hierarchical coefficients
    mu_beta_ODBA = rnorm(1, 0, 1),
    mu_beta_feed = rnorm(1, 0, 1),
    beta_ODBA = rnorm(nimble_constants$n_sub_seasons, 0, 0.5),
    beta_feed = rnorm(nimble_constants$n_sub_seasons, 0, 0.5),
    
    
    # ODBA observation standard deviations
    sd_ODBA_obs = runif(nimble_constants$nyears, 0.1, 2)
  )
}


# data

data <- list(
  
  # a loop (nesting propensity)
  arrival_day_breed = dat_annual$scaled_arrival_day, 
  age = dat_annual$age,
  NP = dat_annual$egg_laying_status,
  
  # b loop (ODBA)
  log_ODBA = dat_odba$log_odba,                  # Response variable vector (length nobs2)
  precip2 = dat_odba$scaled_mean_precip,                    # Precipitation covariate
  temp2 = dat_odba$scaled_min_temp,                        # Temperature covariate
  freeze2 = dat_odba$scaled_freeze_events,                    # Freeze binary/covariate
  urban2 = dat_odba$scaled_prop_urban,                      # Urban habitat use
  cropland2 = dat_odba$scaled_prop_cropland,                # Cropland habitat use
  swamp2 = dat_odba$scaled_prop_swamp,                      # Swamp habitat use
  marsh2 = dat_odba$scaled_prop_marsh,                      # Marsh habitat use
  open_water2 = dat_odba$scaled_prop_open_water,            # Open water habitat use
  
  # c loop (PTF)
  precip3 = dat_feed$scaled_mean_precip,
  temp3 = dat_feed$scaled_min_temp,
  freeze3 = dat_feed$scaled_freeze_events,
  urban3 = dat_feed$scaled_prop_urban,
  cropland3 = dat_feed$scaled_prop_cropland,
  swamp3 = dat_feed$scaled_prop_swamp,
  marsh3 = dat_feed$scaled_prop_marsh,
  open_water3 = dat_feed$scaled_prop_open_water,
  feed = dat_feed$feed_fixes_corrected_season,
  n = dat_feed$total_acc_fixes_corrected_season,
  
  ## d loop (FTI)
  
  NS = dat_nest_success$incubation_status,
  migratory = dat_nest_success$migratory-1,
  nest_lat = dat_nest_success$scaled_lat, 
  nest_long = dat_nest_success$scaled_long,
  nest_cropland = dat_nest_success$scaled_prop_cropland,
  nest_peatland = dat_nest_success$scaled_prop_peatland,
  nest_swamp = dat_nest_success$scaled_prop_swamp,
  nest_marsh = dat_nest_success$scaled_prop_marsh,
  
  ## e loop (brood rearing)
  
  BS = dat_brood_success$brood_status,
  brood_min_temp = dat_brood_success$scaled_min_temp,
  brood_precip = dat_brood_success$scaled_mean_precip, 
  brood_city = dat_brood_success$scaled_city,
  brood_swamp = dat_brood_success$scaled_prop_swamp, 
  brood_marsh = dat_brood_success$scaled_prop_marsh, 
  brood_open_water = dat_brood_success$scaled_prop_open_water
  
)

# run NIMBLE model in series ---------------------------
nc <- 3

# build model
model <- nimbleModel(code = model_code, 
                     constants = nimble_constants,  
                     data =  data, 
                     inits = inits(),
                     buildDerivs = T)


parameters_to_save <- c( 
  # alphas
  # "alpha_ODBA", 
  # "alpha_feed",
  # 
  "alpha_global_NP",
  "alpha_global_NS",
  "alpha_global_BS",
  
  # betas
  "beta_ODBA_temp", 
  "beta_ODBA_precip", 
  "beta_ODBA_freeze",
  "beta_ODBA_temp_precip",
  "beta_ODBA_urban", 
  "beta_ODBA_cropland", 
  "beta_ODBA_swamp", 
  "beta_ODBA_marsh", 
  "beta_ODBA_open_water", 
  "beta_feed_temp",
  "beta_feed_precip",
  "beta_feed_freeze",
  "beta_feed_temp_precip",
  "beta_feed_urban",
  "beta_feed_cropland",
  "beta_feed_swamp",
  "beta_feed_marsh",
  "beta_feed_open_water",
  "beta_arrival_breed", 
  "beta_age", 
  "beta_NP", 
  "beta_migratory",
  "beta_nest_cropland",
  "beta_nest_peatland",
  "beta_lat",
  "beta_long",
  "beta_nest_swamp",
  "beta_nest_marsh", 
  "beta_brood_min_temp", 
  "beta_brood_precip", 
  "beta_brood_temp_precip", 
  "beta_brood_city", 
  "beta_brood_swamp", 
  "beta_brood_marsh", 
  "beta_brood_open_water",
  
  # hierarchical
  "beta_ODBA", 
  "beta_feed",
  "mu_beta_ODBA",
  "mu_beta_feed"
)


# configure MCMC
mcmc_Conf  <- configureMCMC(model, print = F)

mcmc_Conf$removeSamplers(c('beta_ODBA','beta_feed','alpha_global_NP', 'beta_NP', 'alpha_global_NS', 'alpha_global_BS'))
mcmc_Conf$printSamplers()

# mcmc_Conf$addSampler(target = c('beta_ODBA','beta_feed','alpha_global_NP'), # beta odba, beta feed, alpha_global_np
#                 type = 'sampler_AF_slice')
# 
# # correlated within submodels
# mcmc_Conf$addSampler(target = c('beta_NP', 'alpha_global_NS', 'alpha_global_BS'), # beta_NP, alpha_global_NS
#                 type = 'sampler_AF_slice')

# going to them all together since hierarchnically linked

mcmc_Conf$addSampler(target = c('beta_ODBA','beta_feed','alpha_global_NP', 'beta_NP', 'alpha_global_NS', 'alpha_global_BS'), # beta odba, beta feed, alpha_global_np
                     type = 'sampler_AF_slice', 
                     control = list(adaptInterval = 100))


# mcmc_Conf$addSampler(target = c('alpha_global_BS'), # alpha_global_BS
#                 type = 'sampler_slice') # slice

# if it's just one parameter within model, use "slice" ?? potentially

mcmc_Conf$resetMonitors()
mcmc_Conf$addMonitors(parameters_to_save)

# build MCMC
modelMCMC  <- buildMCMC(mcmc_Conf)

# compile model and MCMC
Cmodel     <- compileNimble(model)
CmodelMCMC <- compileNimble(modelMCMC, project = model)

# AF slice sampler in NIMBLE
# use with correlated parameters


samples <- runMCMC(CmodelMCMC,
                   niter = 300000,
                   nburnin = 60000,
                   thin = 15,
                   nchains = nc,
                   inits = inits,
                   samplesAsCodaMCMC = T) # started at 9:47

save(samples, file = "results/full_fac_26OCT2026_300000_updatedsampler_adapt_alltogether.RData")

load("results/full_fac_21OCT2026_100000.RData")

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

#MCMCplot(samples)

MCMCtrace(mcmc_list, type = "trace",
          filename = paste0("results/traceplots/full_fac_26OCT2026_300000_adapt_updatedsampler_alltogether.pdf"))

# Calculate tail probabilities
# See proportion of samples above or below 0 (above for negative effects, below for positive effects)
prop_above_or_below <- function(x) {
  if(median(x) > 0) {return(mean(x < 0))}
  else {return(mean(x > 0))}
}

# calculate summary statistics
sum_stats <- MCMCsummary(mcmc_list, func = function(x) prop_above_or_below(x), func_name = "prop") %>%
  tibble::rownames_to_column(var = "parameter")

write.csv(sum_stats, "results/summarystats_fac_26Oct2026_fullrun.csv")


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

write.csv(samples_long, "results/posteriors_fac_26Oct2026_fullrun.csv")
# 
# nest <- samples_long %>%
# filter(parameter %in% c("beta_lat", "beta_long", "beta_migratory", "beta_nest_cropland", "beta_nest_marsh", "beta_nest_peatland", "beta_nest_swamp"))
# 
# early <- samples_long %>%
#   filter(parameter %in% c("beta_age", "beta_arrival_breed"))
# 
# ggplot(nest, aes(x = parameter, y = posterior)) +  
#   geom_violin(
#     position = position_dodge(width = 0.7),
#     scale = "width",   # Ensures all violins scale uniformly so they look neat
#     trim = FALSE,      # Prevents cutting off the tails of the distribution
#     linewidth = 0.5,
#     alpha = 0.6
#   ) +
#   theme_minimal() +
#   geom_hline(yintercept = 0)
#   
# 
# odba_habitat <- samples_long %>%
#   extract(
#     parameter, 
#     into = c("model", "habitat", "season"), 
#     regex = "^beta_([^_]+)_(.*?)\\[([0-9]+)\\]$") %>%
#   filter(habitat %in% c("cropland", "urban", "marsh", "swamp", "open_water"), 
#          model == "feed") %>%
#   mutate(season = as.factor(season))
# 
# odba_weather <- samples_long %>%
#   extract(
#     parameter, 
#     into = c("model", "weather", "season"), 
#     regex = "^beta_([^_]+)_(.*?)\\[([0-9]+)\\]$") %>%
#   filter(weather %in% c("freeze", "precip", "temp"), 
#          model == "feed") %>%
#   mutate(season = as.factor(season))
# 
# 
# ggplot(odba_weather, aes(x = season, y = posterior, fill = weather, color = weather)) +  
#   geom_violin(
#     position = position_dodge(width = 0.7),
#     scale = "width",   # Ensures all violins scale uniformly so they look neat
#     trim = FALSE,      # Prevents cutting off the tails of the distribution
#     linewidth = 0.5,
#     alpha = 0.6
#   ) +
#   theme_minimal() +
#   geom_hline(yintercept = 0)
