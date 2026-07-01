#### Reducing to one nesting attempt per bird-year -- Code for Cassidy
# Author: Ilsa Griebel
# Date: 29 May 2026
# Taken from Rscript for compiling all variables for abdu full annual cycle model
# 

#### adding nest dates ####

# new file with dates for ALL attempts
nest_dates <- tbl(conn, "nest_dates_and_locations_for_all_attempts_30Jun2026") %>%
  collect()

# add nest duration column
nest_dates$nest_duration  <- nest_dates$nest_end - nest_dates$nest_start

# number of bird-years with assigned dates 
n1 <- unique(nest_dates$birdid_year) 

# number of bird-years with more than one attempt
sum <- nest_dates %>% group_by(birdid_year) %>%
  summarize(n = n())

sum %>% group_by(n) %>% count() 

# checking just birds classified as hatched; first filter to only hatched birds
nest_dates_hatched <- nest_dates %>% filter(status_incubation == "hatched")

# check number of bird-years with more than one attempt with dates
sum_hatched <- nest_dates_hatched %>% group_by(birdid_year) %>%
  summarize(n = n())

sum_hatched %>% group_by(n) %>% count() 

# if multiple attempts, keep the one that has attempt status == hatched (which is the one that overlaps with incubation window range (and if two do, use one that is visited the most.. but none have two that overlap, so left out!) and then recheck how many dates overlap
nest_dates_hatched_oneattempt <- nest_dates_hatched %>%
  group_by(birdid_year) %>%
  filter(attempt_status == "successful") %>%
  ungroup()

n_hatched <- unique(nest_dates_hatched_oneattempt$birdid_year) 

# check number of bird-years with more than one attempt with dates
sum_hatched <- nest_dates_hatched_oneattempt %>% group_by(birdid_year) %>%
  summarize(n = n())

sum_hatched %>% group_by(n) %>% count() # 0 bird-years with more than one attempt

# keep one attempt per bird version 
nest_dates_hatched <- nest_dates_hatched_oneattempt

# checking just birds classified as NOT hatched; first filter to only not hatched birds
nest_dates_failed <- nest_dates %>% filter(status_incubation != "hatched" | is.na(status_incubation) == TRUE)

n_failed <- unique(nest_dates_failed$birdid_year) # 428 bird years

# check number of bird-years with more than one attempt with dates
sum_failed <- nest_dates_failed %>% group_by(birdid_year) %>%
  summarize(n = n())

sum_failed %>% group_by(n) %>% count() # 135 bird-years with more than one attempt

sum_failed_multiple_attempts <- sum_failed %>% filter(n > 1)

# if multiple attempts, keep recurse/ruleset over window method and if still more than one attempt, keep longest attempt
nest_dates_failed_recurse_only <- nest_dates_failed %>%
  filter(date_assignment_method=="recurse")

n_recurse <- unique(nest_dates_failed_recurse_only$birdid_year) 

# check number of bird-years with more than one attempt with dates
sum_failed <- nest_dates_failed_recurse_only %>% group_by(birdid_year) %>%
  summarize(n = n())

sum_failed %>% group_by(n) %>% count() 

# keep only orginal mutiple attempt birds to see how many birds were reduced to one attempt by using only recurse dates
sum_failed <- sum_failed %>% filter(birdid_year %in% sum_failed_multiple_attempts$birdid_year)

sum_failed %>% group_by(n) %>% count() #

# if multiple attempts, keep recurse/ruleset over window method and if still more than one attempt, keep longest attempt
nest_dates_failed_recurse_only <- nest_dates_failed_recurse_only %>%
  group_by(birdid_year) %>%
  filter(nest_duration==max(nest_duration)) 

# check number of bird-years with more than one attempt with dates
sum_failed <- nest_dates_failed_recurse_only %>% group_by(birdid_year) %>%
  summarize(n = n())

sum_failed %>% group_by(n) %>% count() 

# if still more than one attempt, keep first attempt
nest_dates_failed_recurse_only <- nest_dates_failed_recurse_only %>%
  group_by(birdid_year) %>%
  filter(nest_start==min(nest_start)) 

# check number of bird-years with more than one attempt with dates
sum_failed <- nest_dates_failed_recurse_only %>% group_by(birdid_year) %>%
  summarize(n = n())

sum_failed %>% group_by(n) %>% count() 

n_recurse <- unique(nest_dates_failed_recurse_only$birdid_year) 

# get birds that didn't have any attempts using recurse method (i.e. dates only set by window method)
nest_dates_failed_window_only <- nest_dates_failed %>%
  filter(!(birdid_year %in% nest_dates_failed_recurse_only$birdid_year))

n_window <- unique(nest_dates_failed_window_only$birdid_year) 

# check number of bird-years with more than one attempt with dates
sum_failed <- nest_dates_failed_window_only %>% group_by(birdid_year) %>%
  summarize(n = n())

sum_failed %>% group_by(n) %>% count() # 24 bird-years with more than one attempt once reduced to window dates only

# if multiple attempts, keep longest attempt
nest_dates_failed_window_only <- nest_dates_failed_window_only %>%
  group_by(birdid_year) %>%
  filter(nest_duration==max(nest_duration)) 

# check number of bird-years with more than one attempt with dates
sum_failed <- nest_dates_failed_window_only %>% group_by(birdid_year) %>%
  summarize(n = n())

sum_failed %>% group_by(n) %>% count() 

# if still more than one attempt, keep first attempt
nest_dates_failed_window_only <- nest_dates_failed_window_only %>%
  group_by(birdid_year) %>%
  filter(nest_start==min(nest_start)) 

# check number of bird-years with more than one attempt with dates
sum_failed <- nest_dates_failed_window_only %>% group_by(birdid_year) %>%
  summarize(n = n())

sum_failed %>% group_by(n) %>% count()

n_window <- unique(nest_dates_failed_window_only$birdid_year) #

# combine all types of attempt outcomes back together, now that all reduced to one attempt per bird
#nest_dates2 <- rbind(nest_dates_defer, nest_dates_failed, nest_dates_hatched) # went from 505 attempts with dates to 394 attempts (number of bird-years with dates, although now some have NA for dates and will replace with window dates)
nest_dates2 <- rbind(nest_dates_failed_recurse_only, nest_dates_failed_window_only, nest_dates_hatched)

#### adding brood dates ####

## add brood status
# load classified brood data
broodrearing <- tbl(conn, "mall_broodrearing_classified_IncompleteDataBirdIncluded_30Jun26") %>%
  collect()

# select only columns you need
broodrearing <- broodrearing %>% dplyr::select(birdid_year, status_early, status_late, status_early_late)

# merge with list of nested birds
nested_birds4 <- merge(nest_dates2, broodrearing, by = "birdid_year", all.x = TRUE)

## setting brood dates
nested_birds5 <- nested_birds4 %>%
  # set brood start date as day of nest_end for all birds that hatched a nest
  mutate(brood_start=ifelse(is.na(status_early_late), NA, nest_end)) %>%
  mutate(brood_start=as.Date(brood_start)) %>%
  # if failed early, set brood rearing dates as 1 to 15 days post-hatch;
  # otherwise, set brood rearing dates as 1 to 30 days post-hatch for failed late and fledged
  mutate(brood_end=ifelse(status_early == "no_brood", brood_start+15, brood_start+30)) %>%
  mutate(brood_end=as.Date(brood_end)) 

# add band_year column
nested_birds5$band_year <- gsub("-", "", nested_birds5$birdid_year)

# reduce to necessary columns
nested_birds6 <- nested_birds5 %>% dplyr::select(band_year, birdid_year, nest_start, nest_end, brood_start, brood_end, nest_lat, nest_long, attempt_new)

# save to database
# dbRemoveTable(conn, "nest_and_brood_dates")
dbWriteTable(conn, "nest_and_brood_dates_30Jun26", nested_birds6, append = TRUE, row.names = FALSE)

