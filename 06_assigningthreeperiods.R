# preparing egg-laying, nesting, and brood rearing

#load libraries

library(tidyverse)
library(RPostgres)
library(lubridate)

# connect to database
conn <- dbConnect(
  Postgres(),
  dbname = "acc_gps_data_mallards",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "mallard_ducks"
)

# reduced 1 attempt per individual 

nesting_birds <- tbl(conn, "nest_and_brood_dates_ILSA") %>%
  collect()

nesting_birds

# bringing in reproductive status and additional dates as needed

egg_laying_incubation_status <- tbl(conn, "nest_dates_and_locations_for_all_attempts_ILSA") %>%
  collect() %>%
  select(
    birdid_year, status_comb, status_laying, status_incubation,
    attempt_new, date_assignment_method  ) 

egg_laying_incubation_status %>%
  group_by(status_comb, date_assignment_method) %>%
  summarize(count_method = n())


#------------------------------------------------------------------------------#
##### creating nesting (egglaying AND incubation) dataset
#------------------------------------------------------------------------------#

# taking the 1 attempt per individual and joining it with status and extra dates 

dates_and_status <- nesting_birds %>%
  left_join(egg_laying_incubation_status, by = c("birdid_year", "attempt_new"))  %>%
  
  # sometimes the model just classified egg laying and not incubation. cannot tell if these are successful or failed incubation 
  
  filter(!is.na(status_incubation))

#### for Mitch - percentage that you'd have to throw out

dates_and_status %>%
  group_by(status_comb, date_assignment_method) %>%
  summarize(count_method = n())

# egg-laying defer would obviously be fine because if they didn't attempt incubation, then the dates wouldn't be in that
# all 332 egg_laying defer would be good

# egg_laying_failed has 49/371 birds that were using the window method (13%)

dates_and_status %>%
filter(status_comb == "egg_laying_failed", 
       date_assignment_method == "recurse") %>%
mutate(na = ifelse(is.na(inc_end) == TRUE, "yes", "no")) %>%
group_by(na) %>%
summarize(count_method = n())

# 92 / 322 = 29% of birds classified with the recurse function also cannot be split up between egg laying and incubation

# egg_laying_hatched has 13/153 birds that were using window method (8%)

dates_and_status %>%
  filter(status_comb == "egg_laying_hatched", 
         date_assignment_method == "recurse") %>%
  mutate(na = ifelse(is.na(lay_start) == TRUE, "yes", "no")) %>%
  group_by(na) %>%
  summarize(count_method = n())

# 31 / 141 of the birds using the recurse method didn't have a lay start...22% 

# so, all egg_laying_defered could be kept in, 42% of egg_laying_failed would be removed, and 30% of egg_laying_hatched would be removed

# with that being said, let's just create a NESTING data set here.

nest_data <- dates_and_status %>%
  mutate(status_nesting = ifelse(status_incubation == "hatched", "hatched", "failed")) %>%
  dplyr::select(band_year:nest_end, nest_lat, nest_long, status_nesting) 

table(nest_data$status_nesting)

write.csv(nest_data, "results/all_NESTING_successful_failed_take2.csv")

test <- read_csv("results/all_NESTING_successful_failed.csv")

table(test$status_nesting)


#------------------------------------------------------------------------------#
##### creating egg laying dataset
#------------------------------------------------------------------------------#

egg_layingdataset <- dates_and_status %>%
  
  # sometimes the model just classified egg laying and not incubation. cannot tell if these are successful or failed incubation 
  
  filter(!is.na(status_incubation)) %>%
  
  # assigning laying dates to be the same as incubation start date if needed
  
  mutate(
    lay_end = if_else(
      is.na(lay_end),
      inc_start,
      lay_end
    ),
    
    # if the bird is egg laying defer, that means their egg laying attempt was a fail
    # if the bird was egg_laying_failed or egg_laying_hatched, it had to have had a successful egg laying 
    
    egg_laying_status = case_when(
      status_comb == "egg_laying_defer" ~ "failed", 
      TRUE ~ "successful"),
    
    # using the nesting dates from the nesting_birds as dates for egg laying (because it just steals the obvious egg laying dates)
    # but, for failed + successful incubation, the nesting_birds data set includes egg laying and incubation (takes start of egg laying and end of incubation) so I need to pull from the recurse function (hence why we can't use window)
    
    egg_laying_start = if_else(egg_laying_status == "failed", nest_start, lay_start ), 
    egg_laying_end = if_else(egg_laying_status == "failed", nest_end, lay_end),
    
    # add 10 days for some birds that don't have a lay start
    
    egg_laying_start = if_else(is.na(egg_laying_start), lay_end - 10, egg_laying_start))

write.csv(egg_layingdataset, "results/all_egg_laying_successful_failed_TESTRUN.csv")


### need to figure out incubation once I talk with Ilsa...some of them you can't parse out either

# for now, I can play around with the above data set for working on code
#   
#     
#   incubation_status = case_when(
#     status_comb == "egg_laying_failed" ~ "failed", 
#     status_comb == "egg_laying_hatched" ~ "hatched", 
#     TRUE ~ "no_incubation"), 
#   
#     # if inc_start is NA, use nesting
#   
#   incubation_start = inc_start, 
#   incubation_end = inc_end)
# %>%
#   select(birdid_year, egg_laying_start, egg_laying_end, status, nest_lat, nest_long)
# 
# #------------------------------------------------------------------------------#
# ###------ creating incubation dataset
# #------------------------------------------------------------------------------#
# 
incubation_dataset <- dates_and_status %>%
  filter(status_comb %in% c("egg_laying_failed", "egg_laying_hatched"), 
         
         # this definetly isn't what i want to do but just doing for now for practice 
         is.na(inc_start) == FALSE) %>%
  mutate(incubation_status = ifelse(status_comb == "egg_laying_failed", "failed", "success")) %>%
  dplyr::select(birdid_year,band_year, incubation_status,  inc_start, inc_end)

write.csv(incubation_dataset, "results/all_incubation_dataset_successful_failed_TESTRUN.csv")


#------------------------------------------------------------------------------#
##### creating brood rearing
#------------------------------------------------------------------------------#

brooding_birds <- nesting_birds %>%
  filter(is.na(brood_start) == FALSE) %>%
  dplyr::select(band_year, birdid_year, brood_start, brood_end)

brood_status <- tbl(conn, "mall_broodrearing_classified_IncompleteDataBirdsIncluded") %>%
  collect() %>%
  dplyr::select(birdid_year, status_early_late) %>%
  mutate(brood_reaing_status = ifelse(status_early_late == "successful_brood_early_successful_brood_late", "success", "failed"))

brood_status <- brooding_birds %>%
  left_join(brood_status, by = "birdid_year") 

write.csv(brood_status, "results/all_broodrearing_successful_failed_TESTRUN.csv")

