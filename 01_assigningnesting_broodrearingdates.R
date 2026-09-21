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

nesting_birds <- tbl(conn, "nest_and_brood_dates_6Jul26") %>%
  collect() %>%
  distinct()

nesting_birds

# bringing in reproductive status and additional dates as needed

nesting_birds_all_attempts <- tbl(conn, "nest_dates_and_locations_for_all_attempts_6Jul26") %>%
  collect() %>%
  dplyr::select(birdid_year, nest_start, nest_end, lay_start, lay_end, inc_start, inc_end, status_comb, attempt_status, date_assignment_method, nest_location_method) 


test <- nesting_birds_all_attempts %>%
  mutate(days_inc = nest_end - nest_start)

nesting_birds_dates <- nesting_birds %>%
  left_join(nesting_birds_all_attempts, by = c("birdid_year", "nest_start", "nest_end"))


# #------------------------------------------------------------------------------#
# ###------ creating incubation dataset
# # #------------------------------------------------------------------------------#

incubation_dataset <- nesting_birds_dates %>%
  
  # getting rid of egg_laying_defer (because these birds were nonincubating) and egg_laying_NA (we don't know the status of these guys)
  # basically just say it incubated
  
  filter(status_comb %in% c("egg_laying_failed", "egg_laying_hatched")) %>%
  
  # egg_laying_failed and egg_laying_NA are failed incubation 
  
  mutate(incubation_status = ifelse(status_comb == "egg_laying_hatched", "success", "failed")) %>%
  
  # for successful birds, crank back 26 days
  
  mutate(nest_start_updated = as.Date(case_when(incubation_status == "success" ~ nest_end - 26, 
                                                
                                                # they don't have inc_start if they were never within > 50% of the nest (so use nesting dates)
                                                
                                                incubation_status == "failed" & is.na(inc_start) ~ nest_start, 
                                                incubation_status == "failed" & !is.na(inc_start) ~ inc_start, 
                                                TRUE ~ inc_start))) %>%
  
  dplyr::select(birdid_year,band_year, incubation_status, date_assignment_method, nest_location_method, inc_start = nest_start_updated, inc_end = nest_end, nest_lat, nest_long) %>%
  
  # how many days long
  
  mutate(days_inc = inc_end - inc_start, 
         day_greater_26 = ifelse(incubation_status == "failed" & days_inc > 26, "Yes", "No")) %>%
  
  # for failed birds with days longer than 26, I'm only going to take the first 26
  
  mutate(inc_end = as.Date(ifelse(day_greater_26 == "Yes", inc_start + 26, inc_end)))

### check number of birds per category

nesting_birds_dates %>%
  filter(status_comb %in% c("egg_laying_failed", "egg_laying_hatched")) %>%
  mutate(
    incubation_status = ifelse(status_comb == "egg_laying_hatched",
                               "success", "failed"),
    start_date_rule = case_when(
      incubation_status == "success" ~ "Success: nest_end - 26 days",
      incubation_status == "failed" & is.na(inc_start) ~ "Failed: used nest_start",
      incubation_status == "failed" & !is.na(inc_start) ~ "Failed: used inc_start"
    )
  ) %>%
  count(start_date_rule)


write.csv(incubation_dataset, "results/incubation_dataset_successful_failed_dates_17Aug26_FINAL.csv")

#------------------------------------------------------------------------------#
##### creating nesting (egglaying AND incubation) dataset
#------------------------------------------------------------------------------#

# taking the 1 attempt per individual and joining it with status and extra dates 

dates_and_status <- nesting_birds %>%
  left_join(egg_laying_incubation_status, by = c("birdid_year", "attempt_new")) 
# 
# #### for Mitch - percentage that you'd have to throw out
# 
# dates_and_status %>%
#   group_by(status_comb, date_assignment_method) %>%
#   summarize(count_method = n())
# 
# # egg-laying defer would obviously be fine because if they didn't attempt incubation, then the dates wouldn't be in that
# # all 332 egg_laying defer would be good
# 
# # egg_laying_failed has 49/371 birds that were using the window method (13%)
# 
# dates_and_status %>%
# filter(status_comb == "egg_laying_failed", 
#        date_assignment_method == "recurse") %>%
# mutate(na = ifelse(is.na(inc_end) == TRUE, "yes", "no")) %>%
# group_by(na) %>%
# summarize(count_method = n())
# 
# # 92 / 322 = 29% of birds classified with the recurse function also cannot be split up between egg laying and incubation
# 
# # egg_laying_hatched has 13/153 birds that were using window method (8%)
# 
# dates_and_status %>%
#   filter(status_comb == "egg_laying_hatched", 
#          date_assignment_method == "recurse") %>%
#   mutate(na = ifelse(is.na(lay_start) == TRUE, "yes", "no")) %>%
#   group_by(na) %>%
#   summarize(count_method = n())
# 
# # 31 / 141 of the birds using the recurse method didn't have a lay start...22% 
# 
# # so, all egg_laying_defered could be kept in, 42% of egg_laying_failed would be removed, and 30% of egg_laying_hatched would be removed

# 
# nest_data <- dates_and_status %>%
#   mutate(status_nesting = ifelse(status_incubation == "hatched", "hatched", "failed"), 
#          status_nesting = ifelse(is.na(status_nesting) == TRUE, "failed", status_nesting)) %>%
#   dplyr::select(band_year:nest_end, nest_lat, nest_long, status_nesting) 
# 
# table(nest_data$status_nesting)
# 
# write.csv(nest_data, "results/all_NESTING_successful_failed_6Jul26.csv")

# 
# #------------------------------------------------------------------------------#
# ##### creating egg laying dataset
# #------------------------------------------------------------------------------#
# 
# egg_layingdataset <- dates_and_status %>%
#   
#   # sometimes the model just classified egg laying and not incubation. cannot tell if these are successful or failed incubation 
#   
#   filter(!is.na(status_incubation)) %>%
#   
#   # assigning laying dates to be the same as incubation start date if needed
#   
#   mutate(
#     lay_end = if_else(
#       is.na(lay_end),
#       inc_start,
#       lay_end
#     ),
#     
#     # if the bird is egg laying defer, that means their egg laying attempt was a fail
#     # if the bird was egg_laying_failed or egg_laying_hatched, it had to have had a successful egg laying 
#     
#     egg_laying_status = case_when(
#       status_comb == "egg_laying_defer" ~ "failed", 
#       TRUE ~ "successful"),
#     
#     # using the nesting dates from the nesting_birds as dates for egg laying (because it just steals the obvious egg laying dates)
#     # but, for failed + successful incubation, the nesting_birds data set includes egg laying and incubation (takes start of egg laying and end of incubation) so I need to pull from the recurse function (hence why we can't use window)
#     
#     egg_laying_start = if_else(egg_laying_status == "failed", nest_start, lay_start ), 
#     egg_laying_end = if_else(egg_laying_status == "failed", nest_end, lay_end),
#     
#     # add 10 days for some birds that don't have a lay start
#     
#     egg_laying_start = if_else(is.na(egg_laying_start), lay_end - 10, egg_laying_start))
# 
# write.csv(egg_layingdataset, "results/all_egg_laying_successful_failed_TESTRUN.csv")


#------------------------------------------------------------------------------#
##### creating brood rearing
#------------------------------------------------------------------------------#

brooding_birds <- nesting_birds %>%
  filter(is.na(brood_start) == FALSE) %>%
  dplyr::select(band_year, birdid_year, brood_start, brood_end)

brood_status <- tbl(conn, "mall_broodrearing_classified_IncompleteDataBirdIncluded_6Jul26") %>%
  collect() %>%
  dplyr::select(birdid_year, status_early_late) %>%
  mutate(brood_reaing_status = ifelse(status_early_late == "successful_brood_early_successful_brood_late", "success", "failed"))

brood_status <- brooding_birds %>%
  left_join(brood_status, by = "birdid_year") 

write.csv(brood_status, "results/all_broodrearing_successful_failed_15Jul26.csv")

#------------------------------------------------------------------------------#
##### all incubation attempts for Daria 7/22/2026
#------------------------------------------------------------------------------#

inc_daria <- tbl(conn, "nest_dates_and_locations_for_all_attempts_6Jul26") %>%
  collect() %>% 
  filter(status_incubation != "defer", 
         date_assignment_method != "window", 
         is.na(inc_start) == FALSE) %>%
  dplyr::select(birdid_year, inc_start, inc_end, nest_lat, nest_long, attempt_status, attempt_new, status_incubation)

write.csv(inc_daria, "results/incubation_dates_Daria_22Jul26.csv")

