######################## 00_season_delination.R ##################

# Code: Taking GPS points / date and assigning season (migration, winter, summer) for FAC model
# Date: 18 Sep 2026
# Author: Cassidy Waldrep

# Description: 

# season delineations for FAC with data from 2022 - sep 15 2026

# Load packages and functions

library(tidyverse)
library(RPostgres)
library(geosphere)
library(sf)
library(adehabitatLT)
library(tmap)
library(dplyr)
library(gridExtra)
library(grid)
library(RColorBrewer)
library(patchwork)
library(data.table)

#-------------------------------------------------------------------------#
###### connect to database and read in GPS data ###########################
#-------------------------------------------------------------------------#

conn <- dbConnect(
  Postgres(),
  dbname = "acc_gps_data_mallards",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "mallard_ducks"
)


all_gps_data <- tbl(conn, "gps_data_2022_sep2026_hdop10_200speed_filtered") %>%
  dplyr::select(device_id, UTC_datetime, UTC_date, Latitude, Longitude, bandnum, hdop) %>%
  filter(hdop < 10, 
         
         # removing this bird because it has like 10 days and can't assign very well. It may have been filtered out later anyways 
         
         !(bandnum == "213720084" & year(UTC_date) == 2025),
         
         # removing this bird and year because has a huge gap in the spring that isn't picked up
         
         !(bandnum == "222779074" & year(UTC_date) == 2023)) %>%
  
  mutate(year = year(UTC_date)) %>%
  mutate(birdid_year = paste(bandnum, year, sep = "_")) %>%
  collect() 

### Check for birds with large gaps as we can't accurately assess migration dates

seasonal_gaps <- all_gps_data %>%
  arrange(bandnum, year, UTC_date) %>%
  group_by(bandnum, year) %>%
  mutate(
    date_diff = as.numeric(UTC_date - lag(UTC_date)),
    month = month(UTC_date),
    prev_month = month(lag(UTC_date)),
    
    # Check if both dates are in spring or both in fall
    is_spring_gap = (month %in% 3:6) & (prev_month %in% 3:6),
    is_fall_gap   = (month %in% 9:11) & (prev_month %in% 9:11)
  ) %>%
  ungroup() %>%
  filter(
    (is_spring_gap | is_fall_gap) & date_diff >= 10
  ) 

length(unique(seasonal_gaps$birdid_year)) # 62 and then add the two from above, so 41 in total

# Get the birds to exclude
#for time activity budget manuscript, I accidentally filtered out all band numbers, not just the birdids...this moved my data set from 1593 birds to 1639...might want to change in review

bad_birds_birdidyear <- unique(seasonal_gaps$birdid_year)
# bad_birds_bandnum <- unique(seasonal_gaps$bandnum)

all_gps_data <- all_gps_data %>%
  filter(!birdid_year %in% bad_birds_birdidyear) 
# # this was removing the whole bird, not just the bad year
# all_gps_data<- all_gps_data %>%
#   filter(!bandnum %in% bad_birds_bandnum)

#-------------------------------------------------------------------------#
###### CALCULATE HOURLY DISPLACEMENT MOVED AND MOLT ##################
#-------------------------------------------------------------------------#

# computes hourly displacement to visualize migration thresholds
# Then combines hourly displacement to calculate daily distance 

# for visualizing migration threshold we only want hourly intervals

threshold_hours_hourly <- 1

test <- all_gps_data %>%
  arrange(bandnum, UTC_datetime) %>%
  group_by(bandnum) %>%
  
  # lag times are the hour before 
  
  mutate(
    lag_lon = lag(Longitude),
    lag_lat = lag(Latitude),
    
    # time since lag time is also calculated
    
    dt_hours = as.numeric(difftime(UTC_datetime, lag(UTC_datetime), units = "hours")),
    
    # only gaps of 1 hours or less are calculated via distHaversine (distance between two points)
    
    step_m = if_else(
      !is.na(dt_hours) & dt_hours <= threshold_hours_hourly,
      distHaversine(cbind(lag_lon, lag_lat),
                    cbind(Longitude, Latitude)),
      NA_real_), 
    step_km = step_m / 1000, 
    log_step_km = log(step_km),
    month = month(UTC_date))

distanceplot <- ggplot(test) +
  geom_density(aes(x = log_step_km), 
               linewidth = 1) +
  geom_vline(xintercept = log(c(0.4, 35)), 
             linetype = "dashed", 
             linewidth = 1,
             color = "darkblue") +
  scale_x_continuous(
    breaks = log(c(0.01, 0.4, 1, 10, 35, 100)),
    labels = c("0.01", "0.4", "1", "10", "35", "100"),
    name = "Step length (km, log scale)"
  ) +
  labs(y = "Density") +
  ggpubr::theme_pubr() +
  theme(axis.line = element_line(linewidth = 1), 
        axis.title = element_text(size = 13))

distanceplot

# ggsave("figures/publication figures/suppmat_figure1.png", 
#        plot = distanceplot, 
#        width = 7, 
#        height = 6, 
#        dpi = 600)

# calculating lagged distances of 1 hours to look at molt

bird_lag_distance <- all_gps_data %>%
  arrange(bandnum, UTC_datetime) %>%
  mutate(year = year(UTC_date)) %>%
  
  # Add row number within each bird-year group
  group_by(bandnum, year) %>%
  mutate(row_num = row_number()) %>%
  
  # Calculate lagged distance
  mutate(
    lag_lon = lag(Longitude),
    lag_lat = lag(Latitude),
    dt_hours = as.numeric(difftime(UTC_datetime, lag(UTC_datetime), units = "hours")),
    step_m = if_else(
      !is.na(dt_hours) & dt_hours <= 1.1,
      distHaversine(cbind(lag_lon, lag_lat), cbind(Longitude, Latitude)),
      NA_real_),
    step_km = step_m / 1000,
    log_step_km = log(step_km),
    month = month(UTC_date)
  ) %>%
  ungroup() %>%
  group_by(bandnum, year, month, UTC_date) %>%
  summarize(mean_step_km = mean(step_km, na.rm = TRUE), .groups = "drop") %>%
  filter(
    (month(UTC_date) %in% 8:10) |                      # August, September, October
      (month(UTC_date) == 11 & day(UTC_date) <= 1)       # November 1
  )

# Flag days below threshold for molting

df <- bird_lag_distance %>%
  group_by(bandnum, year) %>%
  arrange(UTC_date) %>%
  mutate(below_threshold = mean_step_km <= 0.4, 
         
         # Step 2: Identify contiguous "runs"
         
         run_id = rleid(below_threshold))

# Filter for potential molting times (happens with about 500 of the birds)

runs_df <- df %>%
  group_by(bandnum, year, run_id) %>%
  filter(all(below_threshold)) %>%
  summarise(
    start_date = min(UTC_date),
    end_date = max(UTC_date),
    duration = as.numeric(end_date - start_date + 1),
    .groups = "drop"
  ) %>%
  filter(duration >= 20, duration <= 60) %>%
  group_by(bandnum, year) %>%
  slice_min(start_date, with_ties = FALSE) %>%
  ungroup()

saveRDS(runs_df, file = "runs_df_2022_sep2026.RDS")

df %>%
  group_by(bandnum, year, run_id) %>%
  filter(all(below_threshold)) %>%
  summarise(
    start_date = min(UTC_date),
    end_date = max(UTC_date),
    duration = as.numeric(end_date - start_date + 1),
    .groups = "drop"
  ) %>%
  group_by(bandnum, year) %>%
  slice_min(start_date, with_ties = FALSE) %>%
  ungroup()


#-------------------------------------------------------------------------#
###### CALCULATE DAILY DISPLACEMENT AND ASSIGN MIGRATORY STATUS ############
#-------------------------------------------------------------------------#

# if threshold is greater than 72 hours, step length wont be calculated between current and lag. This is to prevent huge step lengths that could occur during migration but without check in

## this is showing that 35 km is probably around what the migration threshold would be
# also coupled with beatty et al., 2014

threshold_hours <- 72

daily_displacement <- all_gps_data %>%
  arrange(bandnum, UTC_datetime) %>%
  group_by(bandnum, UTC_date) %>%
  
  # this takes the first time for each day for each bird 
  
  slice_min(UTC_datetime, n = 1) %>%
  ungroup() %>%
  
  # for each bird, line up today’s first fix with yesterday’s
  
  arrange(bandnum, UTC_date) %>%
  group_by(bandnum) %>%
  mutate(
    prev_lon = lag(Longitude),
    prev_lat = lag(Latitude), 
    prev_time     = lag(UTC_datetime),
    dt_prev_hours = as.numeric(
      difftime(UTC_datetime, prev_time, units = "hours")),
    
    # only calculates displacement for daily intervals...if more than daily, no displacement
    
    daily_displacement_m = if_else(
      dt_prev_hours <= threshold_hours,
      distHaversine(
        cbind(prev_lon, prev_lat),
        cbind(Longitude, Latitude)
      ),
      NA_real_
    ),
    daily_displacement_km = daily_displacement_m / 1000
  ) 

# in case we need a daily displacement plot...we can see it looks the same as hourly. 

distanceplot <- ggplot(daily_displacement) +
  geom_density(aes(x = log(daily_displacement_km)), 
               linewidth = 1) +
  geom_vline(xintercept = log(c(0.4, 35)), 
             linetype = "dashed", 
             linewidth = 1,
             color = "darkblue") +
  scale_x_continuous(
    breaks = log(c(0.01, 0.4, 1, 10, 35, 100)),
    labels = c("0.01", "0.4", "1", "10", "35", "100"),
    name = "Step length (km, log scale)"
  ) +
  labs(y = "Density") +
  ggpubr::theme_pubr() +
  theme(axis.line = element_line(linewidth = 1), 
        axis.title = element_text(size = 13))

# creates a ruleset that > 35 km is a "migratory movement"
# but, only birds that make a migratory movement AND have displacement in July are considered migratory birds

total_distance_measurements <- daily_displacement %>%
  mutate(
    julian_date = yday(UTC_date),
    more_than_35 = daily_displacement_km >= 35,
    year = year(UTC_date)
  ) %>%
  group_by(bandnum, year) %>%
  arrange(UTC_date) %>%  # ensure it's sorted
  mutate(
    last_obs    = max(UTC_date),
    cutoff_date = make_date(year, 4, 25), # need to update in time activity supp mat
    moved_over_35 = any(daily_displacement_km >= 35, na.rm = TRUE),
    
    # Jan 1 to July 1
    
    mid_date = make_date(year, 7, 1),
    mid_idx = which.min(abs(as.numeric(UTC_date - mid_date))),
    
    # Find second date
    
    second_lon = Longitude[2],
    second_lat = Latitude[2],
    mid_lon    = Longitude[mid_idx],
    mid_lat    = Latitude[mid_idx],
    
    # molt migration for residents?
    
    moved_late_summer = any(
      daily_displacement_km >= 35 & month(UTC_date) %in% 6:8,
      na.rm = TRUE),
    
    net_disp_mid = distHaversine(
      cbind(second_lon, second_lat),
      cbind(mid_lon, mid_lat)
    ) / 1000,
    
    # Movement of 35km+ before mid_date (e.g., potential spring migration)
    moved_early_season = any(
      daily_displacement_km >= 35 & UTC_date < mid_date,
      na.rm = TRUE),
    
    moved_over_35_winter = any(
      daily_displacement_km >= 35 & month(UTC_date) %in% c(1, 2),
      na.rm = TRUE),
    status = case_when(
      moved_over_35 & moved_early_season == TRUE & net_disp_mid >= 75    ~ "migratory",
      !moved_over_35 & last_obs < cutoff_date  ~ "unknown",
      moved_over_35_winter == TRUE & last_obs < cutoff_date  ~ "unknown", # birds that have winter movements but nothing else
      moved_over_35 & net_disp_mid < 75 & moved_late_summer ~ "partial_migrant", # grabbing the partial migrants that molt migrate, will figure out
      !moved_over_35 & net_disp_mid < 75      ~ "resident",
      moved_over_35 & net_disp_mid < 75       ~ "resident",
      TRUE                                     ~ "migratory" # no one actually gets assigned this
    )
  ) %>%
  ungroup() 

#### shows each band number, year, and status

status_table <- total_distance_measurements %>%
  distinct(bandnum, year, status) 

#-------------------------------------------------------------------------#
###### MIGRATORY BIRD RULESET ##################
#-------------------------------------------------------------------------#

# keeping only migratory birds...then looking at their migration > 35 km
# following ruleset is pretty complicated. Basically, it looks for all movements greater than 35 and sees how many days are between them. There's a few rules that need to be followed (no winter movement, surpress small north movement). These work well for eastern mallards based off of explortatory data analysis. Add or take away rules based on your study species 

# based on coluccy et al., 2020 that says stopovers aren't longer than 30
tol_spring <- 21 # stop over for spring
tol_fall   <- 35 # stop over for fall
tol_summer <- 5

migration_bouts_clean <- total_distance_measurements %>%
  arrange(bandnum, UTC_date) %>%
  group_by(bandnum) %>%
  
  # according to my banding mortality analysis, I want to censor banding day and the day after 
  
  mutate(
    first_day = min(UTC_date),
    censor_day = first_day + 1
  ) %>%
  filter(!(UTC_date == first_day | UTC_date == censor_day)) %>%
  ungroup() %>%
  
  # do the following for all migratory birds 
  
  filter(bandnum %in% (status_table %>% 
                         filter(status == "migratory") %>% 
                         pull(bandnum))) %>%
  arrange(bandnum, UTC_date) %>%
  group_by(bandnum) %>%
  mutate(
    ruleset_morethan35 = case_when(
      is.na(daily_displacement_km) ~ FALSE,
      
      # suppress movement during Jan 1–31
      (daily_displacement_km >= 35) &
        (month(UTC_date) == 1) &
        (day(UTC_date) <= 31) ~ FALSE,
      
      # suppress small northern movement during  Feb (1-28)
      (daily_displacement_km >= 35 & daily_displacement_km <= 100) &
        (month(UTC_date) == 2) &
        (day(UTC_date) <= 28) ~ FALSE,
      
      # suppress small northern movement during Mar (1-10)
      (daily_displacement_km >= 35 & daily_displacement_km <= 100) &
        (month(UTC_date) == 3) &
        (day(UTC_date) <= 10) ~ FALSE,
      
      # all other valid movement
      daily_displacement_km >= 35 ~ TRUE,
      
      TRUE ~ FALSE
    ),
    
    is_mig2 = {
      r         <- rle(ruleset_morethan35)
      vals      <- r$values
      lens      <- r$lengths
      ends      <- cumsum(lens)
      starts    <- ends - lens + 1
      true_runs <- which(vals)
      first_run <- true_runs[1]
      
      for(i in seq_along(vals)) {
        if (!vals[i] && i > first_run && i < length(vals)) {
          run_dates <- UTC_date[starts[i]:ends[i]]
          gap_days  <- as.integer(max(run_dates) - min(run_dates))
          
          this_tol <- case_when(
            all(month(run_dates) %in% 2:6)                              ~ tol_spring,
            any(month(run_dates) %in% 7)                                ~ tol_summer,
            TRUE                                                       ~ tol_fall
          )
          
          # Only bridge if calendar gap ≤ seasonal tol
          if (gap_days <= this_tol) {
            vals[i] <- TRUE
          }
        }
      }
      
      inverse.rle(list(lengths = lens, values = vals))
    },
    
    episode = cumsum(is_mig2 != lag(is_mig2, default = FALSE))
  ) %>%
  ungroup() %>%
  
  # keep only migratory days
  filter(is_mig2) %>%
  group_by(bandnum, episode) %>%
  summarise(
    
    # because the daily displacement is lagged, the movement actually starts the day before 
    
    start        = first(UTC_date) - 1,
    end          = last(UTC_date),
    year         = year(first(UTC_date)),
    total_dist   = sum(daily_displacement_km, na.rm = TRUE),
    start_lon    = first(Longitude),
    start_lat    = first(Latitude),
    end_lon      = last(Longitude),
    end_lat      = last(Latitude),
    .groups = "drop"
  )

# this selects the day before the first day of migration. I extract this so that I can calculate the net displacement of the migration bout

most_recent_pre_mig <- total_distance_measurements %>%
  dplyr::select(bandnum, UTC_date, pre_lon = Longitude, pre_lat = Latitude) %>%
  right_join(migration_bouts_clean, by = "bandnum") %>%
  
  # filters for dates before spring migration starts 
  
  filter(UTC_date < start) %>%
  group_by(bandnum, episode) %>%
  
  # filters for the maximum date before spring migration 
  
  slice_max(UTC_date, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(bandnum, episode, pre_lon, pre_lat)

# Join and calculate net displacement from start of migration to end
# also adds in fall migration bouts

bout_summary <- migration_bouts_clean %>%
  left_join(most_recent_pre_mig, by = c("bandnum", "episode")) %>%
  mutate(
    start = start - 1,
    net_disp_km = ifelse(
      !is.na(pre_lon) & !is.na(pre_lat) & !is.na(end_lon) & !is.na(end_lat),
      distHaversine(cbind(pre_lon, pre_lat), cbind(end_lon, end_lat)) / 1000,
      NA_real_
    )
  ) %>%
  
  # Add number of fall bouts per bird-year
  
  group_by(bandnum, year) %>%
  mutate(n_fall_bouts = sum(month(start) >= 8 & month(start) <= 12, na.rm = TRUE)) %>%
  ungroup() %>%
  
  rowwise() %>%
  mutate(
    molt_start = runs_df$start_date[
      which(runs_df$bandnum == bandnum & runs_df$year == year)
    ][1],
    
    bad = any(
      # 1) They move and come back.
      net_disp_km > 0 & net_disp_km < 50,
      
      # 2) Pre-molt migration within X days — ONLY IF greater than 2 fall bouts
      (
        n_fall_bouts >= 2 &&
          !is.na(molt_start) &&
          start < molt_start &&
          as.numeric(molt_start - start) <= 55 
      ),
      
      
      # 3) Molt-window movement pattern (only if molt_start is missing)
      is.na(molt_start) &&
        (
          (month(start) == 7 & day(start) >= 15) |
            month(start) == 8 |
            (month(start) == 9 & day(start) <= 10)
        ) &&
        (
          (total_dist > 50 & (end_lat - start_lat) >= 0) |  # long northward
            (total_dist < 300 & (end_lat - start_lat) < 0)    # short southward
        )
    ) 
  ) %>%
  ungroup() %>%
  filter(!bad) %>%
  dplyr::select(-bad, -starts_with(c("start_lon","start_lat","end_lon","end_lat","net_disp_km")))


#### label migratory seasons as spring or fall
# if between Feb - July, spring migration
# if between Aug - Dec, fall migration

# label each bout as spring vs fall
bouts <- bout_summary %>%
  mutate(
    year = year(start),
    mig_season = case_when(
      start >= as.Date(paste0(year, "-01-01")) & start <= as.Date(paste0(year, "-07-31")) ~ "spring_migration",
      start >= as.Date(paste0(year, "-08-01")) & start <= as.Date(paste0(year, "-12-31")) ~ "fall_migration",
      TRUE ~ NA_character_
    )
  )

# split into spring vs fall tables

library(dplyr)

# 1) pick only the first spring‐migration window per bird–year
spring_bouts <- bouts %>%
  filter(mig_season == "spring_migration") %>%
  dplyr::select(bandnum, spring_start = start, spring_end = end, year) %>%
  group_by(bandnum, year) %>%
  slice_min(spring_start, n = 1)

fall_bouts <- bouts %>%
  filter(mig_season == "fall_migration") %>%
  dplyr::select(bandnum, fall_start = start, fall_end = end, year) %>%
  group_by(bandnum, year) %>%
  slice_min(fall_start, n = 1)

# join to your full daily df -> this has migratory and resident birds

df_seasons <-  total_distance_measurements %>%
  filter(status %in% c("migratory", "unknown")) %>%
  
  # bring in that year's spring & fall windows
  
  left_join(spring_bouts, by = c("bandnum", "year"), relationship = "many-to-many") %>%
  left_join(fall_bouts, by = c("bandnum", "year"), relationship = "many-to-many") %>%
  
  # bring in *next* spring_start for winter definition
  
  left_join(
    spring_bouts %>%
      transmute(bandnum,
                year = year - 1,           # join to last year's spring
                next_spring_start = spring_start),
    by = c("bandnum", "year")
  ) %>%
  
  # now assign seasons
  
  mutate(
    season = case_when(
      
      # 1) spring migration
      UTC_date >= spring_start    & UTC_date <= spring_end   ~ "spring_migration",
      
      # 2) summer: after spring_end AND (before fall_start OR there was no fall_start)
      UTC_date >  spring_end      & 
        (UTC_date < fall_start | is.na(fall_start))          ~ "summer",
      
      # 3) fall migration (only if fall_start exists)
      !is.na(fall_start)  &
        UTC_date >= fall_start     & UTC_date <= fall_end     ~ "fall_migration",
      
      # 4) everything else → winter
      TRUE                                                   ~ "winter"
    )
  ) %>%
  dplyr::select(-spring_start, -spring_end,
                -fall_start, -fall_end,
                -next_spring_start) 

# merge back in with total distance measurements to include resident birds and then join that with hourly GPS data

all_birds_season_distance <- all_gps_data %>%
  dplyr::select(device_id, bandnum, UTC_date, UTC_datetime, Latitude, Longitude) %>%
  left_join(
    total_distance_measurements %>%
      left_join(
        df_seasons %>%  dplyr::select(bandnum, UTC_date, season),
        by = c("bandnum","UTC_date")
      ) %>%
      dplyr::select(bandnum, UTC_date, julian_date, status, season, year, daily_displacement_km) %>%
      distinct(bandnum, UTC_date, .keep_all = TRUE),  # drop duplicates
    by = c("bandnum","UTC_date")
  ) %>%
  
  ## # I need to manually assign "228747526" dates as the ruleset just won't work with it
  # year 2024 - spring migration ended 127, fall migration started 238
  
  mutate(season = case_when(
    bandnum == "228747526" & julian_date > 127 & julian_date < 238 ~ "summer",
    bandnum == "228747526" & julian_date > 238 ~ "fall_migration",
    TRUE ~ season  # keep existing season for all other cases
  ))


#-------------------------------------------------------------------------#
###### Resident birds seasonal dates ##################
#-------------------------------------------------------------------------#

#### spring
# 1) get spring‐migration dates with real Date

# don't include 228747526 because I know it didn't assign right.

spring_df <- df_seasons %>%
  filter(status == "migratory", season == "spring_migration", bandnum != "228747526") %>%
  mutate(
    date = as.Date(julian_date - 1, origin = "2020-01-01")
  )
# 
# ###### quickly looking at latest start of spring migration ######
# 
# # start of spring
spring_df %>%
  dplyr::select(bandnum, year, julian_date) %>%
  group_by(bandnum, year) %>%
  slice_head() %>%
  group_by(year) %>%
  summarize(max_date = quantile(julian_date, 0.95, na.rm=TRUE))
# 
# # average is around April 24th 

#######--

# 2) compute cutoffs per year
cutoffs <- spring_df %>%
  group_by(year) %>%
  summarise(
    low_j   = quantile(julian_date, 0.10, na.rm=TRUE),
    high_j  = quantile(julian_date, 0.9, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    low_date  = as.Date(low_j  - 1, origin = "2020-01-01"),
    high_date = as.Date(high_j - 1, origin = "2020-01-01")
  )


# 3) join cutoffs back to spring_df
spring_df2 <- spring_df %>%
  left_join(cutoffs, by = "year")

# 4) plot, faceted by year
spring <- ggplot(spring_df2, aes(x = date)) +
  geom_bar(
    fill  = "#4292C6",
    color = "#4292C6"
  ) +
  # per‐year cutoff lines
  geom_vline(aes(xintercept = low_date),  linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = high_date), linetype = "dashed", color = "red") +
  # per‐year text labels at bottom of each panel
  geom_label(
    data    = cutoffs,
    aes(
      x      = low_date,
      y      = 75,
      label  = format(low_date, "%b %d")
    ),
    vjust   = -0.5,
    fill    = "white",
    color   = "red"
  ) +
  geom_label(
    data    = cutoffs,
    aes(
      x      = high_date,
      y      = 75,
      label  = format(high_date, "%b %d")
    ),
    fill    = "white",
    vjust   = -0.5,
    color   = "red"
  ) +
  facet_wrap(~ year, ncol = 1) +
  scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%b %d",
    expand      = expansion(add = 0)
  ) +
  ylim(0, 130) +
  ggpubr::theme_pubr() +
  theme(axis.text.x = element_text(size = 8), 
        text = element_text(family = "Times New Roman"), 
        plot.title = element_text(face = "bold")) +  
  labs(
    x        = "Date",
    y        = "Number of birds in spring migration",
    title    = "(a) Spring Migration"
  )

### fall
# 1) get fall‐migration dates with real Date
fall_df <- df_seasons %>%
  filter(status == "migratory", season == "fall_migration", bandnum != "228747526") %>%
  mutate(
    date = as.Date(julian_date - 1, origin = "2020-01-01")
  )

# 2) compute cutoffs per year
cutoffs <- fall_df %>%
  group_by(year) %>%
  summarise(
    low_j   = quantile(julian_date, 0.10, na.rm=TRUE),
    high_j  = quantile(julian_date, 0.90, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    low_date  = as.Date(low_j  - 1, origin = "2020-01-01"),
    high_date = as.Date(high_j - 1, origin = "2020-01-01")
  )

# 3) join cutoffs back to spring_df
fall_df2 <- fall_df %>%
  left_join(cutoffs, by = "year")

# 4) plot, faceted by year
fall <- ggplot(fall_df2, aes(x = date)) +
  geom_bar(
    fill  = "#4292C6",
    color = "#4292C6"
  ) +
  # per‐year cutoff lines
  geom_vline(aes(xintercept = low_date),  linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = high_date), linetype = "dashed", color = "red") +
  # per‐year text labels at bottom of each panel
  geom_label(
    data    = cutoffs,
    aes(
      x      = low_date,
      y      = 22,
      label  = format(low_date, "%b %d")
    ),
    vjust   = -0.5,
    fill    = "white",
    color   = "red"
  ) +
  geom_label(
    data    = cutoffs,
    aes(
      x      = high_date,
      y      = 22,
      label  = format(high_date, "%b %d")
    ),
    fill    = "white",
    vjust   = -0.5,
    color   = "red"
  ) +
  facet_wrap(~ year, ncol = 1) +
  ylim(0, 40) +
  scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%b %d",
    expand      = expansion(add = 0)
  ) +
  ggpubr::theme_pubr() +
  theme(axis.text.x = element_text(size = 8), 
        text = element_text(family = "Times New Roman"), 
        plot.title = element_text(face = "bold")) +
  labs(
    x        = "Date",
    y        = "Number of birds in fall migration",
    title    = "(b) Fall Migration"
  )

formarkdown <- spring + fall

#ggsave("figures/suppmat_figure3_updated.png", plot = formarkdown, width = 10, height = 7)

### making the cutoffs be the seasons for residents 

cutoffs <- df_seasons %>%
  filter(status == "migratory", bandnum != "228747526") %>%
  mutate(
    date = as.Date(julian_date - 1, origin = "2020-01-01")
  ) %>%
  group_by(season, year) %>%
  filter(season %in% c("spring_migration","fall_migration")) %>%
  summarise(
    low  = quantile(julian_date, 0.10, na.rm = TRUE),
    high = quantile(julian_date, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = season,
    values_from = c(low, high)
  ) %>%
  mutate(
    low_fall_migration = if_else(
      year == 2026, mean(low_fall_migration[year < 2026], na.rm = TRUE),
      low_fall_migration
    ),
    high_fall_migration = if_else(
      year == 2026, mean(high_fall_migration[year < 2026], na.rm = TRUE),
      high_fall_migration
    )
  )

# 2) Assign seasons for your residents
all_birds_season_distance <- all_birds_season_distance %>%
  left_join(cutoffs, by = "year") %>%
  mutate(
    season = case_when(
      status == "migratory" ~ season,
      
      status %in% c("resident", "partial_migrant") &
        julian_date >= low_spring_migration &
        julian_date <= high_spring_migration ~ "spring_migration",
      
      status %in% c("resident", "partial_migrant") &
        julian_date >= low_fall_migration &
        julian_date <= high_fall_migration ~ "fall_migration",
      
      status %in% c("resident", "partial_migrant") &
        julian_date >  high_spring_migration &
        julian_date <  low_fall_migration ~ "summer",
      
      status %in% c("resident", "partial_migrant") ~ "winter",
      
      TRUE ~ season
    )
  ) %>%
  dplyr::select(-low_fall_migration, -high_spring_migration, -low_spring_migration, -high_fall_migration) %>%
  mutate(partial_status = (ifelse(status == "partial_migrant", "quantiles", NA)))


######## dealing with partial migratns ############
# spring migration, but don't migrate back in the fall
# latest fall migration start is Dec 24

## three categories of partial migrants
# molt migrater in June / weird ness (probably partial from above)
# no spring migration, but fall migration (can't find any of these)
# spring migration, but no fall migration 

# check - do any birds have winter at the end of June? These may be molt migrants not captured

all_birds_season_distance %>%
  filter(status == "migratory",
         season == "winter",
         month(UTC_date) %in% c(6), 
         day(UTC_date) %in% c(25:31)) %>%
  distinct(bandnum, year)

## birds that may have been molt migrating?
partial_migrant_molt <- all_birds_season_distance %>%
  filter(
    status == "migratory",
    season == "winter",
    month(UTC_date) == 6,
    day(UTC_date) %in% 25:31
  ) %>%
  distinct(bandnum, year)


# Step 2: Update status with correct bandnum-year pairing
all_birds_season_distance_withmolt <- all_birds_season_distance %>%
  left_join(partial_migrant_molt %>% mutate(is_partial = TRUE),
            by = c("bandnum", "year")) %>%
  mutate(status = replace(status, is_partial == TRUE, "partial_migrant")
  ) %>%
  
  # for molt migrants, their spring migration and winter before that needs to be defined by migratory birds
  
  left_join(cutoffs, by = "year") %>%
  mutate(
    season = case_when(
      status == "migratory" ~ season,
      status == "resident" ~ season, 
      status == "unknown" ~ season, 
      
      
      is_partial == TRUE &
        julian_date >= low_spring_migration &
        julian_date <= high_spring_migration ~ "spring_migration",
      
      is_partial == TRUE &
        julian_date >= low_fall_migration &
        julian_date <= high_fall_migration ~ "fall_migration",
      
      is_partial == TRUE &
        julian_date >  high_spring_migration &
        julian_date <  low_fall_migration ~ "summer",
      
      is_partial == TRUE ~ "winter",
      
      TRUE ~ season)) %>%
  dplyr::select(-low_fall_migration, -high_spring_migration, -low_spring_migration, -high_fall_migration, -is_partial) %>%
  
  # i think all the partial migrants at this point are molt migrants??
  
  mutate(partial_status = ifelse(status == "partial_migrant", "quantile", NA))

partial_migrant_fall <- all_birds_season_distance_withmolt %>%
  filter(status == "migratory",
         season == "summer",
         month(UTC_date) %in% c(12), 
         day(UTC_date) %in% c(27:31)) %>%
  distinct(bandnum, year)


# Step 2: Update status with correct bandnum-year pairing
all_birds_season_distance_final <- all_birds_season_distance_withmolt %>%
  left_join(partial_migrant_fall %>% mutate(is_partial = TRUE),
            by = c("bandnum", "year")) %>%
  mutate(
    partial_status = if_else(
      is_partial %in% TRUE,
      
      # since these partial migrants don't have a fall migration, we're assigning them based on residents.
      
      "spring_migration_fall_quantile",
      partial_status
    )
  ) %>%
  mutate(status = replace(status, is_partial %in% TRUE, "partial_migrant")) %>%
  
  # for partial migrants, their fall migration and winter after that needs to be defined by migratory birds
  
  left_join(cutoffs, by = "year") %>%
  mutate(
    season = case_when(
      partial_status == "spring_migration_fall_quantile" &
        julian_date >= low_fall_migration &
        julian_date <= high_fall_migration ~ "fall_migration",
      TRUE ~ season
    )
  ) %>%
  
  # Step 3: Set winter for dates after fall migration
  mutate(
    season = case_when(
      partial_status == "spring_migration_fall_quantile" &
        julian_date > high_fall_migration ~ "winter",
      TRUE ~ season
    )
  ) %>% 
  dplyr::select(-low_fall_migration, -high_spring_migration, -low_spring_migration, -high_fall_migration, -is_partial) 


dbWriteTable(conn, "four_season_status_dailydisplacement_2022_sep2026", all_birds_season_distance_final, overwrite = TRUE, append = FALSE, row.names = FALSE)

# how many of each
all_birds_season_distance_final %>%
  dplyr::select(bandnum, status, partial_status) %>%
  distinct() %>%
  group_by(status) %>%
  summarize(count = n())

all_birds_season_distance_final %>%
  filter(status == "partial_migrant") %>%
  dplyr::select(bandnum, partial_status) %>%
  distinct() %>%
  group_by(partial_status) %>%
  summarize(count = n())


## checking updated results 

update <- all_birds_season_distance_final %>%
  mutate(birdid_year = paste(bandnum, sep = "_", year))
# 
# update %>%
#   dplyr::select(bandnum, status) %>%
#   distinct() %>%
#   group_by(status) %>%
# #   summarize(count = n())
# 
# forilsa <- update %>%
#   dplyr::select(birdid_year, device_id, bandnum, UTC_date, julian_date, status, season, year, partial_status) %>%
#   distinct()
# 
# length(unique(update$birdid_year))
# 
# write.csv(forilsa, "migration_forIlsa30June2026.csv")
# 
# ilsa <- read_csv("migration_forIlsa18Nov2025.csv") %>%
#   mutate(birdid_year = paste(bandnum, sep = "", year))
# 
# 
# length(unique(ilsa$birdid_year))
# 
# # checking for ilsa
# data <- tbl(conn, "four_season_status_dailydisplacement_2022_2025_update30June26") %>%
#   collect() %>%
#   mutate(bird_year = paste(bandnum, year, sep = "_"))
# 
# data_test <- data %>%
#   dplyr::select(bird_year, season, UTC_date, status) %>%
#   distinct()
# 
# thirty <- data_test %>%
#   group_by(bird_year, season, status) %>%
#   summarize(count = n()) %>%
#   filter(season == "summer",
#          count < 31,
#          status != "resident")
# 
# # saving data set for jack
# 
# mall_migration <- data %>%
#   dplyr::select(year, status, bandnum) %>%
#   distinct()
# 
# write.csv(mall_migration, "data/mall_migration_status_2022_2025.csv")

#-------------------------------------------------------------------------#
###### PLOTTING ##################
# #-------------------------------------------------------------------------#
# df <- tbl(conn, "four_season_status_dailydisplacement_2022_2025") %>%
#   collect() 

plot_bird_seasons <- function(df, band_id, year_val, interactive = TRUE) {
  # 1) filter to one bird and one year, then make sf
  df_sf <- df %>%
    filter(bandnum == band_id, year == year_val) %>%
    st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)
  
  # 2) ensure season has the right order
  df_sf$season <- factor(
    df_sf$season,
    levels = c("spring_migration","summer","fall_migration","winter")
  )
  
  # 3) choose view vs plot
  if (interactive) {
    tmap_mode("view")
  } else {
    tmap_mode("plot")
  }
  
  # 4) build the map
  base_map <- tm_shape(df_sf) +
    tm_dots(
      col        = "season",
      palette    = "Set2",
      size       = 1,
      title      = "Season",
      # in interactive mode, show the UTC_datetime in the popup
      popup.vars = c("Date/Time" = "UTC_datetime")
    )
  
  # 5) if static, add text labels of the datetime
  if (!interactive) {
    base_map <- base_map +
      tm_text(
        text      = "UTC_datetime",
        size      = 0.5,
        just      = "left",
        ymod      = 1
      )
  }
  
  # 6) finalize layout
  base_map +
    tm_layout(
      title          = paste("Bird", band_id, "–", year_val),
      legend.outside = TRUE
    )
}

plot_bird_seasons(df, "213738409", 2022)

test<- all_birds_season_distance_final %>% filter(bandnum == "222779073", 
                                                  year == "2024", 
                                                  season == "spring_migration")


#-------------------------------------------------------------------------#
###### DAILY DISPLACEMENT GRAPHS ##################
#-------------------------------------------------------------------------#


#— 1) select a random 100 birds that are resident or migratory

bird_year_sample <- all_birds_season_distance_final %>%
  
  filter(year == 2026) %>%
  # 
  # # Keep birds that are in your molt dataset
  # filter(bandnum %in% (runs_df %>% pull(bandnum))) %>%
  
  # # Identify August fall migration points
  # filter(season == "fall_migration",
  #        month(UTC_date) == 8) %>%
  
  # Collapse to bird-year level
  distinct(bandnum, year) %>%
  
  # Sample
  slice_sample(n = 100)

df <- all_birds_season_distance_final %>%
  semi_join(bird_year_sample, by = c("bandnum", "year"))
#— 1) Define a consistent color palette for seasons
season_levels <- c("spring_migration","summer","fall_migration","winter")
season_cols   <- setNames(brewer.pal(4, "Set2"), season_levels)

#— 2) Map‐making function with ggplot
make_map <- function(tmp) {
  
  states <-
    rnaturalearth::ne_states(
      country = c("canada", "united states of america", "greenland"),
      returnclass = "sf"
    )
  
  
  min_long <- min(tmp$Longitude) - 0.4
  max_long <- max(tmp$Longitude) + 0.4
  min_lat <- min(tmp$Latitude) - 0.4
  max_lat <- max(tmp$Latitude) + 0.4
  
  ggplot(data = states) + 
    geom_sf(fill = "grey95", color = "grey50") +
    geom_point(data = tmp, aes(x = Longitude, y = Latitude, color = season),
               size = 1.3) +
    scale_color_manual(values = season_cols) +
    coord_sf(xlim = c(min_long,max_long),
             ylim = c(min_lat, max_lat),
             expand = F) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(legend.position = "none")
}

#— 3) Displacement‐plot function (same as before)
make_disp <- function(tmp) {
  ggplot(tmp, aes(x = UTC_date, y = daily_displacement_km, color = season)) +
    geom_line() +
    scale_color_manual(values = season_cols) +
    labs(x = "Date", y = "Daily displacement (km)") +
    theme_minimal()
}


#— 4) Prepare the list of bird–year combos
bird_years <- df %>%
  distinct(bandnum, year) %>%
  arrange(bandnum, year)

#— 5) Open one PDF device
pdf("2026_migration.pdf", width = 12, height = 6)

for (i in seq_len(nrow(bird_years))) {
  bn <- bird_years$bandnum[i]
  yr <- bird_years$year[i]
  
  tmp <- df %>%
    filter(bandnum == bn, year == yr) %>%
    mutate(season = factor(season, levels = season_levels))
  
  bird_status <- unique(tmp$status)
  
  map_gg  <- make_map(tmp) +
    ggtitle(paste("Bird", bn, "–", yr, "–", bird_status))
  
  disp_gg <- make_disp(tmp) +
    ggtitle(paste("Daily displacement:", bn, yr, "-", bird_status))
  
  print(map_gg + disp_gg + 
          plot_layout(ncol = 2, widths = c(3, 2)) &
          theme(plot.title = element_text(hjust = 0.5)))
}

dev.off()


### graph for manuscript ####

seasons <- tbl(conn, "four_season_status_dailydisplacement_2022_2025") %>%
  collect() 

my_colors <- c("winter" = "#decab5", "spring_migration" = "#08306b", 
               "summer" = "#add8e6", "fall_migration" = "#a6615e")

common_theme <- theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "right")

states <- ne_states(country = c("united states of america", "canada"), returnclass = "sf")

data1 <- seasons %>%
  filter(bandnum == "228701157", 
         year == "2024")

min_long <- min(data1$Longitude) - 3
max_long <- max(data1$Longitude) + 2
min_lat <- min(data1$Latitude) - 1
max_lat <- max(data1$Latitude) + 1


pa <- ggplot(data = states) + 
  geom_sf(fill = "grey95", color = "grey50") +
  geom_point(data = data1, aes(x = Longitude, y = Latitude, color = season),
             size = 1.3) +
  scale_color_manual(values = my_colors, 
                     breaks = c("winter", "spring_migration", "summer", "fall_migration"),
                     labels = function(x) str_to_sentence(str_replace_all(x, "_", " "))) +
  coord_sf(xlim = c(min_long,max_long),
           ylim = c(min_lat, max_lat),
           expand = F) +
  labs(x = "Longitude", y = "Latitude", title = "(a) Ind 1: Movement") +
  common_theme 

pb <- ggplot(data1, aes(x = UTC_date, y = daily_displacement_km, color = season)) +
  geom_segment(aes(xend = UTC_date, yend = 0), size = 1) +
  scale_color_manual(values = my_colors, guide = "none") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(y = "Daily displacement (km)", x = "", title = "(b) Ind 1: Displacement") +
  common_theme 


data2 <- seasons %>%
  filter(bandnum == "222779292", 
         year == "2023")

min_long <- min(data2$Longitude) - 8
max_long <- max(data2$Longitude) + 8
min_lat <- min(data2$Latitude) - 1
max_lat <- max(data2$Latitude) + 1


pc <- ggplot(data = states) + 
  geom_sf(fill = "grey95", color = "grey50") +
  geom_point(data = data2, aes(x = Longitude, y = Latitude, color = season),
             size = 1.3) +
  scale_color_manual(values = my_colors, 
                     breaks = c("winter", "spring_migration", "summer", "fall_migration"),
                     labels = function(x) str_to_sentence(str_replace_all(x, "_", " "))) +
  coord_sf(xlim = c(min_long,max_long),
           ylim = c(min_lat, max_lat),
           expand = F) +
  labs(x = "Longitude", y = "Latitude", title = "(c) Ind 2: Movement") +
  common_theme +
  theme(legend.position = "none")

pd <- ggplot(data2, aes(x = UTC_date, y = daily_displacement_km, color = season)) +
  geom_segment(aes(xend = UTC_date, yend = 0), size = 1) +
  scale_color_manual(values = my_colors, guide = "none") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(y = "Daily displacement (km)", x = "", title = "(d) Ind 2: Displacement") +
  common_theme 


first <- (pa | pb) 

second <- (pc | pd) 

# Combine the two rows
combined <- first / second

# Force the 50/50 split and collect the legend
combined_plot <- combined + 
  plot_layout(guides = 'collect', widths = c(1, 1)) & 
  theme(legend.position = "bottom")

ggsave(combined_plot, 
       height = 6, 
       width = 8, 
       dpi = 600, 
       file = "figures_tables/suppmat_distancemap.png")
