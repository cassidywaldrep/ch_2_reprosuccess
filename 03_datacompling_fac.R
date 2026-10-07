######################## 02_compliningdata_fac.R ##################

# Code: Complining variables needed for FAC
# Date: 18 Sep 2026
# Author: Cassidy Waldrep

# Load packages and functions

library(tidyverse)
library(RPostgres)
library(sf)
library(ggmap)
library(tigris)
library(readxl)

#-------------------------------------------------------------------------#
###### moving tables into new postgres database (only do once) #############
#-------------------------------------------------------------------------#

conn <- dbConnect(
  Postgres(),
  dbname = "acc_gps_data_mallards",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "mallard_ducks"
)
# 
# # proportion of time feeding and odba
# #
# daily_activity <- tbl(conn, "all_daily_activity_budget_2022_sep2026_80%fixes") %>%
#   collect()
# 
# # nesting propensity / ETI
# 
# early_term <- tbl(conn, "mall_egglaying_classified_last6daysAnd1inc_updatedsep2026") %>%
#   collect()
# 
# # full term incubation
# 
# incubation <- tbl(conn, "mall_incubation_classified_updatedsep2026") %>%
#   collect()
# 
# # brood rearing
# 
# brood <- tbl(conn, "mall_broodrearing_classified_IncompleteDataBirdIncluded_15sep2026") %>%
#   collect()
# 
# # gps data, migratory status, and periods
# 
# ecological_periods <- tbl(conn, "eight_season_status_dailydisplacement_2022_2026_updatedsep2026") %>%
#   collect()

conn <- dbConnect(
  Postgres(),
  dbname = "ch2_facmodel",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "mallard_ducks"
)
# 
# dbWriteTable(conn, "daily_ptf_odba", daily_activity, append = FALSE, row.names = FALSE)
# dbWriteTable(conn, "ecologicalperiods", ecological_periods, overwrite = TRUE, append = FALSE, row.names = FALSE)
# dbWriteTable(conn, "fourseason_migratory_status", ecological_periods, overwrite = TRUE, append = FALSE, row.names = FALSE)
# dbWriteTable(conn, "mall_egglaying_classified_last6daysAnd1inc_updatedsep2026", early_term, overwrite = TRUE, append = FALSE, row.names = FALSE)
# dbWriteTable(conn, "mall_incubation_classified_updatedsep2026", incubation, overwrite = TRUE, append = FALSE, row.names = FALSE)
# dbWriteTable(conn, "mall_broodrearing_classified_IncompleteDataBirdIncluded_15sep2026", brood, overwrite = TRUE, append = FALSE, row.names = FALSE)


### reading in breeding data

inc <- read_csv("results/incubation_dataset_successful_failed_dates_ch2_updatedsep2026.csv") %>%
  group_by(band_year) %>%
  
  # a 2026 bird had two successes 
  
  arrange(inc_start) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  
  # filtering based on chapter 3 data
  
  filter(band_year != "228775579_2024", 
         band_year != "224788925_2023") %>%
  mutate(inc_start = case_when(band_year == "213720084_2024" ~ as.Date("2024-04-29"), 
                               band_year ==  "216756560_2025" ~ as.Date("2025-07-11"), 
                               band_year ==  "222779086_2022" ~ as.Date("2022-05-07"), 
                               band_year ==  "225737950_2022" ~ as.Date("2022-04-14"), 
                               TRUE ~ inc_start), 
         year = year(inc_start))

# number of incubating birds 

length(unique(inc$band_year))

# number of incubating birds per YEAR 

table(inc$year)

### reading in brood data

brood <- read_csv("results/all_broodrearing_successful_failed_ch2_updatedsep2026.csv") %>%
  group_by(band_year) %>%
  
  # a 2026 bird had two successes 
  
  arrange(brood_start) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  filter(band_year != "225769466_2023") %>% 
  mutate(brood_end = case_when(birdid_year == "228716656_2024" ~ as.Date("2024-07-15"), 
                               birdid_year ==  "228701843_2024" ~ as.Date("2024-06-28"), 
                               birdid_year == "224728526_2024" ~ as.Date("2024-07-10"), 
                               TRUE ~ brood_end), 
         year = year(brood_end)) 
  
# number of brood rearing birds 

length(unique(brood$band_year))

# number of incubating birds per YEAR 

table(brood$year)

#-------------------------------------------------------------------------#
###### summarizing period data #############
#-------------------------------------------------------------------------#

## first, get dates for sub-seasons, so you can summarize all variables by sub-season 
# 1 = winter (Jan 15 (or whenever captured) until 2 weeks before the start of spring migration) - for non-migratory, continue winter until early breeding
#	2 = late winter (2 weeks prior to spring migration) - do not include for non-migratory birds
#	3 = early migration (use each individual’s median migration date to split migration in half) - do not include for non-migratory birds
#	4 = late migration - do not include for non-migratory birds
# 5 = early breeding (first two weeks after arriving on breeding grounds) - for non-migratory, do two weeks before the start of nesting if bird nests; otherwise, do two weeks before the mean nesting start date of non-migratory individuals
#	6 = nesting (nesting period; use nesting dates ) 
#	7 = brood rearing (brood period)

# I need to take the average PTF and ODBA for each season...winter, late_winter, early spring migration, late spring migration, and early_breeding
# First, I think it will be best to label all the dates for all mallards and the season that they're in. For nesting / brood rearing, I need to bring that in

masterlist <- read_excel("data/mall_9_17_2026.xlsx", 
                         sheet = "MALL") %>%
  janitor::clean_names() %>%
  dplyr::select(band_number, banding_date) %>%
  mutate(banding_date = as.Date(banding_date), 
         year = year(banding_date), 
         bandnum = as.numeric(gsub("-", "", band_number))) %>%
  distinct(bandnum, year, .keep_all = TRUE)

ecological_periods <- tbl(conn, "ecologicalperiods") %>%
  collect() %>%
  
  # to best match up with birds that were just banded, starting all birds at Jan 15th if possible, ending Sep 15th
  
  filter(julian_date >= 15, 
         julian_date <= 258) %>%
  mutate(
    UTC_date = as.Date(UTC_date),
    year = year(UTC_date)
  ) %>%
  
  # filtering out the banding date and the day after but only on the year the bird was banded 
  
  left_join(
    masterlist %>%
      select(bandnum, year, banding_date),
    by = c("bandnum", "year")
  ) %>%
  filter(is.na(banding_date) | UTC_date > banding_date + 1) %>%
  dplyr::select(bandnum, UTC_date, status, season = season2, year, status, partial_status) %>%
  distinct() %>%
  mutate(bandnum = as.character(bandnum), 
         band_year = paste(bandnum, year, sep = "_")) 

mean_nest_start_date <- ecological_periods %>%
  left_join(inc %>% dplyr::select(band_year, incubation_status, inc_start, inc_end), by = "band_year") %>%
  
  # extracting resident, unknown, and partial migrants that did not migrate in the spring 
  
  filter(status %in% c("resident", "unknown") | (status == "partial_migrant" & partial_status == "quantile")) %>%
  distinct(bandnum, year, status, inc_start) %>%
  
  # only filtering those that nested
  
  filter(!is.na(inc_start)) %>%
  group_by(year) %>%
  
  # determining what the mean nest start date is for resident birds that nest
  
  summarize(mean_start = mean(inc_start, na.rm = TRUE), .groups = "drop")


ecological_periods_updated <- ecological_periods %>%
  
  # Bring in incubation and brood-rearing datasets
  
  left_join(inc %>% dplyr::select(band_year, incubation_status, inc_start, inc_end), by = "band_year") %>%
  left_join(brood %>% dplyr::select(band_year, brood_reaing_status, brood_start, brood_end), by = "band_year") %>%
  
  # Bring in pre-calculated yearly mean nesting start dates for non migratory birds
  left_join(mean_nest_start_date, by = "year") %>%
  
  mutate(
    # Create a  flag for non migratory birds 
    
    is_special = status %in% c("resident", "unknown") | (status == "partial_migrant" & partial_status == "quantile"),
    
    # assigning seasonal dates 
    
    season = case_when(
      # bird has a nest so take the nesting dates and assign
      
      !is.na(inc_start) & UTC_date >= inc_start & UTC_date <= inc_end ~ "nesting",
      
      # bird has a brood, so take brooding dates and assign
      
      !is.na(brood_start) & UTC_date >= brood_start & UTC_date <= brood_end ~ "brood_rearing",
      
      # for nonmigratory birds, assign early breeding as two weeks before their nesting date if they did nest
      
      is_special & !is.na(inc_start) & UTC_date >= (inc_start - 14) & UTC_date < inc_start ~ "early_breeding", 
      
      # for nonmigratory birds that did not nest, assign early breeding as two weeks before the mean start of nesting that year 
      
      is_special & is.na(inc_start) & !is.na(mean_start) & UTC_date >= (mean_start - 14) & UTC_date < mean_start ~ "early_breeding",
      
      # if nonmigratory and they did nest, assign after incubation after breeding to filter out later (this will not impact birds with a brood, only those that didn't have a brood)
      
      is_special & !is.na(inc_end) & UTC_date > inc_end ~ "after_breeding",
      
      # for nonmigratory birds that did not nest, after breeding comes after mean_start (since these birds will only have winter and early_breeding)
      
      is_special & is.na(inc_end) & !is.na(mean_start) & UTC_date > mean_start ~ "after_breeding",
      
      # for nonmigratory birds, winter should be anytime before early breeding (so either early breeding before their nesting period, or early breeding before the mean start of the nesting period)
      
      is_special & !is.na(inc_start) & UTC_date < (inc_start - 14) ~ "winter",
      is_special & is.na(inc_start) & !is.na(mean_start) & UTC_date < (mean_start - 14) ~ "winter",
      
      # Default: Keep original season for all other birds (migrants, etc.)
      TRUE ~ season
    )
  ) %>%
  # Clean up temporary helper column
  dplyr::select(-is_special)


# now, to reduce the seasons, we only want winter, late_winter, spring_migration, late_spring_migration, early_breeding, nesting, and brood_rearing

ecological_periods_final <- ecological_periods_updated %>%
  filter(season %in% c("winter", "late_winter", "spring_migration", "late_spring_migration", "early_breeding", "nesting", "brood_rearing")) %>%
  dplyr::select(bandnum, UTC_date, status, season, year, incubation_status, brood_reaing_status) %>%
  mutate(band_year = paste(bandnum, year, sep = "_"))

test <- ecological_periods_final %>%
  dplyr::select(bandnum, year, incubation_status, brood_reaing_status) %>%
  distinct()

# make sure these are 612 and 174 from the above reproduction data 

table(test$incubation_status)
table(test$brood_reaing_status)

# total of 7 seasons

total_seasons <- ecological_periods_final %>%
  dplyr::select(bandnum, year, season) %>%
  distinct() %>%
  group_by(bandnum, year) %>%
  summarize(count = n())

hist(total_seasons$count) # looks like we have a lot with five seasons which is great!
dim(total_seasons) # 1736 birdid_year

#-------------------------------------------------------------------------#
###### calculating PTF and ODBA per season #############
#-------------------------------------------------------------------------#

# reading in the full ptf and odba data set and then merging with the subseasons 

daily_ptf_odba <- tbl(conn, "daily_ptf_odba") %>%
  collect() %>%
  dplyr::select(bandnum, UTC_date = date, mean_ODBA, num_fixes, corrected_prop_feed) %>%

  mutate(num_feed_fixes_corrected = round(corrected_prop_feed * as.numeric(num_fixes)),
         year = year(UTC_date),
         bandnum = as.character(gsub("-", "", bandnum)),
          band_year = paste(bandnum, year, sep = "_"))

length(unique(daily_ptf_odba$band_year)) # 1836 birdid_year 

daily_ptf_odba_season_status <- ecological_periods_final %>%
 left_join(daily_ptf_odba, by = c("band_year", "UTC_date", "bandnum", "year"))

# 1696 birdid_year (we lost like 40 birds) - possible they had < 80% fixes of ACC, or didn't take ACC data, etc

# ecological_periods_final %>%
#   distinct(band_year) %>%
#   anti_join(
#     daily_ptf_odba %>% distinct(band_year),
#     by = "band_year"
#   )

# calculating the amount of missing data per season...only keeping > 80%
# this is normally because there's GPS that was taken, but no ACC...

season_missing_proportions <- daily_ptf_odba_season_status %>%
  group_by(band_year, season) %>%
  summarise(
    total_records = n(),
    missing_count = sum(is.na(mean_ODBA)),
    prop_missing = missing_count / total_records,
    .groups = "drop"
  )

# Identify which seasons have 20% or less missing data
valid_seasons <- season_missing_proportions %>%
  filter(prop_missing <= 0.5)

# Filter your original dataframe to keep only those valid seasons
cleaned_df <- daily_ptf_odba_season_status %>%
  inner_join(
    valid_seasons %>% dplyr::select(band_year, season), 
    by = c("band_year", "season")
  )

length(unique(cleaned_df$band_year)) # 1676 birdid_year (lost a few more, again, probably not enough ACC)

#-------------------------------------------------------------------------#
###### reproductive status #############
#-------------------------------------------------------------------------#

early_term <- tbl(conn, "mall_egglaying_classified_last6daysAnd1inc_updatedsep2026") %>%
  collect() %>%
  dplyr::select(c("birdid_year", "breeding_outcome")) %>%
  rename(egg_laying_status=breeding_outcome) %>%
  distinct()

early_term %>% group_by(egg_laying_status) %>% count()

incubation <- tbl(conn, "mall_incubation_classified_updatedsep2026") %>%
  collect() %>%
  dplyr::select(c("birdid_year", "breeding_outcome")) %>%
  rename(incubation_status=breeding_outcome) %>%
  distinct()

incubation %>% group_by(incubation_status) %>% count()

broodrearing <- tbl(conn, "mall_broodrearing_classified_IncompleteDataBirdIncluded_15sep2026") %>%
  collect() %>%
  dplyr::select(c("birdid_year", "status_early_late"))  %>%
  rename(brood_status=status_early_late) %>%
  distinct()

broodrearing %>% group_by(brood_status) %>% count()

band_repro_output <- early_term %>%
  mutate(egg_laying_status = ifelse(egg_laying_status == "nested", 1, 0)) %>%
  left_join(incubation %>%
          filter(incubation_status != "defer") %>%
          mutate(incubation_status = ifelse(incubation_status == "hatched", 1, 0))) %>%
  left_join(broodrearing %>%
              mutate(brood_status = ifelse(brood_status == "successful_brood_early_successful_brood_late", 1, 0))) %>%
  mutate(
    bandnum = gsub("-", "", sub("_\\d{4}$", "", birdid_year)),
    year = str_extract(birdid_year, "\\d{4}$")
  ) %>%
  dplyr::select(-birdid_year)
            

period_behaviour_repro <- cleaned_df %>%
  mutate(year = as.character(year)) %>%
  dplyr::select(bandnum, year, band_year, status, UTC_date, season, mean_ODBA:num_feed_fixes_corrected) %>%
  left_join(band_repro_output, by = c("bandnum", "year"))

# so, I now have my summarized behaviour and repro metrics per day for all my birds (hopefully)

length(unique(period_behaviour_repro$band_year)) # still at 1676 bird year instances

#-------------------------------------------------------------------------#
###### adding weather for ODBA and PTF model #############
#-------------------------------------------------------------------------#

library(weathermetrics) 


precip <- tbl(conn, "hourly_precip_gps") %>%
  collect() %>%
  dplyr::select(bandnum, UTC_date, precip, year) %>%
  group_by(bandnum, UTC_date) %>%
  summarize(sum_precip = sum(precip), 
            
            # converting to mm bc ERA5 is in m
            
            sum_precip_mm = sum_precip*1000)

temp <- tbl(conn, "hourly_temp_gps") %>%
  collect() %>%
  group_by(bandnum, UTC_date) %>%
  mutate(temp_c = kelvin.to.celsius(t2m)) %>%
  summarize(temp_below_0=ifelse(any(temp_c<0), TRUE, FALSE),
            min_daily_temp = min(temp_c), 
            average_daily_temp = mean(temp_c), 
            max_temp = max(temp_c)) %>%
  mutate(temp_remained_below_0=ifelse(max_temp<0, TRUE, FALSE))

precip_temp <- merge(precip, temp, by = c("bandnum", "UTC_date")) %>%
  mutate(bandnum = as.character(bandnum))

# final data set for model of weather covariates on behaviour 

freeze_events <- period_behaviour_repro %>%
  left_join(precip_temp, by = c("bandnum", "UTC_date")) %>%
  group_by(bandnum, year, season, egg_laying_status, incubation_status, brood_status, status) %>%
  mutate(day_of_year = yday(UTC_date)) %>%
  filter(temp_remained_below_0==TRUE) %>%
  arrange(bandnum, year, UTC_date) %>% 
  group_by(bandnum, year, season) %>%
  mutate(
    # Create a 'run_id' to mark the start of a new consecutive sequence
    run_id = cumsum(c(TRUE, diff(day_of_year) > 1))
  ) %>%
  group_by(bandnum, year, season,run_id) %>%
  mutate(
    # Count the number of days within each consecutive sequence
    consecutive_days = n()
  ) %>%
  ungroup() %>% 
  filter(consecutive_days >= 5) %>%
  distinct(bandnum, year, season, run_id, consecutive_days, .keep_all = TRUE) %>% 
    mutate(num_of_5_days_freeze_events = round(consecutive_days/5, digits = 0)) %>%
  group_by(bandnum, year, season) %>%
  summarize(num_of_5_days_freeze_events = sum(num_of_5_days_freeze_events)) %>%
  filter(season == "winter")


precip_temp_behaviour <- period_behaviour_repro %>%
  left_join(precip_temp, by = c("bandnum", "UTC_date")) %>%
  group_by(bandnum, year, season, egg_laying_status, incubation_status, brood_status, status) %>%
  summarize(mean_ODBA_season = mean(mean_ODBA, na.rm = TRUE), 
            feed_fixes_corrected_season = sum(num_feed_fixes_corrected, na.rm = TRUE),
            total_acc_fixes_corrected_season = sum(num_fixes, na.rm = TRUE), 
            mean_daily_precip_season = mean(sum_precip_mm, na.rm = TRUE), 
            mean_min_daily_temp_season = mean(min_daily_temp, na.rm = TRUE), 
            mean_daily_temp_season = mean(average_daily_temp, na.rm = TRUE)) %>%
  left_join(freeze_events, by = c("bandnum", "year", "season")) %>%
  mutate(num_of_5_days_freeze_events = ifelse(is.na(num_of_5_days_freeze_events), 0, num_of_5_days_freeze_events), 
         band_year = paste(bandnum, year, sep = "_"))

hist(precip_temp_behaviour$mean_daily_precip_season)
hist(precip_temp_behaviour$mean_min_daily_temp_season)
hist(precip_temp_behaviour$mean_daily_temp_season)
hist(precip_temp_behaviour$num_of_5_days_freeze_events)

length(unique(precip_temp_behaviour$band_year)) # still 1676 after adding weather data

#-------------------------------------------------------------------------#
###### adding breeding area to incubation dataset #############
#-------------------------------------------------------------------------#

# data <- inc %>%
#   mutate(year = year(inc_start),
#          
#          # based on Roberts et al., 2023
#          
#          east_86 = ifelse(nest_long < -86, "No", "Yes")) 
# 
# # Load US shapefile and transform 
# us_shapefile <- states(class = "sf") %>%
#   st_transform(crs = 4326)
# 
# # Convert GPS points dataframe to sf points
# gps_points_sf <- st_as_sf(data, coords = c("nest_long", "nest_lat"), crs = 4326)
# 
# # Spatial join to find which points intersect with US shapefile
# joined_data <- st_join(gps_points_sf, us_shapefile, join = st_intersects)
# 
# 
# joined_data_breedingpop <- joined_data %>%
#   mutate(
#     country = ifelse(!is.na(NAME), "US", "Canada")
#   ) %>%
#   group_by(birdid_year, year) %>%
#   mutate(breeding_area = 
#            case_when(STUSPS %in% c("CT", "DE", "MA", "MD", "NJ", "NY", "PA", "VA", "RI", "NH", "NC", "SC", "VT", "WV") ~ "AFWBS", 
#                      STUSPS == "ME" | is.na(NAME) == TRUE & east_86 == "Yes" ~ "ESA", 
#                      TRUE ~ "outside"
#            )) %>%
#   st_drop_geometry() %>%
#   dplyr::select(birdid_year, breeding_area)
# 
# inc_breedingpop <- left_join(data, joined_data_breedingpop, by = c("birdid_year", "year")) %>%
#   mutate(birdid = sub("_\\d{4}$", "", birdid_year)) %>%
#   mutate(bandnum = as.character(gsub("-", "", birdid)), 
#          year = as.character(year)) %>%
#   dplyr::select(bandnum, year, breeding_area)
# 
# ## ADDING IN BREEDING LOCATION 
# 
# precip_temp_behaviour_area <- precip_temp_behaviour %>%
#   left_join(inc_breedingpop, by = c("bandnum", "year"))

## double check
# 
# min_long <- min(inc_breedingpop$nest_long) - 1
# max_long <- max(inc_breedingpop$nest_long) + 1
# min_lat <- min(inc_breedingpop$nest_lat) - 1
# max_lat <- max(inc_breedingpop$nest_lat) + 1
# 
# # this is creating the bounds of the graph
# bbox <- c(left = min_long,
#           bottom = min_lat,
#           right = max_long,
#           top = max_lat)
# 
# # using the bounds, pick a design and a zoom. The higher the zoom, the more detailed yet longer to run
# 
# ggmap::register_stadiamaps("a086ba11-dbb4-4636-bdac-0beb061ee470")
# 
# mapa <- get_stadiamap(bbox = bbox, maptype = "alidade_smooth", zoom = 5)
# 
# ggmap(mapa) #make sure it worked
# 
# ggmap(mapa) +
#   geom_point(data = inc_breedingpop,
#              aes(x = nest_long, y = nest_lat, color = breeding_area),
#              size = 2.5, alpha = 0.8) +
#   scale_color_brewer(palette = "Dark2") +
#   labs(color = "Breeding Area")  +
#   guides(
#     color = guide_legend(direction = "horizontal")  ) +
#   theme(
#     legend.position = c(0.75, 0.88),
#     legend.background = element_rect(fill = "white", color = "black"),
#     legend.text = element_text(size = 6),
#     legend.title = element_text(size = 7)
#   )

#-------------------------------------------------------------------------#
###### calculating age #############
#-------------------------------------------------------------------------#

age_data <- readxl::read_excel("data/mall_9_17_2026.xlsx", 
                                 sheet = "MALL") %>%
  janitor::clean_names() %>%
  mutate(year = as.character(year(banding_date))) %>%
  dplyr::select(bandnum = band_number, age, year) %>%
  mutate(bandnum = as.character(gsub("-", "", bandnum)))
  
# most of the birds are going to be ASY EXCEPT for those banded as an SY. So, I can make all the NA values ASY 

precip_temp_behaviour_age <- precip_temp_behaviour %>%
  left_join(age_data, by = c("bandnum", "year")) %>%
  mutate(age = ifelse(is.na(age) == TRUE, "ASY", age))

length(unique(precip_temp_behaviour_age$band_year)) # still 1676 after adding weather data

#-------------------------------------------------------------------------#
###### migratory status #############
#-------------------------------------------------------------------------#

status <- tbl(conn, "ecologicalperiods") %>%
  collect() %>%
  dplyr::select(bandnum, status, year, partial_status) %>%
  distinct() %>%
  mutate(bandnum = as.character(bandnum), 
         year = as.factor(year),
    mig_status = case_when(status == "partial_migrant" & partial_status == "spring_migration_fall_quantile" ~ "Yes", 
                                status == "partial_migrant" & partial_status == "quantile" ~ "No", 
                                status == "resident" ~ "No", 
                                status == "migratory" ~ "Yes"))

precip_temp_behaviour_age_mig <- precip_temp_behaviour_age %>%
  left_join(status %>%
              dplyr::select(bandnum, year, mig_status), by = c("bandnum", "year")) %>%
  group_by(bandnum, year) %>%
  mutate(
    mig_status = case_when(
      is.na(mig_status) & any(season == "nesting", na.rm = TRUE) ~ "No",
      TRUE ~ mig_status
    )
  ) %>%
  ungroup()

#-------------------------------------------------------------------------#
###### calculating arrival date #############
#-------------------------------------------------------------------------#

# arrival date for migratory birds and partial migrants
# resident birds and unknown will not get an arrival date

arrival_date <- tbl(conn, "ecologicalperiods") %>%
  collect() %>%
  dplyr::select(bandnum, UTC_date, status, season = season2, year, partial_status) %>%
  distinct() %>%
  
  # only filtering birds that took a spring migration 
  
  filter(status == "migratory" |
         (status == "partial_migrant" & partial_status == "spring_migration_fall_quantile")) %>%
  mutate(bandnum = as.character(bandnum)) %>%
  
  # pulling out the very first date that these individuals had early_breeding (which is the arrival date)
  
  filter(season == "early_breeding") %>%
  group_by(bandnum, year) %>%
  slice_min(UTC_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(arrival_date = "UTC_date")  %>%
  mutate(bandnum = as.character(gsub("-", "", bandnum)), 
         year = as.character(year), 
         julian_arrival = yday(arrival_date)) %>%
  dplyr::select(bandnum, julian_arrival, year)

hist(arrival_date$julian_arrival)

## adding in nest location from incubation dataset

nest_location <- inc %>%
  dplyr::select(band_year, nest_lat, nest_long) %>%
  separate(band_year, into = c("bandnum", "year"))

precip_temp_behaviour_age_mig_arrival <- precip_temp_behaviour_age_mig %>%
  left_join(arrival_date, by = c("bandnum", "year")) %>%
  left_join(nest_location, by = c("bandnum", "year"))

length(unique(precip_temp_behaviour_age_mig_arrival$band_year)) # still have 1676 bird years!

# at this point here is what is in my data frame
# - all reproductive statuses
# - subseason precipitation and minimum temperature
# - nesting propensity covariates: arrival date, age, ODBA, PTF
# - incubation covariates: migratory status, breeding area

# what do I need?
# - habitat for weather model 
# - habitat for incubation and brood rearing model

#-------------------------------------------------------------------------#
### bring in all GPS data, assign a season, and calculate habitat ########
#-------------------------------------------------------------------------#

# bringing in all GPS points from the migration data set (it should have all, but technically not filtered to the correct season)

# also need

ecological_periods <- tbl(conn, "ecologicalperiods") %>%
  collect() %>%
  
  # to best match up with birds that were just banded, starting all birds at Jan 15th if possible, ending Sep 15th
  
  filter(julian_date >= 15, 
         julian_date <= 258) %>%
  dplyr::select(bandnum, year, UTC_datetime, UTC_date, Latitude, Longitude) %>%
  mutate(bandnum = as.character(bandnum), 
         year = as.character(year))

# next, we need to assign each GPS point a habitat
# load Erika's landcover/wetland layer

habitat_raster <- raster::raster("nalcms_updated.tif")

# Convert GPS points to sf object

gps.sf <- ecological_periods %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(habitat_raster))
  
# extract landcover values  for GPS locations 

gps.sf$habitat <- raster::extract(habitat_raster, gps.sf)

### overlapping points with urban / rural city centers

### US DATA
# For the 2020 Census, an urban area will comprise a densely settled core of census blocks that meet minimum housing unit density and/or population density requirements. This includes adjacent territory containing non-residential urban land uses. To qualify as an urban area, the territory identified according to criteria must encompass at least 2,000 housing units or have a population of at least 5,000.
# https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural.html

us_data <- st_read("data/tl_2023_us_uac20") %>%
  st_transform(4326) %>%
  mutate(country = "US")

# CANADA DATA
# A population centre (POPCTR) has a population of at least 1,000 and a population density of 400 persons or more per square kilometre, based on population counts from the current Census of Population. All areas outside population centres are classified as rural areas.
# https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=geo049a

canada_data <- st_read("data/lpc_000b21a_e") %>%
  st_transform(4326) %>%
  mutate(country = "Canada")

all_data <- bind_rows(us_data, canada_data) %>%
  st_transform(crs = st_crs(habitat_raster)) %>%
  dplyr::select(country, geometry)

gps_habitat <- gps.sf %>%
  st_join(all_data) %>%
  mutate(city_center = ifelse(is.na(country), 0, 1)) %>% 
  mutate(Landcover = case_when(
    habitat == 0 ~ "water", 
    habitat %in% 1:6 ~ "forest",
    habitat %in% 7:8 ~ "shrubland",
    habitat %in% 9:12 ~ "grassland",
    
    # Wetlands stay separate
    habitat == 14 ~ "wetland",
    habitat %in% c(20, 21, 22) ~ "peatland",
    habitat == 23 ~ "swamp",
    habitat == 24 ~ "marsh",
    habitat == 25 ~ "open_shallow_water",
    
    habitat == 15 ~ "cropland", 
    habitat %in% c(13,16) ~ "barren",
    habitat == 17 ~ "urban",
    habitat == 18 ~ "water",
    habitat == 19 ~ "snow_ice",
    
    TRUE ~ NA_character_
  )) %>%
  st_drop_geometry() 

# now that we have habitat data, this needs to be combined with the season data to get proportions

dates_seasons <- period_behaviour_repro %>%
  dplyr::select(bandnum, year, UTC_date, season)

gps_habitat_seasons <- gps_habitat %>%
  left_join(dates_seasons, by = c("bandnum", "UTC_date", "year")) %>%
  filter(!is.na(season))

season_missing_gps <- gps_habitat_seasons %>%
  group_by(bandnum, year, season) %>%
  summarise(
    total_rows = n(),
    unique_days = n_distinct(UTC_date),
    total_potential_gps = unique_days *24, 
    prop_data = total_rows / total_potential_gps, 
    .groups = 'drop')

valid_seasons <- season_missing_gps %>%
  filter(prop_data >= 0.5)

# Filter your original dataframe to keep only those valid seasons
cleaned_df <- gps_habitat_seasons %>%
  inner_join(
    valid_seasons %>% dplyr::select(bandnum, year, season), 
    by = c("bandnum", "year", "season")
  )

habitat_proportions_wide <- cleaned_df %>%
  dplyr::select(-city_center, -country) %>%
  group_by(bandnum, year, season) %>%
  count(bandnum, year, season, Landcover) %>%
  group_by(bandnum, year, season) %>%
  mutate(
    proportion = n / sum(n)
  )  %>%
  dplyr::select(bandnum, year, season, Landcover, proportion) %>%
  tidyr::pivot_wider(
    names_from = Landcover,
    values_from = proportion,
    values_fill = 0
  )

city_center_prop <- cleaned_df %>%
  group_by(bandnum, year, season) %>%
  summarise(
    city_center_proportion = mean(city_center, na.rm = TRUE), 
    .groups = "drop"
  )

precip_temp_behaviour_age_mig_arrival_habitat <- habitat_proportions_wide %>%
  left_join(city_center_prop, by = c("bandnum", "year", "season")) %>%
  left_join(precip_temp_behaviour_age_mig_arrival, by = c("bandnum", "year", "season"))

length(unique(precip_temp_behaviour_age_mig_arrival_habitat$band_year)) # now at 1647 once we filtered out some seasons with lack of GPS data
summary(precip_temp_behaviour_age_mig_arrival_habitat)

dbWriteTable(conn, "fac_covariates_2022_sep2026", precip_temp_behaviour_age_mig_arrival_habitat, overwrite = TRUE, append = FALSE, row.names = FALSE)

write.csv(precip_temp_behaviour_age_mig_arrival_habitat, "data/fac_covariates_2022_sep2026.csv")

test <- read_csv("data/fac_covariates_2022_sep2026.csv")


# at this point here is what is in my data frame
# - all reproductive statuses
# - subseason precipitation and minimum temperature
# - nesting propensity covariates: arrival date, age, ODBA, PTF
# - incubation covariates: migratory status, breeding area
# - habitat for weather model 
# - habitat for incubation and brood rearing model
# - location of nest

### looking into winter habitat ###

winter <- precip_temp_behaviour_age_mig_arrival_habitat %>%
  filter(season %in% c("winter", "late_winter")) %>%
  dplyr::select(bandnum, year, season, cropland:barren) %>%
  pivot_longer(cols = cropland:barren, 
               names_to = "habitat", 
               values_to = "proportion") %>%
  group_by(season, habitat) %>%
  summarize(
    mean_prop = mean(proportion, na.rm = TRUE),
    se = sd(proportion, na.rm = TRUE) / sqrt(n()),
    # 95% confidence interval using the t-distribution
    ci = se * qt(0.975, df = max(1, n() - 1)),
    lower = mean_prop - ci,
    upper = mean_prop + ci,
    .groups = "drop")

ggplot(winter, aes(x = habitat, y = mean_prop)) + 
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  facet_wrap(~season) +
  theme_bw() +
  labs(
    y = "Mean Proportion",
    x = "Habitat"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####### checking for colinearity ######

data_scaled <- precip_temp_behaviour_age_mig_arrival_habitat %>% 
  ungroup() %>%
  mutate(total_acc_fixes_corrected_season = as.numeric(total_acc_fixes_corrected_season),
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
         age=as.factor(age),
         status=as.factor(status), 
         log_odba = log(mean_ODBA_season))

# set reference category for age and flyway
data_scaled$age <- relevel(data_scaled$age, ref = "SY")
data_scaled$status<- relevel(data_scaled$status, ref = "resident")

library(lmerTest)
library(performance)

# model 1: effects of prebreeding weather and habitat use on prebreeding behaviour
model1_odba <- lmer(log_odba ~ scaled_freeze_events + scaled_min_temp*scaled_mean_precip + 
                   scaled_prop_urban + scaled_prop_cropland + scaled_prop_swamp + 
                     scaled_prop_marsh + scaled_prop_open_water + (1|season), 
                    data = data_scaled)

summary(model1_odba)
check_collinearity(model1_odba)

model1_ptf <- glmer(cbind(feed_fixes_corrected_season, total_acc_fixes_corrected_season) ~ scaled_freeze_events + scaled_min_temp*scaled_mean_precip + 
                      scaled_prop_urban + scaled_prop_cropland + scaled_prop_swamp + 
                      scaled_prop_marsh + scaled_prop_open_water + (1|season), 
                    data = data_scaled,
                    family = binomial(link = "logit"))

summary(model1_ptf)
check_collinearity(model1_ptf)

# model 2: effects of prebreeding behaviour, migration characteristics, mercury exposure 
#           and age on nesting propensity

# summarize data for input to model
data_scaled$prop_feed <- data_scaled$feed_fixes_corrected_season / data_scaled$total_acc_fixes_corrected_season
data_scaled$scaled_prop_feed <- as.numeric(scale(data_scaled$prop_feed))
data_scaled$scaled_odba <- as.numeric(scale(data_scaled$log_odba))


model2a <- glmer(egg_laying_status ~  scaled_odba + scaled_prop_feed + (1|season), 
                 data = data_scaled,
                 family = binomial(link = "logit"))
summary(model2a)
check_collinearity(model2a)

model2b <- glmer(egg_laying_status ~  scaled_odba +  scaled_prop_feed + scaled_first_day + age + (1|season), 
                 data = data_scaled,
                 family = binomial(link = "logit"))
summary(model2b)
check_collinearity(model2b)

# Create a custom function to find the most common class
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)]) # Remove NA values and get unique items
  ux[which.max(tabulate(match(x, ux)))]
}

# reduce non-time varying variables to only one row per band-year
non_time_varying_variables <- data_scaled %>% group_by(bandnum, year) %>%
  summarize(egg_laying_status=mean(egg_laying_status),
            scaled_first_day=mean(scaled_first_day),
            age=get_mode(age))

model2c <- glm(egg_laying_status ~  scaled_first_day + 
                 age, 
               data = non_time_varying_variables,
               family = binomial(link = "logit"))

summary(model2c)
check_collinearity(model2c)

# model 3: effects of habitat use and flyway on nesting success
model3 <- glm(incubation_status ~ scaled_prop_cropland + scaled_prop_peatland +
                scaled_prop_swamp + scaled_prop_marsh  + nest_lat + nest_long + status, 
              data = data_scaled %>%
                filter(season == "nesting", 
                       status %in% c("resident", "migratory")),
              family = binomial(link = "logit"))

library(FRK)
test <- data_scaled %>%
  filter(season == "nesting", 
         status %in% c("resident", "migratory"))

G <- auto_basis(data = test[,c("nest_long","nest_lat")] %>%
                  SpatialPoints(),           # To sp obj
                nres = 1,                         # One resolution
                type = "Gaussian")  


S <- eval_basis(basis = G,                       # basis functions
                s = test[,c("nest_long","nest_lat")] %>%
                  as.matrix()) %>%            # conv. to matrix
  as.matrix()                                 # conv. to matrix

colnames(S) <- paste0("B", 1:ncol(S)) # assign column names

spatial <- cbind(test,S) 

a <- glm(incubation_status ~ scaled_prop_cropland + scaled_prop_peatland +
                scaled_prop_swamp + scaled_prop_marsh  + nest_lat + nest_long + status + B1 +
           B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + B12 + B13 + B14 + B15 + B16 + B17 + B18, 
              data = spatial,
              family = binomial(link = "logit"))

summary(a)   

# can't do the interaction  

summary(model3)
check_collinearity(model3)

nest <- data_scaled %>%
  filter(!is.na(nest_lat) == TRUE)

ggplot(nest, aes(x = nest_lat, y = nest_long)) +
  geom_point()

# model 4: effects of weather and habitat use on nbrood success
model4 <- glm(brood_status ~ scaled_min_temp + scaled_mean_precip  + scaled_prop_swamp + scaled_city + scaled_prop_marsh + scaled_prop_open_water, 
              data = data_scaled %>%
                filter(season == "brood_rearing"),
              family = binomial(link = "logit"))

summary(model4)
check_collinearity(model4)

