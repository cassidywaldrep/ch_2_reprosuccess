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
  ungroup()

brood <- read_csv("results/all_broodrearing_successful_failed_ch2_updatedsep2026.csv") %>%
  group_by(band_year) %>%
  
  # a 2026 bird had two successes 
  
  arrange(brood_start) %>%
  slice_head(n = 1) %>%
  ungroup()

#-------------------------------------------------------------------------#
###### summarizing period data #############
#-------------------------------------------------------------------------#

#### first, get dates for sub-seasons, so you can summarize all variables by sub-season ####
# 1 = winter (Jan 15 (or whenever captured) until 2 weeks before the start of spring migration) - for non-migratory, continue winter until early breeding
#	2 = late winter (2 weeks prior to spring migration) - do not include for non-migratory birds
#	3 = early migration (use each individual’s median migration date to split migration in half) - do not include for non-migratory birds
#	4 = late migration - do not include for non-migratory birds
# 5 = early breeding (first two weeks after arriving on breeding grounds) - for non-migratory, do two weeks before the start of nesting if bird nests; otherwise, do two weeks before the mean nesting start date of non-migratory individuals
#	6 = nesting (nesting period; use nesting dates ) 
#	7 = brood rearing (brood period)

# I need to take the average PTF and ODBA for each season...winter, late_winter, early spring migration, late spring migration, and early_breeding
# First, I think it will be best to label all the dates for all mallards and the season that they're in. For nesting / brood rearing, I need to bring that in

ecological_periods <- tbl(conn, "ecologicalperiods") %>%
  collect() %>%
  
  # to best match up with birds that were just banded, starting all birds at Jan 15th if possible, ending Sep 15th
  
  filter(julian_date >= 15, 
         julian_date <= 258) %>%
  dplyr::select(bandnum, UTC_date, status, season = season2, year, status, partial_status) %>%
  distinct() %>%
  mutate(bandnum = as.character(bandnum), 
         band_year = paste(bandnum, year, sep = "_")) 

mean_nest_start_date <- ecological_periods %>%
  left_join(inc %>% dplyr::select(band_year, incubation_status, inc_start, inc_end), by = "band_year") %>%
  filter(status %in% c("resident", "unknown") | (status == "partial_migrant" & partial_status == "quantile")) %>%
  distinct(bandnum, year, status, inc_start) %>%
  filter(!is.na(inc_start)) %>%
  group_by(year) %>%
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
    
    season = case_when(
      !is.na(inc_start) & UTC_date >= inc_start & UTC_date <= inc_end ~ "nesting",
      !is.na(brood_start) & UTC_date >= brood_start & UTC_date <= brood_end ~ "brood_rearing",
      is_special & !is.na(inc_start) & UTC_date >= (inc_start - 14) & UTC_date < inc_start ~ "early_breeding",
      is_special & is.na(inc_start) & !is.na(mean_start) & UTC_date >= (mean_start - 14) & UTC_date < mean_start ~ "early_breeding",
      is_special & !is.na(inc_end) & UTC_date > inc_end ~ "after_breeding",
      is_special & is.na(inc_end) & !is.na(mean_start) & UTC_date > mean_start ~ "after_breeding",
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
  dplyr::select(bandnum, UTC_date, status, season, year, incubation_status, brood_reaing_status)

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

daily_ptf_odba_season_status <- ecological_periods_final %>%
 left_join(daily_ptf_odba, by = c("bandnum", "UTC_date", "year"))

# calculating the amount of missing data per season...only keeping > 70%

season_missing_proportions <- daily_ptf_odba_season_status %>%
  group_by(bandnum, year, season) %>%
  summarise(
    total_records = n(),
    missing_count = sum(is.na(mean_ODBA)),
    prop_missing = missing_count / total_records,
    .groups = "drop"
  )

# Identify which seasons have 20% or less missing data
valid_seasons <- season_missing_proportions %>%
  filter(prop_missing <= 0.2)

# Filter your original dataframe to keep only those valid seasons
cleaned_df <- daily_ptf_odba_season_status %>%
  inner_join(
    valid_seasons %>% dplyr::select(bandnum, year, season), 
    by = c("bandnum", "year", "season")
  )


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
  dplyr::select(bandnum, year, status, UTC_date, season, mean_ODBA:num_feed_fixes_corrected) %>%
  left_join(band_repro_output, by = c("bandnum", "year"))

# so, I now have my summarized behaviour and repro metrics per day for all my birds (hopefully)

#-------------------------------------------------------------------------#
###### adding weather for ODBA and PTF model #############
#-------------------------------------------------------------------------#

library(weathermetrics) 

precip <- tbl(conn, "hourly_precip_gps") %>%
  collect() %>%
  dplyr::select(bandnum, UTC_date, precip, year) %>%
  group_by(bandnum, UTC_date) %>%
  summarize(sum_precip = sum(precip), 
            sum_precip_mm = sum_precip*1000)

temp <- tbl(conn, "hourly_temp_gps") %>%
  collect() %>%
  group_by(bandnum, UTC_date) %>%
  mutate(temp_c = kelvin.to.celsius(t2m)) %>%
  summarize(min_daily_temp = min(temp_c), 
            average_daily_temp = mean(temp_c)) %>%
  mutate(freeze = ifelse(average_daily_temp < 0, 1, 0))

precip_temp <- merge(precip, temp, by = c("bandnum", "UTC_date")) %>%
  mutate(bandnum = as.character(bandnum))

# final data set for model of weather covariates on behaviour 

# CHECK WHY THERE MAY BE PRECIP NAS 
precip_temp_behaviour <- period_behaviour_repro %>%
  left_join(precip_temp, by = c("bandnum", "UTC_date")) %>%
  group_by(bandnum, year, season, egg_laying_status, incubation_status, brood_status, status) %>%
  summarize(mean_ODBA_season = mean(mean_ODBA, na.rm = TRUE), 
            feed_fixes_corrected_season = sum(num_feed_fixes_corrected, na.rm = TRUE),
            total_acc_fixes_corrected_season = sum(num_fixes, na.rm = TRUE), 
            mean_daily_precip_season = mean(sum_precip_mm, na.rm = TRUE), 
            mean_min_daily_temp_season = mean(min_daily_temp, na.rm = TRUE), 
            mean_daily_temp_season = mean(average_daily_temp, na.rm = TRUE), 
            prop_freeze_days = mean(freeze, na.rm = TRUE))


#-------------------------------------------------------------------------#
###### adding breeding area to incubation dataset #############
#-------------------------------------------------------------------------#

data <- inc %>%
  mutate(year = year(inc_start),
         
         # based on Roberts et al., 2023
         
         east_86 = ifelse(nest_long < -86, "No", "Yes")) 

# Load US shapefile and transform 
us_shapefile <- states(class = "sf") %>%
  st_transform(crs = 4326)

# Convert GPS points dataframe to sf points
gps_points_sf <- st_as_sf(data, coords = c("nest_long", "nest_lat"), crs = 4326)

# Spatial join to find which points intersect with US shapefile
joined_data <- st_join(gps_points_sf, us_shapefile, join = st_intersects)


joined_data_breedingpop <- joined_data %>%
  mutate(
    country = ifelse(!is.na(NAME), "US", "Canada")
  ) %>%
  group_by(birdid_year, year) %>%
  mutate(breeding_area = 
           case_when(STUSPS %in% c("CT", "DE", "MA", "MD", "NJ", "NY", "PA", "VA", "RI", "NH", "NC", "SC", "VT", "WV") ~ "AFWBS", 
                     STUSPS == "ME" | is.na(NAME) == TRUE & east_86 == "Yes" ~ "ESA", 
                     TRUE ~ "outside"
           )) %>%
  st_drop_geometry() %>%
  dplyr::select(birdid_year, breeding_area)

inc_breedingpop <- left_join(data, joined_data_breedingpop, by = c("birdid_year", "year")) %>%
  mutate(birdid = sub("_\\d{4}$", "", birdid_year)) %>%
  mutate(bandnum = as.character(gsub("-", "", birdid)), 
         year = as.character(year)) %>%
  dplyr::select(bandnum, year, breeding_area)

## ADDING IN BREEDING LOCATION 

precip_temp_behaviour_area <- precip_temp_behaviour %>%
  left_join(inc_breedingpop, by = c("bandnum", "year"))

## double check

min_long <- min(inc_breedingpop$nest_long) - 1
max_long <- max(inc_breedingpop$nest_long) + 1
min_lat <- min(inc_breedingpop$nest_lat) - 1
max_lat <- max(inc_breedingpop$nest_lat) + 1

# this is creating the bounds of the graph
bbox <- c(left = min_long,
          bottom = min_lat,
          right = max_long,
          top = max_lat)

# using the bounds, pick a design and a zoom. The higher the zoom, the more detailed yet longer to run

ggmap::register_stadiamaps("a086ba11-dbb4-4636-bdac-0beb061ee470")

mapa <- get_stadiamap(bbox = bbox, maptype = "alidade_smooth", zoom = 5)

ggmap(mapa) #make sure it worked

ggmap(mapa) +
  geom_point(data = inc_breedingpop,
             aes(x = nest_long, y = nest_lat, color = breeding_area),
             size = 2.5, alpha = 0.8) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Breeding Area")  +
  guides(
    color = guide_legend(direction = "horizontal")  ) +
  theme(
    legend.position = c(0.75, 0.88),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7)
  )

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

precip_temp_behaviour_area_age <- precip_temp_behaviour_area %>%
  left_join(age_data, by = c("bandnum", "year")) %>%
  mutate(age = ifelse(is.na(age) == TRUE, "ASY", age))


#-------------------------------------------------------------------------#
###### calculating arrival date #############
#-------------------------------------------------------------------------#

# arrival date for migratory birds and partial migrants
# resident birds and unknown will not get an arrival date

arrival_date <- tbl(conn, "ecologicalperiods") %>%
  collect() %>%
  dplyr::select(bandnum, UTC_date, status, season = season2, year, partial_status) %>%
  distinct() %>%
  filter(status == "migratory" |
         (status == "partial_migrant" & partial_status == "spring_migration_fall_quantile")) %>%
  mutate(bandnum = as.character(bandnum)) %>%
  filter(season == "early_breeding") %>%
  # Group by bird and year
  group_by(bandnum, year) %>%
  # Find the row with the minimum (earliest) UTC_date for each group
  slice_min(UTC_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(arrival_date = "UTC_date")  %>%
  mutate(bandnum = as.character(gsub("-", "", bandnum)), 
         year = as.character(year), 
         julian_arrival = yday(arrival_date)) %>%
  dplyr::select(bandnum, julian_arrival, year)

precip_temp_behaviour_area_age_arrival <- precip_temp_behaviour_area_age %>%
  left_join(arrival_date, by = c("bandnum", "year"))

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
habitat_raster <- raster("nalcms_updated.tif")


# Convert GPS points to sf object
gps.sf <- ecological_periods %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(habitat_raster))

#Create legend for habitat raster
nalc_legend <- data.frame(
  Landcover = 0:25,
  class = c("Water", 
            "Temperate or sub-polar needleleaf forest", "Sub-polar taiga needleleaf forest",
            "Tropical or sub-tropical broadleaf evergreen forest", "Tropical or sub-tropical broadleaf deciduous forest",
            "Temperate or sub-polar broadleaf deciduous forest", "Mixed Forest",
            "Tropical or sub-tropical shrubland", "Temperate or sub-polar shrubland",
            "Tropical or sub-tropical grassland", "Temperate or sub-polar grassland",
            "Sub-polar or polar shrubland-lichen-moss", "Sub-polar or polar grassland-lichen-moss",
            "Sub-polar or polar barren-lichen-moss", "Wetland", "Cropland",
            "Barren lands", "Urban", "Water", "Snow and Ice", "Bog", "Fen",
            "Peatland", "Swamp", "Marsh", "Open shallow waters"
  ))
  
  
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
  
  # some of the seasons are NA, that's okay. It means that they are either winter or summer, or were part of a bird where that season was removed due to missing data
  
  filter(!is.na(season))

habitat_proportions_wide <- gps_habitat_seasons %>%
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

city_center_prop <- gps_habitat_seasons %>%
  group_by(bandnum, year, season) %>%
  summarise(
    city_center_proportion = mean(city_center, na.rm = TRUE), 
    .groups = "drop"
  )

precip_temp_behaviour_area_age_arrival_habitat <- habitat_proportions_wide %>%
  left_join(city_center_prop, by = c("bandnum", "year", "season")) %>%
  left_join(precip_temp_behaviour_area_age_arrival, by = c("bandnum", "year", "season"))

# at this point here is what is in my data frame
# - all reproductive statuses
# - subseason precipitation and minimum temperature
# - nesting propensity covariates: arrival date, age, ODBA, PTF
# - incubation covariates: migratory status, breeding area
# - habitat for weather model 
# - habitat for incubation and brood rearing model

precip_temp_behaviour_area_age_arrival_habitat %>%
  ungroup() %>%
  dplyr::select(bandnum, year, breeding_area) %>%
  distinct() %>%
  count(breeding_area)

ggplot(precip_temp_behaviour_area_age_arrival_habitat %>%
         filter(!is.na(incubation_status), 
                season == "early_breeding"), 
       aes(x = city_center_proportion, y = incubation_status)) +
  geom_point() + 
  geom_smooth()


####### checking for colinearity ######

data_scaled <- precip_temp_behaviour_area_age_arrival_habitat %>% 
  ungroup() %>%
  mutate(total_acc_fixes_corrected_season = as.numeric(total_acc_fixes_corrected_season),
      scaled_mean_precip = as.numeric(scale(mean_daily_precip_season)), 
         scaled_min_temp = as.numeric(scale(mean_min_daily_temp_season)), 
         scaled_prop_freeze_events = as.numeric(scale(prop_freeze_days)), 
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
         breeding_area=as.factor(breeding_area),
         status=as.factor(status), 
         log_odba = log(mean_ODBA_season))

# set reference category for age and flyway
data_scaled$age <- relevel(data_scaled$age, ref = "SY")
data_scaled$breeding_area <- relevel(data_scaled$breeding_area, ref = "ESA")
data_scaled$status<- relevel(data_scaled$status, ref = "resident")

library(lmerTest)
library(performance)

# model 1: effects of prebreeding weather and habitat use on prebreeding behaviour
model1_odba <- lmer(log_odba ~ scaled_prop_freeze_events + scaled_min_temp + scaled_mean_precip + 
                   + scaled_prop_urban + scaled_prop_cropland + scaled_prop_swamp + 
                     scaled_prop_marsh + scaled_prop_open_water + (1|season), 
                    data = data_scaled)

check_collinearity(model1_odba)

model1_ptf <- glmer(cbind(feed_fixes_corrected_season, total_acc_fixes_corrected_season) ~ scaled_prop_freeze_events + scaled_min_temp + scaled_mean_precip + 
                      + scaled_prop_urban + scaled_prop_cropland + scaled_prop_swamp + 
                      scaled_prop_marsh + scaled_prop_open_water + (1|season), 
                    data = data_scaled,
                    family = binomial(link = "logit"))

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

check_collinearity(model2a)

model2b <- glmer(egg_laying_status ~  scaled_odba +  scaled_prop_feed + scaled_first_day + age + (1|season), 
                 data = data_scaled,
                 family = binomial(link = "logit"))

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

check_collinearity(model2c)

# model 3: effects of habitat use and flyway on nesting success
model3 <- glm(incubation_status ~ scaled_prop_cropland + scaled_prop_peatland +
                scaled_prop_swamp + scaled_prop_marsh + scaled_city + 
                breeding_area + status, 
              data = data_scaled %>%
                filter(season == "nesting", 
                       status %in% c("resident", "migratory")),
              family = binomial(link = "logit"))

check_collinearity(model3)

# model 4: effects of weather and habitat use on nbrood success
model4 <- glm(brood_status ~ scaled_min_temp + scaled_mean_precip  + scaled_prop_swamp + scaled_prop_marsh + scaled_prop_open_water, 
              data = data_scaled %>%
                filter(season == "brood_rearing"),
              family = binomial(link = "logit"))

check_collinearity(model4)
