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

# conn <- dbConnect(
#   Postgres(),
#   dbname = "acc_gps_data_mallards",
#   host = "localhost",
#   port = 5432,
#   user = "postgres",
#   password = "mallard_ducks"
# )

# proportion of time feeding and odba
#
# daily_activity <- tbl(conn, "all_daily_activity_budget_2022_sep2026_80%fixes") %>%
#   collect()
#
# gps data, migratory status, and periods
ecological_periods <- tbl(conn, "eight_season_status_dailydisplacement_2022_2026_updatedsep2026") %>%
  collect()


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


### reading in breeding data

brood <- read_csv("results/all_broodrearing_successful_failed_ch2_updatedsep2026.csv")

inc <- read_csv("results/incubation_dataset_successful_failed_dates_ch2_updatedsep2026.csv") %>%
  group_by(band_year) %>%
  
  # a 2026 bird had two successes 
  
  arrange(inc_start) %>%
  slice_head(n = 1) %>%
  ungroup()

#-------------------------------------------------------------------------#
###### GPS data, PTF, and ODBA #############
#-------------------------------------------------------------------------#

# I need to take the average PTF and ODBA for each season...winter, late_winter, early spring migration, late spring migration, and early_breeding

ecological_periods <- tbl(conn, "ecologicalperiods") %>%
  collect() %>%
  filter(julian_date >= 15, 
         julian_date <= 258) %>%
  dplyr::select(bandnum, UTC_date, status, season = season2, year, status) %>%
  distinct() %>%
  mutate(bandnum = as.character(bandnum), 
         band_year = paste(bandnum, year, sep = "_")) %>%
  
  # only keeping birds that attempted incubation (614 bird years
  
  filter(band_year %in% inc$band_year)

daily_ptf_odba <- tbl(conn, "daily_ptf_odba") %>%
  collect() %>%
  dplyr::select(bandnum, UTC_date = date, mean_ODBA, num_fixes, corrected_prop_feed) %>%
  
  mutate(num_feed_fixes_corrected = round(corrected_prop_feed * as.numeric(num_fixes)), 
         year = year(UTC_date), 
         bandnum = as.character(gsub("-", "", bandnum)), 
         band_year = paste(bandnum, year, sep = "_")) %>%
  
  # only keeping birds that attempted incubation (613 bird years - probably lost one without ACC data or something)
  
  filter(band_year %in% inc$band_year)

daily_ptf_odba_season_status <- ecological_periods %>%
  left_join(daily_ptf_odba, by = c("bandnum", "UTC_date", "band_year", "year")) 

# for unknown birds, residents, and partial migrants (quantile), need to update seasons
# according to Ilsa, they should have winter until two weeks before breeding which will then be called early breeding

# first, filtering out all birds with a spring migration
nonmigratory_status <- tbl(conn, "ecologicalperiods") %>%
  collect() %>%
  filter(julian_date >= 15, 
         julian_date <= 258) %>%
  dplyr::select(bandnum, status, year, partial_status) %>%
  filter(
    status %in% c("resident", "unknown") | 
      (status == "partial_migrant" & partial_status == "quantile")) %>%
  distinct() %>%
  mutate(band_year = paste(bandnum, year, sep = "_"))

nonmigratory_status_repro <- nonmigratory_status %>%
  filter(band_year %in% inc$band_year) %>%
  mutate(bandnum = as.character(bandnum)) %>%
  left_join(inc, by = "band_year") %>%
  dplyr::select(bandnum, year, incubation_status, inc_start, inc_end) 

# updating seasons 

daily_ptf_odba_season_status_update <- daily_ptf_odba_season_status %>%
  left_join(
    nonmigratory_status_repro, by = c("bandnum", "year")
  ) %>%
  mutate(
    original_season = season, # Save existing season for non-nesting birds
    
    UTC_date = ymd(UTC_date),
    inc_start = ymd(inc_start),
    inc_end = ymd(inc_end),
    
    # 2. Apply rules conditionally
    season = case_when(
      # If the bird is NOT in the nesting dataset, keep its original season
      is.na(inc_start) ~ original_season,
      
      # During incubation -> "nesting"
      UTC_date >= inc_start & UTC_date <= inc_end ~ "nesting",
      
      # After incubation ends -> "summer"
      UTC_date > inc_end ~ "summer",
      
      # Two weeks before incubation start (adjust label if needed, e.g., "winter" or another term)
      UTC_date >= (inc_start - days(14)) & UTC_date < inc_start ~ "early_breeding", # or update label
      
      # Default for all other dates for nesting birds -> "winter"
      TRUE ~ "winter"
    )
  ) %>%
  dplyr::select(-original_season) # Clean up temporary column

# unknown <- daily_ptf_odba_season_status_update %>%
#   filter(status == "resident")

# calculating the amount of missing data per season...only keeping > 70%

season_missing_proportions <- daily_ptf_odba_season_status_update %>%
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
cleaned_df <- daily_ptf_odba_season_status_update %>%
  inner_join(
    valid_seasons %>% dplyr::select(bandnum, year, season), 
    by = c("bandnum", "year", "season")
  )

# period_ptf_odba_status <- cleaned_df %>%
#   group_by(bandnum, breeding_year, season, status) %>%
#   summarize(mean_ODBA = mean(mean_ODBA, na.rm = TRUE), 
#             feed_fixes_corrected=sum(num_feed_fixes_corrected, na.rm = TRUE),
#             total_acc_fixes_corrected=sum(num_fixes, na.rm = TRUE))

#-------------------------------------------------------------------------#
###### adding weather for ODBA and PTF model #############
#-------------------------------------------------------------------------#

library(weathermetrics) 

precip <- tbl(conn, "daily_precip_gps") %>%
  collect() %>%
  dplyr::select(bandnum, UTC_date, precip)

temp <- tbl(conn, "hourly_temp_gps") %>%
  collect() %>%
  group_by(bandnum, UTC_date) %>%
  mutate(temp_c = kelvin.to.celsius(t2m)) %>%
  summarize(min_daily_temp = min(temp_c), 
            average_daily_temp = mean(temp_c))

precip_temp <- merge(precip, temp, by = c("bandnum", "UTC_date")) %>%
  mutate(bandnum = as.character(bandnum))

# final data set for model of weather covariates on behaviour 

# CHECK WHY THERE MAY BE PRECIP NAS 
precip_temp_behaviour <- cleaned_df %>%
  left_join(precip_temp, by = c("bandnum", "UTC_date")) %>%
  group_by(bandnum, year, season, status) %>%
  summarize(mean_ODBA_season = mean(mean_ODBA, na.rm = TRUE), 
            feed_fixes_corrected_season = sum(num_feed_fixes_corrected, na.rm = TRUE),
            total_acc_fixes_corrected_season = sum(num_fixes, na.rm = TRUE), 
            mean_precip_season = mean(precip, na.rm = TRUE), 
            mean_min_daily_temp_season = mean(min_daily_temp, na.rm = TRUE), 
            mean_daily_temp_season = mean(average_daily_temp, na.rm = TRUE))


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


# Per year, determine the percentage of the breeding season where a bird is in Canada vs the US

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
  mutate(bandnum = as.character(gsub("-", "", birdid))) %>%
  dplyr::select(bandnum, year, birdid_year, incubation_status, inc_start, inc_end, nest_lat, nest_long, breeding_area)


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
  mutate(year = year(banding_date)) %>%
  dplyr::select(bandnum = band_number, age, year) %>%
  mutate(bandnum = as.character(gsub("-", "", bandnum)))
  
# most of the birds are going to be ASY EXCEPT for those banded as an SY. So, I can make all the NA values ASY 

inc_breedingpop_age <- inc_breedingpop %>%
  left_join(age_data, by = c("bandnum", "year")) %>%
  mutate(age = ifelse(is.na(age) == TRUE, "ASY", age))


#-------------------------------------------------------------------------#
###### calculating arrival date #############
#-------------------------------------------------------------------------#

arrival_date <- tbl(conn, "ecologicalperiods") %>%
  collect() %>%
  dplyr::select(bandnum, UTC_date, status, season = season2, year, status) %>%
  distinct() %>%
  mutate(bandnum = as.character(bandnum)) %>%
  filter(season == "early_breeding") %>%
  # Group by bird and year
  group_by(bandnum, year) %>%
  # Find the row with the minimum (earliest) UTC_date for each group
  slice_min(UTC_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(arrival_date = "UTC_date")  %>%
  mutate(bandnum = as.character(gsub("-", "", bandnum))) %>%
  dplyr::select(bandnum, arrival_date, year, status)


#-------------------------------------------------------------------------#
###### covariates for incubation model #############
#-------------------------------------------------------------------------#

nesting_covariates <- inc_breedingpop_age %>%
  left_join(arrival_date, by = c("bandnum", "year")) %>%
  dplyr::select(bandnum, year, incubation_status, breeding_area, age, arrival_date)


incubation_model_data <- precip_temp_behaviour %>%
  left_join(nesting_covariates, by = c("bandnum", "year")) %>%
  filter(is.na(incubation_status) == FALSE) 

### dealing with unknown and resident birds



