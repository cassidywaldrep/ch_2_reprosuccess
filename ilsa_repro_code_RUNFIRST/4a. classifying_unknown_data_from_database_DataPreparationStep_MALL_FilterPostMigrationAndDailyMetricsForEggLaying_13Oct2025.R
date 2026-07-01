
#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: Reproductive Metrics - Machine Learning
# Date: 9 Oct 2025
# Author: Ilsa Griebel
# Description: Pull mallard/black duck data from database, filter GPS and ACC data to post-migration and calculate daily summary metrics
# needed for egg laying algorithm

#**********************************************************************************************************************************
#**********************************************************************************************************************************
# load libraries
library(dplyr)
library(RPostgres)
library(lubridate)
library(tidyr)
library(amt)
library(geosphere)
library(adehabitatHR) 
library(timeDate)
library(tidyverse)
library(gridExtra)
library(grid)
library(suntools)
#library(roll)
library(zoo)

#setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics/MigrationDates_EMALL")
#mig_Dates <- read.csv("season_forILSA.csv")
#dbWriteTable(conn, "seasons_labelled_EMALL", mig_Dates, append = TRUE, row.names = FALSE)
#rm(mig_Dates)

# connect to database
conn <- dbConnect(
  Postgres(),
  dbname = "acc_gps_data_mallards",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "mallard_ducks"
)

# read in masterfile
masterfile <- tbl(conn, "md_31Mar26") %>%
  collect()


# keep only the last deployment of each year
masterfile <- masterfile %>% 
  mutate(year=year(deploy_start)) %>%
  group_by(tagID, year) %>%
  filter(deploy_start == max(deploy_start), 
         !year == 2026) %>%
  ungroup()

# read in migration dates
seasons_labelled <- tbl(conn, "four_season_status_dailydisplacement_2022_2025_update30June26") %>%
  collect()

# summarize migration dates to start date of each season
mig_dates <- seasons_labelled %>% group_by(device_id, bandnum, year, status, season, partial_status) %>%
 summarise(start_date = min(UTC_date),
           end_date = max(UTC_date))

rm(seasons_labelled)

# check what earliest end of spring migration is
mig_dates_spring <- mig_dates %>% filter(season == "spring_migration")

mig_dates_spring <- mig_dates_spring %>%
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum))))

summary_spring_migration <- mig_dates_spring %>% group_by(year) %>%
  summarize(min_end_date=min(end_date))

rm(mig_dates, summary_spring_migration)

## earliest end of spring migration is Feb 19 in 2022, Feb 8 in 2023, Feb 11 in 2024 and Feb 28 in 2025

#### calculate daily median DDIST for sunrise hours only ####

# remove table from database when re-running
#dbRemoveTable(conn, name = "ddist_nsd_sunrise_hours")

#### 2022 ####
## first grab from earliest end spring migration date to July 31
gps_2022 <- tbl(conn, "gps_data_2022_2025_unfiltered") %>%
  filter(UTC_datetime > "2022-02-19 00:00:00" & UTC_datetime <= "2022-07-31 23:59:59") %>%
  collect() 

#n_1 <- unique(gps_2022$device_id)

# remove duplicates
gps_2022 <- gps_2022 %>% distinct ()

#n_2 <- unique(gps_2022$device_id)

# reduce gps to only one deployment per transmitter per year
gps_2022 <- gps_2022 %>%
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  filter(birdid %in% masterfile$bandnum)

#n_3 <- unique(gps_2022$device_id)

# remove lat/longs = 0 and filter to one location per hour
# convert date/time column to POSIXct format
gps_2022 <- gps_2022 %>%
  mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")) %>% 
  # remove erroneous (0,0) points from when sat count = 0
  filter(Latitude != 0 | Longitude != 0) %>%
  # select relevant columns
  dplyr::select(c(device_id,
                  birdid,
                  timestamp, Latitude, Longitude)) %>%
  # add column for hourly intervals
  mutate(hour = floor_date(timestamp, "1 hour")) %>% 
  # retain closest data point to each hourly interval
  group_by(device_id, hour) %>% filter(timestamp == min(timestamp)) %>%
  ungroup()

#n_4 <- unique(gps_2022$device_id)

# adding some more variables
gps_2022 <- gps_2022 %>%
  mutate(
         day_of_year = yday(timestamp),
         year = year(timestamp), 
         month = month(timestamp), 
         day = day(timestamp),
         date = as.Date(timestamp))

# filter to start of migration
# Note: average spring migration end dates: 2022 = May 3, 2023 = May 9, 2024 = May 4, 2025 = May 9.

gps_2022 <- merge(gps_2022, mig_dates_spring, by = c("birdid", "device_id", "year"))
gps_2022 <- gps_2022 %>% 
  mutate(
    # Wrap the string in as.Date() so both sides match
    end_date = if_else(status == "resident", 
                       as.Date("2022-03-01"), 
                       as.Date(end_date)),
    
    # Do the same for the second logic gate
    end_date = if_else(status == "partial_migrant" & partial_status == "quantile", 
                       as.Date("2022-03-01"), 
                       as.Date(end_date))
  )

gps_2022 <- gps_2022 %>%
  filter(date > end_date)

# calculate sunrise for each location
gps_2022$timestamp <- as_datetime(gps_2022$timestamp, tz = "UTC")
cols <- c("Latitude", "Longitude", "timestamp") ## Select columns that can't contain NA values
loc_na <- gps_2022[!complete.cases(gps_2022[cols]),] ## new gps_2022frame with NA values in lat, lon, or ts
loc <- gps_2022[complete.cases(gps_2022[cols]),] ## new gps_2022frame with no NA values in lat, lon, or ts
loc$sunrise <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunrise')$time
#loc$sunset <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunset')$time
gps.sunrise <- merge(loc, loc_na, all = TRUE)

rm(loc, loc_na)

# summarize sunrise data to one value per day per bird
gps.sunrise_summarized <- gps.sunrise %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(birdid, year, date) %>% 
  summarize(median_sunrise = median(sunrise))

rm(gps.sunrise)

# send sunrise times to db
dbWriteTable(conn, "daily_sunrise_times_30June26", gps.sunrise_summarized, append = TRUE, row.names = FALSE)

# add sunrise information to gps data
gps_2022 <- merge(gps_2022, gps.sunrise_summarized,by = c("birdid", "year", "date"), all = TRUE)

rm(gps.sunrise_summarized)

# check sample size
#nrow(gps_2022 %>%
 #      distinct(birdid,year))

# confirming both timestamp and sunrise is in UTC (they are) -- also did additional checks of this with doublechecking sunrise time in location on internet, all is good!!!
#attr(gps_2022$timestamp, "tzone")
#attr(gps_2022$median_sunrise, "tzone")

# add columns for 1 and 5 hours post-sunrise
gps_2022 <- gps_2022 %>%
  mutate(sunrise_1 = median_sunrise + (1*60*60),
         sunrise_5 = median_sunrise + (5*60*60)
  )

# remove days with < 6 gps fixes per day (calculate # of fixes per day, merge with gps data, filter out any days with < 3 fixes)
fixes_per_day <- gps_2022 %>% group_by(birdid, year, date) %>% 
  summarise(count=n())

#hist(as.numeric(fixes_per_day$count))

gps_2022 <- merge(gps_2022, fixes_per_day, by = c("birdid", "year", "date"))

rm(fixes_per_day)

gps_2022 <- gps_2022 %>% filter(count >= 6)

# reduce gps dataset to all hours except 1 to 5 hours post-sunrise
gps_2022_not_sunrise <- gps_2022%>% group_by(birdid, year, date) %>% 
  filter(timestamp < sunrise_1 | timestamp > sunrise_5) %>%
  ungroup()

# reduce gps dataset to only 1 to 5 hours post-sunrise
gps_2022 <- gps_2022%>% group_by(birdid, year, date) %>% 
  filter(timestamp >= sunrise_1) %>%
  filter(timestamp <= sunrise_5) %>%
  ungroup()



# workflow to calculate NSD for each individual, year, day
nsd.dat <- gps_2022 %>%
  # convert to amt track
  make_track(.x = Longitude, .y = Latitude, .t = timestamp, 
             id = birdid, day = day_of_year, year = year, crs = 4326) %>% 
  # reproject to a projected coordinate system with m as linear units
  transform_coords(crs_from = 4326, crs_to = 2163) %>% # alec crs_to = 3574, experimenting with different one
  # nest data by device id and day
  nest(data = -c(id, year, day)) %>%
  # calculate NSD
  mutate(nsd = map(data, nsd)) %>%
  # combine into single data frame (workaround for bug with "unnest()")
  pmap_dfr(function(...)data.frame(...)) %>% 
  # rename columns
  rename(birdid = id, day_of_year = day, easting = data.x_, northing = data.y_, timestamp = data.t_)

# remove first data point from each day (i.e., only retain distance between first fix of day and all subsequent fixes)
nsd.dat <- nsd.dat %>% group_by(birdid, year, day_of_year) %>% 
  filter(timestamp > min(timestamp))

# calculate mean daily NSD
nsd.dat.daily <- nsd.dat %>% mutate(date = date(timestamp)) %>% group_by(birdid, year, date) %>% 
  summarise(mean_nsd_sunrise = mean(nsd), n_points = n()) 

rm(nsd.dat)

nsd.dat <- gps_2022_not_sunrise %>%
  # convert to amt track
  make_track(.x = Longitude, .y = Latitude, .t = timestamp, 
             id = birdid, day = day_of_year, year = year, crs = 4326) %>% 
  # reproject to a projected coordinate system with m as linear units
  transform_coords(crs_from = 4326, crs_to = 2163) %>% # alec crs_to = 3574, experimenting with different one
  # nest data by device id and day
  nest(data = -c(id, year, day)) %>%
  # calculate NSD
  mutate(nsd = map(data, nsd)) %>%
  # combine into single data frame (workaround for bug with "unnest()")
  pmap_dfr(function(...)data.frame(...)) %>% 
  # rename columns
  rename(birdid = id, day_of_year = day, easting = data.x_, northing = data.y_, timestamp = data.t_)

# remove first data point from each day (i.e., only retain distance between first fix of day and all subsequent fixes)
nsd.dat <- nsd.dat %>% group_by(birdid, year, day_of_year) %>% 
  filter(timestamp > min(timestamp))

# calculate mean daily NSD
nsd.dat.daily_not_sunrise <- nsd.dat %>% mutate(date = date(timestamp)) %>% group_by(birdid, year, date) %>% 
  summarise(mean_nsd_not_sunrise = mean(nsd), n_points = n()) 

rm(nsd.dat)

nsd.dat.daily <- merge(nsd.dat.daily, nsd.dat.daily_not_sunrise, by = c("birdid", "year", "date"))

# calculate distance between successive median daily locations (DDIST; i.e., among-day movement) 
# workflow to calculate  median DDIST for each individual, year, day
ddist.dat <- gps_2022  %>%  group_by(birdid, year, date) %>%
  # calculate median and mean daily location as median and mean lat/long for each day
  summarise(median.location.lat = median(Latitude), median.location.long = median(Longitude)) %>% 
  # add column for median and mean daily location of one day lag
  mutate(median.location.lat.lag = lag(median.location.lat), median.location.long.lag = lag(median.location.long),
         # calculate median and mean DDIST using the Vincenty Ellipsoid method
         median_ddist = distVincentyEllipsoid(matrix(c(median.location.long, median.location.lat), ncol = 2), 
                                              matrix(c(median.location.long.lag, median.location.lat.lag), ncol = 2)))

rm(gps_2022)

### combine NSD, DDIST data ####
# List of dataframes to be merged 
df_list <- list(nsd.dat.daily, #20956 obs
                ddist.dat#, # 21252 obs
                #mcp.dat.daily#, # 20625 obs 
                #acc.dat
) # 21401 obs
breeding.metrics <- Reduce(function(x, y) merge(x, y,by = c("birdid", "year", "date")), df_list) %>% 
  drop_na() %>% 
  dplyr::select(birdid, date, year, median_ddist, #mean_ddist, 
                #mcp75area,
                mean_nsd_sunrise, mean_nsd_not_sunrise#, mean_ODBA,mean_abs_x
  )
#n_5 <- unique(breeding.metrics$birdid)

# send daily stats to db
dbWriteTable(conn, "ddist_nsd_sunrise_hours_30June26", breeding.metrics, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(ddist.dat, nsd.dat.daily, breeding.metrics)

#### 2023 ####
## first grab from earliest end spring migration date to July 31
gps_2023 <- tbl(conn, "gps_data_2022_2025_unfiltered") %>%
  filter(UTC_datetime > "2023-02-08 00:00:00" & UTC_datetime <= "2023-07-31 23:59:59") %>%
  collect() 

#n_1 <- unique(gps_2023$device_id)

# remove duplicates
gps_2023 <- gps_2023 %>% distinct ()

#n_2 <- unique(gps_2023$device_id)

# reduce sum stats to only one deployment per transmitter per year
gps_2023 <- gps_2023 %>%
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  filter(birdid %in% masterfile$bandnum)

#n_3 <- unique(gps_2023$device_id)

# remove lat/longs = 0 and filter to one location per hour
# convert date/time column to POSIXct format
gps_2023 <- gps_2023 %>%
  mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")) %>% 
  # remove erroneous (0,0) points from when sat count = 0
  filter(Latitude != 0 | Longitude != 0) %>%
  # select relevant columns
  dplyr::select(c(device_id,
                  birdid,
                  timestamp, Latitude, Longitude)) %>%
  # add column for hourly intervals
  mutate(hour = floor_date(timestamp, "1 hour")) %>% 
  # retain closest data point to each hourly interval
  group_by(device_id, hour) %>% filter(timestamp == min(timestamp)) %>%
  ungroup()

#n_4 <- unique(gps_2023$device_id)

# adding some more variables
gps_2023 <- gps_2023 %>%
  mutate(
    day_of_year = yday(timestamp),
    year = year(timestamp), 
    month = month(timestamp), 
    day = day(timestamp),
    date = as.Date(timestamp))

# filter to start of migration
gps_2023 <- merge(gps_2023, mig_dates_spring, by = c("birdid", "device_id", "year"))
gps_2023 <- gps_2023 %>% 
  mutate(
    # Wrap the string in as.Date() so both sides match
    end_date = if_else(status == "resident", 
                       as.Date("2023-03-01"), 
                       as.Date(end_date)),
    
    # Do the same for the second logic gate
    end_date = if_else(status == "partial_migrant" & partial_status == "quantile", 
                       as.Date("2023-03-01"), 
                       as.Date(end_date))
  )

gps_2023 <- gps_2023 %>%
  filter(date > end_date)

# calculate sunrise for each location
gps_2023$timestamp <- as_datetime(gps_2023$timestamp, tz = "UTC")
cols <- c("Latitude", "Longitude", "timestamp") ## Select columns that can't contain NA values
loc_na <- gps_2023[!complete.cases(gps_2023[cols]),] ## new gps_2023frame with NA values in lat, lon, or ts
loc <- gps_2023[complete.cases(gps_2023[cols]),] ## new gps_2023frame with no NA values in lat, lon, or ts
loc$sunrise <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunrise')$time
#loc$sunset <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunset')$time
gps.sunrise <- merge(loc, loc_na, all = TRUE)

rm(loc, loc_na)

# summarize sunrise data to one value per day per bird
gps.sunrise_summarized <- gps.sunrise %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(birdid, year, date) %>% 
  summarize(median_sunrise = median(sunrise))

rm(gps.sunrise)

# send sunrise times to db
dbWriteTable(conn, "daily_sunrise_times_30June26", gps.sunrise_summarized, append = TRUE, row.names = FALSE)

# add sunrise information to gps data
gps_2023 <- merge(gps_2023, gps.sunrise_summarized,by = c("birdid", "year", "date"), all = TRUE)

rm(gps.sunrise_summarized)

# check sample size
#nrow(gps_2023 %>%
#      distinct(birdid,year))

# confirming both timestamp and sunrise is in UTC (they are) -- also did additional checks of this with doublechecking sunrise time in location on internet, all is good!!!
#attr(gps_2023$timestamp, "tzone")
#attr(gps_2023$median_sunrise, "tzone")

# add columns for 1 and 5 hours post-sunrise
gps_2023 <- gps_2023 %>%
  mutate(sunrise_1 = median_sunrise + (1*60*60),
         sunrise_5 = median_sunrise + (5*60*60)
  )

# remove days with < 6 gps fixes per day (calculate # of fixes per day, merge with gps data, filter out any days with < 3 fixes)
fixes_per_day <- gps_2023 %>% group_by(birdid, year, date) %>% 
  summarise(count=n())

#hist(as.numeric(fixes_per_day$count))

gps_2023 <- merge(gps_2023, fixes_per_day, by = c("birdid", "year", "date"))

rm(fixes_per_day)

gps_2023 <- gps_2023 %>% filter(count >= 6)

# reduce gps dataset to all hours except 1 to 5 hours post-sunrise
gps_2023_not_sunrise <- gps_2023%>% group_by(birdid, year, date) %>% 
  filter(timestamp < sunrise_1 | timestamp > sunrise_5) %>%
  ungroup()

# reduce gps dataset to only 1 to 5 hours post-sunrise
gps_2023 <- gps_2023%>% group_by(birdid, year, date) %>% 
  filter(timestamp >= sunrise_1) %>%
  filter(timestamp <= sunrise_5) %>%
  ungroup()



# workflow to calculate NSD for each individual, year, day
nsd.dat <- gps_2023 %>%
  # convert to amt track
  make_track(.x = Longitude, .y = Latitude, .t = timestamp, 
             id = birdid, day = day_of_year, year = year, crs = 4326) %>% 
  # reproject to a projected coordinate system with m as linear units
  transform_coords(crs_from = 4326, crs_to = 2163) %>% # alec crs_to = 3574, experimenting with different one
  # nest data by device id and day
  nest(data = -c(id, year, day)) %>%
  # calculate NSD
  mutate(nsd = map(data, nsd)) %>%
  # combine into single data frame (workaround for bug with "unnest()")
  pmap_dfr(function(...)data.frame(...)) %>% 
  # rename columns
  rename(birdid = id, day_of_year = day, easting = data.x_, northing = data.y_, timestamp = data.t_)

# remove first data point from each day (i.e., only retain distance between first fix of day and all subsequent fixes)
nsd.dat <- nsd.dat %>% group_by(birdid, year, day_of_year) %>% 
  filter(timestamp > min(timestamp))

# calculate mean daily NSD
nsd.dat.daily <- nsd.dat %>% mutate(date = date(timestamp)) %>% group_by(birdid, year, date) %>% 
  summarise(mean_nsd_sunrise = mean(nsd), n_points = n()) 

rm(nsd.dat)

nsd.dat <- gps_2023_not_sunrise %>%
  # convert to amt track
  make_track(.x = Longitude, .y = Latitude, .t = timestamp, 
             id = birdid, day = day_of_year, year = year, crs = 4326) %>% 
  # reproject to a projected coordinate system with m as linear units
  transform_coords(crs_from = 4326, crs_to = 2163) %>% # alec crs_to = 3574, experimenting with different one
  # nest data by device id and day
  nest(data = -c(id, year, day)) %>%
  # calculate NSD
  mutate(nsd = map(data, nsd)) %>%
  # combine into single data frame (workaround for bug with "unnest()")
  pmap_dfr(function(...)data.frame(...)) %>% 
  # rename columns
  rename(birdid = id, day_of_year = day, easting = data.x_, northing = data.y_, timestamp = data.t_)

# remove first data point from each day (i.e., only retain distance between first fix of day and all subsequent fixes)
nsd.dat <- nsd.dat %>% group_by(birdid, year, day_of_year) %>% 
  filter(timestamp > min(timestamp))

# calculate mean daily NSD
nsd.dat.daily_not_sunrise <- nsd.dat %>% mutate(date = date(timestamp)) %>% group_by(birdid, year, date) %>% 
  summarise(mean_nsd_not_sunrise = mean(nsd), n_points = n()) 

rm(nsd.dat)

nsd.dat.daily <- merge(nsd.dat.daily, nsd.dat.daily_not_sunrise, by = c("birdid", "year", "date"))

# calculate distance between successive median daily locations (DDIST; i.e., among-day movement) 
# workflow to calculate  median DDIST for each individual, year, day
ddist.dat <- gps_2023  %>%  group_by(birdid, year, date) %>%
  # calculate median and mean daily location as median and mean lat/long for each day
  summarise(median.location.lat = median(Latitude), median.location.long = median(Longitude)) %>% 
  # add column for median and mean daily location of one day lag
  mutate(median.location.lat.lag = lag(median.location.lat), median.location.long.lag = lag(median.location.long),
         # calculate median and mean DDIST using the Vincenty Ellipsoid method
         median_ddist = distVincentyEllipsoid(matrix(c(median.location.long, median.location.lat), ncol = 2), 
                                              matrix(c(median.location.long.lag, median.location.lat.lag), ncol = 2)))

rm(gps_2023)

### combine NSD, DDIST data ####
# List of dataframes to be merged 
df_list <- list(nsd.dat.daily, #20956 obs
                ddist.dat#, # 21252 obs
                #mcp.dat.daily#, # 20625 obs 
                #acc.dat
) # 21401 obs
breeding.metrics <- Reduce(function(x, y) merge(x, y,by = c("birdid", "year", "date")), df_list) %>% 
  drop_na() %>% 
  dplyr::select(birdid, date, year, median_ddist, #mean_ddist, 
                #mcp75area,
                mean_nsd_sunrise, mean_nsd_not_sunrise#, mean_ODBA,mean_abs_x
  )
#n_5 <- unique(breeding.metrics$birdid)

# send daily stats to db
dbWriteTable(conn, "ddist_nsd_sunrise_hours_30June26", breeding.metrics, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(ddist.dat, nsd.dat.daily, breeding.metrics)

#### 2024 ####
## first grab from earliest end spring migration date to July 31
gps_2024 <- tbl(conn, "gps_data_2022_2025_unfiltered") %>%
  filter(UTC_datetime > "2024-02-11 00:00:00" & UTC_datetime <= "2024-07-31 23:59:59") %>%
  collect() 

#n_1 <- unique(gps_2024$device_id)

# remove duplicates
gps_2024 <- gps_2024 %>% distinct ()

#n_2 <- unique(gps_2024$device_id)

# reduce sum stats to only one deployment per transmitter per year
gps_2024 <- gps_2024 %>%
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  filter(birdid %in% masterfile$bandnum)

#n_3 <- unique(gps_2024$device_id)

# remove lat/longs = 0 and filter to one location per hour
# convert date/time column to POSIXct format
gps_2024 <- gps_2024 %>%
  mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")) %>% 
  # remove erroneous (0,0) points from when sat count = 0
  filter(Latitude != 0 | Longitude != 0) %>%
  # select relevant columns
  dplyr::select(c(device_id,
                  birdid,
                  timestamp, Latitude, Longitude)) %>%
  # add column for hourly intervals
  mutate(hour = floor_date(timestamp, "1 hour")) %>% 
  # retain closest data point to each hourly interval
  group_by(device_id, hour) %>% filter(timestamp == min(timestamp)) %>%
  ungroup()

#n_4 <- unique(gps_2024$device_id)

# adding some more variables
gps_2024 <- gps_2024 %>%
  mutate(
    day_of_year = yday(timestamp),
    year = year(timestamp), 
    month = month(timestamp), 
    day = day(timestamp),
    date = as.Date(timestamp))

# filter to start of migration

gps_2024 <- merge(gps_2024, mig_dates_spring, by = c("birdid", "device_id", "year"))
gps_2024 <- gps_2024 %>% 
  mutate(
    # Wrap the string in as.Date() so both sides match
    end_date = if_else(status == "resident", 
                       as.Date("2024-03-01"), 
                       as.Date(end_date)),
    
    # Do the same for the second logic gate
    end_date = if_else(status == "partial_migrant" & partial_status == "quantile", 
                       as.Date("2024-03-01"), 
                       as.Date(end_date))
  )

gps_2024 <- gps_2024 %>%
  filter(date > end_date)

# calculate sunrise for each location
gps_2024$timestamp <- as_datetime(gps_2024$timestamp, tz = "UTC")
cols <- c("Latitude", "Longitude", "timestamp") ## Select columns that can't contain NA values
loc_na <- gps_2024[!complete.cases(gps_2024[cols]),] ## new gps_2024frame with NA values in lat, lon, or ts
loc <- gps_2024[complete.cases(gps_2024[cols]),] ## new gps_2024frame with no NA values in lat, lon, or ts
loc$sunrise <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunrise')$time
#loc$sunset <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunset')$time
gps.sunrise <- merge(loc, loc_na, all = TRUE)

rm(loc, loc_na)

# summarize sunrise data to one value per day per bird
gps.sunrise_summarized <- gps.sunrise %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(birdid, year, date) %>% 
  summarize(median_sunrise = median(sunrise))

rm(gps.sunrise)

# send sunrise times to db
dbWriteTable(conn, "daily_sunrise_times_30June26", gps.sunrise_summarized, append = TRUE, row.names = FALSE)

# add sunrise information to gps data
gps_2024 <- merge(gps_2024, gps.sunrise_summarized,by = c("birdid", "year", "date"), all = TRUE)

rm(gps.sunrise_summarized)

# check sample size
#nrow(gps_2024 %>%
#      distinct(birdid,year))

# confirming both timestamp and sunrise is in UTC (they are) -- also did additional checks of this with doublechecking sunrise time in location on internet, all is good!!!
#attr(gps_2024$timestamp, "tzone")
#attr(gps_2024$median_sunrise, "tzone")

# add columns for 1 and 5 hours post-sunrise
gps_2024 <- gps_2024 %>%
  mutate(sunrise_1 = median_sunrise + (1*60*60),
         sunrise_5 = median_sunrise + (5*60*60)
  )

# remove days with < 6 gps fixes per day (calculate # of fixes per day, merge with gps data, filter out any days with < 3 fixes)
fixes_per_day <- gps_2024 %>% group_by(birdid, year, date) %>% 
  summarise(count=n())

#hist(as.numeric(fixes_per_day$count))

gps_2024 <- merge(gps_2024, fixes_per_day, by = c("birdid", "year", "date"))

rm(fixes_per_day)

gps_2024 <- gps_2024 %>% filter(count >= 6)

# reduce gps dataset to all hours except 1 to 5 hours post-sunrise
gps_2024_not_sunrise <- gps_2024%>% group_by(birdid, year, date) %>% 
  filter(timestamp < sunrise_1 | timestamp > sunrise_5) %>%
  ungroup()

# reduce gps dataset to only 1 to 5 hours post-sunrise
gps_2024 <- gps_2024%>% group_by(birdid, year, date) %>% 
  filter(timestamp >= sunrise_1) %>%
  filter(timestamp <= sunrise_5) %>%
  ungroup()



# workflow to calculate NSD for each individual, year, day
nsd.dat <- gps_2024 %>%
  # convert to amt track
  make_track(.x = Longitude, .y = Latitude, .t = timestamp, 
             id = birdid, day = day_of_year, year = year, crs = 4326) %>% 
  # reproject to a projected coordinate system with m as linear units
  transform_coords(crs_from = 4326, crs_to = 2163) %>% # alec crs_to = 3574, experimenting with different one
  # nest data by device id and day
  nest(data = -c(id, year, day)) %>%
  # calculate NSD
  mutate(nsd = map(data, nsd)) %>%
  # combine into single data frame (workaround for bug with "unnest()")
  pmap_dfr(function(...)data.frame(...)) %>% 
  # rename columns
  rename(birdid = id, day_of_year = day, easting = data.x_, northing = data.y_, timestamp = data.t_)

# remove first data point from each day (i.e., only retain distance between first fix of day and all subsequent fixes)
nsd.dat <- nsd.dat %>% group_by(birdid, year, day_of_year) %>% 
  filter(timestamp > min(timestamp))

# calculate mean daily NSD
nsd.dat.daily <- nsd.dat %>% mutate(date = date(timestamp)) %>% group_by(birdid, year, date) %>% 
  summarise(mean_nsd_sunrise = mean(nsd), n_points = n()) 

rm(nsd.dat)

nsd.dat <- gps_2024_not_sunrise %>%
  # convert to amt track
  make_track(.x = Longitude, .y = Latitude, .t = timestamp, 
             id = birdid, day = day_of_year, year = year, crs = 4326) %>% 
  # reproject to a projected coordinate system with m as linear units
  transform_coords(crs_from = 4326, crs_to = 2163) %>% # alec crs_to = 3574, experimenting with different one
  # nest data by device id and day
  nest(data = -c(id, year, day)) %>%
  # calculate NSD
  mutate(nsd = map(data, nsd)) %>%
  # combine into single data frame (workaround for bug with "unnest()")
  pmap_dfr(function(...)data.frame(...)) %>% 
  # rename columns
  rename(birdid = id, day_of_year = day, easting = data.x_, northing = data.y_, timestamp = data.t_)

# remove first data point from each day (i.e., only retain distance between first fix of day and all subsequent fixes)
nsd.dat <- nsd.dat %>% group_by(birdid, year, day_of_year) %>% 
  filter(timestamp > min(timestamp))

# calculate mean daily NSD
nsd.dat.daily_not_sunrise <- nsd.dat %>% mutate(date = date(timestamp)) %>% group_by(birdid, year, date) %>% 
  summarise(mean_nsd_not_sunrise = mean(nsd), n_points = n()) 

rm(nsd.dat)

nsd.dat.daily <- merge(nsd.dat.daily, nsd.dat.daily_not_sunrise, by = c("birdid", "year", "date"))

# calculate distance between successive median daily locations (DDIST; i.e., among-day movement) 
# workflow to calculate  median DDIST for each individual, year, day
ddist.dat <- gps_2024  %>%  group_by(birdid, year, date) %>%
  # calculate median and mean daily location as median and mean lat/long for each day
  summarise(median.location.lat = median(Latitude), median.location.long = median(Longitude)) %>% 
  # add column for median and mean daily location of one day lag
  mutate(median.location.lat.lag = lag(median.location.lat), median.location.long.lag = lag(median.location.long),
         # calculate median and mean DDIST using the Vincenty Ellipsoid method
         median_ddist = distVincentyEllipsoid(matrix(c(median.location.long, median.location.lat), ncol = 2), 
                                              matrix(c(median.location.long.lag, median.location.lat.lag), ncol = 2)))

rm(gps_2024)

### combine NSD, DDIST data ####
# List of dataframes to be merged 
df_list <- list(nsd.dat.daily, #20956 obs
                ddist.dat#, # 21252 obs
                #mcp.dat.daily#, # 20625 obs 
                #acc.dat
) # 21401 obs
breeding.metrics <- Reduce(function(x, y) merge(x, y,by = c("birdid", "year", "date")), df_list) %>% 
  drop_na() %>% 
  dplyr::select(birdid, date, year, median_ddist, #mean_ddist, 
                #mcp75area,
                mean_nsd_sunrise, mean_nsd_not_sunrise#, mean_ODBA,mean_abs_x
  )
#n_5 <- unique(breeding.metrics$birdid)

# send daily stats to db
dbWriteTable(conn, "ddist_nsd_sunrise_hours_30June26", breeding.metrics, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(ddist.dat, nsd.dat.daily, breeding.metrics)

#### 2025 ####
## first grab from earliest end spring migration date to July 31
gps_2025 <- tbl(conn, "gps_data_2022_2025_unfiltered") %>%
  filter(UTC_datetime > "2025-02-28 00:00:00" & UTC_datetime <= "2025-07-31 23:59:59") %>%
  collect() 

#n_1 <- unique(gps_2025$device_id)

# remove duplicates
gps_2025 <- gps_2025 %>% distinct ()

#n_2 <- unique(gps_2025$device_id)

# reduce sum stats to only one deployment per transmitter per year
gps_2025 <- gps_2025 %>%
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  filter(birdid %in% masterfile$bandnum)

#n_3 <- unique(gps_2025$device_id)

# remove lat/longs = 0 and filter to one location per hour
# convert date/time column to POSIXct format
gps_2025 <- gps_2025 %>%
  mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")) %>% 
  # remove erroneous (0,0) points from when sat count = 0
  filter(Latitude != 0 | Longitude != 0) %>%
  # select relevant columns
  dplyr::select(c(device_id,
                  birdid,
                  timestamp, Latitude, Longitude)) %>%
  # add column for hourly intervals
  mutate(hour = floor_date(timestamp, "1 hour")) %>% 
  # retain closest data point to each hourly interval
  group_by(device_id, hour) %>% filter(timestamp == min(timestamp)) %>%
  ungroup()

#n_4 <- unique(gps_2025$device_id)

# adding some more variables
gps_2025 <- gps_2025 %>%
  mutate(
    day_of_year = yday(timestamp),
    year = year(timestamp), 
    month = month(timestamp), 
    day = day(timestamp),
    date = as.Date(timestamp))

# filter to start of migration

gps_2025 <- merge(gps_2025, mig_dates_spring, by = c("birdid", "device_id", "year"))
gps_2025 <- gps_2025 %>% 
  mutate(
    # Wrap the string in as.Date() so both sides match
    end_date = if_else(status == "resident", 
                       as.Date("2025-03-01"), 
                       as.Date(end_date)),
    
    # Do the same for the second logic gate
    end_date = if_else(status == "partial_migrant" & partial_status == "quantile", 
                       as.Date("2025-03-01"), 
                       as.Date(end_date))
  )

gps_2025 <- gps_2025 %>%
  filter(date > end_date)

# calculate sunrise for each location
gps_2025$timestamp <- as_datetime(gps_2025$timestamp, tz = "UTC")
cols <- c("Latitude", "Longitude", "timestamp") ## Select columns that can't contain NA values
loc_na <- gps_2025[!complete.cases(gps_2025[cols]),] ## new gps_2025frame with NA values in lat, lon, or ts
loc <- gps_2025[complete.cases(gps_2025[cols]),] ## new gps_2025frame with no NA values in lat, lon, or ts
loc$sunrise <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunrise')$time
#loc$sunset <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunset')$time
gps.sunrise <- merge(loc, loc_na, all = TRUE)

rm(loc, loc_na)

# summarize sunrise data to one value per day per bird
gps.sunrise_summarized <- gps.sunrise %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(birdid, year, date) %>% 
  summarize(median_sunrise = median(sunrise))

rm(gps.sunrise)

# send sunrise times to db
dbWriteTable(conn, "daily_sunrise_times_30June26", gps.sunrise_summarized, append = TRUE, row.names = FALSE)

# add sunrise information to gps data
gps_2025 <- merge(gps_2025, gps.sunrise_summarized,by = c("birdid", "year", "date"), all = TRUE)

rm(gps.sunrise_summarized)

# check sample size
#nrow(gps_2025 %>%
#      distinct(birdid,year))

# confirming both timestamp and sunrise is in UTC (they are) -- also did additional checks of this with doublechecking sunrise time in location on internet, all is good!!!
#attr(gps_2025$timestamp, "tzone")
#attr(gps_2025$median_sunrise, "tzone")

# add columns for 1 and 5 hours post-sunrise
gps_2025 <- gps_2025 %>%
  mutate(sunrise_1 = median_sunrise + (1*60*60),
         sunrise_5 = median_sunrise + (5*60*60)
  )

# remove days with < 6 gps fixes per day (calculate # of fixes per day, merge with gps data, filter out any days with < 3 fixes)
fixes_per_day <- gps_2025 %>% group_by(birdid, year, date) %>% 
  summarise(count=n())

#hist(as.numeric(fixes_per_day$count))

gps_2025 <- merge(gps_2025, fixes_per_day, by = c("birdid", "year", "date"))

rm(fixes_per_day)

gps_2025 <- gps_2025 %>% filter(count >= 6)

# reduce gps dataset to all hours except 1 to 5 hours post-sunrise
gps_2025_not_sunrise <- gps_2025%>% group_by(birdid, year, date) %>% 
  filter(timestamp < sunrise_1 | timestamp > sunrise_5) %>%
  ungroup()

# reduce gps dataset to only 1 to 5 hours post-sunrise
gps_2025 <- gps_2025%>% group_by(birdid, year, date) %>% 
  filter(timestamp >= sunrise_1) %>%
  filter(timestamp <= sunrise_5) %>%
  ungroup()



# workflow to calculate NSD for each individual, year, day
nsd.dat <- gps_2025 %>%
  # convert to amt track
  make_track(.x = Longitude, .y = Latitude, .t = timestamp, 
             id = birdid, day = day_of_year, year = year, crs = 4326) %>% 
  # reproject to a projected coordinate system with m as linear units
  transform_coords(crs_from = 4326, crs_to = 2163) %>% # alec crs_to = 3574, experimenting with different one
  # nest data by device id and day
  nest(data = -c(id, year, day)) %>%
  # calculate NSD
  mutate(nsd = map(data, nsd)) %>%
  # combine into single data frame (workaround for bug with "unnest()")
  pmap_dfr(function(...)data.frame(...)) %>% 
  # rename columns
  rename(birdid = id, day_of_year = day, easting = data.x_, northing = data.y_, timestamp = data.t_)

# remove first data point from each day (i.e., only retain distance between first fix of day and all subsequent fixes)
nsd.dat <- nsd.dat %>% group_by(birdid, year, day_of_year) %>% 
  filter(timestamp > min(timestamp))

# calculate mean daily NSD
nsd.dat.daily <- nsd.dat %>% mutate(date = date(timestamp)) %>% group_by(birdid, year, date) %>% 
  summarise(mean_nsd_sunrise = mean(nsd), n_points = n()) 

rm(nsd.dat)

nsd.dat <- gps_2025_not_sunrise %>%
  # convert to amt track
  make_track(.x = Longitude, .y = Latitude, .t = timestamp, 
             id = birdid, day = day_of_year, year = year, crs = 4326) %>% 
  # reproject to a projected coordinate system with m as linear units
  transform_coords(crs_from = 4326, crs_to = 2163) %>% # alec crs_to = 3574, experimenting with different one
  # nest data by device id and day
  nest(data = -c(id, year, day)) %>%
  # calculate NSD
  mutate(nsd = map(data, nsd)) %>%
  # combine into single data frame (workaround for bug with "unnest()")
  pmap_dfr(function(...)data.frame(...)) %>% 
  # rename columns
  rename(birdid = id, day_of_year = day, easting = data.x_, northing = data.y_, timestamp = data.t_)

# remove first data point from each day (i.e., only retain distance between first fix of day and all subsequent fixes)
nsd.dat <- nsd.dat %>% group_by(birdid, year, day_of_year) %>% 
  filter(timestamp > min(timestamp))

# calculate mean daily NSD
nsd.dat.daily_not_sunrise <- nsd.dat %>% mutate(date = date(timestamp)) %>% group_by(birdid, year, date) %>% 
  summarise(mean_nsd_not_sunrise = mean(nsd), n_points = n()) 

rm(nsd.dat)

nsd.dat.daily <- merge(nsd.dat.daily, nsd.dat.daily_not_sunrise, by = c("birdid", "year", "date"))

# calculate distance between successive median daily locations (DDIST; i.e., among-day movement) 
# workflow to calculate  median DDIST for each individual, year, day
ddist.dat <- gps_2025  %>%  group_by(birdid, year, date) %>%
  # calculate median and mean daily location as median and mean lat/long for each day
  summarise(median.location.lat = median(Latitude), median.location.long = median(Longitude)) %>% 
  # add column for median and mean daily location of one day lag
  mutate(median.location.lat.lag = lag(median.location.lat), median.location.long.lag = lag(median.location.long),
         # calculate median and mean DDIST using the Vincenty Ellipsoid method
         median_ddist = distVincentyEllipsoid(matrix(c(median.location.long, median.location.lat), ncol = 2), 
                                              matrix(c(median.location.long.lag, median.location.lat.lag), ncol = 2)))

rm(gps_2025)

### combine NSD, DDIST data ####
# List of dataframes to be merged 
df_list <- list(nsd.dat.daily, #20956 obs
                ddist.dat#, # 21252 obs
                #mcp.dat.daily#, # 20625 obs 
                #acc.dat
) # 21401 obs
breeding.metrics <- Reduce(function(x, y) merge(x, y,by = c("birdid", "year", "date")), df_list) %>% 
  drop_na() %>% 
  dplyr::select(birdid, date, year, median_ddist, #mean_ddist, 
                #mcp75area,
                mean_nsd_sunrise, mean_nsd_not_sunrise#, mean_ODBA,mean_abs_x
  )
#n_5 <- unique(breeding.metrics$birdid)

# send daily stats to db
dbWriteTable(conn, "ddist_nsd_sunrise_hours_30June26", breeding.metrics, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(ddist.dat, nsd.dat.daily, breeding.metrics)

##### calculating daily odba and absx (sunrise hours and non-sunrise hours) ####
# remove table from database when re-running
#dbRemoveTable(conn, name = "odba_absx_sunrise_and_not_sunrise")

#### 2022 ####

## grab all data from earliest end of spring migration (if before Mar 1) to end of July for each year (will filter by migration once data is more condensed)
sum_stats_2022 <- tbl(conn, "sum_stats_acc_data_2022_2025") %>%
  filter(UTC_datetime > "2022-02-19 00:00:00" & UTC_datetime <= "2022-07-31 23:59:59") %>%
  collect() 

# remove duplicates
sum_stats_2022 <- sum_stats_2022 %>% distinct ()

# check if any NAs
sum_stats_2022 %>% 
  filter(if_any(everything(), is.na))

# reduce sum stats to only one deployment per transmitter per year
sum_stats_2022 <- sum_stats_2022 %>%
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum))),
         year=year(UTC_datetime),
         date=as.Date(UTC_datetime)) %>%
  filter(birdid %in% masterfile$bandnum)

# bring in 2022 sunrise times
sunrise_times_2022 <- tbl(conn, "daily_sunrise_times_30June26") %>%
  filter(year == "2022") %>%
  collect() 

# merge with sum_stats
sum_stats_2022 <- merge(sum_stats_2022, sunrise_times_2022, by = (c("birdid", "year", "date")))

rm(sunrise_times_2022)

# add columns for 1 and 5 hours post-sunrise
sum_stats_2022 <- sum_stats_2022 %>%
  mutate(sunrise_1 = median_sunrise + (1*60*60),
         sunrise_5 = median_sunrise + (5*60*60),
         timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  )

# confirming both timestamp and sunrise is in UTC (they are) -- also did additional checks of this with doublechecking sunrise time in location on internet, all is good!!!
attr(sum_stats_2022$timestamp, "tzone")
attr(sum_stats_2022$median_sunrise, "tzone")

sum.stats_1_5_ilsa <- sum_stats_2022 %>% group_by(birdid, year, date) %>% 
  filter(timestamp >= sunrise_1) %>%
  filter(timestamp <= sunrise_5) %>%
  ungroup()

sum.stats_not_1_5_ilsa <- sum_stats_2022 %>% group_by(birdid, year, date) %>% 
  filter(timestamp < sunrise_1 | timestamp > sunrise_5) %>%
  ungroup()

rm(sum_stats_2022)

# caclulate daily ODBA and absx values
daily_odba_1_5_ilsa <- sum.stats_1_5_ilsa %>% mutate(month = month(timestamp)) %>% 
  group_by(device_id, birdid, year, date) %>%
  summarise(mean_ODBA_sunrise = mean(odba), num_fixes_sunrise = n(), prop_fixes_sunrise = n()/144, # change to # bursts per day
            mean_abs_x_sunrise = mean(abs(mean.x)),
            month = mean(month))

daily_odba_not_1_5_ilsa <- sum.stats_not_1_5_ilsa %>% mutate(month = month(timestamp)) %>% 
  group_by(device_id, birdid, year, date) %>%
  summarise(mean_ODBA_not_sunrise = mean(odba), num_fixes_not_sunrise = n(), prop_fixes_not_sunrise = n()/144, # change to # bursts per day
            mean_abs_x_not_sunrise = mean(abs(mean.x)),
            month = mean(month))

rm(sum.stats_1_5, sum.stats_not_1_5)

daily_odba_ilsa <- merge(daily_odba_1_5_ilsa, daily_odba_not_1_5_ilsa, by = c("device_id", "birdid", "year", "month", "date")) 

rm(daily_odba_1_5, daily_odba_not_1_5)

# filter to start of migration
daily_odba_ilsa <- merge(daily_odba_ilsa, mig_dates_spring, by = c("birdid", "device_id", "year"))

daily_odba_ilsa <- daily_odba_ilsa %>% 
  mutate(
    # Wrap the string in as.Date() so both sides match
    end_date = if_else(status == "resident", 
                       as.Date("2022-03-01"), 
                       as.Date(end_date)),
    
    # Do the same for the second logic gate
    end_date = if_else(status == "partial_migrant" & partial_status == "quantile", 
                       as.Date("2022-03-01"), 
                       as.Date(end_date))
  )


daily_odba_ilsa <- daily_odba_ilsa %>%
  filter(date > end_date) %>%
  mutate(mean_ODBA_ratio=mean_ODBA_sunrise/mean_ODBA_not_sunrise,
         mean_abs_x_ratio=mean_abs_x_sunrise/mean_abs_x_not_sunrise)

# send daily stats to db
dbWriteTable(conn, "odba_absx_sunrise_and_not_sunrise_30June26", daily_odba_ilsa, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(daily_odba_ilsa)

#### 2023 ####
sum_stats_2023 <- tbl(conn, "sum_stats_acc_data_2022_2025") %>%
  filter(UTC_datetime > "2023-02-08 00:00:00" & UTC_datetime <= "2023-07-31 23:59:59") %>%
  collect() 

# remove duplicates
sum_stats_2023 <- sum_stats_2023 %>% distinct ()

# reduce sum stats to only one deployment per transmitter per year
sum_stats_2023 <- sum_stats_2023 %>%
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum))),
         year=year(UTC_datetime),
         date=as.Date(UTC_datetime)) %>%
  filter(birdid %in% masterfile$bandnum)

# bring in 2023 sunrise times
sunrise_times_2023 <- tbl(conn, "daily_sunrise_times_30June26") %>%
  filter(year == "2023") %>%
  collect() 

# merge with sum_stats
sum_stats_2023 <- merge(sum_stats_2023, sunrise_times_2023, by = (c("birdid", "year", "date")))

rm(sunrise_times_2023)

# add columns for 1 and 5 hours post-sunrise
sum_stats_2023 <- sum_stats_2023 %>%
  mutate(sunrise_1 = median_sunrise + (1*60*60),
         sunrise_5 = median_sunrise + (5*60*60),
         timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  )

# confirming both timestamp and sunrise is in UTC (they are) -- also did additional checks of this with doublechecking sunrise time in location on internet, all is good!!!
#attr(sum_stats_2023$timestamp, "tzone")
#attr(sum_stats_2023$median_sunrise, "tzone")

sum.stats_1_5 <- sum_stats_2023 %>% group_by(birdid, year, date) %>% 
  filter(timestamp >= sunrise_1) %>%
  filter(timestamp <= sunrise_5) %>%
  ungroup()

sum.stats_not_1_5 <- sum_stats_2023 %>% group_by(birdid, year, date) %>% 
  filter(timestamp < sunrise_1 | timestamp > sunrise_5) %>%
  ungroup()

rm(sum_stats_2023)

# caclulate daily ODBA and absx values
daily_odba_1_5 <- sum.stats_1_5 %>% mutate(month = month(timestamp)) %>% 
  group_by(device_id, birdid, year, date) %>%
  summarise(mean_ODBA_sunrise = mean(odba), num_fixes_sunrise = n(), prop_fixes_sunrise = n()/144, # change to # bursts per day
            mean_abs_x_sunrise = mean(abs(mean.x)),
            month = mean(month))

daily_odba_not_1_5 <- sum.stats_not_1_5 %>% mutate(month = month(timestamp)) %>% 
  group_by(device_id, birdid, year, date) %>%
  summarise(mean_ODBA_not_sunrise = mean(odba), num_fixes_not_sunrise = n(), prop_fixes_not_sunrise = n()/144, # change to # bursts per day
            mean_abs_x_not_sunrise = mean(abs(mean.x)),
            month = mean(month))

rm(sum.stats_1_5, sum.stats_not_1_5)

daily_odba <- merge(daily_odba_1_5, daily_odba_not_1_5, by = c("device_id", "birdid", "year", "month", "date"))

rm(daily_odba_1_5, daily_odba_not_1_5)

# filter to start of migration
# Note: average spring migration end dates: 2023 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.
daily_odba <- merge(daily_odba, mig_dates_spring, by = c("birdid", "device_id", "year"))


daily_odba <- daily_odba %>% 
  mutate(
    # Wrap the string in as.Date() so both sides match
    end_date = if_else(status == "resident", 
                       as.Date("2023-03-01"), 
                       as.Date(end_date)),
    
    # Do the same for the second logic gate
    end_date = if_else(status == "partial_migrant" & partial_status == "quantile", 
                       as.Date("2023-03-01"), 
                       as.Date(end_date))
  )

daily_odba <- daily_odba %>%
  filter(date > end_date) %>%
  mutate(mean_ODBA_ratio=mean_ODBA_sunrise/mean_ODBA_not_sunrise,
         mean_abs_x_ratio=mean_abs_x_sunrise/mean_abs_x_not_sunrise)

# send daily stats to db
dbWriteTable(conn, "odba_absx_sunrise_and_not_sunrise_30June26", daily_odba, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(daily_odba)

#### 2024 ####

sum_stats_2024 <- tbl(conn, "sum_stats_acc_data_2022_2025") %>%
  filter(UTC_datetime > "2024-02-11 00:00:00" & UTC_datetime <= "2024-07-31 23:59:59") %>%
  collect() 

# remove duplicates
sum_stats_2024 <- sum_stats_2024 %>% distinct ()

# reduce sum stats to only one deployment per transmitter per year
sum_stats_2024 <- sum_stats_2024 %>%
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum))),
         year=year(UTC_datetime),
         date=as.Date(UTC_datetime)) %>%
  filter(birdid %in% masterfile$bandnum)

# bring in 2024 sunrise times
sunrise_times_2024 <- tbl(conn, "daily_sunrise_times_30June26") %>%
  filter(year == "2024") %>%
  collect() 

# merge with sum_stats
sum_stats_2024 <- merge(sum_stats_2024, sunrise_times_2024, by = (c("birdid", "year", "date")))

rm(sunrise_times_2024)

# add columns for 1 and 5 hours post-sunrise
sum_stats_2024 <- sum_stats_2024 %>%
  mutate(sunrise_1 = median_sunrise + (1*60*60),
         sunrise_5 = median_sunrise + (5*60*60),
         timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  )

# confirming both timestamp and sunrise is in UTC (they are) -- also did additional checks of this with doublechecking sunrise time in location on internet, all is good!!!
#attr(sum_stats_2024$timestamp, "tzone")
#attr(sum_stats_2024$median_sunrise, "tzone")

sum.stats_1_5 <- sum_stats_2024 %>% group_by(birdid, year, date) %>% 
  filter(timestamp >= sunrise_1) %>%
  filter(timestamp <= sunrise_5) %>%
  ungroup()

sum.stats_not_1_5 <- sum_stats_2024 %>% group_by(birdid, year, date) %>% 
  filter(timestamp < sunrise_1 | timestamp > sunrise_5) %>%
  ungroup()

rm(sum_stats_2024)

# caclulate daily ODBA and absx values
daily_odba_1_5 <- sum.stats_1_5 %>% mutate(month = month(timestamp)) %>% 
  group_by(device_id, birdid, year, date) %>%
  summarise(mean_ODBA_sunrise = mean(odba), num_fixes_sunrise = n(), prop_fixes_sunrise = n()/144, # change to # bursts per day
            mean_abs_x_sunrise = mean(abs(mean.x)),
            month = mean(month))

daily_odba_not_1_5 <- sum.stats_not_1_5 %>% mutate(month = month(timestamp)) %>% 
  group_by(device_id, birdid, year, date) %>%
  summarise(mean_ODBA_not_sunrise = mean(odba), num_fixes_not_sunrise = n(), prop_fixes_not_sunrise = n()/144, # change to # bursts per day
            mean_abs_x_not_sunrise = mean(abs(mean.x)),
            month = mean(month))

rm(sum.stats_1_5, sum.stats_not_1_5)

daily_odba <- merge(daily_odba_1_5, daily_odba_not_1_5, by = c("device_id", "birdid", "year", "month", "date"))

rm(daily_odba_1_5, daily_odba_not_1_5)

# filter to start of migration
# Note: average spring migration end dates: 2024 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.
daily_odba <- merge(daily_odba, mig_dates_spring, by = c("birdid", "device_id", "year"))

daily_odba <- daily_odba %>% 
  mutate(
    # Wrap the string in as.Date() so both sides match
    end_date = if_else(status == "resident", 
                       as.Date("2024-03-01"), 
                       as.Date(end_date)),
    
    # Do the same for the second logic gate
    end_date = if_else(status == "partial_migrant" & partial_status == "quantile", 
                       as.Date("2024-03-01"), 
                       as.Date(end_date))
  )

daily_odba <- daily_odba %>%
  filter(date > end_date) %>%
  mutate(mean_ODBA_ratio=mean_ODBA_sunrise/mean_ODBA_not_sunrise,
         mean_abs_x_ratio=mean_abs_x_sunrise/mean_abs_x_not_sunrise)

# send daily stats to db
dbWriteTable(conn, "odba_absx_sunrise_and_not_sunrise_30June26", daily_odba, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(daily_odba)


#### 2025 ####
sum_stats_2025 <- tbl(conn, "sum_stats_acc_data_2022_2025") %>%
  filter(UTC_datetime > "2025-02-28 00:00:00" & UTC_datetime <= "2025-07-31 23:59:59") %>%
  collect() 

# remove duplicates
sum_stats_2025 <- sum_stats_2025 %>% distinct ()

# reduce sum stats to only one deployment per transmitter per year
sum_stats_2025 <- sum_stats_2025 %>%
  mutate(bandnum2=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  filter(bandnum2 %in% masterfile$bandnum)

# reduce sum stats to only one deployment per transmitter per year
sum_stats_2025 <- sum_stats_2025 %>%
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum))),
         year=year(UTC_datetime),
         date=as.Date(UTC_datetime)) %>%
  filter(birdid %in% masterfile$bandnum)

# bring in 2025 sunrise times
sunrise_times_2025 <- tbl(conn, "daily_sunrise_times_30June26") %>%
  filter(year == "2025") %>%
  collect() 

# merge with sum_stats
sum_stats_2025 <- merge(sum_stats_2025, sunrise_times_2025, by = (c("birdid", "year", "date")))

rm(sunrise_times_2025)

# add columns for 1 and 5 hours post-sunrise
sum_stats_2025 <- sum_stats_2025 %>%
  mutate(sunrise_1 = median_sunrise + (1*60*60),
         sunrise_5 = median_sunrise + (5*60*60),
         timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  )

# confirming both timestamp and sunrise is in UTC (they are) -- also did additional checks of this with doublechecking sunrise time in location on internet, all is good!!!
#attr(sum_stats_2025$timestamp, "tzone")
#attr(sum_stats_2025$median_sunrise, "tzone")

sum.stats_1_5 <- sum_stats_2025 %>% group_by(birdid, year, date) %>% 
  filter(timestamp >= sunrise_1) %>%
  filter(timestamp <= sunrise_5) %>%
  ungroup()

sum.stats_not_1_5 <- sum_stats_2025 %>% group_by(birdid, year, date) %>% 
  filter(timestamp < sunrise_1 | timestamp > sunrise_5) %>%
  ungroup()

rm(sum_stats_2025)

# caclulate daily ODBA and absx values
daily_odba_1_5 <- sum.stats_1_5 %>% mutate(month = month(timestamp)) %>% 
  group_by(device_id, birdid, year, date) %>%
  summarise(mean_ODBA_sunrise = mean(odba), num_fixes_sunrise = n(), prop_fixes_sunrise = n()/144, # change to # bursts per day
            mean_abs_x_sunrise = mean(abs(mean.x)),
            month = mean(month))

daily_odba_not_1_5 <- sum.stats_not_1_5 %>% mutate(month = month(timestamp)) %>% 
  group_by(device_id, birdid, year, date) %>%
  summarise(mean_ODBA_not_sunrise = mean(odba), num_fixes_not_sunrise = n(), prop_fixes_not_sunrise = n()/144, # change to # bursts per day
            mean_abs_x_not_sunrise = mean(abs(mean.x)),
            month = mean(month))

rm(sum.stats_1_5, sum.stats_not_1_5)

daily_odba <- merge(daily_odba_1_5, daily_odba_not_1_5, by = c("device_id", "birdid", "year", "month", "date"))

rm(daily_odba_1_5, daily_odba_not_1_5)

# filter to start of migration
# Note: average spring migration end dates: 2025 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.
daily_odba <- merge(daily_odba, mig_dates_spring, by = c("birdid", "device_id", "year"))
daily_odba <- daily_odba %>% 
  mutate(
    # Wrap the string in as.Date() so both sides match
    end_date = if_else(status == "resident", 
                       as.Date("2025-03-01"), 
                       as.Date(end_date)),
    
    # Do the same for the second logic gate
    end_date = if_else(status == "partial_migrant" & partial_status == "quantile", 
                       as.Date("2025-03-01"), 
                       as.Date(end_date))
  )
daily_odba <- daily_odba %>%
  filter(date > end_date) %>%
  mutate(mean_ODBA_ratio=mean_ODBA_sunrise/mean_ODBA_not_sunrise,
         mean_abs_x_ratio=mean_abs_x_sunrise/mean_abs_x_not_sunrise)

# send daily stats to db
dbWriteTable(conn, "odba_absx_sunrise_and_not_sunrise_30June26", daily_odba, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(daily_odba)

##### check that all data was brought in if left running ####
daily_odba_absx_propfly <- tbl(conn, "odba_absx_sunrise_and_not_sunrise_30June26") %>%
  collect()


max(daily_odba_absx_propfly$date)
min(daily_odba_absx_propfly$date)

daily_odba_absx_propfly_ILSA$birdid_year <- paste(daily_odba_absx_propfly_ILSA$birdid, daily_odba_absx_propfly_ILSA$year, sep="_")

n <- unique(daily_odba_absx_propfly_ILSA$birdid_year)

db <- tbl(conn, "ddist_nsd_sunrise_hours_30June26") %>%
  collect()

max(db$date)
min(db$date)

db$birdid_year <- paste(db$birdid, db$year, sep="_")
n <- unique(db$birdid)
n2 <- unique(db$birdid_year)
