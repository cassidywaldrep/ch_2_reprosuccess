
#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: Reproductive Metrics - Machine Learning
# Date: 9 Oct 2025
# Author: Ilsa Griebel
# Description: Pull mallard/black duck data from database, filter GPS and ACC data to post-migration and calculate necessary 
# daily summary metrics needed for input to incubation and brood rearing algorithms

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
library(caret)
library(xgboost)
#library(roll)
library(zoo)

# setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics/MigrationDates_EMALL")
# mig_Dates <- read.csv("migration_forIlsa18Nov2025.csv")
# dbWriteTable(conn, "seasons_labelled_EMALL", mig_Dates, append = TRUE, row.names = FALSE)
# rm(mig_Dates)
# 
# setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics/MigrationDates_EMALL")
# md <- read.csv("md_5Nov2025_forIlsa.csv")
# dbWriteTable(conn, "md", md, append = TRUE, row.names = FALSE)
# rm(md)

# setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics/Compiled Datasets for Machine Learning")
# gps <- read.csv("MALL_gps_data_unfiltered_combined_14Nov2025.csv")
# dbWriteTable(conn, "gps_data", gps, append = TRUE, row.names = FALSE)
# rm(gps)

# load behaviour classification algorithm
ABDU_RF_OPT_G <- readRDS("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Classifying ACC into Behaviours/AccelerationProcessing/ABDU_RF_OPT_G.RDS") 
ABDU_RF_OPT_G <- readRDS("ABDU_RF_OPT_G.rds")

# read in masterfile
masterfile <- tbl(conn, "md_31Mar26") %>%
  collect()

# keep only the last deployment of each year (birds that died right away we don't care about)

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

##### calculating daily odba, absx and prop fly ####


# remove table from database when re-running
#dbRemoveTable(conn, name = "daily_odba_absx_propfly")

## grab all data from earliest end of spring migration (if before Mar 1) to end of July for each year (will filter by migration once data is more condensed)
sum_stats_2022 <- tbl(conn, "sum_stats_acc_data_2022_2025") %>%
  filter(UTC_datetime > "2022-02-19 00:00:00" & UTC_datetime <= "2022-09-15 23:59:59") %>%
  collect() 

# remove duplicates
sum_stats_2022 <- sum_stats_2022 %>% distinct ()

# check if any NAs
sum_stats_2022 %>% 
  filter(if_any(everything(), is.na))

# reduce sum stats to only one deployment per transmitter per year
sum_stats_2022 <- sum_stats_2022 %>%
  mutate(bandnum2=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  filter(bandnum2 %in% masterfile$bandnum)

predictions <- predict(ABDU_RF_OPT_G, newdata = sum_stats_2022) %>%
  bind_cols(sum_stats_2022) %>%
  rename("behaviour" = '...1')

rm(sum_stats_2022)

# caclulate daily ODBA and absx values
daily_odba <- predictions %>% mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"), date = date(UTC_datetime), month = month(UTC_datetime)) %>% 
  group_by(device_id, bandnum2, date) %>%
  summarise(mean_ODBA = mean(odba), num_fixes = n(), prop_fixes = n()/144, # change to # bursts per day
            mean_abs_x = mean(abs(mean.x)),
            prop_fly = mean(case_when(behaviour == "fly" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_feed = mean(case_when(behaviour == "feed" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_stationary = mean(case_when(behaviour == "rest" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_preen = mean(case_when(behaviour == "preen" ~ 1, TRUE ~ 0), na.rm = TRUE),
            month = mean(month)) %>%
  rename(birdid=bandnum2) %>%
  mutate(year=year(date))


# Function from Resheff et al. 2022 to compute the confusion matrix correction for time budgets

#' @param conf_mat : confusion matrix --> rows are actual and columns are predicted (rf.confusion.matrix2 in code)
#' @param observed_budget : uncorrected time budget (the daily proportions for everything)
#' @return The corrected time budget

compute_correction_time_budget <- function (conf_mat, observed_budget){
  # Normalize the rows
  
  row_normalized_conf_mat <- conf_mat / rowSums(conf_mat) # shows the proportion that were given that category
  
  # Compute the inverse - transpose of the confusion matrix
  
  inv_transposed_conf <- solve(t(row_normalized_conf_mat))
  
  return(t(inv_transposed_conf %*% observed_budget)) # %*% is matrix multiplication
}

# extract the confusion matrix 
conf_matrix <- confusionMatrix(ABDU_RF_OPT_G) 

# run the function on your data...make sure to extract the confusion matrix table
corrected_daily_sums <- compute_correction_time_budget(conf_matrix$table, 
                                                       t(as.data.frame(daily_odba[,c("prop_feed","prop_fly", "prop_preen", "prop_stationary")])))

# rename columns
corrected_daily_sums <- as.data.frame(corrected_daily_sums) %>% 
  rename(corrected_prop_fly = fly, corrected_prop_feed = feed, 
         corrected_prop_stationary = rest, corrected_prop_preen = preen)

# combine with full data
daily_odba <-
  # add corrected values to original data set 
  bind_cols(daily_odba, corrected_daily_sums) %>%
  
  # if less than 1/144 (basically, less than one the lowest amount of burst possible), make 0
   mutate(across(starts_with("corrected_prop_"),
                ~ if_else(. < 0.006944444, 0, .)), 
         
         # more than 1 isn't possible, so bring those values down to 1
         across(starts_with("corrected_prop"), 
                ~ if_else(. > 1, 1, .)))

# filter to start of migration
# Note: average spring migration end dates: 2022 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.
daily_odba <- merge(daily_odba, mig_dates_spring, by = c("birdid", "device_id", "year"))
daily_odba <- daily_odba %>% 
    mutate(
    # Wrap the string in as.Date() so both sides match
    end_date = if_else(status == "resident", 
                       as.Date("2022-03-01"), 
                       as.Date(end_date)),
    
    # Do the same for the second logic gate
    end_date = if_else(status == "partial_migrant" & partial_status == "quantile", 
                       as.Date("2022-03-01"), 
                       as.Date(end_date)))%>% 
  filter(date > end_date)

# send daily stats to db
dbWriteTable(conn, "daily_odba_absx_propfly_30Jun26", daily_odba, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(corrected_daily_sums, daily_odba)

sum_stats_2023 <- tbl(conn, "sum_stats_acc_data_2022_2025") %>%
  filter(UTC_datetime > "2023-02-08 00:00:00" & UTC_datetime <= "2023-09-15 23:59:59") %>%
  collect() 

# remove duplicates
sum_stats_2023 <- sum_stats_2023 %>% distinct ()

# reduce sum stats to only one deployment per transmitter per year
sum_stats_2023 <- sum_stats_2023 %>%
  mutate(bandnum2=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  filter(bandnum2 %in% masterfile$bandnum)

predictions <- predict(ABDU_RF_OPT_G, newdata = sum_stats_2023) %>%
  bind_cols(sum_stats_2023) %>%
  rename("behaviour" = '...1')

rm(sum_stats_2023)

# caclulate daily ODBA and absx values
daily_odba <- predictions %>% mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"), date = date(UTC_datetime), month = month(UTC_datetime)) %>% 
  group_by(device_id, bandnum2, date) %>%
  summarise(mean_ODBA = mean(odba), num_fixes = n(), prop_fixes = n()/144, # change to # bursts per day
            mean_abs_x = mean(abs(mean.x)),
            prop_fly = mean(case_when(behaviour == "fly" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_feed = mean(case_when(behaviour == "feed" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_stationary = mean(case_when(behaviour == "rest" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_preen = mean(case_when(behaviour == "preen" ~ 1, TRUE ~ 0), na.rm = TRUE),
            month = mean(month)) %>%
  rename(birdid=bandnum2) %>%
  mutate(year=year(date))

rm(predictions)

# Function from Resheff et al. 2022 to compute the confusion matrix correction for time budgets

#' @param conf_mat : confusion matrix --> rows are actual and columns are predicted (rf.confusion.matrix2 in code)
#' @param observed_budget : uncorrected time budget (the daily proportions for everything)
#' @return The corrected time budget

compute_correction_time_budget <- function (conf_mat, observed_budget){
  # Normalize the rows
  
  row_normalized_conf_mat <- conf_mat / rowSums(conf_mat) # shows the proportion that were given that category
  
  # Compute the inverse - transpose of the confusion matrix
  
  inv_transposed_conf <- solve(t(row_normalized_conf_mat))
  
  return(t(inv_transposed_conf %*% observed_budget)) # %*% is matrix multiplication
}

# extract the confusion matrix 
conf_matrix <- confusionMatrix(ABDU_RF_OPT_G) 

# run the function on your data...make sure to extract the confusion matrix table
corrected_daily_sums <- compute_correction_time_budget(conf_matrix$table, 
                                                       t(as.data.frame(daily_odba[,c("prop_feed","prop_fly", "prop_preen", "prop_stationary")])))

# rename columns
corrected_daily_sums <- as.data.frame(corrected_daily_sums) %>% 
  rename(corrected_prop_fly = fly, corrected_prop_feed = feed, 
         corrected_prop_stationary = rest, corrected_prop_preen = preen)

# combine with full data
daily_odba <-
  # add corrected values to original data set 
  bind_cols(daily_odba, corrected_daily_sums) %>%
  
  # if less than 1/144 (basically, less than one the lowest amount of burst possible), make 0
  mutate(across(starts_with("corrected_prop_"),
                ~ if_else(. < 0.006944444, 0, .)), 
         
         # more than 1 isn't possible, so bring those values down to 1
         across(starts_with("corrected_prop"), 
                ~ if_else(. > 1, 1, .)))

# filter to start of migration
# Note: average spring migration end dates: 2022 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.
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
  filter(date > end_date)

# send daily stats to db
dbWriteTable(conn, "daily_odba_absx_propfly_30Jun26", daily_odba, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(corrected_daily_sums, daily_odba)

sum_stats_2024 <- tbl(conn, "sum_stats_acc_data_2022_2025") %>%
  filter(UTC_datetime > "2024-02-11 00:00:00" & UTC_datetime <= "2024-09-15 23:59:59") %>%
  collect() 

# remove duplicates
sum_stats_2024 <- sum_stats_2024 %>% distinct ()

# reduce sum stats to only one deployment per transmitter per year
sum_stats_2024 <- sum_stats_2024 %>%
  mutate(bandnum2=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  filter(bandnum2 %in% masterfile$bandnum)

predictions <- predict(ABDU_RF_OPT_G, newdata = sum_stats_2024) %>%
  bind_cols(sum_stats_2024) %>%
  rename("behaviour" = '...1')

rm(sum_stats_2024)

# caclulate daily ODBA and absx values
daily_odba <- predictions %>% mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"), date = date(UTC_datetime), month = month(UTC_datetime)) %>% 
  group_by(device_id, bandnum2, date) %>%
  summarise(mean_ODBA = mean(odba), num_fixes = n(), prop_fixes = n()/144, # change to # bursts per day
            mean_abs_x = mean(abs(mean.x)),
            prop_fly = mean(case_when(behaviour == "fly" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_feed = mean(case_when(behaviour == "feed" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_stationary = mean(case_when(behaviour == "rest" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_preen = mean(case_when(behaviour == "preen" ~ 1, TRUE ~ 0), na.rm = TRUE),
            month = mean(month)) %>%
  rename(birdid=bandnum2) %>%
  mutate(year=year(date))

rm(predictions)

# Function from Resheff et al. 2022 to compute the confusion matrix correction for time budgets

#' @param conf_mat : confusion matrix --> rows are actual and columns are predicted (rf.confusion.matrix2 in code)
#' @param observed_budget : uncorrected time budget (the daily proportions for everything)
#' @return The corrected time budget

compute_correction_time_budget <- function (conf_mat, observed_budget){
  # Normalize the rows
  
  row_normalized_conf_mat <- conf_mat / rowSums(conf_mat) # shows the proportion that were given that category
  
  # Compute the inverse - transpose of the confusion matrix
  
  inv_transposed_conf <- solve(t(row_normalized_conf_mat))
  
  return(t(inv_transposed_conf %*% observed_budget)) # %*% is matrix multiplication
}

# extract the confusion matrix 
conf_matrix <- confusionMatrix(ABDU_RF_OPT_G) 

# run the function on your data...make sure to extract the confusion matrix table
corrected_daily_sums <- compute_correction_time_budget(conf_matrix$table, 
                                                       t(as.data.frame(daily_odba[,c("prop_feed","prop_fly", "prop_preen", "prop_stationary")])))

# rename columns
corrected_daily_sums <- as.data.frame(corrected_daily_sums) %>% 
  rename(corrected_prop_fly = fly, corrected_prop_feed = feed, 
         corrected_prop_stationary = rest, corrected_prop_preen = preen)

# combine with full data
daily_odba <-
  # add corrected values to original data set 
  bind_cols(daily_odba, corrected_daily_sums) %>%
  
  # if less than 1/144 (basically, less than one the lowest amount of burst possible), make 0
  mutate(across(starts_with("corrected_prop_"),
                ~ if_else(. < 0.006944444, 0, .)), 
         
         # more than 1 isn't possible, so bring those values down to 1
         across(starts_with("corrected_prop"), 
                ~ if_else(. > 1, 1, .)))

# filter to start of migration
# Note: average spring migration end dates: 2022 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.
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
  ) %>%
  filter(date > end_date)

# send daily stats to db
dbWriteTable(conn, "daily_odba_absx_propfly_30Jun26", daily_odba, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(corrected_daily_sums, daily_odba)

sum_stats_2025 <- tbl(conn, "sum_stats_acc_data_2022_2025") %>%
  filter(UTC_datetime > "2025-02-28 00:00:00" & UTC_datetime <= "2025-09-15 23:59:59") %>%
  collect() 

# remove duplicates
sum_stats_2025 <- sum_stats_2025 %>% distinct ()

# reduce sum stats to only one deployment per transmitter per year
sum_stats_2025 <- sum_stats_2025 %>%
  mutate(bandnum2=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  filter(bandnum2 %in% masterfile$bandnum)

predictions <- predict(ABDU_RF_OPT_G, newdata = sum_stats_2025) %>%
  bind_cols(sum_stats_2025) %>%
  rename("behaviour" = '...1')

rm(sum_stats_2025)

# caclulate daily ODBA and absx values
daily_odba <- predictions %>% mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"), date = date(UTC_datetime), month = month(UTC_datetime)) %>% 
  group_by(device_id, bandnum2, date) %>%
  summarise(mean_ODBA = mean(odba), num_fixes = n(), prop_fixes = n()/144, # change to # bursts per day
            mean_abs_x = mean(abs(mean.x)),
            prop_fly = mean(case_when(behaviour == "fly" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_feed = mean(case_when(behaviour == "feed" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_stationary = mean(case_when(behaviour == "rest" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_preen = mean(case_when(behaviour == "preen" ~ 1, TRUE ~ 0), na.rm = TRUE),
            month = mean(month)) %>%
  rename(birdid=bandnum2) %>%
  mutate(year=year(date))

rm(predictions)

# Function from Resheff et al. 2022 to compute the confusion matrix correction for time budgets

#' @param conf_mat : confusion matrix --> rows are actual and columns are predicted (rf.confusion.matrix2 in code)
#' @param observed_budget : uncorrected time budget (the daily proportions for everything)
#' @return The corrected time budget

compute_correction_time_budget <- function (conf_mat, observed_budget){
  # Normalize the rows
  
  row_normalized_conf_mat <- conf_mat / rowSums(conf_mat) # shows the proportion that were given that category
  
  # Compute the inverse - transpose of the confusion matrix
  
  inv_transposed_conf <- solve(t(row_normalized_conf_mat))
  
  return(t(inv_transposed_conf %*% observed_budget)) # %*% is matrix multiplication
}

# extract the confusion matrix 
conf_matrix <- confusionMatrix(ABDU_RF_OPT_G) 

# run the function on your data...make sure to extract the confusion matrix table
corrected_daily_sums <- compute_correction_time_budget(conf_matrix$table, 
                                                       t(as.data.frame(daily_odba[,c("prop_feed","prop_fly", "prop_preen", "prop_stationary")])))

# rename columns
corrected_daily_sums <- as.data.frame(corrected_daily_sums) %>% 
  rename(corrected_prop_fly = fly, corrected_prop_feed = feed, 
         corrected_prop_stationary = rest, corrected_prop_preen = preen)

# combine with full data
daily_odba <-
  # add corrected values to original data set 
  bind_cols(daily_odba, corrected_daily_sums) %>%
  
  # if less than 1/144 (basically, less than one the lowest amount of burst possible), make 0
  mutate(across(starts_with("corrected_prop_"),
                ~ if_else(. < 0.006944444, 0, .)), 
         
         # more than 1 isn't possible, so bring those values down to 1
         across(starts_with("corrected_prop"), 
                ~ if_else(. > 1, 1, .)))

# filter to start of migration
# Note: average spring migration end dates: 2022 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.
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
  ) %>%
  filter(date > end_date)

# send daily stats to db
dbWriteTable(conn, "daily_odba_absx_propfly_30Jun26", daily_odba, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(corrected_daily_sums, daily_odba)

#### calculate daily median DDIST, mean NSD and mean 75% MCP ####
rm(conf_matrix, ABDU_RF_OPT_G)
# remove table from database when re-running
#dbRemoveTable(conn, name = "daily_ddist_nsd_mcp")

#### 2022 ####
## first grab from earliest end spring migration date to July 31
gps_2022 <- tbl(conn, "gps_data_2022_2025_unfiltered") %>%
  filter(UTC_datetime > "2022-02-19 00:00:00" & UTC_datetime <= "2022-09-15 23:59:59") %>%
  collect() 

#n_1 <- unique(gps_2022$device_id)

# remove duplicates
gps_2022 <- gps_2022 %>% distinct ()

#n_2 <- unique(gps_2022$device_id)

# reduce sum stats to only one deployment per transmitter per year
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
# Note: average spring migration end dates: 2022 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.

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
  ) %>%
  filter(date > end_date)


### Calculate daily mean net squared displacement (NSD; i.e., within-day movement) ####
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
  summarise(mean_nsd = mean(nsd), n_points = n()) 

### calculate distance between successive mean and median daily locations (DDIST; i.e., among-day movement) ####
# workflow to calculate mean and median DDIST for each individual, year, day
ddist.dat <- gps_2022  %>%  group_by(birdid, year, date) %>%
  # calculate median and mean daily location as median and mean lat/long for each day
  summarise(median.location.lat = median(Latitude), median.location.long = median(Longitude)) %>% 
  # add column for median and mean daily location of one day lag
  mutate(median.location.lat.lag = lag(median.location.lat), median.location.long.lag = lag(median.location.long),
         # calculate median and mean DDIST using the Vincenty Ellipsoid method
         median_ddist = distVincentyEllipsoid(matrix(c(median.location.long, median.location.lat), ncol = 2), 
                                              matrix(c(median.location.long.lag, median.location.lat.lag), ncol = 2)))

### calculate 75% Minimum Convex Polygon around daily locations ####
mcp.dat <- gps_2022  %>% group_by(birdid, year) %>%
  # remove first and last day for each bird
  filter(date > min(date)) %>%
  filter(date < max(date))

#fixes.per.day <- mcp.dat %>% group_by(birdid, date) %>%
 # summarize(fixperday=n())

#fixes.per.day_less5 <- fixes.per.day %>%
 # filter(fixperday < 5)

#min(fixes.per.day$fixperday)

# MCP in parallel ####
library(doParallel)
library(parallel)
library(foreach)


#birdid can repeat in different years, so it need to be listed with the year
#birds <- unique(mcp.dat$birdid)
birds <- mcp.dat %>%
  distinct(birdid,year)

## allowing parallel 
no_cores <- parallel::detectCores() - 2
clt <- makePSOCKcluster(no_cores)
registerDoParallel(clt)
mcp.dat.daily<-foreach(i=1:nrow(birds), .combine=rbind, .packages=c('dplyr','adehabitatHR','foreach')) %dopar% {
  temp <- mcp.dat %>% filter(birdid == birds[i,1],year == birds[i,2])
  days <- unique(temp$date)
  foreach(j=1:length(days), .combine=rbind, .packages=c('dplyr','adehabitatHR')) %do% {
    temp2 <- temp %>% filter(date == days[j])
    if (nrow(temp2) >=5) {
      temp3 <- temp2[1,c(1,11)] # c(1,9) for training data, c(1,10) for wild bird data
      coordinates(temp2) <- c("Longitude", "Latitude")
      proj4string(temp2) <- CRS("+proj=longlat +datum=WGS84")  ## for example
      t_UTM <- spTransform(temp2, CRS("+proj=aeqd +ellps=WGS84"))
      t.mcp <- as.data.frame(mcp(t_UTM, percent = 75, unout = "km2"))
      temp3$mcp75area <- t.mcp$area
      return(temp3)
    } 
  }
}
stopCluster(clt)

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()

mcp.dat.daily <- mcp.dat.daily %>%
  mutate(year=year(date))

### combine NSD, DDIST, MCP and ODBA data ####
# List of dataframes to be merged 
df_list <- list(nsd.dat.daily, #20956 obs
                ddist.dat, # 21252 obs
                mcp.dat.daily#, # 20625 obs 
                #acc.dat
                ) # 21401 obs
breeding.metrics <- Reduce(function(x, y) merge(x, y,by = c("birdid", "year", "date")), df_list) %>% 
  drop_na() %>% 
  dplyr::select(birdid, date, year, median_ddist, #mean_ddist, 
                mcp75area,
                mean_nsd#, mean_ODBA,mean_abs_x
                )
#n_5 <- unique(breeding.metrics$birdid)

# send daily stats to db
dbWriteTable(conn, "daily_ddist_nsd_mcp_30Jun26", breeding.metrics, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(nsd.dat,mcp.dat, ddist.dat, gps.breed, nsd.dat.daily, mcp.dat.daily)

#### 2023 ####
## first grab from earliest end spring migration date to July 31
gps_2023 <- tbl(conn, "gps_data_2022_2025_unfiltered") %>%
  filter(UTC_datetime > "2023-02-08 00:00:00" & UTC_datetime <= "2023-09-15 23:59:59") %>%
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
# Note: average spring migration end dates: 2022 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.

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
  ) %>%
  filter(date > end_date)


### Calculate daily mean net squared displacement (NSD; i.e., within-day movement) ####
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
  summarise(mean_nsd = mean(nsd), n_points = n()) 

### calculate distance between successive mean and median daily locations (DDIST; i.e., among-day movement) ####
# workflow to calculate mean and median DDIST for each individual, year, day
ddist.dat <- gps_2023  %>%  group_by(birdid, year, date) %>%
  # calculate median and mean daily location as median and mean lat/long for each day
  summarise(median.location.lat = median(Latitude), median.location.long = median(Longitude)) %>% 
  # add column for median and mean daily location of one day lag
  mutate(median.location.lat.lag = lag(median.location.lat), median.location.long.lag = lag(median.location.long),
         # calculate median and mean DDIST using the Vincenty Ellipsoid method
         median_ddist = distVincentyEllipsoid(matrix(c(median.location.long, median.location.lat), ncol = 2), 
                                              matrix(c(median.location.long.lag, median.location.lat.lag), ncol = 2)))

### calculate 75% Minimum Convex Polygon around daily locations ####
mcp.dat <- gps_2023  %>% group_by(birdid, year) %>%
  # remove first and last day for each bird
  filter(date > min(date)) %>%
  filter(date < max(date))

#fixes.per.day <- mcp.dat %>% group_by(birdid, date) %>%
# summarize(fixperday=n())

#fixes.per.day_less5 <- fixes.per.day %>%
# filter(fixperday < 5)

#min(fixes.per.day$fixperday)

# MCP in parallel ####
library(doParallel)
library(parallel)
library(foreach)


#birdid can repeat in different years, so it need to be listed with the year
#birds <- unique(mcp.dat$birdid)
birds <- mcp.dat %>%
  distinct(birdid,year)

## allowing parallel 
no_cores <- parallel::detectCores() - 2
clt <- makePSOCKcluster(no_cores)
registerDoParallel(clt)
mcp.dat.daily<-foreach(i=1:nrow(birds), .combine=rbind, .packages=c('dplyr','adehabitatHR','foreach')) %dopar% {
  temp <- mcp.dat %>% filter(birdid == birds[i,1],year == birds[i,2])
  days <- unique(temp$date)
  foreach(j=1:length(days), .combine=rbind, .packages=c('dplyr','adehabitatHR')) %do% {
    temp2 <- temp %>% filter(date == days[j])
    if (nrow(temp2) >=5) {
      temp3 <- temp2[1,c(1,11)] # c(1,9) for training data, c(1,10) for wild bird data
      coordinates(temp2) <- c("Longitude", "Latitude")
      proj4string(temp2) <- CRS("+proj=longlat +datum=WGS84")  ## for example
      t_UTM <- spTransform(temp2, CRS("+proj=aeqd +ellps=WGS84"))
      t.mcp <- as.data.frame(mcp(t_UTM, percent = 75, unout = "km2"))
      temp3$mcp75area <- t.mcp$area
      return(temp3)
    } 
  }
}
stopCluster(clt)

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()

mcp.dat.daily <- mcp.dat.daily %>%
  mutate(year=year(date))

### combine NSD, DDIST, MCP and ODBA data ####
# List of dataframes to be merged 
df_list <- list(nsd.dat.daily, #20956 obs
                ddist.dat, # 21252 obs
                mcp.dat.daily#, # 20625 obs 
                #acc.dat
) # 21401 obs
breeding.metrics <- Reduce(function(x, y) merge(x, y,by = c("birdid", "year", "date")), df_list) %>% 
  drop_na() %>% 
  dplyr::select(birdid, date, year, median_ddist, #mean_ddist, 
                mcp75area,
                mean_nsd#, mean_ODBA,mean_abs_x
  )
#n_5 <- unique(breeding.metrics$birdid)

# send daily stats to db
dbWriteTable(conn, "daily_ddist_nsd_mcp_30Jun26", breeding.metrics, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(nsd.dat,mcp.dat, ddist.dat, gps.breed, nsd.dat.daily, mcp.dat.daily)

#### 2024 ####
## first grab from earliest end spring migration date to July 31
gps_2024 <- tbl(conn, "gps_data_2022_2025_unfiltered") %>%
  filter(UTC_datetime > "2024-02-11 00:00:00" & UTC_datetime <= "2024-09-15 23:59:59") %>%
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
# Note: average spring migration end dates: 2022 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.

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
  )  %>%
  filter(date > end_date)


### Calculate daily mean net squared displacement (NSD; i.e., within-day movement) ####
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
  summarise(mean_nsd = mean(nsd), n_points = n()) 

### calculate distance between successive mean and median daily locations (DDIST; i.e., among-day movement) ####
# workflow to calculate mean and median DDIST for each individual, year, day
ddist.dat <- gps_2024  %>%  group_by(birdid, year, date) %>%
  # calculate median and mean daily location as median and mean lat/long for each day
  summarise(median.location.lat = median(Latitude), median.location.long = median(Longitude)) %>% 
  # add column for median and mean daily location of one day lag
  mutate(median.location.lat.lag = lag(median.location.lat), median.location.long.lag = lag(median.location.long),
         # calculate median and mean DDIST using the Vincenty Ellipsoid method
         median_ddist = distVincentyEllipsoid(matrix(c(median.location.long, median.location.lat), ncol = 2), 
                                              matrix(c(median.location.long.lag, median.location.lat.lag), ncol = 2)))

### calculate 75% Minimum Convex Polygon around daily locations ####
mcp.dat <- gps_2024  %>% group_by(birdid, year) %>%
  # remove first and last day for each bird
  filter(date > min(date)) %>%
  filter(date < max(date))

#fixes.per.day <- mcp.dat %>% group_by(birdid, date) %>%
# summarize(fixperday=n())

#fixes.per.day_less5 <- fixes.per.day %>%
# filter(fixperday < 5)

#min(fixes.per.day$fixperday)

# MCP in parallel ####
library(doParallel)
library(parallel)
library(foreach)


#birdid can repeat in different years, so it need to be listed with the year
#birds <- unique(mcp.dat$birdid)
birds <- mcp.dat %>%
  distinct(birdid,year)

## allowing parallel 
no_cores <- parallel::detectCores() - 2
clt <- makePSOCKcluster(no_cores)
registerDoParallel(clt)
mcp.dat.daily<-foreach(i=1:nrow(birds), .combine=rbind, .packages=c('dplyr','adehabitatHR','foreach')) %dopar% {
  temp <- mcp.dat %>% filter(birdid == birds[i,1],year == birds[i,2])
  days <- unique(temp$date)
  foreach(j=1:length(days), .combine=rbind, .packages=c('dplyr','adehabitatHR')) %do% {
    temp2 <- temp %>% filter(date == days[j])
    if (nrow(temp2) >=5) {
      temp3 <- temp2[1,c(1,11)] # c(1,9) for training data, c(1,10) for wild bird data
      coordinates(temp2) <- c("Longitude", "Latitude")
      proj4string(temp2) <- CRS("+proj=longlat +datum=WGS84")  ## for example
      t_UTM <- spTransform(temp2, CRS("+proj=aeqd +ellps=WGS84"))
      t.mcp <- as.data.frame(mcp(t_UTM, percent = 75, unout = "km2"))
      temp3$mcp75area <- t.mcp$area
      return(temp3)
    } 
  }
}
stopCluster(clt)

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()

mcp.dat.daily <- mcp.dat.daily %>%
  mutate(year=year(date))

### combine NSD, DDIST, MCP and ODBA data ####
# List of dataframes to be merged 
df_list <- list(nsd.dat.daily, #20956 obs
                ddist.dat, # 21252 obs
                mcp.dat.daily#, # 20625 obs 
                #acc.dat
) # 21401 obs
breeding.metrics <- Reduce(function(x, y) merge(x, y,by = c("birdid", "year", "date")), df_list) %>% 
  drop_na() %>% 
  dplyr::select(birdid, date, year, median_ddist, #mean_ddist, 
                mcp75area,
                mean_nsd#, mean_ODBA,mean_abs_x
  )
#n_5 <- unique(breeding.metrics$birdid)

# send daily stats to db
dbWriteTable(conn, "daily_ddist_nsd_mcp_30Jun26", breeding.metrics, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(nsd.dat,mcp.dat, ddist.dat, gps.breed, nsd.dat.daily, mcp.dat.daily)

#### 2025 ####
## first grab from earliest end spring migration date to July 31
gps_2025 <- tbl(conn, "gps_data_2022_2025_unfiltered") %>%
  filter(UTC_datetime > "2025-02-28 00:00:00" & UTC_datetime <= "2025-09-15 23:59:59") %>%
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
# Note: average spring migration end dates: 2022 = Apr 30, 2023 = May 9, 2024 = May 4, 2025 = May 7.

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
  ) %>%
  filter(date > end_date)


### Calculate daily mean net squared displacement (NSD; i.e., within-day movement) ####
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
  summarise(mean_nsd = mean(nsd), n_points = n()) 

### calculate distance between successive mean and median daily locations (DDIST; i.e., among-day movement) ####
# workflow to calculate mean and median DDIST for each individual, year, day
ddist.dat <- gps_2025  %>%  group_by(birdid, year, date) %>%
  # calculate median and mean daily location as median and mean lat/long for each day
  summarise(median.location.lat = median(Latitude), median.location.long = median(Longitude)) %>% 
  # add column for median and mean daily location of one day lag
  mutate(median.location.lat.lag = lag(median.location.lat), median.location.long.lag = lag(median.location.long),
         # calculate median and mean DDIST using the Vincenty Ellipsoid method
         median_ddist = distVincentyEllipsoid(matrix(c(median.location.long, median.location.lat), ncol = 2), 
                                              matrix(c(median.location.long.lag, median.location.lat.lag), ncol = 2)))

### calculate 75% Minimum Convex Polygon around daily locations ####
mcp.dat <- gps_2025  %>% group_by(birdid, year) %>%
  # remove first and last day for each bird
  filter(date > min(date)) %>%
  filter(date < max(date))

#fixes.per.day <- mcp.dat %>% group_by(birdid, date) %>%
# summarize(fixperday=n())

#fixes.per.day_less5 <- fixes.per.day %>%
# filter(fixperday < 5)

#min(fixes.per.day$fixperday)

# MCP in parallel ####
library(doParallel)
library(parallel)
library(foreach)


#birdid can repeat in different years, so it need to be listed with the year
#birds <- unique(mcp.dat$birdid)
birds <- mcp.dat %>%
  distinct(birdid,year)

## allowing parallel 
no_cores <- parallel::detectCores() - 2
clt <- makePSOCKcluster(no_cores)
registerDoParallel(clt)
mcp.dat.daily<-foreach(i=1:nrow(birds), .combine=rbind, .packages=c('dplyr','adehabitatHR','foreach')) %dopar% {
  temp <- mcp.dat %>% filter(birdid == birds[i,1],year == birds[i,2])
  days <- unique(temp$date)
  foreach(j=1:length(days), .combine=rbind, .packages=c('dplyr','adehabitatHR')) %do% {
    temp2 <- temp %>% filter(date == days[j])
    if (nrow(temp2) >=5) {
      temp3 <- temp2[1,c(1,11)] # c(1,9) for training data, c(1,10) for wild bird data
      coordinates(temp2) <- c("Longitude", "Latitude")
      proj4string(temp2) <- CRS("+proj=longlat +datum=WGS84")  ## for example
      t_UTM <- spTransform(temp2, CRS("+proj=aeqd +ellps=WGS84"))
      t.mcp <- as.data.frame(mcp(t_UTM, percent = 75, unout = "km2"))
      temp3$mcp75area <- t.mcp$area
      return(temp3)
    } 
  }
}
stopCluster(clt)

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()

mcp.dat.daily <- mcp.dat.daily %>%
  mutate(year=year(date))

### combine NSD, DDIST, MCP and ODBA data ####
# List of dataframes to be merged 
df_list <- list(nsd.dat.daily, #20956 obs
                ddist.dat, # 21252 obs
                mcp.dat.daily#, # 20625 obs 
                #acc.dat
) # 21401 obs
breeding.metrics <- Reduce(function(x, y) merge(x, y,by = c("birdid", "year", "date")), df_list) %>% 
  drop_na() %>% 
  dplyr::select(birdid, date, year, median_ddist, #mean_ddist, 
                mcp75area,
                mean_nsd#, mean_ODBA,mean_abs_x
  )
#n_5 <- unique(breeding.metrics$birdid)

# send daily stats to db
dbWriteTable(conn, "daily_ddist_nsd_mcp_30Jun26", breeding.metrics, append = TRUE, row.names = FALSE)

# remove dataframes and start next year
rm(nsd.dat,mcp.dat, ddist.dat, gps.breed, nsd.dat.daily, mcp.dat.daily)

#### check that all data was brought in if left running ####
daily_odba_absx_propfly <- tbl(conn, "daily_odba_absx_propfly_30Jun26") %>%
  collect()


max(daily_odba_absx_propfly$date)
min(daily_odba_absx_propfly$date)

daily_odba_absx_propfly$birdid_year <- paste(daily_odba_absx_propfly$birdid, daily_odba_absx_propfly$year, sep="_")

n <- unique(daily_odba_absx_propfly$birdid_year)

db <- tbl(conn, "daily_ddist_nsd_mcp_25Jun26") %>%
  collect()

max(db$date)
min(db$date)

db$birdid_year <- paste(db$birdid, db$year, sep="_")
n1 <- unique(db$birdid)
n2 <- unique(db$birdid_year)



