######################## Bringing in temperature and precipitation data ##################

# Code: Reading in temperature / precipitation data for models, averaged over the time period

# Date: 21 Sept 2026
# Author: Cassidy Waldrep

# Load packages and functions

library(ggplot2)
library(tidyverse)
library(raster)
library(ecmwfr)
library(ncdf4)  
library(RPostgres)
library(sf)


conn <- dbConnect(
  Postgres(),
  dbname = "ch2_facmodel",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "mallard_ducks"
)

#-------------------------------------------------------------------------#
###### Reading in data ###################
#-------------------------------------------------------------------------#

ecological_periods <- tbl(conn, "ecologicalperiods") %>%
  collect() %>%
  dplyr::select(bandnum, UTC_datetime, UTC_date, Latitude, Longitude, status, season = season2, year) %>%
  mutate(month = month(UTC_date)) %>%
  filter(season %in% c("winter", "late_winter", "spring_migration", "late_spring_migration", "early_breeding", "summer")) %>%
  mutate(
    day_of_year = yday(UTC_date), 
    hour_of_year = ((day_of_year)-1)*24+hour(UTC_datetime)) 

#### download weather data from ERA5 ####

# Izzy and Ilsa have code to do this on the computer (like using an API code), but I decided to just download from the website


#-------------------------------------------------------------------------#
###### Temperature function  ###################
#-------------------------------------------------------------------------#

# Function to extract temperature for a specific day and hour subset

extract.t2m.by.hour <- function(dat, h, t2m.array, lon, lat, y2) {
  mig.dat.hour <- dat %>% filter(hour_of_year == h)
  
  if (nrow(mig.dat.hour) != 0) {
    # Need to add plus 1 because ERA5 defines hour as 1 starting at 00:00, not hour 0
    r <- raster(t(t2m.array[,,h+1]), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), 
                crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
    mig.dat.hour.sp <- st_as_sf(mig.dat.hour, coords = c("Longitude", "Latitude"), crs = 4326)
    mig.dat.hour <- mig.dat.hour %>% mutate(t2m = extract(r, mig.dat.hour.sp))
  }
  
  print(paste(Sys.time(), "year =", y2, "hour of year =", h))
  return(mig.dat.hour)
}

extract.t2m.by.year <- function(mig.dat, y){
  mig.dat.year <- mig.dat %>% dplyr::filter(year == !!y)
  
  if(nrow(mig.dat.year) != 0){
    t2m.nc <- nc_open(paste0("data/temp.", y, ".nc")) 
    
    # LOAD ONCE PER YEAR (This eliminates the major slowdown)
    lon <- ncvar_get(t2m.nc, "longitude") 
    lat <- ncvar_get(t2m.nc, "latitude", verbose = FALSE) 
    t2m.array <- ncvar_get(t2m.nc, "t2m") 
    nc_close(t2m.nc) # Safe to close early since data is now in memory
    
    # Loop over unique hours instead of days
    unique.hours <- sort(unique(mig.dat.year$hour_of_year))
    mig.dat.all.hours <- lapply(unique.hours, extract.t2m.by.hour, 
                                dat = mig.dat.year, t2m.array = t2m.array, 
                                lon = lon, lat = lat, y2 = y)
    
    mig.dat.all.hours <- bind_rows(mig.dat.all.hours)
    return(mig.dat.all.hours)
  }
}

start <- Sys.time()
alltemp <- lapply(2022:2026, extract.t2m.by.year, mig.dat = ecological_periods) 
end <- Sys.time()
print(end - start)


alltemp_binded <- bind_rows(alltemp[!sapply(alltemp, is.null)]) 

 dbWriteTable(conn, "hourly_temp_gps", alltemp_binded, append = FALSE, row.names = FALSE)



#-------------------------------------------------------------------------#
###### Precipitation function  ###################
#-------------------------------------------------------------------------#

extract.precip.by.day <- function(dat, d, nc){
  mig.dat.day <- dat %>% dplyr::filter(day_of_year == d)
  lon <- ncvar_get(nc, "lon") # longitude: deg 0 - 360
  lat <- ncvar_get(nc, "lat", verbose = F) # latitude: deg -90 0 90
  precip.array <- ncvar_get(nc, "precip") # extract precip data
  r <- rotate(raster(t(precip.array[,,d]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                     crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))) # convert to raster
  
  
  mig.dat.day.sp <- st_as_sf(mig.dat.day, coords = c("Longitude", "Latitude"), crs=4326)
  mig.dat.day <- mig.dat.day %>% ungroup() %>% mutate(precip = terra::extract(r, mig.dat.day.sp))
  
  
  print(paste(Sys.time(), "day of year =", d))
  
  return(mig.dat.day)
}


# function to extract precipitation for all days in a given year
# mig.dat = GPS data for all years, y = year
extract.precip.by.year <- function(mig.dat, y){
  mig.dat.year <- mig.dat %>% dplyr::filter(year == y)
  if(nrow(mig.dat.year)!=0){
    precip.nc <- nc_open(paste0("data/precip/precip.",y,".nc")) # MAKE SURE TO CHANGE FOR YOUR DATA
    unique.days <- unique(mig.dat.year$day_of_year)
    mig.dat.all.days <- lapply(unique.days, extract.precip.by.day, dat = mig.dat.year, nc = precip.nc)
    mig.dat.all.days <- bind_rows(mig.dat.all.days)
    nc_close(precip.nc)
    return(mig.dat.all.days)
  }
}

allprecip <- lapply(2022:2025, extract.precip.by.year, mig.dat = nesting_data) # hourly GPS locations

allprecip_binded <- bind_rows(allprecip[!sapply(allprecip, is.null)]) 


# brooding_data_summarized_precip <- allprecip_binded %>%
#   group_by(birdid_year) %>%
#   summarize(mean_precip = mean(precip)) 
# 
# write.csv(brooding_data_summarized_precip, "data/gps_with_weather/brooding_data_summarized_precip_6Feb2026.csv")

nesting_data_summarized_precip <- allprecip_binded %>%
  group_by(birdid_year_attempt) %>%
  summarize(mean_precip = mean(precip)) 

write.csv(nesting_data_summarized_precip, "data/gps_with_weather/nesting_data_summarized_precip_6Feb2026.csv")


