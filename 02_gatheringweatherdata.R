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
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=overview 
# make sure to download the cdf version


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

# dbWriteTable(conn, "hourly_temp_gps", alltemp_binded, append = FALSE, row.names = FALSE, overwrite = TRUE)

#-------------------------------------------------------------------------#
###### Precipitation function  ###################
#-------------------------------------------------------------------------#
 
 ecological_periods_day <- ecological_periods %>%
   dplyr::select(bandnum, UTC_date, Latitude, Longitude, year, day_of_year) %>%
   group_by(bandnum, UTC_date) %>%
   slice_head(n = 1)
   
   
 # first, download from noaa: https://psl.noaa.gov/data/gridded/data.cpc.globalprecip.html
 extract.precip.by.year <- function(mig.dat, y) {
   
   mig.dat.year <- mig.dat %>%
     dplyr::filter(year == y)
   
   if (nrow(mig.dat.year) != 0) {
     
     # 1. Open the NetCDF file directly as a SpatRaster stack
     nc_path <- paste0("data/precip.", y, ".nc")
     r_stack <- terra::rast(nc_path)
     
     # 2. Set CRS and rotate from 0-360 to -180-180 longitude
     terra::crs(r_stack) <- "EPSG:4326"
     r_stack <- terra::rotate(r_stack)
     
     # 3. Loop through each unique day of the year present in your data
     unique.days <- unique(mig.dat.year$day_of_year)
     
     mig.dat.all.days <- lapply(unique.days, function(d) {
       
       mig.dat.day <- mig.dat.year %>%
         dplyr::filter(day_of_year == d)
       
       # Convert day points to terra SpatVector (faster and native to terra)
       mig.dat.day.vect <- terra::vect(
         mig.dat.day, 
         geom = c("Longitude", "Latitude"), 
         crs = "EPSG:4326"
       )
       
       # Extract precipitation from the raster layer matching day of year `d`
       # (Assumes layers in the NetCDF correspond 1:1 with day_of_year)
       precip_values <- terra::extract(r_stack[[d]], mig.dat.day.vect)[, 2]
       
       mig.dat.day <- mig.dat.day %>%
         ungroup() %>%
         mutate(precip = precip_values)
       
       return(mig.dat.day)
     })
     
     mig.dat.all.days <- bind_rows(mig.dat.all.days)
     print(paste(Sys.time(), "Completed year =", y))
     
     return(mig.dat.all.days)
   }
 }
 
 # Run across your years
 allprecip <- lapply(2022:2026, extract.precip.by.year, mig.dat = ecological_periods_day)
 allprecip_binded <- bind_rows(allprecip[!sapply(allprecip, is.null)])
 
 summary(allprecip_binded)
 
 # about 6 Nas but probably due to the points being over an ocean, just taking from the day before. 
 
 allprecip_binded_final <- allprecip_binded %>%
   group_by(bandnum) %>%
   arrange(bandnum, UTC_date) %>%
   fill(precip, .direction = c("down"))

# dbWriteTable(conn, "daily_precip_gps", allprecip_binded_final, append = FALSE, row.names = FALSE, overwrite = TRUE)



