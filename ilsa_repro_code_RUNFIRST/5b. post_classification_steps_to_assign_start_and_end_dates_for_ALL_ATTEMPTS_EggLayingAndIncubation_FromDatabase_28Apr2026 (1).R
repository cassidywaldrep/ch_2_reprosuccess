# Assigning nest dates and location for attempts that did not receive dates and 
# locations from recurse + ruleset approach (code 5)
# Author: Ilsa Griebel
# Creation Date: 29 Apr 2026

#load libraries
library(tidyverse)
library(RPostgres)
library(lubridate)

# connect to database
conn <- dbConnect(
  Postgres(),
  dbname = "ABDU",
  #dbname = "EMALL",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "blackducks"
)

# combining nest dates and window dates ####
# load nest dates

nest_dates <- tbl(conn, "MALL2022to2025_StartEndDates_IncubationEgg_NestLocation_30Jun26") %>%
collect()

# reduce nest dates to only birds classified as nested by the egg laying model
nest_dates <- nest_dates %>% filter(status_laying == "egg_laying")

n_nested <- unique(nest_dates$birdid_year)

# load window dates
window_dates <- tbl(conn, "window_date_ranges_for_all_attempts_30June26") %>%
  collect()

# reduce window dates to only birds classified as nested by egg laying model
window_dates <- window_dates %>% filter(status_laying == "egg_laying")

n_window <- unique(window_dates$birdid_year) # 914 mallards

# load max absx window dates
max_absx_dates <- tbl(conn, "max_absx_window_dates_30June26") %>%
  collect()

# rename window columns to distinguish max absx windows from full window range
max_absx_dates <- max_absx_dates %>%
  rename(egg_range_start_attempt1_absx=egg_range_start_attempt1,
         egg_range_start_attempt2_absx=egg_range_start_attempt2,
         egg_range_start_attempt3_absx=egg_range_start_attempt3,
         egg_range_start_attempt4_absx=egg_range_start_attempt4,
         egg_range_start_attempt5_absx=egg_range_start_attempt5,
         egg_range_start_attempt6_absx=egg_range_start_attempt6,
         egg_range_end_attempt1_absx=egg_range_end_attempt1,
         egg_range_end_attempt2_absx=egg_range_end_attempt2,
         egg_range_end_attempt3_absx=egg_range_end_attempt3,
         egg_range_end_attempt4_absx=egg_range_end_attempt4,
         egg_range_end_attempt5_absx=egg_range_end_attempt5,
         egg_range_end_attempt6_absx=egg_range_end_attempt6,
         fail_inc_range_start_absx=fail_inc_range_start,
         fail_inc_range_end_absx=fail_inc_range_end,
         succ_inc_range_start_absx=succ_inc_range_start,
         succ_inc_range_end_absx=succ_inc_range_end
  )

# remove status columns (use from windows df)
max_absx_dates <- max_absx_dates %>% dplyr::select(-status_laying, -status_incubation, -status_comb)


# merge with window dates
window_dates <- left_join(window_dates, max_absx_dates, by = "birdid_year")

n <- unique(window_dates$birdid_year)

### making a plan:
## for FAC, birds that didn't have dates, I used:
### for failed/successful incubation, single window with greatest mean ABSX that was classified as failed/successful
### for non-incubating/NA incubation, used full window date range classified as nesting by the egg laying model

## Only used recurse+ruleset dates IF they overlapped with window date ranges 
## classified as hatched for hatched birds, classified as failed for failed birds, 
## classified as nested for non-incubating/NA birds 


# add rows to window dates so each bird has a row for every attempt (same as nest dates df) 
# and has nest dates set based on window ranges
ids <- unique(window_dates$birdid_year)

new_data <- data.frame()

for (i in 1:length(ids)) {
  # filter window_dates to one bird_year
  one_bird <- window_dates %>% filter(birdid_year == ids[i])
  
  # add attempt column
  one_bird$attempt <- NA
  one_bird$nest_start_window_method <- NA
  one_bird$nest_end_window_method <- NA
  
  if (!is.na(one_bird$egg_range_start_attempt1)) {
    temp <- one_bird
    temp$attempt <- "1"
    temp$nest_start_window_method <- temp$egg_range_start_attempt1
    temp$nest_end_window_method <- temp$egg_range_end_attempt1+7
    new_data <- rbind(new_data, temp)
  }
  
  if (!is.na(one_bird$egg_range_start_attempt2)) {
    temp <- one_bird
    temp$attempt <- "2"
    temp$nest_start_window_method <- temp$egg_range_start_attempt2
    temp$nest_end_window_method <- temp$egg_range_end_attempt2+7
    new_data <- rbind(new_data, temp)
  }
  
  if (!is.na(one_bird$egg_range_start_attempt3)) {
    temp <- one_bird
    temp$attempt <- "3"
    temp$nest_start_window_method <- temp$egg_range_start_attempt3
    temp$nest_end_window_method <- temp$egg_range_end_attempt3+7
    new_data <- rbind(new_data, temp)
  }
  
  if (!is.na(one_bird$egg_range_start_attempt4)) {
    temp <- one_bird
    temp$attempt <- "4"
    temp$nest_start_window_method <- temp$egg_range_start_attempt4
    temp$nest_end_window_method <- temp$egg_range_end_attempt4+7
    new_data <- rbind(new_data, temp)
  }
  
  if (!is.na(one_bird$egg_range_start_attempt5)) {
    temp <- one_bird
    temp$attempt <- "5"
    temp$nest_start_window_method <- temp$egg_range_start_attempt5
    temp$nest_end_window_method <- temp$egg_range_end_attempt5+7
    new_data <- rbind(new_data, temp)
  }
  
  if (!is.na(one_bird$egg_range_start_attempt6)) {
    temp <- one_bird
    temp$attempt <- "6"
    temp$nest_start_window_method <- temp$egg_range_start_attempt6
    temp$nest_end_window_method <- temp$egg_range_end_attempt6+7
    new_data <- rbind(new_data, temp)
  }
  
  if (!is.na(one_bird$fail_inc_range_start) & one_bird$status_incubation == "failed") {
    temp <- one_bird
    temp$attempt <- "f"
    temp$nest_start_window_method <- temp$fail_inc_range_start_absx
    temp$nest_end_window_method <- temp$fail_inc_range_end_absx+26
    new_data <- rbind(new_data, temp)
  }
  
  if (!is.na(one_bird$succ_inc_range_start)) {
    temp <- one_bird
    temp$attempt <- "h"
    temp$nest_start_window_method <- temp$succ_inc_range_start_absx
    temp$nest_end_window_method <- temp$succ_inc_range_end_absx+26
    new_data <- rbind(new_data, temp)
  }
  
  
}

window_dates2 <- new_data

n <- unique(window_dates2$birdid_year)

# set start and end nest dates using egg laying and incubation dates
nest_dates <- nest_dates %>%
  mutate(nest_start=ifelse(is.na(lay_start), inc_start, lay_start),
         nest_end=ifelse(is.na(lay_end), inc_end, lay_end)) %>%
  mutate(nest_start=as.Date(nest_start),
         nest_end=as.Date(nest_end))

# flag any attempts that overlap with the successful incubation date range
nest_dates$overlap_with_succ_range <- "N"
# loop over all rows of start_end_dates df
for (i in 1:nrow(nest_dates)) {
  if (nest_dates$status_comb[i] == "egg_laying_hatched") {
    # Define your date ranges
    start_date1 <- as.Date(nest_dates$nest_start[i])
    end_date1 <- as.Date(nest_dates$nest_end[i])
    
    start_date2 <- as.Date(nest_dates$succ_inc_range_start[i])
    end_date2 <- as.Date(nest_dates$succ_inc_range_end[i])
    
    # Create Interval objects
    interval1 <- interval(start_date1, end_date1)
    interval2 <- interval(start_date2, end_date2)
    
    if(int_overlaps(interval1, interval2)) {
      nest_dates$overlap_with_succ_range[i] <- "Y"
    }
  }
  
  i <- i + 1
}

# flag any attempts that overlap with the failed incubation date range
nest_dates$overlap_with_fail_range <- "N"
# loop over all rows of start_end_dates df
for (i in 1:nrow(nest_dates)) {
  if (nest_dates$status_comb[i] == "egg_laying_failed") {
    # Define your date ranges
    start_date1 <- as.Date(nest_dates$nest_start[i])
    end_date1 <- as.Date(nest_dates$nest_end[i])
    
    start_date2 <- as.Date(nest_dates$fail_inc_range_start[i])
    end_date2 <- as.Date(nest_dates$fail_inc_range_end[i])
    
    # Create Interval objects
    interval1 <- interval(start_date1, end_date1)
    interval2 <- interval(start_date2, end_date2)
    
    if(int_overlaps(interval1, interval2)) {
      nest_dates$overlap_with_fail_range[i] <- "Y"
    }
  }
  
  i <- i + 1
}

# check how many attempts there are
unique(nest_dates$attempt) # 1:5

# flag any attempts that overlap with the nested date range from egg laying model
nest_dates$overlap_with_nest_range <- "N"
# loop over all rows of start_end_dates df
for (i in 1:nrow(nest_dates)) {
  #if (nest_dates$status_comb[i] == "egg_laying_defer" | nest_dates$status_comb[i] == "egg_laying_NA" ) { # removed this because now I want to keep all attempts, so checking this for any status bird
    if (nest_dates$attempt[i] == "1") {
      # Define your date ranges
      start_date1 <- as.Date(nest_dates$nest_start[i])
      end_date1 <- as.Date(nest_dates$nest_end[i])
      
      start_date2 <- as.Date(nest_dates$egg_range_start_attempt1[i])
      end_date2 <- as.Date(nest_dates$egg_range_end_attempt1[i])
      
      # Create Interval objects
      interval1 <- interval(start_date1, end_date1)
      interval2 <- interval(start_date2, end_date2)
      
      if(int_overlaps(interval1, interval2)) {
        nest_dates$overlap_with_nest_range[i] <- "Y"
      }
      
    } else if (nest_dates$attempt[i] == "2") {
      # Define your date ranges
      start_date1 <- as.Date(nest_dates$nest_start[i])
      end_date1 <- as.Date(nest_dates$nest_end[i])
      
      start_date2 <- as.Date(nest_dates$egg_range_start_attempt2[i])
      end_date2 <- as.Date(nest_dates$egg_range_end_attempt2[i])
      
      # Create Interval objects
      interval1 <- interval(start_date1, end_date1)
      interval2 <- interval(start_date2, end_date2)
      
      if(int_overlaps(interval1, interval2)) {
        nest_dates$overlap_with_nest_range[i] <- "Y"
      }
      
    } else if (nest_dates$attempt[i] == "3") {
      # Define your date ranges
      start_date1 <- as.Date(nest_dates$nest_start[i])
      end_date1 <- as.Date(nest_dates$nest_end[i])
      
      start_date2 <- as.Date(nest_dates$egg_range_start_attempt3[i])
      end_date2 <- as.Date(nest_dates$egg_range_end_attempt3[i])
      
      # Create Interval objects
      interval1 <- interval(start_date1, end_date1)
      interval2 <- interval(start_date2, end_date2)
      
      if(int_overlaps(interval1, interval2)) {
        nest_dates$overlap_with_nest_range[i] <- "Y"
      }
      
    } else if (nest_dates$attempt[i] == "4") {
      # Define your date ranges
      start_date1 <- as.Date(nest_dates$nest_start[i])
      end_date1 <- as.Date(nest_dates$nest_end[i])
      
      start_date2 <- as.Date(nest_dates$egg_range_start_attempt4[i])
      end_date2 <- as.Date(nest_dates$egg_range_end_attempt4[i])
      
      # Create Interval objects
      interval1 <- interval(start_date1, end_date1)
      interval2 <- interval(start_date2, end_date2)
      
      if(int_overlaps(interval1, interval2)) {
        nest_dates$overlap_with_nest_range[i] <- "Y"
      }
      
    } else if (nest_dates$attempt[i] == "5") {
      # Define your date ranges
      start_date1 <- as.Date(nest_dates$nest_start[i])
      end_date1 <- as.Date(nest_dates$nest_end[i])
      
      start_date2 <- as.Date(nest_dates$egg_range_start_attempt5[i])
      end_date2 <- as.Date(nest_dates$egg_range_end_attempt5[i])
      
      # Create Interval objects
      interval1 <- interval(start_date1, end_date1)
      interval2 <- interval(start_date2, end_date2)
      
      if(int_overlaps(interval1, interval2)) {
        nest_dates$overlap_with_nest_range[i] <- "Y"
      }
      
    #} 
  }
  
  
  i <- i + 1
}

# number of bird-years with assigned dates 
n1 <- unique(nest_dates$birdid_year) # 378 ABDU, 749 MALL

# number of bird-years with more than one attempt
sum <- nest_dates %>% group_by(birdid_year) %>%
  summarize(n = n())

sum %>% group_by(n) %>% count() # 105 bird-years with more than one attempt (ABDU), 319 bird-years with more than one attempt (MALL)

# make nest dates NA if the recurse+ruleset dates don't overlap with classified window when they should (h attempts with successful window, f attempts with failed window, 1:5 with egg laying windows)
nest_dates_hatched <- nest_dates %>%
  filter(attempt == "h") %>%
  mutate(nest_start=ifelse(overlap_with_succ_range=="N", NA, nest_start),
         nest_end=ifelse(overlap_with_succ_range=="N", NA, nest_end)) %>%
  mutate(nest_start=as.Date(nest_start),
         nest_end=as.Date(nest_end))

nest_dates_failed <- nest_dates %>%
  filter(attempt == "f") %>%
  mutate(nest_start=ifelse(overlap_with_fail_range=="N", NA, nest_start),
         nest_end=ifelse(overlap_with_fail_range=="N", NA, nest_end)) %>%
  mutate(nest_start=as.Date(nest_start),
         nest_end=as.Date(nest_end))

nest_dates_attempts <- nest_dates %>%
  filter(attempt == "1" | attempt == "2" | attempt == "3"| attempt == "4"| attempt == "5" ) %>%
  mutate(nest_start=ifelse(overlap_with_nest_range=="N", NA, nest_start),
         nest_end=ifelse(overlap_with_nest_range=="N", NA, nest_end)) %>%
  mutate(nest_start=as.Date(nest_start),
         nest_end=as.Date(nest_end))

# bind back together all different types of attempts
nest_dates2 <- rbind(nest_dates_hatched, nest_dates_failed, nest_dates_attempts)

nest_dates2_NAremoved <- nest_dates2 %>% filter(is.na(nest_start) == FALSE)
n2 <- unique(nest_dates2_NAremoved$birdid_year) # 703 with dates assigned (MALL), 341 with dates assigned (ABDU)

## merge nest_dates2_NAremoved and window_dates2 dfs 
# get just the columns you need from nest_dates2_NAremoved
nest_dates2_NAremoved <- nest_dates2_NAremoved %>%
  dplyr::select(birdid_year_nest, nest_start, nest_end, nest_lat, nest_long, num_of_visits) %>%
  rename(nest_start_recurse= nest_start,
         nest_end_recurse=nest_end)

# add birdid_year_nest to window_dates2 df
window_dates2 <- window_dates2 %>%
  mutate(birdid_year_nest = paste(birdid_year, attempt, sep="_"))

# add recurse+ruleset nest dates to window dates df
combined_dates <- left_join(window_dates2, nest_dates2_NAremoved, by = "birdid_year_nest")

n <- unique(combined_dates$birdid_year)

# add date assignment method
combined_dates <- combined_dates %>%
  mutate(date_assignment_method=ifelse(is.na(nest_start_recurse), "window", "recurse"))
  
# add nest date start and end date columns (use recurse method if successful, otherwise use window dates)  
combined_dates <- combined_dates %>%
  mutate(nest_start=ifelse(is.na(nest_start_recurse), nest_start_window_method, nest_start_recurse),
         nest_end=ifelse(is.na(nest_end_recurse), nest_end_window_method, nest_end_recurse)) %>%
  mutate(nest_start=as.Date(nest_start),
         nest_end=as.Date(nest_end))

## remove overlapping attempts
# first sort by birdid_year and then nest_start date, so consecutive attempts are within one row of each other
combined_dates <- combined_dates %>% arrange(birdid_year, nest_start)

# if two consecutive rows for the same bird_year overlap, then select the one that:
 # uses recurse
 # if both use recurse, then select the one that had the most visits
 # if neither use recurse and one is a fail, use the # attempt instead (because egg laying window will be more exact than using full 26 days)
 # if neither use recurse and one is hatched, use the hatched one (26 day window with greatest max absx, will be pretty close)

# loop through all bird_years
ids <- unique(combined_dates$birdid_year)
new_data <- data.frame()
i <- 1

for (i in 1:length(ids)) {
  # get data for one bird_year
  one_bird <- combined_dates %>% filter(birdid_year == ids[i])
  
  j <- 1
  # go through each row one by one
  
  while (j < nrow(one_bird)) {
    # Define your date ranges
    start_date1 <- as.Date(one_bird$nest_start[j])
    end_date1 <- as.Date(one_bird$nest_end[j])
    
    start_date2 <- as.Date(one_bird$nest_start[j+1])
    end_date2 <- as.Date(one_bird$nest_end[j+1])
    
    # Create Interval objects
    interval1 <- interval(start_date1, end_date1)
    interval2 <- interval(start_date2, end_date2)
    
    # get only the two rows of data you are assessing
    two_rows <- one_bird[c(j, j+1),]
    
    # if two consecutive rows for the same bird_year overlap,
    if(int_overlaps(interval1, interval2)) {
     # then select the one that a) uses recurse if one uses recurse and the other uses window method
      if ("recurse" %in% unique(two_rows$date_assignment_method) == TRUE & 
          "window" %in% unique(two_rows$date_assignment_method) == TRUE) {
        row_to_remove <- two_rows %>% filter(date_assignment_method == "window")
        
        
      } else if ("recurse" %in% unique(two_rows$date_assignment_method) == TRUE) { 
        #  if both use recurse, then select the one that had the most visits
        row_to_remove <- two_rows %>% filter(num_of_visits == min(num_of_visits))
        if(nrow(row_to_remove) == 2) { # there is one mallard (2247-74999_2023) with two attempts, both recurse dates, both with same number of visits; just going to keep the earlier of the two)
          row_to_remove <- row_to_remove %>%
            slice_tail(n=1)
        }
        
      } else if ("f" %in% unique(two_rows$attempt) == TRUE) { 
        #  if neither use recurse and one is a fail, use the # attempt instead (because egg laying window will be more exact than using full 26 days)
        row_to_remove <- two_rows %>% filter(attempt == "f")
        
      } else if ("h" %in% unique(two_rows$attempt) == TRUE) {  
        # if neither use recurse and one is hatched, use the hatched one (26 day window with greatest max absx, will be pretty close)
        row_to_remove <- two_rows %>% filter(attempt != "h")
        
      } else  {  
        # if neither use recurse and both are attempts, use the first one
        row_to_remove <- two_rows %>% filter(nest_start == max(nest_start))
        
      }
      one_bird <- one_bird %>% filter(!(birdid_year_nest %in% row_to_remove$birdid_year_nest))
  }
  j <- j + 1
  }
  
  new_data <- rbind(new_data, one_bird)
  
}

# removed 532 rows of overlapping attempts (mallards), 210 rows (ABDU)
combined_dates <- new_data

n <- unique(combined_dates$birdid_year)

# re-run the loop a second time to make sure there are no overlaps left
# loop through all bird_years
ids <- unique(combined_dates$birdid_year)
new_data <- data.frame()
i <- 1

for (i in 1:length(ids)) {
  # get data for one bird_year
  one_bird <- combined_dates %>% filter(birdid_year == ids[i])
  
  j <- 1
  # go through each row one by one
  
  while (j < nrow(one_bird)) {
    # Define your date ranges
    start_date1 <- as.Date(one_bird$nest_start[j])
    end_date1 <- as.Date(one_bird$nest_end[j])
    
    start_date2 <- as.Date(one_bird$nest_start[j+1])
    end_date2 <- as.Date(one_bird$nest_end[j+1])
    
    # Create Interval objects
    interval1 <- interval(start_date1, end_date1)
    interval2 <- interval(start_date2, end_date2)
    
    # get only the two rows of data you are assessing
    two_rows <- one_bird[c(j, j+1),]
    
    # if two consecutive rows for the same bird_year overlap,
    if(int_overlaps(interval1, interval2)) {
      # then select the one that a) uses recurse if one uses recurse and the other uses window method
      if ("recurse" %in% unique(two_rows$date_assignment_method) == TRUE & 
          "window" %in% unique(two_rows$date_assignment_method) == TRUE) {
        row_to_remove <- two_rows %>% filter(date_assignment_method == "window")
        
        
      } else if ("recurse" %in% unique(two_rows$date_assignment_method) == TRUE) { 
        #  if both use recurse, then select the one that had the most visits
        row_to_remove <- two_rows %>% filter(num_of_visits == min(num_of_visits))
        if(nrow(row_to_remove) == 2) { # there is one mallard (2247-74999_2023) with two attempts, both recurse dates, both with same number of visits; just going to keep the earlier of the two)
          row_to_remove <- row_to_remove %>%
            slice_tail(n=1)
        }
        
      } else if ("f" %in% unique(two_rows$attempt) == TRUE) { 
        #  if neither use recurse and one is a fail, use the # attempt instead (because egg laying window will be more exact than using full 26 days)
        row_to_remove <- two_rows %>% filter(attempt == "f")
        
      } else if ("h" %in% unique(two_rows$attempt) == TRUE) {  
        # if neither use recurse and one is hatched, use the hatched one (26 day window with greatest max absx, will be pretty close)
        row_to_remove <- two_rows %>% filter(attempt != "h")
        
      } else  {  
        # if neither use recurse and both are attempts, use the first one
        row_to_remove <- two_rows %>% filter(nest_start == max(nest_start))
        
      }
      one_bird <- one_bird %>% filter(!(birdid_year_nest %in% row_to_remove$birdid_year_nest))
    }
    j <- j + 1
  }
  
  new_data <- rbind(new_data, one_bird)
  
}
# removed 33 rows of overlapping attempts (mallards), 14 for ABDU
combined_dates <- new_data

# repeat again until no rows are removed! 
# loop through all bird_years
ids <- unique(combined_dates$birdid_year)
new_data <- data.frame()
i <- 1

for (i in 1:length(ids)) {
  # get data for one bird_year
  one_bird <- combined_dates %>% filter(birdid_year == ids[i])
  
  j <- 1
  # go through each row one by one
  
  while (j < nrow(one_bird)) {
    # Define your date ranges
    start_date1 <- as.Date(one_bird$nest_start[j])
    end_date1 <- as.Date(one_bird$nest_end[j])
    
    start_date2 <- as.Date(one_bird$nest_start[j+1])
    end_date2 <- as.Date(one_bird$nest_end[j+1])
    
    # Create Interval objects
    interval1 <- interval(start_date1, end_date1)
    interval2 <- interval(start_date2, end_date2)
    
    # get only the two rows of data you are assessing
    two_rows <- one_bird[c(j, j+1),]
    
    # if two consecutive rows for the same bird_year overlap,
    if(int_overlaps(interval1, interval2)) {
      # then select the one that a) uses recurse if one uses recurse and the other uses window method
      if ("recurse" %in% unique(two_rows$date_assignment_method) == TRUE & 
          "window" %in% unique(two_rows$date_assignment_method) == TRUE) {
        row_to_remove <- two_rows %>% filter(date_assignment_method == "window")
        
        
      } else if ("recurse" %in% unique(two_rows$date_assignment_method) == TRUE) { 
        #  if both use recurse, then select the one that had the most visits
        row_to_remove <- two_rows %>% filter(num_of_visits == min(num_of_visits))
        if(nrow(row_to_remove) == 2) { # there is one mallard (2247-74999_2023) with two attempts, both recurse dates, both with same number of visits; just going to keep the earlier of the two)
          row_to_remove <- row_to_remove %>%
            slice_tail(n=1)
        }
        
      } else if ("f" %in% unique(two_rows$attempt) == TRUE) { 
        #  if neither use recurse and one is a fail, use the # attempt instead (because egg laying window will be more exact than using full 26 days)
        row_to_remove <- two_rows %>% filter(attempt == "f")
        
      } else if ("h" %in% unique(two_rows$attempt) == TRUE) {  
        # if neither use recurse and one is hatched, use the hatched one (26 day window with greatest max absx, will be pretty close)
        row_to_remove <- two_rows %>% filter(attempt != "h")
        
      } else  {  
        # if neither use recurse and both are attempts, use the first one
        row_to_remove <- two_rows %>% filter(nest_start == max(nest_start))
        
      }
      one_bird <- one_bird %>% filter(!(birdid_year_nest %in% row_to_remove$birdid_year_nest))
    }
    j <- j + 1
  }
  
  new_data <- rbind(new_data, one_bird)
  
}
# removed 1 row of overlapping attempts (mallards), 0 for ABDU (done for ABDU, can move on to past these loops!!)
combined_dates <- new_data


# repeat yet AGAIN until no rows are removed!
# loop through all bird_years
ids <- unique(combined_dates$birdid_year)
new_data <- data.frame()
i <- 1

for (i in 1:length(ids)) {
  # get data for one bird_year
  one_bird <- combined_dates %>% filter(birdid_year == ids[i])
  
  j <- 1
  # go through each row one by one
  
  while (j < nrow(one_bird)) {
    # Define your date ranges
    start_date1 <- as.Date(one_bird$nest_start[j])
    end_date1 <- as.Date(one_bird$nest_end[j])
    
    start_date2 <- as.Date(one_bird$nest_start[j+1])
    end_date2 <- as.Date(one_bird$nest_end[j+1])
    
    # Create Interval objects
    interval1 <- interval(start_date1, end_date1)
    interval2 <- interval(start_date2, end_date2)
    
    # get only the two rows of data you are assessing
    two_rows <- one_bird[c(j, j+1),]
    
    # if two consecutive rows for the same bird_year overlap,
    if(int_overlaps(interval1, interval2)) {
      # then select the one that a) uses recurse if one uses recurse and the other uses window method
      if ("recurse" %in% unique(two_rows$date_assignment_method) == TRUE & 
          "window" %in% unique(two_rows$date_assignment_method) == TRUE) {
        row_to_remove <- two_rows %>% filter(date_assignment_method == "window")
        
        
      } else if ("recurse" %in% unique(two_rows$date_assignment_method) == TRUE) { 
        #  if both use recurse, then select the one that had the most visits
        row_to_remove <- two_rows %>% filter(num_of_visits == min(num_of_visits))
        if(nrow(row_to_remove) == 2) { # there is one mallard (2247-74999_2023) with two attempts, both recurse dates, both with same number of visits; just going to keep the earlier of the two)
          row_to_remove <- row_to_remove %>%
            slice_tail(n=1)
        }
        
      } else if ("f" %in% unique(two_rows$attempt) == TRUE) { 
        #  if neither use recurse and one is a fail, use the # attempt instead (because egg laying window will be more exact than using full 26 days)
        row_to_remove <- two_rows %>% filter(attempt == "f")
        
      } else if ("h" %in% unique(two_rows$attempt) == TRUE) {  
        # if neither use recurse and one is hatched, use the hatched one (26 day window with greatest max absx, will be pretty close)
        row_to_remove <- two_rows %>% filter(attempt != "h")
        
      } else  {  
        # if neither use recurse and both are attempts, use the first one
        row_to_remove <- two_rows %>% filter(nest_start == max(nest_start))
        
      }
      one_bird <- one_bird %>% filter(!(birdid_year_nest %in% row_to_remove$birdid_year_nest))
    }
    j <- j + 1
  }
  
  new_data <- rbind(new_data, one_bird)
  
}

# removed 1 row of overlapping attempts (mallards)
combined_dates <- new_data

# repeat yet AGAIN AAGAAIIIN until no rows are removed!
# loop through all bird_years
ids <- unique(combined_dates$birdid_year)
new_data <- data.frame()
i <- 1

for (i in 1:length(ids)) {
  # get data for one bird_year
  one_bird <- combined_dates %>% filter(birdid_year == ids[i])
  
  j <- 1
  # go through each row one by one
  
  while (j < nrow(one_bird)) {
    # Define your date ranges
    start_date1 <- as.Date(one_bird$nest_start[j])
    end_date1 <- as.Date(one_bird$nest_end[j])
    
    start_date2 <- as.Date(one_bird$nest_start[j+1])
    end_date2 <- as.Date(one_bird$nest_end[j+1])
    
    # Create Interval objects
    interval1 <- interval(start_date1, end_date1)
    interval2 <- interval(start_date2, end_date2)
    
    # get only the two rows of data you are assessing
    two_rows <- one_bird[c(j, j+1),]
    
    # if two consecutive rows for the same bird_year overlap,
    if(int_overlaps(interval1, interval2)) {
      # then select the one that a) uses recurse if one uses recurse and the other uses window method
      if ("recurse" %in% unique(two_rows$date_assignment_method) == TRUE & 
          "window" %in% unique(two_rows$date_assignment_method) == TRUE) {
        row_to_remove <- two_rows %>% filter(date_assignment_method == "window")
        
        
      } else if ("recurse" %in% unique(two_rows$date_assignment_method) == TRUE) { 
        #  if both use recurse, then select the one that had the most visits
        row_to_remove <- two_rows %>% filter(num_of_visits == min(num_of_visits))
        if(nrow(row_to_remove) == 2) { # there is one mallard (2247-74999_2023) with two attempts, both recurse dates, both with same number of visits; just going to keep the earlier of the two)
          row_to_remove <- row_to_remove %>%
            slice_tail(n=1)
        }
        
      } else if ("f" %in% unique(two_rows$attempt) == TRUE) { 
        #  if neither use recurse and one is a fail, use the # attempt instead (because egg laying window will be more exact than using full 26 days)
        row_to_remove <- two_rows %>% filter(attempt == "f")
        
      } else if ("h" %in% unique(two_rows$attempt) == TRUE) {  
        # if neither use recurse and one is hatched, use the hatched one (26 day window with greatest max absx, will be pretty close)
        row_to_remove <- two_rows %>% filter(attempt != "h")
        
      } else  {  
        # if neither use recurse and both are attempts, use the first one
        row_to_remove <- two_rows %>% filter(nest_start == max(nest_start))
        
      }
      one_bird <- one_bird %>% filter(!(birdid_year_nest %in% row_to_remove$birdid_year_nest))
    }
    j <- j + 1
  }
  
  new_data <- rbind(new_data, one_bird)
  
}

# removed 0 rows of overlapping attempts (mallards) YAYYYYY!!!!
combined_dates <- new_data

# make attempt_new, numbering attempts based on the number of attempts left after removing overlaps
combined_dates <- combined_dates %>%
  # make sure ordered by nest_start date within each bird_year before you number the attempts
  arrange(birdid_year, nest_start) %>%
  group_by(birdid_year) %>%
  mutate(attempt_new = row_number())

rm(max_absx_dates, nest_dates, nest_dates_attempts, nest_dates_failed, nest_dates_hatched, 
   nest_dates2, nest_dates2_NAremoved, new_data, one_bird, row_to_remove, sum, temp, two_rows,
   window_dates, window_dates2)

# assign status to each attempt based on if the nest dates overlap window dates of successful window 
# (use window with max mean absx for successful window)
# NOTE: status is failed attempt for all nested birds except those classified as hatched, 
# which will have one attempt classified as hatched

# flag any attempts that overlap with the successful incubation date range
combined_dates$overlap_with_max_ABSX_succ_range <- "N"
# loop over all rows of start_end_dates df
for (i in 1:nrow(combined_dates)) {
  if (combined_dates$status_comb[i] == "egg_laying_hatched") {
    # Define your date ranges
    start_date1 <- as.Date(combined_dates$nest_start[i])
    end_date1 <- as.Date(combined_dates$nest_end[i])
    
    start_date2 <- as.Date(combined_dates$succ_inc_range_start_absx[i])
    end_date2 <- as.Date(combined_dates$succ_inc_range_end_absx[i])+26
    
    # Create Interval objects
    interval1 <- interval(start_date1, end_date1)
    interval2 <- interval(start_date2, end_date2)
    
    if(int_overlaps(interval1, interval2)) {
      combined_dates$overlap_with_max_ABSX_succ_range[i] <- "Y"
    }
  }
  
  i <- i + 1
}

# count the number of Y's per birdid_year to make sure no body has TWO yes's!!
sum <- combined_dates %>% 
  group_by(birdid_year, overlap_with_max_ABSX_succ_range) %>%
  count() %>% filter(overlap_with_max_ABSX_succ_range == "Y")

max(sum$n) # all hatched birds have only one Y!!!!

# make the attempt that overlaps with max ABSX success range equal to hatched
combined_dates <- combined_dates %>%
  mutate(attempt_status=ifelse(overlap_with_max_ABSX_succ_range == "Y", "successful", "failed"))

combined_dates %>% group_by(attempt_status) %>% count()

# remove any attempts after hatched attempt because of risk of classifying molt or brood rearing behaviour as nesting behaviour
# first filter to any last attempts that are failed for hatched status birds
combined_dates_hatched <- combined_dates %>% filter(status_incubation == "hatched") %>%
  filter(attempt_new==max(attempt_new)) %>%
  filter(attempt_status == "failed")

# remove these bird_id_nests from combined_dates (n = 22 attempts for mallards, 5 for abdu)
combined_dates2 <- combined_dates %>% filter(!(birdid_year_nest %in% combined_dates_hatched$birdid_year_nest))

# repeat filtering until no birds are removed
combined_dates_hatched <- combined_dates2 %>% filter(status_incubation == "hatched") %>%
  filter(attempt_new==max(attempt_new)) %>%
  filter(attempt_status == "failed")

# remove these bird_id_nests from combined_dates (n = 2 attempts for mallards, 0 for abdu)
combined_dates3 <- combined_dates2 %>% filter(!(birdid_year_nest %in% combined_dates_hatched$birdid_year_nest))

# repeat filtering until no birds are removed
combined_dates_hatched <- combined_dates3 %>% filter(status_incubation == "hatched") %>%
  filter(attempt_new==max(attempt_new)) %>%
  filter(attempt_status == "failed")
# 0 attempts!! 

combined_dates <- combined_dates3

rm(combined_dates_hatched, combined_dates2, combined_dates3, sum)

## add nest location as median lat long for window method assigned attempts
# load gps data
gps <- tbl(conn, "hourly_gps_locations_for_birds_with_inc_and_laying_classified") %>%
  collect()

# reduce to only nest dates of birds missing nest locations
missing_nest_location <- combined_dates %>% filter(is.na(nest_lat))

# add date to gps data
gps <- gps %>% mutate(date=as.Date(timestamp))

# loop through each row and get median lat long (can't just merge nest dates with gps and summarize because multiple nest dates for the same bird_year, right???)
i <- 1
for (i in 1:nrow(missing_nest_location)) {
# filter gps to bird_year and only nest dates
  gps_one_bird <- gps %>% filter(birdid_year == missing_nest_location$birdid_year[i]) %>%
    filter(date >= missing_nest_location$nest_start[i] & date <= missing_nest_location$nest_end[i])
  
  missing_nest_location$nest_lat[i] <- median(gps_one_bird$Latitude)
  missing_nest_location$nest_long[i] <- median(gps_one_bird$Longitude)
  
}

# remove missing nest location birds from combined data and add nest location method column
not_missing_nest_locations <- combined_dates %>% filter(!(is.na(nest_lat))) 

not_missing_nest_locations <- not_missing_nest_locations %>%
  mutate(nest_location_method="recurse")

# add nest location method column to missing nest location attempts
missing_nest_location <- missing_nest_location %>%
  mutate(nest_location_method="median")

#then rbind with missing location df that has nest locations added
combined_dates2 <- rbind(not_missing_nest_locations, missing_nest_location)
# number of rows should be the same as combined_dates df (they are!)

# reduce to necessary columns
combined_dates3 <- combined_dates2 %>% 
  dplyr::select(birdid_year_nest, birdid_year, status_laying, status_incubation, 
                status_comb, attempt_status, attempt_new, nest_start, nest_end,
                date_assignment_method, nest_lat, nest_long, nest_location_method) %>%
  arrange(birdid_year, attempt_new)

# save csv for EMALL team
# write.csv(combined_dates3, "C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics/Compiled Datasets for Machine Learning/nest_dates_and_locations_for_all_attempts_MALL_15May2026.csv")
# write.csv(combined_dates3, "C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics/Compiled Datasets for Machine Learning/nest_dates_and_locations_for_all_attempts_ABDU_15May2026.csv")


# save to database
#dbRemoveTable(conn, "nest_dates_and_locations_for_all_attempts_ILSA")
dbWriteTable(conn, "nest_dates_and_locations_for_all_attempts_30Jun2026", combined_dates3, append = TRUE, row.names = FALSE)

rm(list=ls())

# make summaries and figures for nest dates for manuscript ####
# load start_end dates and add bird age
setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics")
dates_abdu <- read.csv("Compiled Datasets for Machine Learning/nest_dates_and_locations_for_all_attempts_ABDU_15May2026.csv")
dates_mall <- read.csv("Compiled Datasets for Machine Learning/nest_dates_and_locations_for_all_attempts_MALL_15May2026.csv")


dates_abdu$species <- "ABDU"
dates_mall$species <- "MALL"

dates <- rbind(dates_abdu, dates_mall)

# summary of percent of dates assigned by each method
dates_abdu %>% group_by(date_assignment_method) %>% count()
422/681 # 62% assigned by recurse method
dates_mall %>% group_by(date_assignment_method) %>% count()
1010/1655 # 61%


# # summary for Josh
# sum_for_Josh <- dates_mall %>%
#   group_by(attempt_new, attempt_status) %>%
#  count()
# 
# n_by_attempt <- dates_mall %>% group_by(attempt_new) %>%
#   count() %>% 
#   rename(total=n)
# 
# sum_for_Josh <- left_join(sum_for_Josh, n_by_attempt, by = "attempt_new")
# 
# sum_for_Josh <- sum_for_Josh %>%
#   mutate(percent=n/total*100) 

# load deployment data
library(readxl)
deploy_abdu <- read_excel("Deployment Files/deployments_18Sept2025.xlsx", sheet = "ABDU") ## change to MALL / ABDU depending what you need!
deploy_mall <- read_excel("Deployment Files/deployments_18Sept2025.xlsx", sheet = "MALL") ## change to MALL / ABDU depending what you need!


# reduce deploy data to one device per year (latest deployment) and format data
deploy_abdu$`Banding Date` <- as.Date(deploy_abdu$`Banding Date`)
deploy_abdu <- deploy_abdu %>% 
  mutate(year=year(`Banding Date`)) %>% 
  group_by(year, `Transmitter Number`) %>%
  filter(`Banding Date` == max(`Banding Date`)) %>% 
  select(c("Transmitter Number", "Band Number", "year", "Age", "Species")) %>%
  rename(device_id=`Transmitter Number`,
         band=`Band Number`,
         age=Age,
         species=Species)

deploy_mall$`Banding Date` <- as.Date(deploy_mall$`Banding Date`)
deploy_mall <- deploy_mall %>% 
  mutate(year=year(`Banding Date`)) %>% 
  group_by(year, `Transmitter Number`) %>%
  filter(`Banding Date` == max(`Banding Date`)) %>% 
  select(c("Transmitter Number", "Band Number", "year", "Age", "Species")) %>%
  rename(device_id=`Transmitter Number`,
         band=`Band Number`,
         age=Age,
         species=Species)

deploy2 <- rbind(deploy_abdu, deploy_mall)

# merge by device (switched to band for MALL), year and KEEP ALL ROWS FROM DATES DF!
dates$band <- substr(dates$birdid_year_nest, start = 1, stop = 10)
dates$year <- substr(dates$birdid_year_nest, start = 12, stop = 15)
dates$attempt <- substr(dates$birdid_year_nest, start = 17, stop = 17)

dates2 <- merge(dates, deploy2, by = c("band", "year", "species"), all.x = TRUE) %>%
  select(c("device_id", "band", "year","species", "attempt_new", "age", "status_comb", "nest_start", "nest_end" ))

# make any age that is NA equal to ASY (NAs are because it is the second year of data from a bird, so year no longer matches banding year and device id - regardless if it was an SY or ASY at banding, it will be (or still be) an adult the next year. Could make this more precise and make ATY if ASY in previous year, but not necessary at this point... )
dates2$age[is.na(dates2$age)] <- "ASY"


# summarize laying dates, number of attempts, nesting outcomes by age
dates2[c("nest_start", "nest_end" )] <- lapply(dates2[c("nest_start", "nest_end" )], 
                                                                     as.Date)

## summaries of nest dates, using FIRST ATTEMPTS ONLY
dates3 <- dates2 %>% filter(attempt_new == "1")

# overall summary
sum_lay_dates <- dates3 %>% filter(is.na(nest_start) == FALSE) %>%
  mutate(nest_start=as.numeric(format(nest_start, "%j"))) %>%
  group_by(species) %>%
  summarise(n =n(),
            mean_nest_start=mean(nest_start),
            median_nest_start=median(nest_start),
            min_nest_start=min(nest_start),
            max_nest_start=max(nest_start),
            sd_nest_start=sd(nest_start))

# by age

sum_lay_dates_by_age <- dates3 %>% filter(is.na(nest_start) == FALSE) %>%
  mutate(nest_start=as.numeric(format(nest_start, "%j"))) %>%
  group_by(species, age) %>%
  summarise(n =n(),
            mean_nest_start=mean(nest_start),
            median_nest_start=median(nest_start),
            min_nest_start=min(nest_start),
            max_nest_start=max(nest_start),
            sd_nest_start=sd(nest_start))

sum_lay_dates_by_year <- dates3 %>% filter(is.na(nest_start) == FALSE) %>%
  mutate(nest_start=as.numeric(format(nest_start, "%j"))) %>%
  group_by(species, year) %>%
  summarise(n =n(),
            mean_nest_start=mean(nest_start),
            median_nest_start=median(nest_start),
            min_nest_start=min(nest_start),
            max_nest_start=max(nest_start),
            sd_nest_start=sd(nest_start))

dates3$nest_start_jul <- as.numeric(format(dates3$nest_start, "%j"))

## density plots of dates by age AND year for manuscript
library(ggpubr)
library(viridis)
n_age <- viridis(3)

tiff("plots_EggLayingAndIncubationCombined_14Sept2025/NestDatesByAgeAndYear_DensityPlots_14May2026.tiff", units="in", width=20, height=8, res=300)
#png("./plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncDatesByAge_DensityPlots.png",width = 800,height = 500,res = 100)
a<-dates3 %>%
  filter(species == "MALL") %>%
  mutate(
    lay_start_fake = as.Date(format(nest_start, "2000-%m-%d")),
    #inc_start_fake = as.Date(format(inc_start, "2000-%m-%d")),
    age=as.factor(age)
  ) %>%
  ggplot() +
  geom_density(aes(x = lay_start_fake, color = factor(age, levels = c("SY", "ASY"))), size = 1) + 
  #geom_density(aes(x = inc_start_fake, color = factor(age, levels = c("SY", "ASY")), linetype = "incubation"), size = 1) + 
  #scale_linetype_manual(values = c("laying" = "dashed", "incubation" = "solid")) +
  #guides(linetype = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = c("SY" = n_age[1],
                                "ASY" = n_age[2]))+
  scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
  # ggtitle("MALL") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  labs(
    color = "Age",
    #linetype = "Start Date",
    x = "Nesting Start Date",
    y = "Density"
  ) + theme(plot.margin = margin(1,0.1,0,0.1, 'cm'),
            # Set size for both axis titles 
            axis.title = element_text(size = 18),
            
            # Set size for both axis text/labels 
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),     # Size for labels
            legend.title = element_text(size = 17)      # Size for title
  )
b<-dates3 %>%
  filter(species == "ABDU") %>%
  mutate(
    lay_start_fake = as.Date(format(nest_start, "2000-%m-%d")),
    #inc_start_fake = as.Date(format(inc_start, "2000-%m-%d")),
    age=as.factor(age)
  ) %>%
  ggplot() +
  geom_density(aes(x = lay_start_fake, color = factor(age, levels = c("SY", "ASY"))), size = 1) + 
  #geom_density(aes(x = inc_start_fake, color = factor(age, levels = c("SY", "ASY")), linetype = "incubation"), size = 1) + 
  #scale_linetype_manual(values = c("laying" = "dashed", "incubation" = "solid")) +
  #guides(linetype = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = c("SY" = n_age[1],
                                "ASY" = n_age[2]))+
  scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
  # ggtitle("MALL") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  labs(
    color = "Age",
    #linetype = "Start Date",
    x = "Nesting Start Date",
    y = "Density"
  ) + theme(plot.margin = margin(1,0.1,0,0.1, 'cm'),
            # Set size for both axis titles 
            axis.title = element_text(size = 18),
            
            # Set size for both axis text/labels 
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),     # Size for labels
            legend.title = element_text(size = 17)      # Size for title
  )
c <- ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
               labels = c("ABDU", "MALL"),
               font.label = list(size = 18, color = "black", face = "bold"))

# by year colours
n_years <- viridis(5)
my_colors <- c("2021" = n_years[1],
               "2022" = n_years[2],
               "2023" = n_years[3],
               "2024" = n_years[4],
               "2025" = n_years[5])
dates3$year <- as.factor(dates3$year)

a<-dates3 %>%
  filter(species == "MALL") %>%
  mutate(
    lay_start_fake = as.Date(format(nest_start, "2000-%m-%d")),
    #inc_start_fake = as.Date(format(inc_start, "2000-%m-%d"))#,
    #year=as.factor(year)
  ) %>%
  ggplot() +
  geom_density(aes(x = lay_start_fake, 
                   color = year#,
                   # color = factor(year, levels = c("2021", "2022", "2023", "2024", "2025")), 
                   ), size = 1) + 
  #scale_linetype_manual(values = c("incubation" = "solid", "laying" = "dashed")) +
  #guides(linetype = guide_legend(reverse = TRUE)) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
  scale_color_manual(values = my_colors,
                     limits = names(my_colors),
                     drop=FALSE
  ) +
  # ggtitle("MALL") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  labs(
    color = "Year:",
    #linetype = "Start Date:",
    x = "Nesting Start Date",
    y = "Density"
  ) + theme(plot.margin = margin(1,0.5,0,0, 'cm'),
            # Set size for both axis titles
            axis.title = element_text(size = 18),
            
            # Set size for both axis text/labels 
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),     # Size for labels
            legend.title = element_text(size = 17)      # Size for title
  )
b<-dates3 %>%
  filter(species == "ABDU") %>%
  mutate(
    lay_start_fake = as.Date(format(nest_start, "2000-%m-%d")),
    #inc_start_fake = as.Date(format(inc_start, "2000-%m-%d"))#,
    #year=as.factor(year)
  ) %>%
  ggplot() +
  geom_density(aes(x = lay_start_fake, 
                   color = year#,
                   # color = factor(year, levels = c("2021", "2022", "2023", "2024", "2025")), 
  ), size = 1) + 
  #scale_linetype_manual(values = c("incubation" = "solid", "laying" = "dashed")) +
  #guides(linetype = guide_legend(reverse = TRUE)) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
  scale_color_manual(values = my_colors,
                     limits = names(my_colors),
                     drop=FALSE
  ) +
  # ggtitle("MALL") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  labs(
    color = "Year:",
    #linetype = "Start Date:",
    x = "Nesting Start Date",
    y = "Density"
  ) + theme(plot.margin = margin(1,0.5,0,0, 'cm'),
            # Set size for both axis titles
            axis.title = element_text(size = 18),
            
            # Set size for both axis text/labels 
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),     # Size for labels
            legend.title = element_text(size = 17)      # Size for title
  )
d <- ggarrange(b, a, ncol=1, nrow=2, common.legend = TRUE, legend = "top",
               labels = c("ABDU", "MALL"),
               font.label = list(size = 18, color = "black", face = "bold"))

ggarrange(c, d, ncol=2, nrow=1, common.legend = FALSE)
dev.off()
rm(b, a, c, d)



# number of attempts

sum_attempts <- dates2 %>% group_by(band, year) %>%
  filter(attempt_new== max(attempt_new)) %>% ungroup() %>% # reducing each bird-year to only one row per attempt and having that one row = the max number of attempts for that year
  mutate(attempt=as.numeric(attempt_new)) %>%
  group_by(species) %>%
  summarise(n =n(),
            mean_attempt=mean(attempt),
            median_attempt=median(attempt),
            min_attempt=min(attempt),
            max_attempt=max(attempt),
            sd_attempt=sd(attempt))

sum_attempts_by_age <- dates2 %>% group_by(band, year) %>%
  filter(attempt_new== max(attempt_new)) %>% ungroup() %>% # reducing each bird-year to only one row per attempt and having that one row = the max number of attempts for that year
  mutate(attempt=as.numeric(attempt_new)) %>%
  group_by(species, age) %>%
  summarise(n =n(),
            mean_attempt=mean(attempt),
            median_attempt=median(attempt),
            min_attempt=min(attempt),
            max_attempt=max(attempt),
            sd_attempt=sd(attempt))

sum_attempts_by_year <- dates2 %>% group_by(band, year) %>%
  filter(attempt_new== max(attempt_new)) %>% ungroup() %>% # reducing each bird-year to only one row per attempt and having that one row = the max number of attempts for that year
  mutate(attempt=as.numeric(attempt_new)) %>%
  group_by(species, year) %>%
  summarise(n =n(),
            mean_attempt=mean(attempt),
            median_attempt=median(attempt),
            min_attempt=min(attempt),
            max_attempt=max(attempt),
            sd_attempt=sd(attempt))

attempts <- dates2 %>% group_by(band, year) %>%
  filter(attempt_new== max(attempt_new)) %>% ungroup() %>%
  mutate(attempt=as.numeric(attempt_new))

png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_NumberOfAttemptsByAge.png")
boxplot(attempt ~ age, data = attempts, ylab = "Number of Attempts", xlab = "Age")
dev.off()

png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_NumberOfAttemptsByYear.png")
boxplot(attempt ~ year, data = attempts, ylab = "Number of Attempts", xlab = "Year")
dev.off()

