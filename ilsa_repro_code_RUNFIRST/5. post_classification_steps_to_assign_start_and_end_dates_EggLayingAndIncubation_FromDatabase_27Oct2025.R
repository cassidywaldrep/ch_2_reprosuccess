# Finding Nest Location and Start and End Dates of Incubation and Egg Laying
# Author: Ilsa Griebel
# Creation Date: 7 Sept 2025

#load libraries
#setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics") # laptop
#library(nestR)
library(amt)
library(tidyverse)
library(suncalc)
library(geosphere) # for calculating distance from nest
library(sf) # for doing projection conversions
library(recurse) # for identifying nest location from recursive movements
library(RPostgres)
library(lubridate)

# connect to database
conn <- dbConnect(
  Postgres(),
 #dbname = "ABDU",
  dbname = "EMALL",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "blackducks"
)

### load labelled/classified incubation and egg laying data and summarize  ####
# incubation MALL classified
labelled.data_incubation <- tbl(conn, "mall_incubation_classified_30June26") %>% # abdu or mall
  collect() 

n_incub <- unique(labelled.data_incubation$birdid_year)

hatched <- labelled.data_incubation %>% filter(breeding_outcome == "hatched") %>%
  group_by(birdid_year) %>%
  slice_sample(n = 1)

# egg laying MALL classified - first 6 days
# labelled.data_laying <- tbl(conn, "mall_egglaying_classified_first6days") %>%
#   collect() 
# # egg laying MALL classified - last 6 days
# labelled.data_laying <- tbl(conn, "mall_egglaying_classified_last6days") %>%
#   collect() 
# egg laying MALL classified - last 6 days + first day incubation
labelled.data_laying <- tbl(conn, "mall_egglaying_classified_last6daysAnd1inc_30Jun26") %>%
  collect() 

nested <- labelled.data_laying %>% filter(breeding_outcome == "nested")

hatched <- hatched %>% filter(birdid_year %in% nested$birdid_year)

# ## save for Erika - settling analysis
# save <- labelled.data_laying %>%
#   group_by(birdid_year) %>%
#   slice_head(n = 1) %>%
#   select(birdid_year, breeding_outcome)
# 
# write.csv(save, "NestPropensityStatusFromEggLayingModel_ForErika_9Jun2026.csv")

n_lay <- unique(labelled.data_laying$birdid_year)

setdiff(unique(labelled.data_incubation$birdid_year), unique(labelled.data_laying$birdid_year))
setdiff(unique(labelled.data_laying$birdid_year), unique(labelled.data_incubation$birdid_year))

# how many classified as nested by laying model are in the incubation dataset?
laying_nesting.only <- labelled.data_laying %>% filter(breeding_outcome=="nested")

setdiff(unique(labelled.data_incubation$birdid_year), unique(laying_nesting.only$birdid_year))
setdiff(unique(laying_nesting.only$birdid_year), unique(labelled.data_incubation$birdid_year))

## how many birds in laying dataset died or went offline during the breeding season?

# calculate max day for each bird-year (last day should be June 24)
max.day_laying <- labelled.data_laying %>% group_by(birdid_year) %>%
  summarize(max_day=yday(max(date)),
            breeding_outcome=unique(breeding_outcome))

# plot histogram (all data combined)
hist(max.day_laying$max_day, freq = TRUE)

# plot histogram (only birds without data to June 24)
max.day_laying_incomplete <- max.day_laying %>% filter(max_day <175) # n = 283 birds for mallards, 339 for black ducks (now 200 after fixing migration filter)
hist(max.day_laying_incomplete$max_day, freq = TRUE)

# plot histogram of defer and nested separately (all data included)
max.day_laying_defer <- max.day_laying %>% filter(breeding_outcome == "defer") # n = 156 birds for mallards, 256 for black ducks (now 189)
max.day_laying_nested <- max.day_laying %>% filter(breeding_outcome == "nested") # n = 833 birds for mallards, 506 for black ducks (now 448)

hist(max.day_laying_defer$max_day, freq = TRUE)
hist(max.day_laying_nested$max_day, freq = TRUE)

# plot histogram of defer and nested separately (only birds without data to June 24)
max.day_laying.incomplete_defer <- max.day_laying_incomplete %>% filter(breeding_outcome == "defer") # n = 62 birds for mallards (39% of defers), 144 (now 80) for black ducks (56% (now 42%) of defers)
max.day_laying.incomplete_nested <- max.day_laying_incomplete %>% filter(breeding_outcome == "nested") # n = 221 birds for mallards (27% of nested), 195 (now 120) for black ducks (39% (now 27%) of nested)

hist(max.day_laying.incomplete_defer$max_day, freq = TRUE)
hist(max.day_laying.incomplete_nested$max_day, freq = TRUE)

## how many birds in incubation dataset died or went offline during the breeding season?

# calculate max day for each bird-year (last day should be July 6)
max.day_incubation <- labelled.data_incubation %>% group_by(birdid_year) %>%
  summarize(max_day=yday(max(date)),
            breeding_outcome=unique(breeding_outcome))

# plot histogram (all data combined)
hist(max.day_incubation$max_day, freq = TRUE)

# plot histogram (only birds without data to June 24)
max.day_incubation_incomplete <- max.day_incubation %>% filter(max_day <187) # n = 287 birds for mallards, 229 (now 121) for black ducks
hist(max.day_incubation_incomplete$max_day, freq = TRUE)

# plot histogram of defer, failed and hatched separately (all data included)
max.day_incubation_defer <- max.day_incubation %>% filter(breeding_outcome == "defer") # n = 467 birds for mallards, 403 for black ducks (now 307)
max.day_incubation_failed <- max.day_incubation %>% filter(breeding_outcome == "failed") # n = 383 birds for mallards, 157 for black ducks (now 152)
max.day_incubation_hatched <- max.day_incubation %>% filter(breeding_outcome == "hatched") # n = 154 birds for mallards, 74 for black ducks (still 74)

hist(max.day_incubation_defer$max_day, freq = TRUE)
hist(max.day_incubation_failed$max_day, freq = TRUE)
hist(max.day_incubation_hatched$max_day, freq = TRUE)

# plot histogram of defer, failed and hatched separately (only birds without data to June 24)
max.day_incubation.incomplete_defer <- max.day_incubation_incomplete %>% filter(breeding_outcome == "defer") # n = 141 birds for mallards (30% of non-incubating), 186 (now 90) for black ducks (46% (now 29%) of non-incubating)
max.day_incubation.incomplete_failed <- max.day_incubation_incomplete %>% filter(breeding_outcome == "failed") # n = 114 birds for mallards (29% of failed), 35 (now 23) for black ducks (22% of failed) (now 15% of failed)
max.day_incubation.incomplete_hatched <- max.day_incubation_incomplete %>% filter(breeding_outcome == "hatched") # n = 32 birds for mallards (21% of hatched), 8 (still 8) for black ducks (11% of hatched) (same)


hist(max.day_incubation.incomplete_defer$max_day, freq = TRUE)
hist(max.day_incubation.incomplete_failed$max_day, freq = TRUE)
hist(max.day_incubation.incomplete_hatched$max_day, freq = TRUE)

# saving incubation and egg laying data for Cassidy
# setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics") # laptop
# write.csv(labelled.data_incubation, "WildBirdData_ClassifiedDatasets_22Aug2025/MALL_2022to2025_IncubationClassified_20Nov2025.csv")
# write.csv(labelled.data_laying, "WildBirdData_ClassifiedDatasets_22Aug2025/MALL_2022to2025_LayingClassified_20Nov2025.csv")

# create data frame with birdid_year, status_laying, status_incubation, egg_range_start, egg_range_end, fail_inc_range_start, 
# fail_inc_range_end, succ_inc_range_start, succ_inc_range_end (after following steps will add nest_lat, nest_long, egg_start, egg_end, inc_start, inc_end)
summary_incubation <- labelled.data_incubation %>% group_by(birdid_year, status) %>%
  summarize(range_start=min(date),
            range_end=max(date))
# reduce labelled data to only egg laying days
labelled.data_laying_days_only <- labelled.data_laying %>% filter(status == "egg_laying") %>%
  mutate(date=as.Date(date))

# adding column to distinguish different egg laying bouts (previously did 3 days, but now trying 7 because the window size is 7)
labelled.data_laying_days_only <- labelled.data_laying_days_only %>%
  group_by(birdid_year) %>%
  group_by(grp = cumsum(c(TRUE, diff(date) > 7)), .add = TRUE) %>%
  ungroup

# create a summary of start and end dates of potential egg laying
summary_laying_dates <- labelled.data_laying_days_only %>% group_by(birdid_year, grp) %>%
  summarise(range_start=min(date),
            range_end=max(date))

# find max number of grp per bird-year
summary_number_per_group <- summary_laying_dates %>% group_by(birdid_year) %>%
  summarise(count_n = n())

summary_number_per_group %>% group_by(count_n) %>% count()

hist(as.numeric(summary_number_per_group$count_n))

max(summary_number_per_group$count_n) # 6 for MALL (<7 days), 5 for ABDU (<7 days)
# create a summary of status for egg laying data  
summary_laying_status <- labelled.data_laying %>% group_by(birdid_year, status) %>%
  summarise()

ids_inc <- unique(labelled.data_incubation$birdid_year)
ids_lay <- unique(labelled.data_laying$birdid_year)
#ids <- intersect(ids_inc, ids_lay) # old method

# now trying to keep all ids even if they only occur in one of the datasets
ids_inc_lay <- c(ids_inc, ids_lay)
ids <- sort(unique(ids_inc_lay)) # n = 1100 for MALL, 643 for ABDU 

# for mall, missing two hatched birds:  "2247-39957_2022" "2497-15601_2025" ???

# get ids in inc that weren't classified by egg laying algorithm
ids_not_classified_by_egg_laying <- setdiff(unique(labelled.data_incubation$birdid_year), unique(labelled.data_laying$birdid_year))

summary <- data.frame()
#birdid_year, status_laying, status_incubation, egg_range_start, egg_range_end, fail_inc_range_start, 
# fail_inc_range_end, succ_inc_range_start, succ_inc_range_end
for (i in 1:length(ids)) {
  #get summary incubation info for one bird-year
  sum_inc <- summary_incubation %>% filter(birdid_year== ids[i])
  sum_inc_hatch <- sum_inc %>% filter(status == "hatched")
  sum_inc_fail <- sum_inc %>% filter(status == "failed")
  sum_inc_defer <- sum_inc %>% filter(status == "defer")
  if(nrow(sum_inc) == 0) {
    new <- data.frame(birdid_year = ids[i], status_incubation = NA,
                      fail_inc_range_start = NA, fail_inc_range_end = NA,
                      succ_inc_range_start = NA, succ_inc_range_end = NA  )
  } else if (nrow(sum_inc_hatch) > 0 & nrow(sum_inc_fail) > 0 ) {  # number of rows can be 1, 2 or 3
    new <- sum_inc_hatch %>%
      rename(succ_inc_range_start=range_start,
            succ_inc_range_end=range_end,
            status_incubation=status)
    fail_start <- sum_inc_fail %>% pull(range_start)
    fail_end <- sum_inc_fail %>% pull(range_end)
    new$fail_inc_range_start <- fail_start
    new$fail_inc_range_end <- fail_end
  } else if (nrow(sum_inc_hatch) > 0 & nrow(sum_inc_fail) == 0 ) {
    new <- sum_inc_hatch %>%
      rename(succ_inc_range_start=range_start,
             succ_inc_range_end=range_end,
             status_incubation=status)
    new$fail_inc_range_start <- NA
    new$fail_inc_range_end <- NA
  }else if (nrow(sum_inc_fail) > 0 ) {
    new <- sum_inc_fail %>%
      rename(fail_inc_range_start=range_start,
             fail_inc_range_end=range_end,
             status_incubation=status)
    new$succ_inc_range_start <- NA
    new$succ_inc_range_end <- NA
  } else {
    new <- sum_inc_defer %>%
      rename(status_incubation=status)
    new$succ_inc_range_start <- NA
    new$succ_inc_range_end <- NA
    new$fail_inc_range_start <- NA
    new$fail_inc_range_end <- NA
  }
  # get laying data (dates) for one  bird
  sum_egg <- summary_laying_dates %>% filter(birdid_year ==ids[i])
  if (nrow(sum_egg)>=6) {
  new$status_laying <- "egg_laying"
  new$egg_range_start_attempt1 <- sum_egg$range_start[1]
  new$egg_range_end_attempt1 <- sum_egg$range_end[1]
  new$egg_range_start_attempt2 <- sum_egg$range_start[2]
  new$egg_range_end_attempt2 <- sum_egg$range_end[2]
  new$egg_range_start_attempt3 <- sum_egg$range_start[3]
  new$egg_range_end_attempt3 <- sum_egg$range_end[3]
  new$egg_range_start_attempt4 <- sum_egg$range_start[4]
  new$egg_range_end_attempt4 <- sum_egg$range_end[4]
  new$egg_range_start_attempt5 <- sum_egg$range_start[5]
  new$egg_range_end_attempt5 <- sum_egg$range_end[5]
  new$egg_range_start_attempt6 <- sum_egg$range_start[6]
  new$egg_range_end_attempt6 <- sum_egg$range_end[6]

} else if (nrow(sum_egg)>=5) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- sum_egg$range_start[2]
    new$egg_range_end_attempt2 <- sum_egg$range_end[2]
    new$egg_range_start_attempt3 <- sum_egg$range_start[3]
    new$egg_range_end_attempt3 <- sum_egg$range_end[3]
    new$egg_range_start_attempt4 <- sum_egg$range_start[4]
    new$egg_range_end_attempt4 <- sum_egg$range_end[4]
    new$egg_range_start_attempt5 <- sum_egg$range_start[5]
    new$egg_range_end_attempt5 <- sum_egg$range_end[5]
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA

  } else if (nrow(sum_egg)>=4) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- sum_egg$range_start[2]
    new$egg_range_end_attempt2 <- sum_egg$range_end[2]
    new$egg_range_start_attempt3 <- sum_egg$range_start[3]
    new$egg_range_end_attempt3 <- sum_egg$range_end[3]
    new$egg_range_start_attempt4 <- sum_egg$range_start[4]
    new$egg_range_end_attempt4 <- sum_egg$range_end[4]
    new$egg_range_start_attempt5 <- NA
    new$egg_range_end_attempt5 <- NA
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA

  } else if (nrow(sum_egg)>=3) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- sum_egg$range_start[2]
    new$egg_range_end_attempt2 <- sum_egg$range_end[2]
    new$egg_range_start_attempt3 <- sum_egg$range_start[3]
    new$egg_range_end_attempt3 <- sum_egg$range_end[3]
    new$egg_range_start_attempt4 <- NA
    new$egg_range_end_attempt4 <- NA
    new$egg_range_start_attempt5 <- NA
    new$egg_range_end_attempt5 <- NA
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA

  } else if(nrow(sum_egg)>=2) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- sum_egg$range_start[2]
    new$egg_range_end_attempt2 <- sum_egg$range_end[2]
    new$egg_range_start_attempt3 <- NA
    new$egg_range_end_attempt3 <- NA
    new$egg_range_start_attempt4 <- NA
    new$egg_range_end_attempt4 <- NA
    new$egg_range_start_attempt5 <- NA
    new$egg_range_end_attempt5 <- NA
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA

  } else if(nrow(sum_egg)>=1) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- NA
    new$egg_range_end_attempt2 <- NA
    new$egg_range_start_attempt3 <- NA
    new$egg_range_end_attempt3 <- NA
    new$egg_range_start_attempt4 <- NA
    new$egg_range_end_attempt4 <- NA
    new$egg_range_start_attempt5 <- NA
    new$egg_range_end_attempt5 <- NA
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA

  } else {
    if (ids[i] %in% ids_not_classified_by_egg_laying) {
      new$status_laying <- NA
    } else {
      new$status_laying <- "defer"
    }
    new$egg_range_start_attempt1 <- NA
    new$egg_range_end_attempt1 <-NA
    new$egg_range_start_attempt2 <- NA
    new$egg_range_end_attempt2 <- NA
    new$egg_range_start_attempt3 <- NA
    new$egg_range_end_attempt3 <- NA
    new$egg_range_start_attempt4 <- NA
    new$egg_range_end_attempt4 <- NA
    new$egg_range_start_attempt5 <- NA
    new$egg_range_end_attempt5 <- NA
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA
    
  }

  # order columns so will be the same no matter what
  new <- new[, c("birdid_year", "status_laying", "status_incubation", "egg_range_start_attempt1", "egg_range_end_attempt1",
                 "egg_range_start_attempt2", "egg_range_end_attempt2", "egg_range_start_attempt3", "egg_range_end_attempt3",
                 "egg_range_start_attempt4", "egg_range_end_attempt4", "egg_range_start_attempt5", "egg_range_end_attempt5",
                 "egg_range_start_attempt6", "egg_range_end_attempt6",
                 "fail_inc_range_start", "fail_inc_range_end", "succ_inc_range_start", "succ_inc_range_end")]
  
  # combine with new summary file
  summary <- rbind(summary, new)
  
  i <- i + 1
}

# check how many status agree/disagree between egg laying and incubation algorithms
# first filter so only one status row per bird (i.e. get rid of nesting attempts greater than one)
#summary$nest_attempt[is.na(summary$nest_attempt)] <- 0
summary$status_comb <- paste(summary$status_laying, summary$status_incubation, sep="_")
#summary_second_attempts_rem <- summary %>% filter(nest_attempt <= 1)
summary%>% group_by(status_comb) %>% count()

summary_hatched <- summary %>% filter(status_comb == "egg_laying_hatched")
setdiff(unique(hatched$birdid_year), unique(summary_hatched$birdid_year))




summary_abdu <- summary
summary_mall <- summary

summary_fullrange <- summary

# save summary of window date ranges for setting dates for full annual cycle model
#dbRemoveTable(conn, "window_date_ranges_for_all_attempts_30June26")
dbWriteTable(conn, "window_date_ranges_for_all_attempts_30June26", summary, append = TRUE, row.names = FALSE)


# # save summary for Erika
# summary <- dbReadTable(conn, "window_date_ranges_for_all_attempts") %>% collect()
# setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/Data for Erika") # laptop
# write.csv(summary, "ABDU_2021to2025_EggLayingAndIncubationClassified_SummaryFormat_11Jun2026.csv")

#### plots of egg laying ranges and incubation ranges ####
summary_defer.rem <- summary %>% filter(status_laying != "defer")
summary_defer.rem[c("egg_range_start_attempt1","egg_range_end_attempt1","egg_range_start_attempt2","egg_range_end_attempt2",
                    "egg_range_start_attempt3","egg_range_end_attempt3", "egg_range_start_attempt4","egg_range_end_attempt4",
                    "egg_range_start_attempt5","egg_range_end_attempt5", "egg_range_start_attempt6","egg_range_end_attempt6",
                    "fail_inc_range_start", "fail_inc_range_end",
                    "succ_inc_range_start", "succ_inc_range_end")] <- lapply(summary_defer.rem[c("egg_range_start_attempt1",
                                                                                                 "egg_range_end_attempt1",
                                                                                                 "egg_range_start_attempt2",
                                                                                                 "egg_range_end_attempt2",
                                                                                                 "egg_range_start_attempt3",
                                                                                                 "egg_range_end_attempt3", 
                                                                                                 "egg_range_start_attempt4","egg_range_end_attempt4",
                                                                                                 "egg_range_start_attempt5","egg_range_end_attempt5", 
                                                                                                 "egg_range_start_attempt6","egg_range_end_attempt6",
                                                                                                 "fail_inc_range_start", "fail_inc_range_end",
                                                                                                 "succ_inc_range_start", "succ_inc_range_end")], 
                                                                             as.Date)
summary_defer.rem$year <- year(summary_defer.rem$egg_range_end_attempt1)
# 
# years <- unique(summary_defer.rem$year)
# # all nesting attempts
# pdf("plots_EggLayingAndIncubationCombined_14Sept2025/EggLayingAndIncubationRanges_unknownABDU_AllNestingAttempts_Last6dEggLaying1dIncAlg_lessThan7d_21Nov2025.pdf", paper = "a4r", width = 10, height = 8)
# for (i in 1:length(years)){
#   d<- summary_defer.rem %>% filter(year==years[i])
#   x <- ggplot(d, aes(y=birdid_year)) +
#     geom_linerange(aes(xmin=fail_inc_range_start, xmax=fail_inc_range_end+28), size=4, color = "blue") +
#     geom_linerange(aes(xmin=egg_range_start_attempt1, xmax=egg_range_end_attempt1+6), size=2, color = "green") +
#     geom_linerange(aes(xmin=egg_range_start_attempt2, xmax=egg_range_end_attempt2+6), size=2, color = "green") +
#     geom_linerange(aes(xmin=egg_range_start_attempt3, xmax=egg_range_end_attempt3+6), size=2, color = "green") +
#     geom_linerange(aes(xmin=egg_range_start_attempt4, xmax=egg_range_end_attempt4+6), size=2, color = "green") +
#     geom_linerange(aes(xmin=egg_range_start_attempt5, xmax=egg_range_end_attempt5+6), size=2, color = "green") +
#     geom_linerange(aes(xmin=egg_range_start_attempt6, xmax=egg_range_end_attempt6+6), size=2, color = "green") +
#     geom_point(aes(x=succ_inc_range_start, color = "incubation start/end")) +
#     geom_point(aes(x=succ_inc_range_end+28, color = "incubation start/end")) +
#     scale_x_date(date_breaks = "20 day", date_labels = "%b %d")+
#     theme(legend.title=element_blank()) +
#     labs( x = "Nesting Date Ranges", y = "Birdid_Year")
#   
#   print(x)
# }
# dev.off()
# 
# # hatched nests only
# summary_defer.rem_hatched <- summary_defer.rem %>% filter(status_incubation == "hatched")
# pdf("plots_EggLayingAndIncubationCombined_14Sept2025/EggLayingAndIncubationRanges_unknownABDU_HatchedNestsOnly_Last6dEggLaying1dIncAlg_lessThan7d_21Nov2025.pdf", paper = "a4r", width = 10, height = 8)
# for (i in 1:length(years)){
#   d<- summary_defer.rem_hatched %>% filter(year==years[i])
#   x <- ggplot(d, aes(y=birdid_year)) +
#     geom_linerange(aes(xmin=fail_inc_range_start, xmax=fail_inc_range_end+28), size=4, color = "blue") +
#     geom_linerange(aes(xmin=egg_range_start_attempt1, xmax=egg_range_end_attempt1+6), size=2, color = "green") +
#     geom_linerange(aes(xmin=egg_range_start_attempt2, xmax=egg_range_end_attempt2+6), size=2, color = "green") +
#     geom_linerange(aes(xmin=egg_range_start_attempt3, xmax=egg_range_end_attempt3+6), size=2, color = "green") +
#     geom_linerange(aes(xmin=egg_range_start_attempt4, xmax=egg_range_end_attempt4+6), size=2, color = "green") +
#     geom_linerange(aes(xmin=egg_range_start_attempt5, xmax=egg_range_end_attempt5+6), size=2, color = "green") +
#     geom_linerange(aes(xmin=egg_range_start_attempt6, xmax=egg_range_end_attempt6+6), size=2, color = "green") +
#     geom_point(aes(x=succ_inc_range_start, color = "incubation start/end")) +
#     geom_point(aes(x=succ_inc_range_end+28, color = "incubation start/end")) +
#     scale_x_date(date_breaks = "20 day", date_labels = "%b %d")+
#     theme(legend.title=element_blank()) +
#     labs( x = "Nesting Date Ranges", y = "Birdid_Year")
#   
#   print(x)
# }
# 
# dev.off()


#### for each nesting attempt, hatch or fail, reduce to one window with the greatest absx_mean #####
# create a summary of start and end dates of potential incubation attempts BUT USING JUST ONE WINDOW PER ATTEMPT (THE ONE WITH THE GREATEST ABSX MEAN)
summary_incubation <- labelled.data_incubation %>% group_by(birdid_year, status) %>%
  filter(Absx_mean == max(Absx_mean)) %>%
  summarize(n_windows=n(),
           range_start=min(date),
            range_end=max(date))

# confirm only one window per bird-year-status
max(summary_incubation$n_windows) # this is one; if this is ever greater than one, add a second step to filter by another variable, eg min ddist, for tie breaker

# create a summary of start and end dates of potential egg laying attempts BUT USING JUST ONE WINDOW PER ATTEMPT (THE ONE WITH THE GREATEST ABSX MEAN)
summary_laying_dates <- labelled.data_laying_days_only %>% group_by(birdid_year, grp) %>%
  filter(Absx_mean == max(Absx_mean)) %>%
  summarise(n_windows=n(),
            range_start=min(date),
            range_end=max(date))

# confirm only one window per bird-year-grp
max(summary_laying_dates$n_windows) # this is one; if this is ever greater than one, add a second step to filter by another variable, eg min ddist, for tie breaker

# find max number of grp per bird-year
summary_number_per_group <- summary_laying_dates %>% group_by(birdid_year) %>%
  summarise(count_n = n())

summary_number_per_group %>% group_by(count_n) %>% count()

hist(as.numeric(summary_number_per_group$count_n))

max(summary_number_per_group$count_n) # 8 for MALL (< 6 days), 6 for MALL (<7 days), 5 for ABDU
# create a summary of status for egg laying data  
summary_laying_status <- labelled.data_laying %>% group_by(birdid_year, status) %>%
  summarise()

ids_inc <- unique(labelled.data_incubation$birdid_year)
ids_lay <- unique(labelled.data_laying$birdid_year)
#ids <- intersect(ids_inc, ids_lay) # old method

# now trying to keep all ids even if they only occur in one of the datasets
ids_inc_lay <- c(ids_inc, ids_lay)
ids <- unique(ids_inc_lay) # n = 1100 for MALL, 773 for ABDU

# get ids in inc that weren't classified by egg laying algorithm
ids_not_classified_by_egg_laying <- setdiff(unique(labelled.data_incubation$birdid_year), unique(labelled.data_laying$birdid_year))

summary <- data.frame()
#birdid_year, status_laying, status_incubation, egg_range_start, egg_range_end, fail_inc_range_start, 
# fail_inc_range_end, succ_inc_range_start, succ_inc_range_end
for (i in 1:length(ids)) {
  #get summary incubation info for one bird-year
  sum_inc <- summary_incubation %>% filter(birdid_year== ids[i])
  sum_inc_hatch <- sum_inc %>% filter(status == "hatched")
  sum_inc_fail <- sum_inc %>% filter(status == "failed")
  sum_inc_defer <- sum_inc %>% filter(status == "defer")
  if(nrow(sum_inc) == 0) {
    new <- data.frame(birdid_year = ids[i], status_incubation = NA,
                      fail_inc_range_start = NA, fail_inc_range_end = NA,
                      succ_inc_range_start = NA, succ_inc_range_end = NA  )
  } else if (nrow(sum_inc_hatch) > 0 & nrow(sum_inc_fail) > 0 ) {  # number of rows can be 1, 2 or 3
    new <- sum_inc_hatch %>%
      rename(succ_inc_range_start=range_start,
             succ_inc_range_end=range_end,
             status_incubation=status)
    fail_start <- sum_inc_fail %>% pull(range_start)
    fail_end <- sum_inc_fail %>% pull(range_end)
    new$fail_inc_range_start <- fail_start
    new$fail_inc_range_end <- fail_end
  } else if (nrow(sum_inc_hatch) > 0 & nrow(sum_inc_fail) == 0 ) {
    new <- sum_inc_hatch %>%
      rename(succ_inc_range_start=range_start,
             succ_inc_range_end=range_end,
             status_incubation=status)
    new$fail_inc_range_start <- NA
    new$fail_inc_range_end <- NA
  }else if (nrow(sum_inc_fail) > 0 ) {
    new <- sum_inc_fail %>%
      rename(fail_inc_range_start=range_start,
             fail_inc_range_end=range_end,
             status_incubation=status)
    new$succ_inc_range_start <- NA
    new$succ_inc_range_end <- NA
  } else {
    new <- sum_inc_defer %>%
      rename(status_incubation=status)
    new$succ_inc_range_start <- NA
    new$succ_inc_range_end <- NA
    new$fail_inc_range_start <- NA
    new$fail_inc_range_end <- NA
  }
  # get laying data (dates) for one  bird
  sum_egg <- summary_laying_dates %>% filter(birdid_year ==ids[i])
  if (nrow(sum_egg)>=6) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- sum_egg$range_start[2]
    new$egg_range_end_attempt2 <- sum_egg$range_end[2]
    new$egg_range_start_attempt3 <- sum_egg$range_start[3]
    new$egg_range_end_attempt3 <- sum_egg$range_end[3]
    new$egg_range_start_attempt4 <- sum_egg$range_start[4]
    new$egg_range_end_attempt4 <- sum_egg$range_end[4]
    new$egg_range_start_attempt5 <- sum_egg$range_start[5]
    new$egg_range_end_attempt5 <- sum_egg$range_end[5]
    new$egg_range_start_attempt6 <- sum_egg$range_start[6]
    new$egg_range_end_attempt6 <- sum_egg$range_end[6]
    
  } else if (nrow(sum_egg)>=5) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- sum_egg$range_start[2]
    new$egg_range_end_attempt2 <- sum_egg$range_end[2]
    new$egg_range_start_attempt3 <- sum_egg$range_start[3]
    new$egg_range_end_attempt3 <- sum_egg$range_end[3]
    new$egg_range_start_attempt4 <- sum_egg$range_start[4]
    new$egg_range_end_attempt4 <- sum_egg$range_end[4]
    new$egg_range_start_attempt5 <- sum_egg$range_start[5]
    new$egg_range_end_attempt5 <- sum_egg$range_end[5]
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA
    
  } else if (nrow(sum_egg)>=4) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- sum_egg$range_start[2]
    new$egg_range_end_attempt2 <- sum_egg$range_end[2]
    new$egg_range_start_attempt3 <- sum_egg$range_start[3]
    new$egg_range_end_attempt3 <- sum_egg$range_end[3]
    new$egg_range_start_attempt4 <- sum_egg$range_start[4]
    new$egg_range_end_attempt4 <- sum_egg$range_end[4]
    new$egg_range_start_attempt5 <- NA
    new$egg_range_end_attempt5 <- NA
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA
    
  } else if (nrow(sum_egg)>=3) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- sum_egg$range_start[2]
    new$egg_range_end_attempt2 <- sum_egg$range_end[2]
    new$egg_range_start_attempt3 <- sum_egg$range_start[3]
    new$egg_range_end_attempt3 <- sum_egg$range_end[3]
    new$egg_range_start_attempt4 <- NA
    new$egg_range_end_attempt4 <- NA
    new$egg_range_start_attempt5 <- NA
    new$egg_range_end_attempt5 <- NA
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA
    
  } else if(nrow(sum_egg)>=2) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- sum_egg$range_start[2]
    new$egg_range_end_attempt2 <- sum_egg$range_end[2]
    new$egg_range_start_attempt3 <- NA
    new$egg_range_end_attempt3 <- NA
    new$egg_range_start_attempt4 <- NA
    new$egg_range_end_attempt4 <- NA
    new$egg_range_start_attempt5 <- NA
    new$egg_range_end_attempt5 <- NA
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA
    
  } else if(nrow(sum_egg)>=1) {
    new$status_laying <- "egg_laying"
    new$egg_range_start_attempt1 <- sum_egg$range_start[1]
    new$egg_range_end_attempt1 <- sum_egg$range_end[1]
    new$egg_range_start_attempt2 <- NA
    new$egg_range_end_attempt2 <- NA
    new$egg_range_start_attempt3 <- NA
    new$egg_range_end_attempt3 <- NA
    new$egg_range_start_attempt4 <- NA
    new$egg_range_end_attempt4 <- NA
    new$egg_range_start_attempt5 <- NA
    new$egg_range_end_attempt5 <- NA
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA
    
  } else {
    if (ids[i] %in% ids_not_classified_by_egg_laying) {
      new$status_laying <- NA
    } else {
      new$status_laying <- "defer"
    }
    new$egg_range_start_attempt1 <- NA
    new$egg_range_end_attempt1 <-NA
    new$egg_range_start_attempt2 <- NA
    new$egg_range_end_attempt2 <- NA
    new$egg_range_start_attempt3 <- NA
    new$egg_range_end_attempt3 <- NA
    new$egg_range_start_attempt4 <- NA
    new$egg_range_end_attempt4 <- NA
    new$egg_range_start_attempt5 <- NA
    new$egg_range_end_attempt5 <- NA
    new$egg_range_start_attempt6 <- NA
    new$egg_range_end_attempt6 <- NA
    
  }
  
  # order columns so will be the same no matter what
  new <- new[, c("birdid_year", "status_laying", "status_incubation", "egg_range_start_attempt1", "egg_range_end_attempt1",
                 "egg_range_start_attempt2", "egg_range_end_attempt2", "egg_range_start_attempt3", "egg_range_end_attempt3",
                 "egg_range_start_attempt4", "egg_range_end_attempt4", "egg_range_start_attempt5", "egg_range_end_attempt5",
                 "egg_range_start_attempt6", "egg_range_end_attempt6",
                 "fail_inc_range_start", "fail_inc_range_end", "succ_inc_range_start", "succ_inc_range_end")]
  
  # combine with new summary file
  summary <- rbind(summary, new)
  
  i <- i + 1
}

# check how many status agree/disagree between egg laying and incubation algorithms
# first filter so only one status row per bird (i.e. get rid of nesting attempts greater than one)
#summary$nest_attempt[is.na(summary$nest_attempt)] <- 0
summary$status_comb <- paste(summary$status_laying, summary$status_incubation, sep="_")
#summary_second_attempts_rem <- summary %>% filter(nest_attempt <= 1)
summary%>% group_by(status_comb) %>% count()

n_summary <- unique(summary$birdid_year)

# save this df with the date ranges for single max absolute x window so you can use this for classifying brood rearing (birds that are missing incubation end dates, will use the max window instead)
#dbRemoveTable(conn, name = "max_absx_window_dates_30June")
dbWriteTable(conn, "max_absx_window_dates_30June26", summary, append = TRUE, row.names = FALSE)


#### using method combining ruleset from Schreven et al. 2021 (modified) and method of nestR ####

#### first, getting just birds that nested and/or incubated for nest locating step and formatting dates ####
# format all dates as dates!
summary[c("egg_range_start_attempt1","egg_range_end_attempt1","egg_range_start_attempt2","egg_range_end_attempt2",
                    "egg_range_start_attempt3","egg_range_end_attempt3", "egg_range_start_attempt4","egg_range_end_attempt4",
          "egg_range_start_attempt5","egg_range_end_attempt5", "egg_range_start_attempt6","egg_range_end_attempt6",
         "fail_inc_range_start", "fail_inc_range_end",
                    "succ_inc_range_start", "succ_inc_range_end")] <- lapply(summary[c("egg_range_start_attempt1",
                                                                                                 "egg_range_end_attempt1",
                                                                                                 "egg_range_start_attempt2",
                                                                                                 "egg_range_end_attempt2",
                                                                                                 "egg_range_start_attempt3",
                                                                                                 "egg_range_end_attempt3", 
                                                                                       "egg_range_start_attempt4",
                                                                                       "egg_range_end_attempt4",
                                                                                       "egg_range_start_attempt5",
                                                                                       "egg_range_end_attempt5",
                                                                                       "egg_range_start_attempt6",
                                                                                       "egg_range_end_attempt6",
                                                                                                 "fail_inc_range_start", "fail_inc_range_end",
                                                                                                 "succ_inc_range_start", "succ_inc_range_end")], 
                                                                             as.Date)
n_summary2 <- unique(summary$birdid_year)

# first remove any nesting attempts after a successful incubation attempt (egg laying algorithm likely classifying brood rearing behaviour)
summary_hatched <- summary %>% filter(status_incubation == "hatched")
summary_rem <- summary %>% filter(status_incubation != "hatched" | is.na(status_incubation))
n_summary_hatched <- unique(summary_hatched$birdid_year)
n_summary_rem <- unique(summary_rem$birdid_year)
summary_hatched<- summary_hatched %>% mutate(egg_range_start_attempt2=ifelse(egg_range_start_attempt2 > succ_inc_range_end+26, NA, egg_range_start_attempt2),
                                   egg_range_end_attempt2=ifelse(egg_range_start_attempt2 > succ_inc_range_end+26, NA, egg_range_end_attempt2),
                                   egg_range_start_attempt3=ifelse(egg_range_start_attempt3 > succ_inc_range_end+26, NA, egg_range_start_attempt3),
                                   egg_range_end_attempt3=ifelse(egg_range_start_attempt3 > succ_inc_range_end+26, NA, egg_range_end_attempt3),
                                   egg_range_start_attempt4=ifelse(egg_range_start_attempt4 > succ_inc_range_end+26, NA, egg_range_start_attempt4),
                                   egg_range_end_attempt4=ifelse(egg_range_start_attempt4 > succ_inc_range_end+26, NA, egg_range_end_attempt4),
                                   egg_range_start_attempt5=ifelse(egg_range_start_attempt5 > succ_inc_range_end+26, NA, egg_range_start_attempt5),
                                   egg_range_end_attempt5=ifelse(egg_range_start_attempt5 > succ_inc_range_end+26, NA, egg_range_end_attempt5),
                                   egg_range_start_attempt6=ifelse(egg_range_start_attempt6 > succ_inc_range_end+26, NA, egg_range_start_attempt6),
                                   egg_range_end_attempt6=ifelse(egg_range_start_attempt6 > succ_inc_range_end+26, NA, egg_range_end_attempt6)
                                   )
summary_hatched[c("egg_range_start_attempt1","egg_range_end_attempt1","egg_range_start_attempt2","egg_range_end_attempt2",
                  "egg_range_start_attempt3","egg_range_end_attempt3", "egg_range_start_attempt4","egg_range_end_attempt4",
                  "egg_range_start_attempt5","egg_range_end_attempt5", "egg_range_start_attempt6","egg_range_end_attempt6",
                  "fail_inc_range_start", "fail_inc_range_end", 
                  "succ_inc_range_start", "succ_inc_range_end")] <- lapply(summary_hatched[c("egg_range_start_attempt1",
                                                                                     "egg_range_end_attempt1",
                                                                                     "egg_range_start_attempt2",
                                                                                     "egg_range_end_attempt2",
                                                                                     "egg_range_start_attempt3",
                                                                                     "egg_range_end_attempt3", 
                                                                                     "egg_range_start_attempt4",
                                                                                     "egg_range_end_attempt4",
                                                                                     "egg_range_start_attempt5",
                                                                                     "egg_range_end_attempt5",
                                                                                     "egg_range_start_attempt6",
                                                                                     "egg_range_end_attempt6",
                                                                                     "fail_inc_range_start", "fail_inc_range_end",
                                                                                     "succ_inc_range_start", "succ_inc_range_end")], 
                                                                           as.Date)
n_summary_hatched2 <- unique(summary_hatched$birdid_year)
n_summary_rem2 <- unique(summary_rem$birdid_year)

summary <- rbind(summary_rem, summary_hatched)
n_summary2 <- unique(summary$birdid_year)

# reduce summary to only birds that had a nesting attempt or incubation
summary_nested <- summary %>% filter(status_laying == "egg_laying" | status_incubation == "failed" | status_incubation == "hatched")
n_summary_nested <- unique(summary_nested$birdid_year) # 512 (now 460) bird-years for abdu, 937 for mall

rm(labelled.data_incubation, labelled.data_laying, labelled.data_laying_days_only, 
   new, sum_egg, sum_inc, sum_inc_defer, sum_inc_fail, sum_inc_hatch,
   summary_defer.rem, summary_defer.rem_hatched, summary_hatched, summary_incubation,
   summary_laying_dates, summary_laying_status, summary_number_per_group, summary_rem)
# 
# #### read GPS data and format (first time only) ####
# gps.breed <- tbl(conn, "gps_data") %>%
#   collect() 
# 
# # remove duplicates
# gps.breed <- gps.breed %>% distinct ()
# 
# colnames(gps.breed)
# 
# #n <- unique(gps.breed$birdid)
# 
# gps.breed <- gps.breed %>% 
#   mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
#   mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
#          year = year(timestamp),
#          birdid_year = paste(birdid, year, sep="_")) %>% 
#   # remove erroneous (0,0) points from when sat count = 0
#   filter(Latitude != 0 | Longitude != 0) %>%
#   # select relevant columns
#   dplyr::select(c(device_id,
#                   birdid, year,
#                   birdid_year,
#                   timestamp, Latitude, Longitude)) %>%
#   filter(birdid_year %in% ids)
# 
# gps.breed <- gps.breed %>% 
#   # add column for hourly intervals
#   mutate(hour = floor_date(timestamp, "1 hour")) %>% 
#   # retain closest data point to each hourly interval
#   group_by(device_id, hour) %>% filter(timestamp == min(timestamp)) %>%
#   ungroup()
# 
# head(gps.breed)
# 
# # save hourly gps locations so don't have to repeat this very often
# dbRemoveTable(conn, "hourly_gps_locations_for_birds_with_inc_and_laying_classified")
# dbWriteTable(conn, "hourly_gps_locations_for_birds_with_inc_and_laying_classified", gps.breed, append = TRUE, row.names = FALSE)

#### read GPS data that is already filtered to one location per hour (after first time) ####
gps.breed <- tbl(conn, "hourly_gps_locations_for_birds_with_inc_and_laying_classified") %>%
  collect() 

n_gps <- unique(gps.breed$birdid_year) # this sample size should equal length of vector called ids! (the number of unique bird-years between egg laying and incubation datasets)

# add nest attempt date ranges to gps data
gps_nesting.dates <- merge(gps.breed, summary_nested, by = "birdid_year")

rm(gps.breed)

gps_nesting.dates <- gps_nesting.dates %>% mutate(date = as.Date(timestamp))

# filter gps to first to fifth (abdu)/sixth (mall) nesting attempts, failed and hatched incubation attempts
gps_first.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt1 & date <= egg_range_end_attempt1+7)
gps_first.nesting.attempts$nesting_attempt <- 1
gps_first.nesting.attempts <- gps_first.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt1-egg_range_start_attempt1)
gps_second.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt2 & date <= egg_range_end_attempt2+7)
gps_second.nesting.attempts$nesting_attempt <- 2
gps_second.nesting.attempts <- gps_second.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt2-egg_range_start_attempt2)
gps_third.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt3 & date <= egg_range_end_attempt3+7)
gps_third.nesting.attempts$nesting_attempt <- 3
gps_third.nesting.attempts <- gps_third.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt3-egg_range_start_attempt3)
gps_fourth.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt4 & date <= egg_range_end_attempt4+7)
gps_fourth.nesting.attempts$nesting_attempt <- 4
gps_fourth.nesting.attempts <- gps_fourth.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt4-egg_range_start_attempt4)
gps_fifth.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt5 & date <= egg_range_end_attempt5+7)
gps_fifth.nesting.attempts$nesting_attempt <- 5
gps_fifth.nesting.attempts <- gps_fifth.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt5-egg_range_start_attempt5)
gps_sixth.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt6 & date <= egg_range_end_attempt6+7)
gps_sixth.nesting.attempts$nesting_attempt <- 6
gps_sixth.nesting.attempts <- gps_sixth.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt6-egg_range_start_attempt6)
gps_hatched.attempts <- gps_nesting.dates %>% filter(date >= succ_inc_range_start & date <= succ_inc_range_end+26)
gps_hatched.attempts$nesting_attempt <- 'h'
gps_hatched.attempts <- gps_hatched.attempts %>%
  mutate(attempt_length=succ_inc_range_end-succ_inc_range_start)
gps_failed.attempts <- gps_nesting.dates %>% 
  filter(date >= fail_inc_range_start & date <= fail_inc_range_end+26)
  #filter(date >= fail_inc_range_start+((fail_inc_range_end-fail_inc_range_start)/2)-10 & date <= fail_inc_range_start+((fail_inc_range_end-fail_inc_range_start)/2)+10)
gps_failed.attempts$nesting_attempt <- 'f'
gps_failed.attempts <- gps_failed.attempts %>%
  mutate(attempt_length=fail_inc_range_end-fail_inc_range_start)


# combine gps data for all nesting attempts
gps_nesting_days <- rbind(gps_first.nesting.attempts, gps_second.nesting.attempts, 
                          gps_third.nesting.attempts, gps_fourth.nesting.attempts, gps_fifth.nesting.attempts, 
                          gps_sixth.nesting.attempts, #gps_seventh.nesting.attempts, ### PUT SIXTH BACK IN MALLARDS!!!!
                          gps_hatched.attempts, gps_failed.attempts)

rm(gps_failed.attempts, gps_first.nesting.attempts, gps_hatched.attempts, 
   gps_nesting.dates, gps_second.nesting.attempts, gps_third.nesting.attempts, 
   gps_fourth.nesting.attempts, gps_fifth.nesting.attempts, 
   gps_sixth.nesting.attempts
   )

# create new id of birdid_year_nest attempt
gps_nesting_days$birdid_year_nest <- paste(gps_nesting_days$birdid_year, gps_nesting_days$nesting_attempt, sep="_")

# use all hours of the day for failed and hatched attempts 
incubation_attempts <- gps_nesting_days %>% filter(nesting_attempt == 'h')
failed_inc_attempts <- gps_nesting_days %>% filter(nesting_attempt == "f")

# # calculate sunrise for each location
# data <- incubation_attempts %>% collect %>% as.data.frame
# data$timestamp <- as_datetime(data$timestamp, tz = "UTC")
# cols <- c("Latitude", "Longitude", "timestamp") ## Select columns that can't contain NA values
# loc_na <- data[!complete.cases(data[cols]),] ## new dataframe with NA values in lat, lon, or ts
# loc <- data[complete.cases(data[cols]),] ## new dataframe with no NA values in lat, lon, or ts
# loc$sunrise <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunrise')$time
# loc$sunset <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunset')$time
# gps.sunrise <- merge(loc, loc_na, all = TRUE)
# 
# # summarize sunrise data to one value per day per bird
# gps.sunrise_summarized <- gps.sunrise %>%
#   mutate(date = as.Date(timestamp)) %>%
#   group_by(birdid_year_nest, year, date) %>% 
#   summarize(median_sunrise = median(sunrise),
#             median_sunset = median(sunset))
# 
# rm(gps.sunrise, data, loc, loc_na)
# 
# # add sunrise information to complete gps data
# incubation_attempts <- merge(incubation_attempts, gps.sunrise_summarized,by = c("birdid_year_nest", "year", "date"), all = TRUE)
# 
# rm(gps.sunrise_summarized)
# 
# # add columns for 1 hour before sunrise and 1 hour after sunset
# incubation_attempts <- incubation_attempts %>%
#   mutate(sunrise_minus1 = median_sunrise - (1*60*60),
#          sunset_plus1 = median_sunset + (1*60*60)
#   )

# used reduce gps data of incubation attempts to only night hours but now using all hours of the day
incubation_attempts <- incubation_attempts %>% #group_by(birdid_year_nest, year, date) %>% 
 # filter(timestamp >= sunset_plus1 | timestamp <= sunrise_minus1) %>%
  #filter(timestamp > median_sunset | timestamp < median_sunrise) %>%
 # ungroup() %>%
  select(c("birdid_year","birdid", "device_id", "timestamp","Latitude",                
           "Longitude" ,             
          # "day_of_year"    ,      
           "year",                 #    "month" ,                
          #"day"   ,                  
           "date"    ,                 "status_laying"  ,          "status_incubation" ,       "egg_range_start_attempt1", "egg_range_end_attempt1" , 
           "egg_range_start_attempt2", "egg_range_end_attempt2",   "egg_range_start_attempt3", "egg_range_end_attempt3" , "egg_range_start_attempt4",
          "egg_range_end_attempt4","egg_range_start_attempt5","egg_range_end_attempt5", "egg_range_start_attempt6", "egg_range_end_attempt6",
         "fail_inc_range_start" ,   
           "fail_inc_range_end" ,      "succ_inc_range_start"  ,   "succ_inc_range_end"    ,   "status_comb"    ,          "nesting_attempt" ,        
           "attempt_length"  ,         "birdid_year_nest" ))

failed_inc_attempts <- failed_inc_attempts %>% 
  select(c("birdid_year","birdid", "device_id", "timestamp","Latitude",                
           "Longitude" ,             
           # "day_of_year"    ,      
           "year",                 #    "month" ,                
           #"day"   ,                  
           "date"    ,                 "status_laying"  ,          "status_incubation" ,       "egg_range_start_attempt1", "egg_range_end_attempt1" , 
           "egg_range_start_attempt2", "egg_range_end_attempt2",   "egg_range_start_attempt3", "egg_range_end_attempt3" , "egg_range_start_attempt4",
           "egg_range_end_attempt4","egg_range_start_attempt5","egg_range_end_attempt5", "egg_range_start_attempt6", "egg_range_end_attempt6",
            "fail_inc_range_start" ,   
           "fail_inc_range_end" ,      "succ_inc_range_start"  ,   "succ_inc_range_end"    ,   "status_comb"    ,          "nesting_attempt" ,        
           "attempt_length"  ,         "birdid_year_nest" ))

# for days from egg laying algorithm, separate into short and long nesting attempts (<=14 days); use only morning hours for short attempts, all hours for long attempts!!
### using only morning hours for ALL EGG LAYING ATTEMPTS NOW!!!
gps_nesting_days <- gps_nesting_days %>% filter(nesting_attempt == "1" | nesting_attempt == "2" | nesting_attempt == "3" |
                                                  nesting_attempt == "4" | nesting_attempt == "5" | nesting_attempt == "6" )
# looking at nest attempt length summary stats, but irrelevant now because all reduced to one window with max absolute x!
sum <- gps_nesting_days %>% group_by(birdid_year_nest) %>%
  summarise(nest_attempt_length=mean(attempt_length)) #%>%
  #mutate(nest_attempt_length=nest_attempt_length+6)
hist(as.numeric(sum$nest_attempt_length))
mean(as.numeric(sum$nest_attempt_length))
median(as.numeric(sum$nest_attempt_length))
# gps_nesting_days_short <- gps_nesting_days %>% filter(attempt_length <= 14)
# gps_nesting_days_long <- gps_nesting_days %>% filter(attempt_length > 14)
# 
# gps_nesting_days_short %>% group_by(nesting_attempt) %>% count()
# gps_nesting_days_long %>% group_by(nesting_attempt) %>% count()

gps_nesting_days_short <- gps_nesting_days

# calculate sunrise for each location
data <- gps_nesting_days_short %>% collect %>% as.data.frame
data$timestamp <- as_datetime(data$timestamp, tz = "UTC")
cols <- c("Latitude", "Longitude", "timestamp") ## Select columns that can't contain NA values
loc_na <- data[!complete.cases(data[cols]),] ## new dataframe with NA values in lat, lon, or ts
loc <- data[complete.cases(data[cols]),] ## new dataframe with no NA values in lat, lon, or ts
loc$sunrise <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunrise')$time
#loc$sunset <- suntools::sunriset(as.matrix(dplyr::select(loc,Longitude,Latitude)),loc$timestamp, POSIXct.out=T, direction='sunset')$time
gps.sunrise <- merge(loc, loc_na, all = TRUE)

# summarize sunrise data to one value per day per bird
gps.sunrise_summarized <- gps.sunrise %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(birdid_year_nest, year, date) %>% 
  summarize(median_sunrise = median(sunrise))

rm(gps.sunrise, data, loc, loc_na)

# add sunrise information to complete gps data
gps_nesting_days_short <- merge(gps_nesting_days_short, gps.sunrise_summarized,by = c("birdid_year_nest", "year", "date"), all = TRUE)

rm(gps.sunrise_summarized)

# add columns for 1 and 5 hours past sunrise
gps_nesting_days_short <- gps_nesting_days_short %>%
  mutate(sunrise_1 = median_sunrise + (1*60*60),
         sunrise_5 = median_sunrise + (5*60*60)
  )

# reduce gps data of short (NOW ALL!) nesting attempts to only sunrise hours
gps_nesting_days_short <- gps_nesting_days_short %>% group_by(birdid_year_nest, year, date) %>% 
  filter(timestamp >= sunrise_1) %>%
  filter(timestamp <= sunrise_5) %>%
  ungroup() %>%
  select(c("birdid_year","birdid", "device_id", "timestamp","Latitude",                
           "Longitude" ,             
           # "day_of_year"    ,      
           "year",                 #    "month" ,                
           #"day"   ,                  
           "date"    ,                 "status_laying"  ,          "status_incubation" ,       "egg_range_start_attempt1", "egg_range_end_attempt1" , 
           "egg_range_start_attempt2", "egg_range_end_attempt2",   "egg_range_start_attempt3", "egg_range_end_attempt3" , "egg_range_start_attempt4",
           "egg_range_end_attempt4","egg_range_start_attempt5","egg_range_end_attempt5", "egg_range_start_attempt6", "egg_range_end_attempt6",
            "fail_inc_range_start" ,   
           "fail_inc_range_end" ,      "succ_inc_range_start"  ,   "succ_inc_range_end"    ,   "status_comb"    ,          "nesting_attempt" ,        
           "attempt_length"  ,         "birdid_year_nest" ))

# gps_nesting_days_long <- gps_nesting_days_long %>% 
#   select(c("birdid_year","birdid", "device_id", "timestamp","Latitude",                
#            "Longitude" ,             
#            # "day_of_year"    ,      
#            "year",                 #    "month" ,                
#            #"day"   ,                  
#            "date"    ,                 "status_laying"  ,          "status_incubation" ,       "egg_range_start_attempt1", "egg_range_end_attempt1" , 
#            "egg_range_start_attempt2", "egg_range_end_attempt2",   "egg_range_start_attempt3", "egg_range_end_attempt3" , "egg_range_start_attempt4",
#            "egg_range_end_attempt4","egg_range_start_attempt5","egg_range_end_attempt5", "egg_range_start_attempt6", "egg_range_end_attempt6",
#            "egg_range_start_attempt7", "egg_range_end_attempt7", "fail_inc_range_start" ,   
#            "fail_inc_range_end" ,      "succ_inc_range_start"  ,   "succ_inc_range_end"    ,   "status_comb"    ,          "nesting_attempt" ,        
#            "attempt_length"  ,         "birdid_year_nest" ))

# combine short and long (ALL!) nesting attempts and successful and failed incubation attempts back together
gps_nesting_days <- rbind(gps_nesting_days_short, 
                          #gps_nesting_days_long, 
                          incubation_attempts, failed_inc_attempts)

rm(gps_nesting_days_long, gps_nesting_days_short, incubation_attempts, incubation_attempts_test, failed_inc_attempts)

# sort gps data by birdid_year_nest and date
gps_nesting_days <- gps_nesting_days[order(gps_nesting_days$birdid_year_nest, gps_nesting_days$timestamp), ]

## find nest locations as recursive movements
# first convert to an equal area projection
points_sf <- st_as_sf(gps_nesting_days, coords = c("Longitude", "Latitude"), crs = 4326) # make into shapefile
mean(gps_nesting_days$Latitude)
mean(gps_nesting_days$Longitude)
laea_proj_string <- "+proj=laea +lat_0=48.6535 +lon_0=-71.94093 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs" # new projection (update with mean lat and long!!!)
points_transformed <- st_transform(points_sf, crs = laea_proj_string)
coordinates <- st_coordinates(points_transformed)
gps_nesting_days_trans <- st_drop_geometry(points_transformed)
gps_nesting_days_trans <- cbind(gps_nesting_days_trans, coordinates)

# reduce to four variables needed 
data <- gps_nesting_days_trans %>% select(c("X", "Y", "timestamp", "birdid_year_nest"))

# reduce to one individual
ids <- unique(data$birdid_year_nest)
nest_locations_recursive <- data.frame()

#i <- 36

#### original method using nest location = most visited location, ties = most time spent ####

for (i in 1:length(ids)) {
  
  one <- data %>% filter(birdid_year_nest==ids[i])
  
  potential_nests <- getRecursions(one, 50) # tried 50 and 100 m but 50 m works much better!! 
  par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
  plot(potential_nests, one, legendPos = c(13, -10))
  
  drawCircle(-15, -10, 2)
  
  hist(potential_nests$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 50)")
  summary(potential_nests$revisits)
  
  #head(potential_nests$revisitStats)
  
  nests <- as.data.frame(potential_nests$revisitStats)
  
  nest <- nests %>% filter(visitIdx == max(visitIdx))
  
  if (nrow(nest) > 1) {
    nest_2 <- nests %>% filter(coordIdx %in% nest$coordIdx) %>%
      group_by(coordIdx) %>% 
      summarise(total_visit_duration=sum(timeInside)) %>%
      filter(total_visit_duration == max(total_visit_duration))
    nest <- nest_2
    
  }
  
  nest_location <- nests %>% filter(coordIdx == nest$coordIdx) %>%
    summarise(birdid_year_nest=max(id),
              nest_x=mean(x),
              nest_y=mean(y),
              num_of_visits=max(visitIdx),
              first_visit=min(entranceTime),
              last_visit=max(entranceTime),
              mean_visit_duration=mean(timeInside),
              total_visit_duration=sum(timeInside),
              shortest_visit=min(timeInside),
              longest_vist=max(timeInside),
              mean_time_between_visits=mean(timeSinceLastVisit, na.rm = TRUE),
              shortest_time_between_visits=min(timeSinceLastVisit, na.rm = TRUE),
              longest_time_between_visits=max(timeSinceLastVisit, na.rm = TRUE))
  
  nest_locations_recursive <- rbind(nest_locations_recursive, nest_location)
  
  i <- i + 1
  
}


#### method using nestr parameters (DON'T USE, DOESN'T WORK AS WELL AS OG METHOD!)####

# for (i in 1:length(ids)) {
#   
#   one <- data %>% filter(birdid_year_nest==ids[i])
#   
#   potential_nests <- getRecursions(one, 50) # tried 50 and 100 m but 50 m works much better!! 
#   par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
#   plot(potential_nests, one, legendPos = c(13, -10))
#   
#   drawCircle(-15, -10, 2)
#   
#   hist(potential_nests$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 50)")
#   summary(potential_nests$revisits)
#   
#   #head(potential_nests$revisitStats)
#   
#   nests <- as.data.frame(potential_nests$revisitStats)
# 
#   # identify potential nests that are visited on at least three consecutive days
#   nests$date <- as.Date(nests$entranceTime)
#   
#  nests_consec_days <- nests %>%
#    arrange(coordIdx, date) %>% # Ensure data is sorted by location ID and Date
#    group_by(coordIdx) %>% # Group by location ID to handle consecutive days per location
#    distinct(date, .keep_all = TRUE) %>% # only keep one visit per day
#    mutate(
#      # Create a grouping variable for consecutive blocks
#      consecutive_block = cumsum(c(TRUE, diff(date) != 1)),
#      # Count consecutive days within each block
#      consecutive_days = sequence(rle(as.numeric(consecutive_block))$lengths)
#    ) %>%
#    ungroup()
#  nests_3consecDays <- nests_consec_days %>% filter(consecutive_days>=3)
#  
#  if (nrow(nests_3consecDays) > 0) {
#  
#  # reduce nests (one location per day) to only locations that were visited at least 3 consecutive days or more
#  nests_consec_days <- nests_consec_days %>% filter(coordIdx %in% nests_3consecDays$coordIdx)
#  
#  # calculate percent of days visited between first and last day visited (i.e., how consistently a nest is visited on a daily basis)
#  days_visited <- nests_consec_days %>% group_by(coordIdx) %>%
#    summarise(first_day = min(date),
#              last_day = max(date),
#              duration= (max(date) - min(date))+1,
#              num_days_visited = n(),
#              max_num_consec_days = max(consecutive_days)) %>%
#    mutate(percent_days_visited=num_days_visited/as.numeric(duration)*100)
#  
#  # reduce days_visited to the location(s) with the higher percent of days visited
#  nest <- days_visited %>% filter(percent_days_visited==max(percent_days_visited))
# 
# #nest <- nests %>% filter(visitIdx == max(visitIdx)) (original method was just to take the location that was visited most frequently)
#  
#  # old method where if there was a tie, used the location that the bird spent the most time at 
#   # if (nrow(nest) > 1) {
#    # nest_2 <- nests %>% filter(coordIdx %in% nest$coordIdx) %>%
#     #  group_by(coordIdx) %>% 
#      # summarise(total_visit_duration=sum(timeInside)) %>%
#       #filter(total_visit_duration == max(total_visit_duration))
#   #  nest <- nest_2
#     
#   #}
#  
#  # if there are more than one nest with the same highest percent of days visited, 
#  # then use the location that the bird spent the most time at on the day with maximum attendance
# 
#    nest_2 <- nests %>% filter(coordIdx %in% nest$coordIdx) %>%
#        group_by(coordIdx, date) %>% 
#       summarise(daily_visit_duration=sum(timeInside)) %>% ungroup() %>%
#      filter(daily_visit_duration == max(daily_visit_duration))
#    
#     # save the maximum daily visit duration
#       max_daily_visit_duration <- nest_2$daily_visit_duration
#    
#        nest <- nest %>% filter(coordIdx %in% nest_2$coordIdx)
#        
#      # save the max number of consecutive days visited 
#        max_num_consec_days <- nest$max_num_consec_days
#        
#        # save the percent of days from first to last day visited that the location was visited 
#        percent_days_visited <- nest$percent_days_visited
#   
#   nest_location <- nests %>% filter(coordIdx == nest$coordIdx) %>%
#     summarise(birdid_year_nest=max(id),
#               nest_x=mean(x),
#               nest_y=mean(y),
#               num_of_visits=max(visitIdx),
#               first_visit=min(entranceTime),
#               last_visit=max(entranceTime),
#               mean_visit_duration=mean(timeInside),
#               total_visit_duration=sum(timeInside),
#               shortest_visit=min(timeInside),
#               longest_vist=max(timeInside),
#               mean_time_between_visits=mean(timeSinceLastVisit, na.rm = TRUE),
#               shortest_time_between_visits=min(timeSinceLastVisit, na.rm = TRUE),
#               longest_time_between_visits=max(timeSinceLastVisit, na.rm = TRUE))
#  
#  
#  nest_location$max_num_consec_days <- max_num_consec_days
#  nest_location$percent_days_visited <- percent_days_visited
#  nest_location$max_daily_visit_duration <- max_daily_visit_duration
#  
#  }
#  else{
#    nest_location <- nests[1,] 
#    nest_location <- nest_location %>%
#      rename(birdid_year_nest=id) %>% 
#         mutate(nest_x=NA,
#                nest_y=NA,
#                num_of_visits=NA,
#                first_visit=NA,
#                last_visit=NA,
#                mean_visit_duration=NA,
#                total_visit_duration=NA,
#                shortest_visit=NA,
#                longest_vist=NA,
#                mean_time_between_visits=NA,
#                shortest_time_between_visits=NA,
#                longest_time_between_visits=NA,
#                max_num_consec_days=NA,
#                percent_days_visited=NA,
#                max_daily_visit_duration=NA
#         ) %>%
#      select(c("birdid_year_nest", "nest_x",
#               "nest_y",
#               "num_of_visits",
#               "first_visit",
#               "last_visit",
#               "mean_visit_duration",
#               "total_visit_duration",
#              "shortest_visit",
#               "longest_vist",
#               "mean_time_between_visits",
#               "shortest_time_between_visits",
#               "longest_time_between_visits",
#              "max_num_consec_days",
#             "percent_days_visited",
#              "max_daily_visit_duration"))
#  }
#  
#   
#   nest_locations_recursive <- rbind(nest_locations_recursive, nest_location)
#   
#   i <- i + 1
#   
# }

#### proceeding with nest locations to set dates ####

# save nests with NA nest location separately (doesn't happen with original method of nest locating)
#no_nest_location <- nest_locations_recursive %>% filter(is.na(nest_x) == TRUE) # 28 attempts when using minimum 3 consecutive days

# remove ids with no nest location 
#nest_locations_recursive <- nest_locations_recursive %>% filter(is.na(nest_x) == FALSE)

# convert nest locations back to lat long
# first convert to an equal area projection
points_sf <- st_as_sf(nest_locations_recursive, coords = c("nest_x", "nest_y"), crs = laea_proj_string) # make into shapefile
points_transformed <- st_transform(points_sf, crs = 4326)
coordinates <- st_coordinates(points_transformed)
nest_locations_recursive_trans <- st_drop_geometry(points_transformed)
nest_locations_recursive_trans <- cbind(nest_locations_recursive_trans, coordinates)

nest_locations_recursive_trans <- nest_locations_recursive_trans %>% 
  rename(nest_long=X,
         nest_lat=Y)

rm(data, gps_nesting_days, nest, nest_2, nest_location, nest_locations_recursive, 
   nests, no_nest_location, one, points_sf, points_transformed, potential_nests, sum,
   gps_nesting_days, gps_nesting_days_trans)

# add nest locations to gps data ### need to use all hours of the day for next step, not just morning hours!!!! 
#### combine gps data for all nesting attempts (this will be all hours of the day again) and include 24 days before start of nesting attempt and 30 days after) ####
gps.breed <- tbl(conn, "hourly_gps_locations_for_birds_with_inc_and_laying_classified") %>%
  collect() 

# add nest attempt date ranges to gps data
gps_nesting.dates <- merge(gps.breed, summary_fullrange, by = "birdid_year")

rm(gps.breed)

gps_nesting.dates <- gps_nesting.dates %>% mutate(date = as.Date(timestamp))

# filter gps to first, second and third attempts (for egg laying attempts, use 12 d before, 18 after, for failed and hatched incubation attempts, use 18 d before/30 after)
gps_first.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt1-12 & date <= egg_range_end_attempt1+18)
gps_first.nesting.attempts$nesting_attempt <- 1
gps_first.nesting.attempts <- gps_first.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt1-egg_range_start_attempt1)
gps_second.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt2-12 & date <= egg_range_end_attempt2+18)
gps_second.nesting.attempts$nesting_attempt <- 2
gps_second.nesting.attempts <- gps_second.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt2-egg_range_start_attempt2)
gps_third.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt3-12 & date <= egg_range_end_attempt3+18)
gps_third.nesting.attempts$nesting_attempt <- 3
gps_third.nesting.attempts <- gps_third.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt3-egg_range_start_attempt3)
gps_fourth.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt4-12 & date <= egg_range_end_attempt4+18)
gps_fourth.nesting.attempts$nesting_attempt <- 4
gps_fourth.nesting.attempts <- gps_fourth.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt4-egg_range_start_attempt4)
gps_fifth.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt5-12 & date <= egg_range_end_attempt5+18)
gps_fifth.nesting.attempts$nesting_attempt <- 5
gps_fifth.nesting.attempts <- gps_fifth.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt5-egg_range_start_attempt5)
gps_sixth.nesting.attempts <- gps_nesting.dates %>% filter(date >= egg_range_start_attempt6-12 & date <= egg_range_end_attempt6+18)
gps_sixth.nesting.attempts$nesting_attempt <- 6
gps_sixth.nesting.attempts <- gps_sixth.nesting.attempts %>%
  mutate(attempt_length=egg_range_end_attempt6-egg_range_start_attempt6)
gps_hatched.attempts <- gps_nesting.dates %>% filter(date >= succ_inc_range_start-18 & date <= succ_inc_range_end+30)
gps_hatched.attempts$nesting_attempt <- 'h'
gps_hatched.attempts <- gps_hatched.attempts %>%
  mutate(attempt_length=succ_inc_range_end-succ_inc_range_start)
gps_failed.attempts <- gps_nesting.dates %>% filter(date >= fail_inc_range_start-18 & date <= fail_inc_range_end+30)
gps_failed.attempts$nesting_attempt <- 'f'
gps_failed.attempts <- gps_failed.attempts %>%
  mutate(attempt_length=fail_inc_range_end-fail_inc_range_start)

# combine gps data for all nesting attempts
gps_nesting_days <- rbind(gps_first.nesting.attempts, gps_second.nesting.attempts, 
                          gps_third.nesting.attempts, gps_fourth.nesting.attempts, gps_fifth.nesting.attempts, 
                          gps_sixth.nesting.attempts, #### ADD SIXTH BACK IN FOR MALLARDS
                          gps_hatched.attempts, gps_failed.attempts)

rm(gps_failed.attempts, gps_first.nesting.attempts, gps_hatched.attempts, 
   gps_nesting.dates, gps_second.nesting.attempts, gps_third.nesting.attempts, 
   gps_fourth.nesting.attempts, gps_fifth.nesting.attempts, 
   gps_sixth.nesting.attempts
   )

unique(gps_nesting_days$status_comb)

#### try using 24 d before/30 d after for hatched nests, 12 d before/ 18 d after for failed nests and 6 d before and 12 d after for egg laying nests (this was before when I was just using the egg laying dates and not including failed and hatched dates) ####
# gps_nesting.dates_hatched <- gps_nesting.dates %>% filter(status_comb == "egg_laying_hatched")
# 
# # filter gps to first, second and third attempts
# gps_first.nesting.attempts <- gps_nesting.dates_hatched %>% filter(date >= egg_range_start_attempt1-24 & date <= egg_range_end_attempt1+30)
# gps_first.nesting.attempts$nesting_attempt <- 1
# gps_first.nesting.attempts <- gps_first.nesting.attempts %>%
#   mutate(attempt_length=egg_range_end_attempt1-egg_range_start_attempt1)
# gps_second.nesting.attempts <- gps_nesting.dates_hatched %>% filter(date >= egg_range_start_attempt2-24 & date <= egg_range_end_attempt2+30)
# gps_second.nesting.attempts$nesting_attempt <- 2
# gps_second.nesting.attempts <- gps_second.nesting.attempts %>%
#   mutate(attempt_length=egg_range_end_attempt2-egg_range_start_attempt2)
# gps_third.nesting.attempts <- gps_nesting.dates_hatched %>% filter(date >= egg_range_start_attempt3-24 & date <= egg_range_end_attempt3+30)
# gps_third.nesting.attempts$nesting_attempt <- 3
# gps_third.nesting.attempts <- gps_third.nesting.attempts %>%
#   mutate(attempt_length=egg_range_end_attempt3-egg_range_start_attempt3)
# 
# # combine gps data for all nesting attempts
# gps_nesting_days_hatched <- rbind(gps_first.nesting.attempts, gps_second.nesting.attempts, gps_third.nesting.attempts)
# 
# gps_nesting.dates_failed <- gps_nesting.dates %>% filter(status_comb == "egg_laying_failed")
# 
# # filter gps to first, second and third attempts
# gps_first.nesting.attempts <- gps_nesting.dates_failed %>% filter(date >= egg_range_start_attempt1-18 & date <= egg_range_end_attempt1+24)
# gps_first.nesting.attempts$nesting_attempt <- 1
# gps_first.nesting.attempts <- gps_first.nesting.attempts %>%
#   mutate(attempt_length=egg_range_end_attempt1-egg_range_start_attempt1)
# gps_second.nesting.attempts <- gps_nesting.dates_failed %>% filter(date >= egg_range_start_attempt2-18 & date <= egg_range_end_attempt2+24)
# gps_second.nesting.attempts$nesting_attempt <- 2
# gps_second.nesting.attempts <- gps_second.nesting.attempts %>%
#   mutate(attempt_length=egg_range_end_attempt2-egg_range_start_attempt2)
# gps_third.nesting.attempts <- gps_nesting.dates_failed %>% filter(date >= egg_range_start_attempt3-18 & date <= egg_range_end_attempt3+24)
# gps_third.nesting.attempts$nesting_attempt <- 3
# gps_third.nesting.attempts <- gps_third.nesting.attempts %>%
#   mutate(attempt_length=egg_range_end_attempt3-egg_range_start_attempt3)
# 
# # combine gps data for all nesting attempts
# gps_nesting_days_failed <- rbind(gps_first.nesting.attempts, gps_second.nesting.attempts, gps_third.nesting.attempts)
# 
# gps_nesting.dates_defer <- gps_nesting.dates %>% filter(status_comb == "egg_laying_defer")
# 
# # filter gps to first, second and third attempts
# gps_first.nesting.attempts <- gps_nesting.dates_defer %>% filter(date >= egg_range_start_attempt1-12 & date <= egg_range_end_attempt1+18)
# gps_first.nesting.attempts$nesting_attempt <- 1
# gps_first.nesting.attempts <- gps_first.nesting.attempts %>%
#   mutate(attempt_length=egg_range_end_attempt1-egg_range_start_attempt1)
# gps_second.nesting.attempts <- gps_nesting.dates_defer %>% filter(date >= egg_range_start_attempt2-12 & date <= egg_range_end_attempt2+18)
# gps_second.nesting.attempts$nesting_attempt <- 2
# gps_second.nesting.attempts <- gps_second.nesting.attempts %>%
#   mutate(attempt_length=egg_range_end_attempt2-egg_range_start_attempt2)
# gps_third.nesting.attempts <- gps_nesting.dates_defer %>% filter(date >= egg_range_start_attempt3-12 & date <= egg_range_end_attempt3+18)
# gps_third.nesting.attempts$nesting_attempt <- 3
# gps_third.nesting.attempts <- gps_third.nesting.attempts %>%
#   mutate(attempt_length=egg_range_end_attempt3-egg_range_start_attempt3)
# 
# # combine gps data for all nesting attempts
# gps_nesting_days_defer <- rbind(gps_first.nesting.attempts, gps_second.nesting.attempts, gps_third.nesting.attempts)
# 
# # combine gps data for all nesting attempts and all nesting outcomes
# gps_nesting_days <- rbind(gps_nesting_days_defer, gps_nesting_days_failed, gps_nesting_days_hatched)

#### continue with assigning dates... ####

# create new id of birdid_year_nest attempt
gps_nesting_days$birdid_year_nest <- paste(gps_nesting_days$birdid_year, gps_nesting_days$nesting_attempt, sep="_")

gps_nesting_days <- merge(gps_nesting_days, nest_locations_recursive_trans, by = "birdid_year_nest", all = TRUE)

gps_nesting_days <- gps_nesting_days %>% filter(is.na(nest_lat)==FALSE)

# calculate distance from nest for each timestamp
gps_nesting_days$distance_to_nest <- distHaversine(
  p1 = cbind(gps_nesting_days$Longitude, gps_nesting_days$Latitude),
  p2 = cbind(gps_nesting_days$nest_long, gps_nesting_days$nest_lat)
)

# add column for whether the bird is within 100 m of nest or not (tried 50, 100 and 150 m.. 100 m was best, but 150 also works quite well and sometimes better for a few birds)
gps_nesting_days$within_radius <- gps_nesting_days$distance_to_nest <= 100

# create daily summary of percentage of points within 100 m of nest
daily_summary <- gps_nesting_days %>%
  group_by(birdid_year_nest, status_comb, date) %>%
  summarise(
    total_locations = n(),
    locations_within_radius = sum(within_radius),
    percentage_within_radius = (locations_within_radius / total_locations) * 100
  )

# add column for if percentage is >= 75%, 50%, 0%
daily_summary <- daily_summary %>%
  mutate(greater_equal_to_75=ifelse(percentage_within_radius >= 75, "Y", "N"),
         greater_equal_to_50=ifelse(percentage_within_radius >= 50, "Y", "N"),
         greater_than_0=ifelse(percentage_within_radius > 0, "Y", "N"))

# create plots by bird of daily summary
library(ggpubr)
library(ggplot2)
library(grid)
summary(daily_summary)
daily_summary$status_comb <- as.factor(daily_summary$status_comb)
daily_summary$greater_equal_to_75 <- as.factor(daily_summary$greater_equal_to_75)
daily_summary$greater_equal_to_50 <- as.factor(daily_summary$greater_equal_to_50)
# 
# list_obs_final<-daily_summary %>%distinct(birdid_year_nest)
# list_obs_final <- list_obs_final[1:100,] # just plotting first 100 to make this faster... 
# setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics") # laptop
# 
# pdf("plots_EggLayingAndIncubationCombined_14Sept2025/AllNestingAttempts_ABDU_PercentagePlots_rad50_OneWindowMaxABSXNestLocation_SunriseForNestAttempts_21Nov2025.pdf",paper="a4r",width=9,height=6)
# for (i in 1:nrow(list_obs_final)){
#   d<-daily_summary %>% filter(birdid_year_nest==list_obs_final[i,2])
#   theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
#   g<-ggarrange(
#     ggplot(d,aes(x=date,y=percentage_within_radius,color=greater_equal_to_50))+
#       geom_point() + 
#       #geom_line(size=1)+
#       geom_hline(yintercept = 50, color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("N" = "black", "Y" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Percentage of Daily Locations within 100 m of Potential Nest Location"),
#     
#     ncol=1,nrow = 1,common.legend = TRUE)
#   g <- g + annotation_custom(
#     grob = textGrob(paste0(list_obs_final[i, 2], ": ", d$status_comb), 
#                     gp = gpar(fontsize = 12, fontface = "bold", col="red")),
#     xmin = 0.7, xmax = Inf, ymin = 0.95, ymax = Inf  )# Adjust the positioning as needed
#   print(g)
# }
# dev.off()

# assign start and end dates of incubation and laying
daily_summary2 <- daily_summary %>% 
  mutate(greater_equal_to_50=ifelse(greater_equal_to_50=="Y", TRUE, FALSE),
         greater_than_0=ifelse(greater_than_0=="Y", TRUE, FALSE)) 

start_inc_dates <- data.frame()
end_inc_dates <- data.frame()
start_lay_dates <- data.frame()
end_lay_dates <- data.frame()

ids <- unique(daily_summary2$birdid_year_nest)

for (i in 1:length(ids)) {
  df <- daily_summary2 %>% filter(birdid_year_nest==ids[i])
  r <- rle(as.character(df$greater_equal_to_50))
  r_lay <- rle(as.character(df$greater_than_0))
  
  # Find the index of the first run of "Y" that has a length of at least 3 (first of three consecutive days >= 50% = start of incubation)
  run_index <- which(r$values == TRUE & r$lengths >= 3)[1]
  # Find the index of the first run of "Y" that has a length of at least 3 (first of three consecutive days > 0% = start of egg laying WHEN NO INCUBATION OCCURS)
  run_index_lay <- which(r_lay$values == TRUE & r_lay$lengths >= 3)[1]
  
  # Check if a valid run was found
  if (!is.na(run_index)) {
    # Calculate the starting row index of this run
    start_row <- ifelse(run_index == 1, 1, sum(r$lengths[1:(run_index - 1)]) + 1)
    
    # Get the length of this specific run
    run_length <- r$lengths[run_index]
    
    # Calculate the ending row index
    end_row <- start_row + run_length - 1
    
    # Extract the entire run
    result <- df[start_row:end_row, , drop = FALSE]
    
    # reduce to only the first day of the run
    result <- result %>% filter(date==min(date))
    
    # add to df with other incubation start dates
    start_inc_dates <- rbind(start_inc_dates, result)
    
    start <- result$date
    
    # filter to before the start of incubation to find the start of egg laying
    df_lay <- df %>% filter(date < start)
    
    # reverse order of dates, so searching backwards from the start of incubation
    df_lay<- df_lay[order(df_lay$date, decreasing = TRUE), ]
    
    r <- rle(as.character(df_lay$greater_than_0))
    
    # Find the index of the first run of "N" that has a length of at least 3 (first day after three consecutive days equal to 0% = the start of egg laying)
    run_index <- which(r$values == FALSE & r$lengths >= 3)[1]
    
    # Check if a valid run was found
    if (!is.na(run_index)) {
      # Calculate the starting row index of this run
      start_row <- ifelse(run_index == 1, 1, sum(r$lengths[1:(run_index - 1)]) + 1)
      
      # Get the length of this specific run
      run_length <- r$lengths[run_index]
      
      # Calculate the ending row index
      end_row <- start_row + run_length - 1
      
      # Extract the entire run
      result <- df_lay[start_row:end_row, , drop = FALSE]
      
      # reduce to only the first day of the run
      result <- result %>% filter(date==max(date))
      result$date <- as.Date(result$date)+1
      
      # add to df with other lay start dates
      start_lay_dates <- rbind(start_lay_dates, result)
      
    } else{
      result <- df[1,]
      result <- result %>% mutate(date=NA)
      start_lay_dates <- rbind(start_lay_dates, result)
    }
    
    # filter to after the start of incubation to find the end of incubation
    df <- df %>% filter(date > start)
    
    r <- rle(as.character(df$greater_equal_to_50))
    
    # Find the index of the first run of "N" that has a length of at least 3 (first day of three consecutive days < 50% = the end of incubation)
    run_index <- which(r$values == FALSE & r$lengths >= 3)[1]
    
    # Check if a valid run was found
    if (!is.na(run_index)) {
      # Calculate the starting row index of this run
      start_row <- ifelse(run_index == 1, 1, sum(r$lengths[1:(run_index - 1)]) + 1)
      
      # Get the length of this specific run
      run_length <- r$lengths[run_index]
      
      # Calculate the ending row index
      end_row <- start_row + run_length - 1
      
      # Extract and print the entire run
      result <- df[start_row:end_row, , drop = FALSE]
      
      result <- result %>% filter(date==min(date))
      
      end_inc_dates <- rbind(end_inc_dates, result)
      
    } else{
      result <- df[1,]
      result <- result %>% mutate(date=NA)
      end_inc_dates <- rbind(end_inc_dates, result)
    }
    result <- df[1,]
    result <- result %>% mutate(date=NA)
    end_lay_dates <- rbind(end_lay_dates, result)
    }  else if(!is.na(run_index_lay)) { # if no incubation, checking for a valid run for egg laying
     
        # Calculate the starting row index of this run
        start_row <- ifelse(run_index_lay == 1, 1, sum(r_lay$lengths[1:(run_index_lay - 1)]) + 1)
        
        # Get the length of this specific run
        run_length <- r_lay$lengths[run_index_lay]
        
        # Calculate the ending row index
        end_row <- start_row + run_length - 1
        
        # Extract the entire run
        result <- df[start_row:end_row, , drop = FALSE]
        
        # reduce to only the first day of the run
        result <- result %>% filter(date==min(date))
        
        # add to df with other lay start dates
        start_lay_dates <- rbind(start_lay_dates, result)
        
        start_lay <- result$date
        
        # filter to after the start of laying to find the end of laying
        df <- df %>% filter(date > start_lay)
        
        r <- rle(as.character(df$greater_than_0))
        
        # Find the index of the first run of "N" that has a length of at least 3 (first day of three consecutive days = 0% = the end of egg laying)
        run_index <- which(r$values == FALSE & r$lengths >= 3)[1]
        
        # Check if a valid run was found
        if (!is.na(run_index)) {
          # Calculate the starting row index of this run
          start_row <- ifelse(run_index == 1, 1, sum(r$lengths[1:(run_index - 1)]) + 1)
          
          # Get the length of this specific run
          run_length <- r$lengths[run_index]
          
          # Calculate the ending row index
          end_row <- start_row + run_length - 1
          
          # Extract and print the entire run
          result <- df[start_row:end_row, , drop = FALSE]
          
          result <- result %>% filter(date==min(date))
          
          end_lay_dates <- rbind(end_lay_dates, result)
        }  else{
          result <- df[1,]
          result <- result %>% mutate(date=NA)
          end_lay_dates <- rbind(end_lay_dates, result)
        }  
        result <- df[1,]
        result <- result %>% mutate(date=NA)
        start_inc_dates <- rbind(start_inc_dates, result)
        end_inc_dates <- rbind(end_inc_dates, result)

    } else {
      result <- df[1,]
      result <- result %>% mutate(date=NA)
      start_lay_dates <- rbind(start_lay_dates, result)
      start_inc_dates <- rbind(start_inc_dates, result)
      end_inc_dates <- rbind(end_inc_dates, result)
      end_lay_dates <- rbind(end_lay_dates, result)
    }
  
  i <- i + 1
  
}

start_inc_dates <- start_inc_dates %>% rename(inc_start=date)
end_inc_dates <- end_inc_dates %>% rename(inc_end=date)
start_lay_dates <- start_lay_dates %>% rename(lay_start=date)
end_lay_dates <- end_lay_dates %>% rename(lay_end=date)

start_end_dates <- cbind(start_inc_dates, end_inc_dates$inc_end, start_lay_dates$lay_start, end_lay_dates$lay_end)

start_end_dates <- start_end_dates %>%
  rename(inc_end= ...10,
         lay_start= ...11,
         lay_end= ...12) %>%
  select("birdid_year_nest", "status_comb", "lay_start", "lay_end", "inc_start", "inc_end")

## remove birds that have failed incubation attempt nest location and dates when they also had hatched incubation attempts classified
start_end_dates$nesting_attempt <- str_sub(start_end_dates$birdid_year_nest, -1, -1)
fail_attempts <- start_end_dates %>% filter(nesting_attempt == "f")
fail_attempts <- fail_attempts %>% filter(status_comb != "egg_laying_hatched") # removes 144 observations (MALL), 75 observations (ABDU)
start_end_dates_fail_rem <- start_end_dates %>% filter(nesting_attempt != "f")
start_end_dates <- rbind(start_end_dates_fail_rem, fail_attempts)

## remove birds with mean daily percent of locations within 100 m of nest location during incubation > 95% (assume that is molt not incubation)
# first add start and end incubation dates to percent df (daily_summary2)
# daily_summary2 <- merge(daily_summary2, start_end_dates, by = "birdid_year_nest")
# # filter to only incubation days
# daily_summary2_inc <- daily_summary2 %>%
#   filter(date > inc_start & date < inc_end)
# # calculate mean daily percent during incubation for each bird_year_nest
# mean_percent_inc <- daily_summary2_inc %>% group_by(birdid_year_nest) %>%
#   summarise(mean_percent_inc=mean(percentage_within_radius),
#             median_percent_inc=median(percentage_within_radius))
# # look at distribution of mean percentages during incubation
# hist(mean_percent_inc$mean_percent_inc)
# hist(mean_percent_inc$median_percent_inc)

### decided not to remove birds based on this above because getting too subjective, so left potential molts in.... 

# add status based on recurse+ruleset method (laying vs. incubating) to make it easier to find overlaps
start_end_dates <- start_end_dates %>%
  mutate(status_recurse=ifelse(is.na(inc_start) == TRUE, "egg laying", "incubating" )) # if an attempt has incubating date, then status from recurse/ruleset will be incubtaing, otherwise egg laying

start_end_dates %>% group_by(status_comb,status_recurse) %>% count()

# add birdid_year to dates df
start_end_dates$birdid_year <- substr(start_end_dates$birdid_year_nest, start = 1, stop = 15)

# add attempt
start_end_dates$attempt <- substr(start_end_dates$birdid_year_nest, start=17, stop = 17)

# add successful incubation date ranges from algorithm classification
start_end_dates <- merge(start_end_dates, summary_fullrange, by = c("birdid_year", "status_comb"), all.x = TRUE)


# change status of attempts with missing dates to "unknown" and move from nesting attempts to a new dataframe of removed attempts
start_end_dates <- start_end_dates %>% ungroup() %>%
  mutate(status_comb = as.character(status_comb)) %>%
  mutate(status_comb=ifelse(is.na(lay_start) == TRUE & status_recurse=="egg laying", "unknown", status_comb )) %>% # every egg laying attempt should have a lay start date (don't require this for incubation attempts)
  mutate(status_comb=ifelse(is.na(inc_start) == FALSE & is.na(inc_end) == TRUE, "unknown", status_comb )) %>% # if an attempt has an inc start date, it should have an inc end date
  mutate(status_comb=ifelse(is.na(inc_start) == TRUE & is.na(lay_end) == TRUE, "unknown", status_comb )) # if an attempt does not have an inc start date, it should have a lay end date

# save unknown status birds separately
attempts_dropped <- start_end_dates %>% filter(status_comb == "unknown") # 103 (old), 173 (now that keeping all birds) observations - ABDU, 734 obs - MALL , now 671 obs for MALL with <7d = same nest attempt, now 577 obs using single window of greatest mean absx, now 272 obs for MALL with most recent method (), now 467 for mallards with all birds included
setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics")
write.csv(attempts_dropped, "Compiled Datasets for Machine Learning/AttemptsRemovedBecauseRecurseFunctionPlusRulesetDidNotFindLayingOrIncubationDates_MALL_SingleWindowMaxAbsx_18Jun2026.csv")

# drop unknown status birds from start end dates df
start_end_dates <- start_end_dates %>% filter(status_comb != "unknown")

start_end_dates %>% group_by(status_comb,status_recurse) %>% count()

# flag any attempts that overlap with the incubation date range
# set start/end dates based on whether an attempt is incubating or not
start_end_dates <- start_end_dates %>%
  mutate(start=ifelse(status_recurse == "incubating", inc_start, lay_start),
         end=ifelse(status_recurse == "incubating", inc_end, lay_end))
start_end_dates$start <- as.Date(start_end_dates$start)
start_end_dates$end <- as.Date(start_end_dates$end)
start_end_dates$overlap_with_inc_range <- "N"
# loop over all rows of start_end_dates df
for (i in 1:nrow(start_end_dates)) {
  if (start_end_dates$status_comb[i] == "egg_laying_hatched") {
    # Define your date ranges
    start_date1 <- as.Date(start_end_dates$start[i])
    end_date1 <- as.Date(start_end_dates$end[i])
    
    start_date2 <- as.Date(start_end_dates$succ_inc_range_start[i])
    end_date2 <- as.Date(start_end_dates$succ_inc_range_end[i])
    
    # Create Interval objects
    interval1 <- interval(start_date1, end_date1)
    interval2 <- interval(start_date2, end_date2)
    
    if(int_overlaps(interval1, interval2)) {
      start_end_dates$overlap_with_inc_range[i] <- "Y"
    }
  }
  
  i <- i + 1
}

# change status of attempts with missing dates to "unknown" and move from nesting attempts to a new dataframe of removed attempts
start_end_dates <- start_end_dates %>% ungroup() %>%
  mutate(status_comb=ifelse(is.na(lay_start) == TRUE & overlap_with_inc_range=="N", "unknown", status_comb )) # only allow missing lay start dates if an incubation attempt overlaps with the successful incubation range from the incubation algorithm

# save unknown status birds separately
attempts_dropped2 <- start_end_dates %>% filter(status_comb == "unknown") # 30 (old), now 42 observations with keeping all birds - ABDU, 734 obs - MALL , now 671 obs for MALL with <7d = same nest attempt, now 577 obs using single window of greatest mean absx, now 272 obs for MALL with most recent method (only keeping missing lay dates for ones that overlap inc range), 239 for MALL now that keeping all birds
#attempts_dropped <- rbind(attempts_dropped, attempts_dropped2)
write.csv(attempts_dropped2, "Compiled Datasets for Machine Learning/AttemptsRemoved_2ndRound_MALL_SingleWindowMaxAbsx_18Jun2026.csv")

# drop unknown status birds from start end dates df
start_end_dates <- start_end_dates %>% filter(status_comb != "unknown")

start_end_dates %>% group_by(status_comb,status_recurse) %>% count()



## remove any attempts with the same dates (removes 141 attempts when using full range of classified dates, 186 removed when using single window of max mean absx for mall, removed 116 for ABDU)
# loop through each birdid_year to check if attempts have the same dates
ids <- unique(start_end_dates$birdid_year)
new_data <- data.frame()
for (i in 1:length(ids)) {
  data <- start_end_dates %>% filter(birdid_year == ids[i])
  data <- data[!duplicated(data[, c("lay_start", "lay_end", "inc_end")]), ]
  new_data <- rbind(new_data, data)
  i <- i + 1
}

start_end_dates <- new_data

rm(new_data, data)

## remove any overlapping attempts (particularly nest attempts that overlap with fails and hatches)
# sort data by birdid_year and lay_start date
start_end_dates <- start_end_dates[order(start_end_dates$birdid_year, start_end_dates$lay_start), ]
start_end_dates <- start_end_dates %>%
  mutate(start=ifelse(status_recurse == "incubating", inc_start, lay_start),
         end=ifelse(status_recurse == "incubating", inc_end, lay_end))
start_end_dates$start <- as.Date(start_end_dates$start)
start_end_dates$end <- as.Date(start_end_dates$end)

# loop through each birdid_year to check if attempts overlap
ids <- unique(start_end_dates$birdid_year)
data_to_remove <- data.frame()
for (i in 1:length(ids)) {
  data <- start_end_dates %>% filter(birdid_year == ids[i])
  if(nrow(data) > 1) { # proceed if birdid_year has multiple attempts
    data$lay_length <- data$lay_end - data$lay_start
    data$lay_length_inc <- data$inc_start - data$lay_start
    # check all rows relative to previous row
    for (j in 1:(nrow(data)-1)) {
      data_subset <- data[j:(j+1),]
      status <- unique(data_subset$status_recurse)
      # Define your date ranges
      start_date1 <- as.Date(data$start[j])
      end_date1 <- as.Date(data$end[j])
      
      start_date2 <- as.Date(data$start[j+1])
      end_date2 <- as.Date(data$end[j+1])

      # Create Interval objects
      interval1 <- interval(start_date1, end_date1)
      interval2 <- interval(start_date2, end_date2)

      if(int_overlaps(interval1, interval2)) { # proceed if the subsequent attempt overlaps the previous attempt
        if ("egg laying" %in% status & "incubating" %in% status) {  # if one of the overlapping are incubating and one egg laying, keep incubating
          rem <- data_subset %>% filter(status_recurse == "egg laying")
          data_to_remove <- rbind(data_to_remove, rem)
        } else if ("egg laying" %in% status) { # if both are egg laying, keep the one with the shortest egg laying period; if the same length, just remove the second one
          if (data$lay_length[j] == data$lay_length[j+1]) {
          rem <- data_subset[j+1,]
          data_to_remove <- rbind(data_to_remove, rem)
          } else {
            rem <- data_subset %>% filter(lay_length != min(lay_length))
            data_to_remove <- rbind(data_to_remove, rem)
          }
        } else if ("incubating" %in% status) { # if both are incubating, keep the one that is classified as successful hatched if possible, otherwise use the one that is classified as failed incubation if possible, otherwise use the one with the shortest egg laying period?? 
          if("h" %in% unique(data_subset$nesting_attempt)) {
            rem <- data_subset %>% filter(nesting_attempt != "h")
            data_to_remove <- rbind(data_to_remove, rem)
          } else if("f" %in% unique(data_subset$nesting_attempt)) { #otherwise use the one that is classified as failed incubation if possible
            rem <- data_subset %>% filter(nesting_attempt != "f")
            data_to_remove <- rbind(data_to_remove, rem)
          } else if(sum(is.na(data_subset$lay_start))==1) { #otherwise use the one with a lay start date if only one is missing a lay start date
            rem <- data_subset %>% filter(is.na(data_subset$lay_start) == TRUE)
            data_to_remove <- rbind(data_to_remove, rem)
          }else if(sum(is.na(data_subset$lay_start))==2) { #if both are missing lay start dates, just remove the second one 
            rem <- data_subset %>% filter(lay_length_inc != min(lay_length_inc))
            data_to_remove <- rbind(data_to_remove, rem)
          }else { # otherwise keep the one with the shortest egg laying period; if the same length, just remove the second one
            if (data$lay_length_inc[j] == data$lay_length_inc[j+1]) {
              rem <- data_subset[j+1,]
              data_to_remove <- rbind(data_to_remove, rem)
            } else {
              rem <- data_subset %>% filter(lay_length_inc != min(lay_length_inc))
              data_to_remove <- rbind(data_to_remove, rem)
          }
        }
      }
      }
      
  j <- j + 1
  }
  
}
  i <- i + 1
  
}

# remove all the overlapping attempts from loop above (150 obs for malls when using single window of max absx , 56 for ABDU)
start_end_dates <- start_end_dates[!start_end_dates$birdid_year_nest %in% data_to_remove$birdid_year_nest, ]

start_end_dates %>% group_by(status_comb,status_recurse) %>% count()

## re-number attempts so actually represent 1, 2, 3, etc. once only attempts with non-NA dates are kept and any overlapping attempts are removed
start_end_dates <- start_end_dates[order(start_end_dates$birdid_year, start_end_dates$lay_start), ]
start_end_dates <- start_end_dates %>% group_by(birdid_year) %>%
  mutate(attempt_new = row_number())

# count number of bird-years (mallards = 626 bird-years using full range of classified dates, 636 bird-years using single window of max absx, 768 keeping all birds in; abdu = 327 (old), 405 (now keeping all birds), 366 (keeping all birds, but redid migration filtering)
n1 <- start_end_dates %>% group_by(birdid_year) %>% count()

# look at number of attempts per bird
start_end_dates %>% group_by(attempt_new) %>% count()

# make a wide version of start_end_dates
start_end_dates_wide <- start_end_dates %>%
  select(c("birdid_year", "attempt_new", "lay_start", "lay_end", "inc_start", "inc_end")) %>%
  pivot_wider(
  names_from = attempt_new, 
  values_from = c(lay_start, lay_end, inc_start, inc_end)
)

start_end_dates_wide <- start_end_dates_wide[, c("birdid_year","lay_start_1", "lay_end_1", "inc_start_1", "inc_end_1",
                                                 "lay_start_2", "lay_end_2", "inc_start_2", "inc_end_2",
                                                 "lay_start_3", "lay_end_3", "inc_start_3", "inc_end_3",
                                                 "lay_start_4", "lay_end_4", "inc_start_4", "inc_end_4"#,
                                                 #"lay_start_5", "lay_end_5", "inc_start_5", "inc_end_5" ### ADD BACK IN FOR MALLARDS (not any more!)!!!
                                                 )]

# merge with date ranges from algorithm
summary_actual.dates.incl <- merge(summary_fullrange, start_end_dates_wide, by = "birdid_year")


# make date range plots with specific lay and inc dates added

summary_actual.dates.incl[c("egg_range_start_attempt1","egg_range_end_attempt1","egg_range_start_attempt2","egg_range_end_attempt2",
                    "egg_range_start_attempt3","egg_range_end_attempt3", "egg_range_start_attempt4","egg_range_end_attempt4",
                    "egg_range_start_attempt5","egg_range_end_attempt5",
                    "egg_range_start_attempt6","egg_range_end_attempt6", "fail_inc_range_start", "fail_inc_range_end",
                    "succ_inc_range_start", "succ_inc_range_end",
                    "lay_start_1", "lay_end_1", "inc_start_1", "inc_end_1",
                    "lay_start_2", "lay_end_2", "inc_start_2", "inc_end_2",
                    "lay_start_3", "lay_end_3", "inc_start_3", "inc_end_3",
                    "lay_start_4", "lay_end_4", "inc_start_4", "inc_end_4"#,
                   # "lay_start_5", "lay_end_5", "inc_start_5", "inc_end_5"
                    )] <- lapply(summary_actual.dates.incl[c("egg_range_start_attempt1",
                                                                                                 "egg_range_end_attempt1",
                                                                                                 "egg_range_start_attempt2",
                                                                                                 "egg_range_end_attempt2",
                                                                                                 "egg_range_start_attempt3",
                                                                                                 "egg_range_end_attempt3", 
                                                             "egg_range_start_attempt4","egg_range_end_attempt4",
                                                             "egg_range_start_attempt5","egg_range_end_attempt5",
                                                             "egg_range_start_attempt6","egg_range_end_attempt6", 
                                                                                                 "fail_inc_range_start", "fail_inc_range_end",
                                                                                                 "succ_inc_range_start", "succ_inc_range_end", 
                                                                                                 "lay_start_1", "lay_end_1", "inc_start_1", "inc_end_1",
                                                                                                 "lay_start_2", "lay_end_2", "inc_start_2", "inc_end_2",
                                                                                                 "lay_start_3", "lay_end_3", "inc_start_3", "inc_end_3",
                                                                                                 "lay_start_4", "lay_end_4", "inc_start_4", "inc_end_4"#,
                                                                                               #  "lay_start_5", "lay_end_5", "inc_start_5", "inc_end_5"
                                                                                                 )], 
                                                                             as.Date)
summary_actual.dates.incl$year <- year(summary_actual.dates.incl$egg_range_end_attempt1)

years <- unique(summary_actual.dates.incl$year)
# all nesting attempts
pdf("plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncRangesAndDates_ABDU_AllAttempts_oneWindowMaxAbsX_RecurseOriginal_RF_21Nov2025.pdf", paper = "a4r", width = 10, height = 8)
for (i in 1:length(years)){
  d<- summary_actual.dates.incl %>% filter(year==years[i])
  x <- ggplot(d, aes(y=birdid_year)) +
    geom_linerange(aes(xmin=fail_inc_range_start, xmax=fail_inc_range_end+28), size=6, color = "azure3") +
    geom_linerange(aes(xmin=succ_inc_range_start, xmax=succ_inc_range_end+28), size=4, color = "plum1") +
    geom_linerange(aes(xmin=egg_range_start_attempt1, xmax=egg_range_end_attempt1+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt2, xmax=egg_range_end_attempt2+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt3, xmax=egg_range_end_attempt3+6), size=2, color = "burlywood4") +
    geom_point(aes(x=lay_start_1, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "black", fill="black", size=4) +
    geom_point(aes(x=lay_end_1, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "black" , size=4) +
    geom_point(aes(x=inc_start_1, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "black", fill="black", size=4) +
    geom_point(aes(x=inc_end_1, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "black", size=4) +
    geom_point(aes(x=lay_start_2, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkorange", fill="darkorange", size=4) +
    geom_point(aes(x=lay_end_2, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkorange", size=4) +
    geom_point(aes(x=inc_start_2, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkorange", fill="darkorange", size=4) +
    geom_point(aes(x=inc_end_2, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkorange", size=4) +
    geom_point(aes(x=lay_start_3, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkmagenta", fill="darkmagenta", size=4) +
    geom_point(aes(x=lay_end_3, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkmagenta", size=4) +
    geom_point(aes(x=inc_start_3, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkmagenta", fill="darkmagenta", size=4) +
    geom_point(aes(x=inc_end_3, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkmagenta", size=4) +
    scale_x_date(date_breaks = "20 day", date_labels = "%b %d")+
    theme(legend.title=element_blank()) +
    labs( x = "Nesting Date Ranges", y = "Birdid_Year")
  
  print(x)
}
dev.off()

# hatched nests only
summary_actual.dates.incl_hatched <- summary_actual.dates.incl %>% filter(status_incubation == "hatched")
pdf("plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncRangesAndDates_ABDU_HatchedOnly_oneWindowMaxAbsX_RecurseOriginal_RF_21Nov2025.pdf", paper = "a4r", width = 10, height = 8)
for (i in 1:length(years)){
  d<- summary_actual.dates.incl_hatched %>% filter(year==years[i])
  x <- ggplot(d, aes(y=birdid_year)) +
    geom_linerange(aes(xmin=fail_inc_range_start, xmax=fail_inc_range_end+28), size=6, color = "azure3") +
    geom_linerange(aes(xmin=succ_inc_range_start, xmax=succ_inc_range_end+28), size=4, color = "plum1") +
    geom_linerange(aes(xmin=egg_range_start_attempt1, xmax=egg_range_end_attempt1+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt2, xmax=egg_range_end_attempt2+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt3, xmax=egg_range_end_attempt3+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt4, xmax=egg_range_end_attempt4+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt5, xmax=egg_range_end_attempt5+6), size=2, color = "burlywood4") +
    geom_point(aes(x=lay_start_1, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "black", fill="black", size=4) +
    geom_point(aes(x=lay_end_1, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "black" , size=4) +
    geom_point(aes(x=inc_start_1, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "black", fill="black", size=4) +
    geom_point(aes(x=inc_end_1, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "black", size=4) +
    geom_point(aes(x=lay_start_2, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkorange", fill="darkorange", size=4) +
    geom_point(aes(x=lay_end_2, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkorange", size=4) +
    geom_point(aes(x=inc_start_2, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkorange", fill="darkorange", size=4) +
    geom_point(aes(x=inc_end_2, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkorange", size=4) +
    geom_point(aes(x=lay_start_3, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkmagenta", fill="darkmagenta", size=4) +
    geom_point(aes(x=lay_end_3, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkmagenta", size=4) +
    geom_point(aes(x=inc_start_3, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkmagenta", fill="darkmagenta", size=4) +
    geom_point(aes(x=inc_end_3, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkmagenta", size=4) +
    geom_point(aes(x=lay_start_4, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkblue", fill="darkblue", size=4) +
    geom_point(aes(x=lay_end_4, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkblue", size=4) +
    geom_point(aes(x=inc_start_4, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkblue", fill="darkblue", size=4) +
    geom_point(aes(x=inc_end_4, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkblue", size=4) +
    # geom_point(aes(x=lay_start_5, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkgreen", fill="darkgreen", size=4) +
    # geom_point(aes(x=lay_end_5, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkgreen", size=4) +
    # geom_point(aes(x=inc_start_5, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkgreen", fill="darkgreen", size=4) +
    # geom_point(aes(x=inc_end_5, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkgreen", size=4) +
    scale_x_date(date_breaks = "20 day", date_labels = "%b %d")+
    theme(legend.title=element_blank()) +
    labs( x = "Nesting Date Ranges", y = "Birdid_Year")
  
  print(x)
}
dev.off()

# failed nests only (by incubation algorithm)
summary_actual.dates.incl_failed <- summary_actual.dates.incl %>% filter(status_incubation == "failed")
pdf("plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncRangesAndDates_ABDU_FailedOnly_oneWindowMaxAbsX_RecurseOriginal_RF_21Nov2025.pdf", paper = "a4r", width = 10, height = 8)
for (i in 1:length(years)){
  d<- summary_actual.dates.incl_failed %>% filter(year==years[i])
  x <- ggplot(d, aes(y=birdid_year)) +
    geom_linerange(aes(xmin=fail_inc_range_start, xmax=fail_inc_range_end+28), size=6, color = "azure3") +
    geom_linerange(aes(xmin=succ_inc_range_start, xmax=succ_inc_range_end+28), size=4, color = "plum1") +
    geom_linerange(aes(xmin=egg_range_start_attempt1, xmax=egg_range_end_attempt1+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt2, xmax=egg_range_end_attempt2+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt3, xmax=egg_range_end_attempt3+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt4, xmax=egg_range_end_attempt4+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt5, xmax=egg_range_end_attempt5+6), size=2, color = "burlywood4") +
    geom_point(aes(x=lay_start_1, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "black", fill="black", size=4) +
    geom_point(aes(x=lay_end_1, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "black" , size=4) +
    geom_point(aes(x=inc_start_1, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "black", fill="black", size=4) +
    geom_point(aes(x=inc_end_1, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "black", size=4) +
    geom_point(aes(x=lay_start_2, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkorange", fill="darkorange", size=4) +
    geom_point(aes(x=lay_end_2, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkorange", size=4) +
    geom_point(aes(x=inc_start_2, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkorange", fill="darkorange", size=4) +
    geom_point(aes(x=inc_end_2, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkorange", size=4) +
    geom_point(aes(x=lay_start_3, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkmagenta", fill="darkmagenta", size=4) +
    geom_point(aes(x=lay_end_3, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkmagenta", size=4) +
    geom_point(aes(x=inc_start_3, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkmagenta", fill="darkmagenta", size=4) +
    geom_point(aes(x=inc_end_3, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkmagenta", size=4) +
    geom_point(aes(x=lay_start_4, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkblue", fill="darkblue", size=4) +
    geom_point(aes(x=lay_end_4, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkblue", size=4) +
    geom_point(aes(x=inc_start_4, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkblue", fill="darkblue", size=4) +
    geom_point(aes(x=inc_end_4, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkblue", size=4) +
    # geom_point(aes(x=lay_start_5, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkgreen", fill="darkgreen", size=4) +
    # geom_point(aes(x=lay_end_5, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkgreen", size=4) +
    # geom_point(aes(x=inc_start_5, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkgreen", fill="darkgreen", size=4) +
    # geom_point(aes(x=inc_end_5, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkgreen", size=4) +
    scale_x_date(date_breaks = "20 day", date_labels = "%b %d")+
    theme(legend.title=element_blank()) +
    labs( x = "Nesting Date Ranges", y = "Birdid_Year")
  
  print(x)
}
dev.off()

# defer nests only (by incubation algorithm)
summary_actual.dates.incl_defer <- summary_actual.dates.incl %>% filter(status_incubation == "defer")
pdf("plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncRangesAndDates_ABDU_DefersOnly_oneWindowMaxAbsX_RecurseOriginal_RF_21Nov2025.pdf", paper = "a4r", width = 10, height = 8)
for (i in 1:length(years)){
  d<- summary_actual.dates.incl_defer %>% filter(year==years[i])
  x <- ggplot(d, aes(y=birdid_year)) +
    geom_linerange(aes(xmin=fail_inc_range_start, xmax=fail_inc_range_end+28), size=6, color = "azure3") +
    geom_linerange(aes(xmin=succ_inc_range_start, xmax=succ_inc_range_end+28), size=4, color = "plum1") +
    geom_linerange(aes(xmin=egg_range_start_attempt1, xmax=egg_range_end_attempt1+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt2, xmax=egg_range_end_attempt2+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt3, xmax=egg_range_end_attempt3+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt4, xmax=egg_range_end_attempt4+6), size=2, color = "burlywood4") +
    geom_linerange(aes(xmin=egg_range_start_attempt5, xmax=egg_range_end_attempt5+6), size=2, color = "burlywood4") +
    geom_point(aes(x=lay_start_1, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "black", fill="black", size=4) +
    geom_point(aes(x=lay_end_1, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "black" , size=4) +
    geom_point(aes(x=inc_start_1, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "black", fill="black", size=4) +
    geom_point(aes(x=inc_end_1, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "black", size=4) +
    geom_point(aes(x=lay_start_2, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkorange", fill="darkorange", size=4) +
    geom_point(aes(x=lay_end_2, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkorange", size=4) +
    geom_point(aes(x=inc_start_2, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkorange", fill="darkorange", size=4) +
    geom_point(aes(x=inc_end_2, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkorange", size=4) +
    geom_point(aes(x=lay_start_3, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkmagenta", fill="darkmagenta", size=4) +
    geom_point(aes(x=lay_end_3, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkmagenta", size=4) +
    geom_point(aes(x=inc_start_3, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkmagenta", fill="darkmagenta", size=4) +
    geom_point(aes(x=inc_end_3, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkmagenta", size=4) +
    geom_point(aes(x=lay_start_4, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkblue", fill="darkblue", size=4) +
    geom_point(aes(x=lay_end_4, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkblue", size=4) +
    geom_point(aes(x=inc_start_4, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkblue", fill="darkblue", size=4) +
    geom_point(aes(x=inc_end_4, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkblue", size=4) +
    # geom_point(aes(x=lay_start_5, color = "recurse+ruleset /n start/end dates"), shape = 21, color = "darkgreen", fill="darkgreen", size=4) +
    # geom_point(aes(x=lay_end_5, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkgreen", size=4) +
    # geom_point(aes(x=inc_start_5, color = "recurse+ruleset /n start/end dates"), shape = 24, color = "darkgreen", fill="darkgreen", size=4) +
    # geom_point(aes(x=inc_end_5, color = "recurse+ruleset /n start/end dates"), shape = 4, color = "darkgreen", size=4) +
    scale_x_date(date_breaks = "20 day", date_labels = "%b %d")+
    theme(legend.title=element_blank()) +
    labs( x = "Nesting Date Ranges", y = "Birdid_Year")
  
  print(x)
}
dev.off()

# merge with nest locations
nest_dates_and_locations <- merge(nest_locations_recursive_trans, start_end_dates, by = "birdid_year_nest")

n_dates_and_locations <- unique(nest_dates_and_locations$birdid_year)
nest_locations_recursive_trans <- nest_locations_recursive_trans %>%
  mutate(birdid_year=substr(birdid_year_nest, 1, 15))
n_locations <- unique(nest_locations_recursive_trans$birdid_year)

# # save nest visit to database when not removing missing date birds (need for assigning dates for FAC model!)
#dbRemoveTable(conn, "NestVisitInfo")
dbWriteTable(conn, "NestVisitInfo_30June26", nest_locations_recursive_trans, append = TRUE, row.names = FALSE)


# # saving old classified data when I filtered out birds without data to July 1
# temp <- tbl(conn, "MALL2022to2025_StartEndDates_IncubationEggLaying_NestLocations") %>%
#   collect()
# dbWriteTable(conn, "MALL2022to2025_StartEndDates_IncubationLaying_NestLocations_old", temp, append = TRUE, row.names = FALSE)

# save start_end dates of incubation to use for applying brood algorithm
write.csv(nest_dates_and_locations, "results/MALL_2022to2025_StartEndDatesOfIncubationLayingPlusNestLocation_AllBirdsIncluded_30Jun2026.csv")
#dbRemoveTable(conn, "MALL2022to2025_StartEndDates_IncubationEggLaying_NestLocations")
dbWriteTable(conn, "MALL2022to2025_StartEndDates_IncubationEgg_NestLocation_30Jun26", nest_dates_and_locations, append = TRUE, row.names = FALSE)


### summarizing data for manuscript ####
### data summary --- now use 5b instead ####
# # load start_end dates and add bird age
# setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics")
# dates_abdu <- read.csv("Compiled Datasets for Machine Learning/ABDU_2021to2025_StartEndDatesOfIncubationLayingPlusNestLocation_AllBirdsIncluded_MigrationFilteringUpdated_7May2026.csv")
# dates_mall <- read.csv("Compiled Datasets for Machine Learning/MALL_2022to2025_StartEndDatesOfIncubationLayingPlusNestLocation_AllBirdsIncluded_15Apr2026.csv")
# 
# dates_abdu$species <- "ABDU"
# dates_mall$species <- "MALL"
# 
# dates <- rbind(dates_abdu, dates_mall)
# 
# 
# # load deployment data
# library(readxl)
# deploy_abdu <- read_excel("Deployment Files/deployments_18Sept2025.xlsx", sheet = "ABDU") ## change to MALL / ABDU depending what you need!
# deploy_mall <- read_excel("Deployment Files/deployments_18Sept2025.xlsx", sheet = "MALL") ## change to MALL / ABDU depending what you need!
# 
# 
# # reduce deploy data to one device per year (latest deployment) and format data
# deploy_abdu$`Banding Date` <- as.Date(deploy_abdu$`Banding Date`)
# deploy_abdu <- deploy_abdu %>% 
#   mutate(year=year(`Banding Date`)) %>% 
#   group_by(year, `Transmitter Number`) %>%
#   filter(`Banding Date` == max(`Banding Date`)) %>% 
#   select(c("Transmitter Number", "Band Number", "year", "Age", "Species")) %>%
#   rename(device_id=`Transmitter Number`,
#          band=`Band Number`,
#          age=Age,
#          species=Species)
# 
# deploy_mall$`Banding Date` <- as.Date(deploy_mall$`Banding Date`)
# deploy_mall <- deploy_mall %>% 
#   mutate(year=year(`Banding Date`)) %>% 
#   group_by(year, `Transmitter Number`) %>%
#   filter(`Banding Date` == max(`Banding Date`)) %>% 
#   select(c("Transmitter Number", "Band Number", "year", "Age", "Species")) %>%
#   rename(device_id=`Transmitter Number`,
#          band=`Band Number`,
#          age=Age,
#          species=Species)
# 
# deploy2 <- rbind(deploy_abdu, deploy_mall)
# 
# # merge by device (switched to band for MALL), year and KEEP ALL ROWS FROM DATES DF!
# dates$band <- substr(dates$birdid_year_nest, start = 1, stop = 10)
# dates$year <- substr(dates$birdid_year_nest, start = 12, stop = 15)
# dates$attempt <- substr(dates$birdid_year_nest, start = 17, stop = 17)
# 
# dates2 <- merge(dates, deploy2, by = c("band", "year", "species"), all.x = TRUE) %>%
#   select(c("device_id", "band", "year","species", "attempt_new", "age", "status_comb", "lay_start", "lay_end", "inc_start", "inc_end" ))
# 
# # make any age that is NA equal to ASY (NAs are because it is the second year of data from a bird, so year no longer matches banding year and device id - regardless if it was an SY or ASY at banding, it will be (or still be) an adult the next year. Could make this more precise and make ATY if ASY in previous year, but not necessary at this point... )
# dates2$age[is.na(dates2$age)] <- "ASY"
# 
# # summarize laying dates, number of attempts, nesting outcomes by age
# dates2[c("lay_start", "lay_end", "inc_start", "inc_end" )] <- lapply(dates2[c("lay_start", "lay_end", "inc_start", "inc_end" )], 
#                                                                                                as.Date)
# sum_lay_dates_by_age <- dates2 %>% filter(is.na(lay_start) == FALSE) %>%
#   mutate(lay_start=as.numeric(format(lay_start, "%j"))) %>%
#   group_by(species, age) %>%
#   summarise(n =n(),
#             mean_lay_start=mean(lay_start),
#             median_lay_start=median(lay_start),
#             min_lay_start=min(lay_start),
#             max_lay_start=max(lay_start),
#             sd_lay_start=sd(lay_start))
# 
# sum_lay_dates_by_year <- dates2 %>% filter(is.na(lay_start) == FALSE) %>%
#   mutate(lay_start=as.numeric(format(lay_start, "%j"))) %>%
#   group_by(species, year) %>%
#   summarise(n =n(),
#             mean_lay_start=mean(lay_start),
#             median_lay_start=median(lay_start),
#             min_lay_start=min(lay_start),
#             max_lay_start=max(lay_start),
#             sd_lay_start=sd(lay_start))
# 
# dates2$lay_start_jul <- as.numeric(format(dates2$lay_start, "%j"))
# 
# # png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_LayStartByAge.png")
# # boxplot(lay_start_jul ~ age, data = dates2, ylab = "Egg Laying Start Date (julian)", xlab = "Age")
# # dev.off()
# # 
# # png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_LayStartByYear.png")
# # boxplot(lay_start_jul ~ year, data = dates2, ylab = "Egg Laying Start Date (julian)", xlab = "Year")
# # dev.off()
# 
# sum_inc_dates_by_age <- dates2 %>% filter(is.na(inc_start) == FALSE) %>%
#   mutate(inc_start=as.numeric(format(inc_start, "%j"))) %>%
#   group_by(species, age) %>%
#   summarise(n =n(),
#             mean_inc_start=mean(inc_start),
#             median_inc_start=median(inc_start),
#             min_inc_start=min(inc_start),
#             max_inc_start=max(inc_start),
#             sd_inc_start=sd(inc_start))
# 
# sum_inc_dates_by_year <- dates2 %>% filter(is.na(inc_start) == FALSE) %>%
#   mutate(inc_start=as.numeric(format(inc_start, "%j"))) %>%
#   group_by(species, year) %>%
#   summarise(n =n(),
#             mean_inc_start=mean(inc_start),
#             median_inc_start=median(inc_start),
#             min_inc_start=min(inc_start),
#             max_inc_start=max(inc_start),
#             sd_inc_start=sd(inc_start))
# 
# dates2$inc_start_jul <- as.numeric(format(dates2$inc_start, "%j"))
# 
# # png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_IncStartByAge.png")
# # boxplot(inc_start_jul ~ age, data = dates2, ylab = "Incubation Start Date (julian)", xlab = "Age")
# # dev.off()
# 
# # png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_IncStartByYear.png")
# # boxplot(inc_start_jul ~ year, data = dates2, ylab = "Incubation Start Date (julian)", xlab = "Year")
# # dev.off()
# 
# ## re-making summaries of laying and incubation, using FIRST ATTEMPTS ONLY
# dates3 <- dates2 %>% filter(attempt_new == "1")
# 
# # overall summary
# sum_lay_dates <- dates3 %>% filter(is.na(lay_start) == FALSE) %>%
#   mutate(lay_start=as.numeric(format(lay_start, "%j"))) %>%
#   group_by(species) %>%
#   summarise(n =n(),
#             mean_lay_start=mean(lay_start),
#             median_lay_start=median(lay_start),
#             min_lay_start=min(lay_start),
#             max_lay_start=max(lay_start),
#             sd_lay_start=sd(lay_start))
# 
# # by age
# 
# sum_lay_dates_by_age <- dates3 %>% filter(is.na(lay_start) == FALSE) %>%
#   mutate(lay_start=as.numeric(format(lay_start, "%j"))) %>%
#   group_by(species, age) %>%
#   summarise(n =n(),
#             mean_lay_start=mean(lay_start),
#             median_lay_start=median(lay_start),
#             min_lay_start=min(lay_start),
#             max_lay_start=max(lay_start),
#             sd_lay_start=sd(lay_start))
# 
# sum_lay_dates_by_year <- dates3 %>% filter(is.na(lay_start) == FALSE) %>%
#   mutate(lay_start=as.numeric(format(lay_start, "%j"))) %>%
#   group_by(species, year) %>%
#   summarise(n =n(),
#             mean_lay_start=mean(lay_start),
#             median_lay_start=median(lay_start),
#             min_lay_start=min(lay_start),
#             max_lay_start=max(lay_start),
#             sd_lay_start=sd(lay_start))
# 
# dates3$lay_start_jul <- as.numeric(format(dates3$lay_start, "%j"))
# 
# # png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_LayStartByAge_1stAttemptsOnly.png")
# # boxplot(lay_start_jul ~ age, data = dates3, ylab = "Egg Laying Start Date (julian)", xlab = "Age")
# # dev.off()
# # 
# # png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_LayStartByAge_1stAttemptsOnly_violin.png")
# # ggplot(dates3, aes(x=age, y=lay_start_jul, fill=age)) + # fill=name allow to automatically dedicate a color for each group
# #   geom_violin()
# # dev.off()
# # 
# # png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_LayStartByYear_1stAttemptsOnly.png")
# # boxplot(lay_start_jul ~ year, data = dates3, ylab = "Egg Laying Start Date (julian)", xlab = "Year")
# # dev.off()
# 
# # incubation start overall 
# sum_inc_dates <- dates3 %>% filter(is.na(inc_start) == FALSE) %>%
#   mutate(inc_start=as.numeric(format(inc_start, "%j"))) %>%
#   group_by(species) %>%
#   summarise(n =n(),
#             mean_inc_start=mean(inc_start),
#             median_inc_start=median(inc_start),
#             min_inc_start=min(inc_start),
#             max_inc_start=max(inc_start),
#             sd_inc_start=sd(inc_start))
# 
# # inc start by age
# 
# sum_inc_dates_by_age <- dates3 %>% filter(is.na(inc_start) == FALSE) %>%
#   mutate(inc_start=as.numeric(format(inc_start, "%j"))) %>%
#   group_by(species, age) %>%
#   summarise(n =n(),
#             mean_inc_start=mean(inc_start),
#             median_inc_start=median(inc_start),
#             min_inc_start=min(inc_start),
#             max_inc_start=max(inc_start),
#             sd_inc_start=sd(inc_start))
# 
# # inc start by year
# 
# sum_inc_dates_by_year <- dates3 %>% filter(is.na(inc_start) == FALSE) %>%
#   mutate(inc_start=as.numeric(format(inc_start, "%j"))) %>%
#   group_by(species, year) %>%
#   summarise(n =n(),
#             mean_inc_start=mean(inc_start),
#             median_inc_start=median(inc_start),
#             min_inc_start=min(inc_start),
#             max_inc_start=max(inc_start),
#             sd_inc_start=sd(inc_start))
# 
# dates3$inc_start_jul <- as.numeric(format(dates3$inc_start, "%j"))
# 
# # png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_IncStartByAge_1stAttemptsOnly.png")
# # boxplot(inc_start_jul ~ age, data = dates3, ylab = "Incubation Start Date (julian)", xlab = "Age")
# # dev.off()
# # 
# # png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_IncStartByYear_1stAttemptsOnly.png")
# # boxplot(inc_start_jul ~ year, data = dates3, ylab = "Incubation Start Date (julian)", xlab = "Year")
# # dev.off()
# 
# ## density plots of dates by age
# library(ggpubr)
# library(viridis)
# n_age <- viridis(3)
# 
# tiff("plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncDatesByAge_DensityPlots_31Mar2026.tiff", units="in", width=8, height=5, res=300)
# #png("./plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncDatesByAge_DensityPlots.png",width = 800,height = 500,res = 100)
# a<-dates3 %>%
#   filter(species == "MALL") %>%
#   mutate(
#     lay_start_fake = as.Date(format(lay_start, "2000-%m-%d")),
#     inc_start_fake = as.Date(format(inc_start, "2000-%m-%d")),
#     age=as.factor(age)
#   ) %>%
#   ggplot() +
#   geom_density(aes(x = lay_start_fake, color = factor(age, levels = c("SY", "ASY")), linetype = "start of laying"), size = 1) + 
#   geom_density(aes(x = inc_start_fake, color = factor(age, levels = c("SY", "ASY")), linetype = "start of incubation"), size = 1) + 
#   scale_linetype_manual(values = c("start of laying" = "dashed", "start of incubation" = "solid")) +
#   guides(linetype = guide_legend(reverse = TRUE)) +
#   scale_color_manual(values = c("SY" = n_age[1],
#                                "ASY" = n_age[2]))+
#   scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
#  # ggtitle("MALL") +
#   theme_classic() + 
#   theme(legend.position = "bottom") +
#   labs(
#     color = "Age",
#     linetype = "Date of Nesting",
#     x = "Date",
#     y = "Density"
#   ) + theme(plot.margin = margin(1,0,0,0, 'cm'))
# b<-dates3 %>%
#   filter(species == "ABDU") %>%
#   mutate(
#     lay_start_fake = as.Date(format(lay_start, "2000-%m-%d")),
#     inc_start_fake = as.Date(format(inc_start, "2000-%m-%d")),
#     age=as.factor(age)
#   ) %>%
#   ggplot() +
#   geom_density(aes(x = lay_start_fake, color = factor(age, levels = c("SY", "ASY")), linetype = "start of laying"), size = 1) + 
#   geom_density(aes(x = inc_start_fake, color = factor(age, levels = c("SY", "ASY")), linetype = "start of incubation"), size = 1) + 
#   scale_linetype_manual(values = c( "start of laying" = "dashed", "start of incubation" = "solid")) +
#   guides(linetype = guide_legend(reverse = TRUE)) +
#   scale_color_manual(values = c("SY" = n_age[1],
#                                 "ASY" = n_age[2]))+
#   scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
#  # ggtitle("ABDU") +
#   theme_classic() + 
#   theme(legend.position = "bottom") +
#   labs(
#     color = "Age",
#     linetype = "Date of Nesting",
#     x = "Date",
#     y = "Density"
#   ) + theme(plot.margin = margin(1,0,0,0, 'cm'))
# ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
#           labels = c("ABDU", "MALL"))
# dev.off()
# rm(a,b)
# 
# ## density plots of dates by year
# library(ggpubr)
# library(stringr)
# library(viridis)
# n_years <- viridis(5)
# "#440154FF" "#3B528BFF" "#21908CFF" "#5DC863FF" "#FDE725FF"
# my_colors <- c("2021" = n_years[1],
#                "2022" = n_years[2],
#                "2023" = n_years[3],
#                "2024" = n_years[4],
#                "2025" = n_years[5])
# dates3$year <- as.factor(dates3$year)
# tiff("plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncDatesByYear_DensityPlots_31Mar2026.tiff", units="in", width=8, height=5, res=300)
# #png("./plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncDatesByYear_DensityPlots.png",width = 800,height = 500,res = 100)
# a<-dates3 %>%
#   filter(species == "MALL") %>%
#  mutate(
#     lay_start_fake = as.Date(format(lay_start, "2000-%m-%d")),
#     inc_start_fake = as.Date(format(inc_start, "2000-%m-%d"))#,
#     #year=as.factor(year)
#   ) %>%
#   ggplot() +
#   geom_density(aes(x = lay_start_fake, 
#                    color = year,
#                    # color = factor(year, levels = c("2021", "2022", "2023", "2024", "2025")), 
#                    linetype = "start of laying"), size = 1) + 
#   geom_density(aes(x = inc_start_fake, 
#                    color = year,
#                    # color = factor(year, levels = c("2021", "2022", "2023", "2024", "2025")), 
#                    linetype = "start of incubation"), size = 1) + 
#   scale_linetype_manual(values = c("start of incubation" = "solid", "start of laying" = "dashed")) +
#   guides(linetype = guide_legend(reverse = TRUE)) +
#   scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
#   scale_color_manual(values = my_colors,
#                      limits = names(my_colors),
#                      drop=FALSE
#                    ) +
#  # ggtitle("MALL") +
#   theme_classic() + 
#   theme(legend.position = "bottom") +
#   labs(
#     color = "Year:",
#     linetype = "Date of Nesting:",
#     x = "Date",
#     y = "Density"
#   ) + theme(plot.margin = margin(1,0,0,0, 'cm'))
# b<-dates3 %>%
#   filter(species == "ABDU") %>%
#   mutate(
#     lay_start_fake = as.Date(format(lay_start, "2000-%m-%d")),
#     inc_start_fake = as.Date(format(inc_start, "2000-%m-%d"))#,
#     #year=as.factor(year)
#   ) %>%
#   ggplot() +
#   geom_density(aes(x = lay_start_fake, 
#                    color = year,
#                   # color = factor(year, levels = c("2021", "2022", "2023", "2024", "2025")), 
#                    linetype = "start of laying"), size = 1) + 
#   geom_density(aes(x = inc_start_fake, 
#                    color = year,
#                   # color = factor(year, levels = c("2021", "2022", "2023", "2024", "2025")), 
#                    linetype = "start of incubation"), size = 1) + 
#   scale_linetype_manual(values = c("start of incubation" = "solid", "start of laying" = "dashed")) +
#   guides(linetype = guide_legend(reverse = TRUE)) +
#   scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
#   scale_color_manual(name='Year',
#       values = my_colors, 
#       limits = names(my_colors),
#                     drop=FALSE
#       ) +
#  # ggtitle("ABDU") +
#   theme_classic() + 
#   theme(legend.position = "bottom") +
#   labs(
#     color = "Year:",
#     linetype = "Date of Nesting:",
#     x = "Date",
#     y = "Density"
#   ) + theme(plot.margin = margin(1,0,0,0, 'cm'))
# ggarrange(b, a, ncol=1, nrow=2, common.legend = TRUE, legend = "top",
#           labels = c("ABDU", "MALL"))
# dev.off()
# rm(b, a)
# 
# ## density plots of dates by age AND year for manuscript
# library(ggpubr)
# library(viridis)
# n_age <- viridis(3)
# 
# tiff("plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncDatesByAgeAndYear_DensityPlots_14May2026.tiff", units="in", width=20, height=8, res=300)
# #png("./plots_EggLayingAndIncubationCombined_14Sept2025/LayAndIncDatesByAge_DensityPlots.png",width = 800,height = 500,res = 100)
# a<-dates3 %>%
#   filter(species == "MALL") %>%
#   mutate(
#     lay_start_fake = as.Date(format(lay_start, "2000-%m-%d")),
#     inc_start_fake = as.Date(format(inc_start, "2000-%m-%d")),
#     age=as.factor(age)
#   ) %>%
#   ggplot() +
#   geom_density(aes(x = lay_start_fake, color = factor(age, levels = c("SY", "ASY")), linetype = "laying"), size = 1) + 
#   geom_density(aes(x = inc_start_fake, color = factor(age, levels = c("SY", "ASY")), linetype = "incubation"), size = 1) + 
#   scale_linetype_manual(values = c("laying" = "dashed", "incubation" = "solid")) +
#   guides(linetype = guide_legend(reverse = TRUE)) +
#   scale_color_manual(values = c("SY" = n_age[1],
#                                 "ASY" = n_age[2]))+
#   scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
#   # ggtitle("MALL") +
#   theme_classic() + 
#   theme(legend.position = "bottom") +
#   labs(
#     color = "Age",
#     linetype = "Start Date",
#     x = "Date",
#     y = "Density"
#   ) + theme(plot.margin = margin(1,0.1,0,0.1, 'cm'),
#             # Set size for both axis titles 
#             axis.title = element_text(size = 18),
#             
#             # Set size for both axis text/labels 
#             axis.text = element_text(size = 14),
#             legend.text = element_text(size = 14),     # Size for labels
#             legend.title = element_text(size = 17)      # Size for title
#             )
# b<-dates3 %>%
#   filter(species == "ABDU") %>%
#   mutate(
#     lay_start_fake = as.Date(format(lay_start, "2000-%m-%d")),
#     inc_start_fake = as.Date(format(inc_start, "2000-%m-%d")),
#     age=as.factor(age)
#   ) %>%
#   ggplot() +
#   geom_density(aes(x = lay_start_fake, color = factor(age, levels = c("SY", "ASY")), linetype = "laying"), size = 1) + 
#   geom_density(aes(x = inc_start_fake, color = factor(age, levels = c("SY", "ASY")), linetype = "incubation"), size = 1) + 
#   scale_linetype_manual(values = c( "laying" = "dashed", "incubation" = "solid")) +
#   guides(linetype = guide_legend(reverse = TRUE)) +
#   scale_color_manual(values = c("SY" = n_age[1],
#                                 "ASY" = n_age[2]))+
#   scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
#   # ggtitle("ABDU") +
#   theme_classic() + 
#   theme(legend.position = "bottom") +
#   labs(
#     color = "Age",
#     linetype = "Start Date",
#     x = "Date",
#     y = "Density"
#   ) + theme(plot.margin = margin(1,0.1,0,0.1, 'cm'), 
#             # Set size for both axis titles 
#             axis.title = element_text(size = 18),
#             
#             # Set size for both axis text/labels 
#             axis.text = element_text(size = 14),
#             legend.text = element_text(size = 14),     # Size for labels
#             legend.title = element_text(size = 17)      # Size for title
#             )
# c <- ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
#           labels = c("ABDU", "MALL"),
#           font.label = list(size = 18, color = "black", face = "bold"))
# 
# # by year colours
# n_years <- viridis(5)
# my_colors <- c("2021" = n_years[1],
#                "2022" = n_years[2],
#                "2023" = n_years[3],
#                "2024" = n_years[4],
#                "2025" = n_years[5])
# dates3$year <- as.factor(dates3$year)
# 
# a<-dates3 %>%
#   filter(species == "MALL") %>%
#   mutate(
#     lay_start_fake = as.Date(format(lay_start, "2000-%m-%d")),
#     inc_start_fake = as.Date(format(inc_start, "2000-%m-%d"))#,
#     #year=as.factor(year)
#   ) %>%
#   ggplot() +
#   geom_density(aes(x = lay_start_fake, 
#                    color = year,
#                    # color = factor(year, levels = c("2021", "2022", "2023", "2024", "2025")), 
#                    linetype = "laying"), size = 1) + 
#   geom_density(aes(x = inc_start_fake, 
#                    color = year,
#                    # color = factor(year, levels = c("2021", "2022", "2023", "2024", "2025")), 
#                    linetype = "incubation"), size = 1) + 
#   scale_linetype_manual(values = c("incubation" = "solid", "laying" = "dashed")) +
#   guides(linetype = guide_legend(reverse = TRUE)) +
#   scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
#   scale_color_manual(values = my_colors,
#                      limits = names(my_colors),
#                      drop=FALSE
#   ) +
#   # ggtitle("MALL") +
#   theme_classic() + 
#   theme(legend.position = "bottom") +
#   labs(
#     color = "Year:",
#     linetype = "Start Date:",
#     x = "Date",
#     y = "Density"
#   ) + theme(plot.margin = margin(1,0.5,0,0, 'cm'),
#             # Set size for both axis titles
#             axis.title = element_text(size = 18),
#             
#             # Set size for both axis text/labels 
#             axis.text = element_text(size = 14),
#             legend.text = element_text(size = 14),     # Size for labels
#             legend.title = element_text(size = 17)      # Size for title
#             )
# b<-dates3 %>%
#   filter(species == "ABDU") %>%
#   mutate(
#     lay_start_fake = as.Date(format(lay_start, "2000-%m-%d")),
#     inc_start_fake = as.Date(format(inc_start, "2000-%m-%d"))#,
#     #year=as.factor(year)
#   ) %>%
#   ggplot() +
#   geom_density(aes(x = lay_start_fake, 
#                    color = year,
#                    # color = factor(year, levels = c("2021", "2022", "2023", "2024", "2025")), 
#                    linetype = "laying"), size = 1) + 
#   geom_density(aes(x = inc_start_fake, 
#                    color = year,
#                    # color = factor(year, levels = c("2021", "2022", "2023", "2024", "2025")), 
#                    linetype = "incubation"), size = 1) + 
#   scale_linetype_manual(values = c("incubation" = "solid", "laying" = "dashed")) +
#   guides(linetype = guide_legend(reverse = TRUE)) +
#   scale_x_date(date_labels = "%d %b", date_breaks = "1 month") +
#   scale_color_manual(name='Year',
#                      values = my_colors, 
#                      limits = names(my_colors),
#                      drop=FALSE
#   ) +
#   # ggtitle("ABDU") +
#   theme_classic() + 
#   theme(legend.position = "bottom") +
#   labs(
#     color = "Year:",
#     linetype = "Start Date:",
#     x = "Date",
#     y = "Density"
#   ) + theme(plot.margin = margin(1,0.5,0,0, 'cm'),
#             # Set size for both axis titles
#             axis.title = element_text(size = 18),
#             
#             # Set size for both axis text/labels 
#             axis.text = element_text(size = 14),
#             legend.text = element_text(size = 14),     # Size for labels
#             legend.title = element_text(size = 17)  )    # Size for title
# d <- ggarrange(b, a, ncol=1, nrow=2, common.legend = TRUE, legend = "top",
#           labels = c("ABDU", "MALL"),
#           font.label = list(size = 18, color = "black", face = "bold"))
# 
# ggarrange(c, d, ncol=2, nrow=1, common.legend = FALSE)
# dev.off()
# rm(b, a, c, d)
# 
# 
# 
# # number of attempts
# 
# sum_attempts <- dates2 %>% group_by(band, year) %>%
#   filter(attempt_new== max(attempt_new)) %>% ungroup() %>% # reducing each bird-year to only one row per attempt and having that one row = the max number of attempts for that year
#   mutate(attempt=as.numeric(attempt_new)) %>%
#   group_by(species) %>%
#   summarise(n =n(),
#             mean_attempt=mean(attempt),
#             median_attempt=median(attempt),
#             min_attempt=min(attempt),
#             max_attempt=max(attempt),
#             sd_attempt=sd(attempt))
# 
# sum_attempts_by_age <- dates2 %>% group_by(band, year) %>%
#   filter(attempt_new== max(attempt_new)) %>% ungroup() %>% # reducing each bird-year to only one row per attempt and having that one row = the max number of attempts for that year
#   mutate(attempt=as.numeric(attempt_new)) %>%
#   group_by(species, age) %>%
#   summarise(n =n(),
#             mean_attempt=mean(attempt),
#             median_attempt=median(attempt),
#             min_attempt=min(attempt),
#             max_attempt=max(attempt),
#             sd_attempt=sd(attempt))
# 
# sum_attempts_by_year <- dates2 %>% group_by(band, year) %>%
#   filter(attempt_new== max(attempt_new)) %>% ungroup() %>% # reducing each bird-year to only one row per attempt and having that one row = the max number of attempts for that year
#   mutate(attempt=as.numeric(attempt_new)) %>%
#   group_by(species, year) %>%
#   summarise(n =n(),
#             mean_attempt=mean(attempt),
#             median_attempt=median(attempt),
#             min_attempt=min(attempt),
#             max_attempt=max(attempt),
#             sd_attempt=sd(attempt))
# 
# attempts <- dates2 %>% group_by(band, year) %>%
#   filter(attempt_new== max(attempt_new)) %>% ungroup() %>%
#   mutate(attempt=as.numeric(attempt_new))
# 
# png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_NumberOfAttemptsByAge.png")
# boxplot(attempt ~ age, data = attempts, ylab = "Number of Attempts", xlab = "Age")
# dev.off()
# 
# png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_NumberOfAttemptsByYear.png")
# boxplot(attempt ~ year, data = attempts, ylab = "Number of Attempts", xlab = "Year")
# dev.off()

### continue here for status summaries ####
setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics")


# re-do outcomes with all birds included (even those that did not have any days classified as egg laying, i.e. defer-defers)
# merge by device, year and KEEP ALL ROWS FROM DATES DF!
summary_abdu$species <- "ABDU"
summary_mall$species <- "MALL"

summary <- rbind(summary_abdu, summary_mall)

write.csv(summary, "plots_EggLayingAndIncubationCombined_14Sept2025/DataForStatusBoxPlotsByAgeAndYear_31Mar2026.csv")

#### to add updated ABDU/MALL data to old MALL/ABDU ####
# load old summary
summary <- read.csv("plots_EggLayingAndIncubationCombined_14Sept2025/DataForStatusBoxPlotsByAgeAndYear_9Jun2026.csv")

# remove abdu
summary_mall_only <- summary %>% filter(species == "MALL") %>% select(-X)

# or remove mall
summary_abdu_only <- summary %>% filter(species == "ABDU") %>% select(-X)

# add new abdu
summary <- rbind(summary_mall_only, summary_abdu)

# add new mall
summary <- rbind(summary_abdu_only, summary_mall)

# save combined summary
write.csv(summary, "plots_EggLayingAndIncubationCombined_14Sept2025/DataForStatusBoxPlotsByAgeAndYear_18Jun2026.csv")


#### start here if you've already saved the status data ####
summary <- read.csv("plots_EggLayingAndIncubationCombined_14Sept2025/DataForStatusBoxPlotsByAgeAndYear_18Jun2026.csv")

summary %>% group_by(species, status_comb) %>% count()

summary$band <- substr(summary$birdid_year, start = 1, stop = 10)
summary$year <- substr(summary$birdid_year, start = 12, stop = 15)
summary2 <- summary %>% ungroup() %>% select(c("band", "year", "species", "status_comb"))

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


summary3 <- merge(summary2, deploy2, by = c("band", "year", "species"), all.x = TRUE) %>%
  select(c("device_id", "band", "species", "year", "age", "status_comb"))

# make any age that is NA equal to ASY (NAs are because it is the second year of data from a bird, so year no longer matches banding year and device id - regardless if it was an SY or ASY at banding, it will be (or still be) an adult the next year. Could make this more precise and make ATY if ASY in previous year, but not necessary at this point... )
summary3$age[is.na(summary3$age)] <- "ASY"

#### SIDE STEP FOR GETTING PERCENT OF BIRDS THAT DIED FOR NESTED BIRDS WITH LESS THAN 26-DAYS OF DATA (don't normally do!) ####
# reduce summary3 to only birds that weren't classified by FTI model (i.e., birds without 26-days of complete data)
summary4 <- summary3 %>% filter(status_comb == "egg_laying_NA") %>%
mutate(birdid_year= paste(band, year, sep="_"))

## first do mallards
summary4_mall <- summary4 %>% filter(species == "MALL")

# add max day 
#### 1. Summarize daily metrics over 28-day window for input to algorithm ####
# connect to database
conn <- dbConnect(
  Postgres(),
  dbname = "EMALL",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "blackducks"
)

# load unclassified data (ACC)
breeding.metrics <- tbl(conn, "daily_odba_absx_propfly") %>%
  collect()

# check data
colnames(breeding.metrics)

# remove ACC daily summary metrics calculate using <25% of daily fixes
breeding.metrics <- breeding.metrics %>% filter(prop_fixes >= 0.25)

# format data (add bird_year columns, format date as a date, select columns)
breeding.metrics2 <- breeding.metrics %>% 
  mutate(birdid_year=(paste(birdid, year, sep="_")),
         date=as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::select(c("birdid_year", "date", "birdid", "year", "device_id", "mean_ODBA", "mean_abs_x"
  ))

# load unclassified data (GPS)
breeding.metrics <- tbl(conn, "daily_ddist_nsd_mcp") %>%
  collect()

# check data
colnames(breeding.metrics)

# format data (add bird_year columns, format date as a date, select columns)
breeding.metrics <- breeding.metrics %>% 
  mutate(birdid_year=(paste(birdid, year, sep="_")),
         date=as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::select(c("birdid_year", "date", "birdid", "year", "median_ddist"
  ))

colnames(breeding.metrics)
summary(breeding.metrics)

colnames(breeding.metrics2)
summary(breeding.metrics2)

# combine gps and acc data
breeding.metrics2 <- merge(breeding.metrics2, breeding.metrics, by = c("birdid_year", "date", "birdid", "year"))

colnames(breeding.metrics2)
summary(breeding.metrics2)

n2 <- unique(breeding.metrics2$birdid_year) # 1230 bird-years

rm(breeding.metrics)

# only apply incubation algorithm to July 31 ( ABDU and MALL) 
filter_2022 <- breeding.metrics2 %>% filter(year == "2022") %>% 
  filter(date <= "2022-07-31")
filter_2023 <- breeding.metrics2 %>% filter(year == "2023") %>% 
  filter(date <= "2023-07-31")
filter_2024 <- breeding.metrics2 %>% filter(year == "2024") %>% 
  filter(date <= "2024-07-31")
filter_2025 <- breeding.metrics2 %>% filter(year == "2025") %>% 
  filter(date <= "2025-07-31")

breeding.metrics2 <- rbind(filter_2022, filter_2023, filter_2024, filter_2025)

breeding.metrics2 <- breeding.metrics2[order(breeding.metrics2$birdid_year, breeding.metrics2$date), ]

# summarize max day by bird-year
max_day_mall <- breeding.metrics2 %>%
  group_by(birdid_year) %>%
  summarize(max_day=max(date))

# merge with summary4
summary4_mall <- merge(summary4_mall, max_day_mall, by = "birdid_year", all.x = TRUE)

## add mortality day
# load deployment data
library(readxl)
deploy_abdu <- read_excel("Deployment Files/deployments_18Sept2025.xlsx", sheet = "ABDU") ## change to MALL / ABDU depending what you need!
deploy_mall <- read_excel("Deployment Files/deployments_18Sept2025.xlsx", sheet = "MALL") ## change to MALL / ABDU depending what you need!


# get mortality date and format data
deploy_mall$mort_date<- as.Date(deploy_mall$`Mortality Date and Time`)
deploy_mall <- deploy_mall %>% 
  mutate(year=year(mort_date))  %>% 
  rename(band=`Band Number`) %>%
  mutate(birdid_year=paste(band, year, sep="_")) %>%
  select(c("birdid_year","mort_date")) 

# add mort date to summary
summary4_mall <- merge(summary4_mall, deploy_mall, by = c("birdid_year"), all.x=TRUE)

# add column that = TRUE if mortality day = max day +/- one day ( >= max day -1 day && <= max day + 1 day )
summary4_mall <- summary4_mall %>%
  mutate(data_incomplete_due_to_mort = ifelse(mort_date >= max_day-1 & mort_date <= max_day + 2, TRUE, FALSE)) 

# also make any NAs (i.e. didn't have a mortality date for that birdid_year) == FALSE
summary4_mall$data_incomplete_due_to_mort[is.na(summary4_mall$data_incomplete_due_to_mort)] <- FALSE

# calculate percent of birds that are TRUE (i.e., percent that data is cutshort due to hen dying)
sum(summary4_mall$data_incomplete_due_to_mort, na.rm = TRUE)/nrow(summary4_mall) # 44% of mallards!


## next do black ducks
summary4_abdu <- summary4 %>% filter(species == "ABDU")

# max day
# connect to database
conn <- dbConnect(
  Postgres(),
  dbname = "ABDU",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "blackducks"
)

# load unclassified data (ACC)
breeding.metrics <- tbl(conn, "daily_odba_absx_propfly") %>%
  collect()

# check data
colnames(breeding.metrics)

# remove ACC daily summary metrics calculate using <25% of daily fixes
breeding.metrics <- breeding.metrics %>% filter(prop_fixes >= 0.25)

# format data (add bird_year columns, format date as a date, select columns)
breeding.metrics2 <- breeding.metrics %>% 
  mutate(birdid_year=(paste(birdid, year, sep="_")),
         date=as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::select(c("birdid_year", "date", "birdid", "year", "device_id", "mean_ODBA", "mean_abs_x"
  ))

# load unclassified data (GPS)
breeding.metrics <- tbl(conn, "daily_ddist_nsd_mcp") %>%
  collect()

# check data
colnames(breeding.metrics)

# format data (add bird_year columns, format date as a date, select columns)
breeding.metrics <- breeding.metrics %>% 
  mutate(birdid_year=(paste(birdid, year, sep="_")),
         date=as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::select(c("birdid_year", "date", "birdid", "year", "median_ddist"
  ))

colnames(breeding.metrics)
summary(breeding.metrics)

colnames(breeding.metrics2)
summary(breeding.metrics2)

# combine gps and acc data
breeding.metrics2 <- merge(breeding.metrics2, breeding.metrics, by = c("birdid_year", "date", "birdid", "year"))

colnames(breeding.metrics2)
summary(breeding.metrics2)

n2 <- unique(breeding.metrics2$birdid_year) # 1233 bird-years (MALL), 858 (ABDU)

rm(breeding.metrics)

# only apply incubation algorithm to July 31 (MALL + ABDU) -- for ABDU, also remove dates before Mar 9 which are from incorrect migration date assignments
filter_2021 <- breeding.metrics2 %>% filter(year == "2021") %>% 
  filter(date <= "2021-07-31")
filter_2022 <- breeding.metrics2 %>% filter(year == "2022") %>% 
  filter(date <= "2022-07-31")
filter_2023 <- breeding.metrics2 %>% filter(year == "2023") %>% 
  filter(date <= "2023-07-31")
filter_2024 <- breeding.metrics2 %>% filter(year == "2024") %>% 
  filter(date <= "2024-07-31")
filter_2025 <- breeding.metrics2 %>% filter(year == "2025") %>% 
  filter(date <= "2025-07-31")


breeding.metrics2 <- rbind(filter_2021, filter_2022, filter_2023, filter_2024, filter_2025)

breeding.metrics2 <- breeding.metrics2[order(breeding.metrics2$birdid_year, breeding.metrics2$date), ]

# summarize max day by bird-year
max_day_abdu <- breeding.metrics2 %>%
  group_by(birdid_year) %>%
  summarize(max_day=max(date))

# merge with summary4
summary4_abdu <- merge(summary4_abdu, max_day_abdu, by = "birdid_year", all.x = TRUE)

## add mortality day
# load deployment data
library(readxl)
deploy_abdu <- read_excel("Deployment Files/deployments_18Sept2025.xlsx", sheet = "ABDU") ## change to MALL / ABDU depending what you need!

# get mortality date and format data
deploy_abdu$mort_date<- as.Date(deploy_abdu$`Mortality Date and Time`)
deploy_abdu <- deploy_abdu %>% 
  mutate(year=year(mort_date))  %>% 
  rename(band=`Band Number`) %>%
  mutate(birdid_year=paste(band, year, sep="_")) %>%
  select(c("birdid_year","mort_date")) 

# add mort date to summary
summary4_abdu <- merge(summary4_abdu, deploy_abdu, by = c("birdid_year"), all.x=TRUE)

# add column that = TRUE if mortality day = max day +/- one day ( >= max day -1 day && <= max day + 1 day )
summary4_abdu <- summary4_abdu %>%
  mutate(data_incomplete_due_to_mort = ifelse(mort_date >= max_day-1 & mort_date <= max_day + 2, TRUE, FALSE)) 

# also make any NAs (i.e. didn't have a mortality date for that birdid_year) == FALSE
summary4_abdu$data_incomplete_due_to_mort[is.na(summary4_abdu$data_incomplete_due_to_mort)] <- FALSE

# calculate percent of birds that are TRUE (i.e., percent that data is cutshort due to hen dying)
sum(summary4_abdu$data_incomplete_due_to_mort, na.rm = TRUE)/nrow(summary4_abdu) # 28% of abdu!


#### onward and upward... ####
## add brood status for making figure
# load classified brood rearing data
# connect to database
conn <- dbConnect(
  Postgres(),
  #dbname = "ABDU",
  dbname = "EMALL",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "blackducks"
)
mall <- tbl(conn, "mall_broodrearing_classified_IncompleteDataBirdsIncluded") %>%
  collect()

mall %>% group_by(status_early_late) %>% count()

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
abdu <- tbl(conn, "abdu_broodrearing_classified_IncompleteDataBirdsIncluded") %>%
  collect()

abdu  %>% group_by(status_early_late) %>% count()

mall$species <- "MALL"
abdu$species <- "ABDU"

brood_rearing <- rbind(abdu, mall)

brood_rearing <- brood_rearing %>%
  dplyr::select(birdid_year, status_early_late)

# rename status to something that makes more sense for plots and for combining with nested status
library(plyr)

unique(brood_rearing$status_early_late)

brood_rearing$status_early_late <- revalue(brood_rearing$status_early_late, c(
                                                   "no_brood_no_brood" = "Hatched, failed brood", 
                                                   "successful_brood_early_no_brood" = "Hatched, failed brood",
                                                   "no_brood_successful_brood_late" = "Hatched, failed brood",
                                                   "successful_brood_early_successful_brood_late" = "Hatched, successful brood"
                                                  ))

detach("package:plyr", unload = TRUE)

unique(brood_rearing$status_early_late)


unique(summary3$status_comb)  

brood_rearing %>% group_by(species, status_early_late) %>% count()

# rename status to something that makes more sense for plots
library(plyr)
# # all failed categories separately (for supp mat summary table)
# summary3$status <- revalue(summary3$status_comb, c("egg_laying_failed" = "Nested, failed incubation",
#                                                     "egg_laying_defer" = "Nested, non-incubating",
#                                                    "egg_laying_NA" = "Nested, failed attempt",
#                                                    "defer_NA" = "Defer",
#                                                       "defer_defer" = "Defer",
#                                                    "NA_failed" = "remove",
#                                                    "NA_defer" = "remove",
#                                                    "egg_laying_hatched" = "Nested, successful incubation",
#                                                    "defer_failed" = "Defer",
#                                                    "NA_hatched" = "remove"))

# one failed category (for main text summary table)
summary3$status <- revalue(summary3$status_comb, c("egg_laying_failed" = "Nested, failed attempt",
                                                   "egg_laying_defer" = "Nested, failed attempt",
                                                   "egg_laying_NA" = "Nested, failed attempt",
                                                   "defer_NA" = "Defer",
                                                   "defer_defer" = "Defer",
                                                   "NA_failed" = "remove",
                                                   "NA_defer" = "remove",
                                                   "egg_laying_hatched" = "Nested, successful attempt",
                                                   "defer_failed" = "Defer",
                                                   "NA_hatched" = "remove"))

detach("package:plyr", unload = TRUE)

unique(summary3$status)  

summary3 <- summary3 %>%
  mutate(status = ifelse(status == "remove", NA, status))

unique(summary3$status)  

summary3 <- summary3[!is.na(summary3$status), ]

unique(summary3$status)  

## for figure, add brood  status to status column
# add birdid_year column to summary3 for merging
summary3 <- summary3 %>%
  mutate(birdid_year=paste(band, year, sep="_"))
# merge summary 3 and brood rearing dataframes
summary3 <- merge(summary3, brood_rearing, by = "birdid_year", all.x = TRUE)
# for all birds of status "Nested, successful attempt", change to brood status
summary3 <- summary3 %>% 
  mutate(status=ifelse(status == "Nested, successful attempt", status_early_late, status))



#### add flyway and BCR for flyway meeting Feb 2026 ####
# add lay dates to summary df
dates4 <- dates3 %>% dplyr::select("band", "year", "species", "lay_start")

summary4 <- merge(summary3, dates4, by = c("band", "year", "species"), all.x = TRUE, )

# replace NA lay dates (defers) with May 15
replacement_date_21 <- as.Date("2021-05-15")
replacement_date_22 <- as.Date("2022-05-15")
replacement_date_23 <- as.Date("2023-05-15")
replacement_date_24 <- as.Date("2024-05-15")
replacement_date_25 <- as.Date("2025-05-15")

summary5 <- summary4 %>%
  mutate(lay_start = as.Date(ifelse(is.na(lay_start) == TRUE & year == "2021", replacement_date_21, lay_start))) %>%
  mutate(lay_start = as.Date(ifelse(is.na(lay_start) == TRUE & year == "2022", replacement_date_22, lay_start))) %>%
  mutate(lay_start = as.Date(ifelse(is.na(lay_start) == TRUE & year == "2023", replacement_date_23, lay_start))) %>%
  mutate(lay_start = as.Date(ifelse(is.na(lay_start) == TRUE & year == "2024", replacement_date_24, lay_start))) %>%
  mutate(lay_start = as.Date(ifelse(is.na(lay_start) == TRUE & year == "2025", replacement_date_25, lay_start))) 

# load gps and reduce to one location per day per bird
conn <- dbConnect(
  Postgres(),
  #dbname = "ABDU",
  dbname = "EMALL",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "blackducks"
)

gps_mall <- tbl(conn, "gps_data") %>%
  collect() %>% 
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
         year = year(timestamp),
         birdid_year = paste(birdid, year, sep="_")) %>% 
  # remove erroneous (0,0) points from when sat count = 0
  filter(Latitude != 0 | Longitude != 0) %>%
  # select relevant columns
  dplyr::select(c(device_id,
                  birdid, year,
                  birdid_year,
                  timestamp, Latitude, Longitude)) %>%
  mutate(lay_start = as.Date(timestamp),
         species="MALL") %>% 
  group_by(birdid_year, lay_start) %>%
  slice_sample(n = 1) %>%
  ungroup()

conn <- dbConnect(
  Postgres(),
  dbname = "ABDU",
  #dbname = "EMALL",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "blackducks"
)

gps_abdu <- tbl(conn, "gps_data") %>%
  collect() %>% 
  mutate(birdid=paste0(substr(bandnum, 1, 4), "-", substr(bandnum, 5, nchar(bandnum)))) %>%
  mutate(timestamp = as.POSIXct(UTC_datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
         year = year(timestamp),
         birdid_year = paste(birdid, year, sep="_")) %>% 
  # remove erroneous (0,0) points from when sat count = 0
  filter(Latitude != 0 | Longitude != 0) %>%
  # select relevant columns
  dplyr::select(c(device_id,
                  birdid, year,
                  birdid_year,
                  timestamp, Latitude, Longitude)) %>%
  mutate(lay_start = as.Date(timestamp),
         species="ABDU") %>% 
  group_by(birdid_year, lay_start) %>%
  slice_sample(n = 1) %>%
  ungroup()

gps <- rbind(gps_mall, gps_abdu)

gps <- gps %>% rename(band=birdid) %>%
  dplyr::select("band", "year", "species", "lay_start", "Latitude", "Longitude")


# add breeding location to summary df
summary6 <- merge(summary5, gps, by = c("band", "year", "species", "lay_start"), all.x = TRUE)

# save so can continue here
write.csv(summary6, "./Plots for Flyway Meeting_19Feb2026/NestingStatusWithBreedingLocation_19Feb2026.csv")

summary <- read.csv("./Plots for Flyway Meeting_19Feb2026/NestingStatusWithBreedingLocation_19Feb2026.csv")

# load shapefile for BCRs
bcr <- st_read("./Maps/BCR_Terrestrial_master_International.shp")

plot(bcr)

# convert gps to sf object
points_sf <- st_as_sf(summary, coords = c("Longitude", "Latitude"), crs = 4326)

# ensure both layers have same crs
points_sf <- st_transform(points_sf, crs = st_crs(bcr))

# add bcr for each breeding location
sf::sf_use_s2(FALSE)
points_with_bcr <- st_join(points_sf, bcr, join = st_within)
sf::sf_use_s2(TRUE)

points_with_bcr %>% group_by(BCR) %>% count() # 64 of 1226 are NAs because in the water presumably?!

# convert results back to dataframe 
points_df <- as.data.frame(points_with_bcr) %>% select(-geometry) %>%
  select(band, year, species, BCR, BCRNAME, Label)

# add to summary
summary2 <- merge(summary, points_df, by = c("band", "year", "species"))

# save breeding locations with BCRs
write.csv(summary2, "./Plots for Flyway Meeting_19Feb2026/NestingStatusWithBreedingLocationAndBCR_19Feb2026.csv")


# load shapefile for flyways ### only for the US so not using!!!
fly <- st_read("./Maps/WaterfowlFlyways.shp")

plot(fly)

# convert gps to sf object
points_sf <- st_as_sf(summary, coords = c("Longitude", "Latitude"), crs = 4326)

# 3. Load North American States/Provinces Shapefile
# Download "Admin 1 – States, Provinces" from Natural Earth
map <- st_read("./Maps/ne_10m_admin_1_states_provinces.shp") 

nc_map <- map %>% filter(adm0_a3 %in% c("USA", "CAN"))

plot(nc_map)

# ensure both layers have same crs
points_sf <- st_transform(points_sf, crs = st_crs(nc_map))

# add state/province to breeding locations
pts_with_state <- st_join(points_sf, nc_map, join = st_within)

pts_with_state %>% group_by(postal) %>% count()

# convert results back to dataframe 
points_df <- as.data.frame(pts_with_state) %>% select(-geometry) %>%
  select(band, year, species, postal)

sum_by_province <- points_df %>% group_by(postal) %>% count() # 55 of 1226 are NAs 

# add to summary
summary3 <- merge(summary2, points_df, by = c("band", "year", "species"))

# add flyway based on province/state
unique(sum_by_province$postal)
AF <- c("CT", "DE", "MA", "MD", "ME",  "NB", "NC","NH", "NJ", "NL", "NS", "NY", "PA", "PE", "QC", "RI", "VA", "VT")
MF <- c("MB", "MI", "NU", "OH", "ON", "TN", "WI", "WV")
CF <- "ND"

summary4 <- summary3 %>% 
  mutate(flyway=NA) %>%
  mutate(flyway=ifelse(postal %in% AF, "AF", flyway)) %>%
  mutate(flyway=ifelse(postal %in% MF, "MF", flyway)) %>%
  mutate(flyway=ifelse(postal %in% CF, "CF", flyway))

summary4 %>% group_by(flyway) %>% count() # 55 NAs

# fill in some NAs using state/province
summary5 <- summary4 %>%
  mutate(BCR=ifelse(is.na(BCR)==TRUE & postal == "CT", 30, BCR)) %>%
  mutate(BCR=ifelse(is.na(BCR)==TRUE & postal == "MD", 30, BCR)) %>%
  mutate(BCR=ifelse(is.na(BCR)==TRUE & postal == "ME", 14, BCR)) %>%
  mutate(BCR=ifelse(is.na(BCR)==TRUE & postal == "NB", 14, BCR)) %>%
  mutate(BCR=ifelse(is.na(BCR)==TRUE & postal == "NJ", 30, BCR)) %>%
  mutate(BCR=ifelse(is.na(BCR)==TRUE & postal == "NS", 14, BCR)) %>%
  mutate(BCR=ifelse(is.na(BCR)==TRUE & postal == "VA", 30, BCR)) %>%
  mutate(flyway=ifelse(is.na(flyway)==TRUE & BCR == 30, "AF", flyway)) %>%
  mutate(flyway=ifelse(is.na(flyway)==TRUE & BCR == 14, "AF", flyway))

summary5 %>% group_by(BCR) %>% count() # 51 of 1226 are NAs, down 13 from 64 :) 
summary5 %>% group_by(flyway) %>% count() # 42 NAs, down 13 from 55 :) 


# save breeding locations with BCRs and flyway
write.csv(summary5, "./Plots for Flyway Meeting_19Feb2026/NestingStatusWithBreedingLocationAndBCRAndFlyway_19Feb2026.csv")

# load breeding locations with BCRs and flyway if starting partway...
setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics")
summary5 <- read.csv("./Plots for Flyway Meeting_19Feb2026/NestingStatusWithBreedingLocationAndBCRAndFlyway_19Feb2026.csv")

# summary of nesting outcomes by flyway (ALL BIRDS)
summary3 <- summary5[!is.na(summary5$flyway), ]

# get totals by flyway
summary3 %>% group_by(species, flyway) %>% count()

# mall
n_af_mall <- 530
n_mf_mall <- 238
# abdu
n_af_abdu <- 324
n_mf_abdu <- 90

sum_outcomes_mf_mall <- summary3 %>% filter(species == "MALL" & flyway == "MF") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_mf_mall*100)

sum_outcomes_mf_mall$flyway <- "MF"
sum_outcomes_mf_mall$species <- "MALL"


sum_outcomes_af_mall <- summary3 %>% filter(species == "MALL" & flyway == "AF") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_af_mall*100)

sum_outcomes_af_mall$flyway <- "AF"
sum_outcomes_af_mall$species <- "MALL"

sum_outcomes_mf_abdu <- summary3 %>% filter(species == "ABDU" & flyway == "MF") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_mf_abdu*100)

sum_outcomes_mf_abdu$flyway <- "MF"
sum_outcomes_mf_abdu$species <- "ABDU"

sum_outcomes_af_abdu <- summary3 %>% filter(species == "ABDU" & flyway == "AF") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_af_abdu*100)

sum_outcomes_af_abdu$flyway <- "AF"
sum_outcomes_af_abdu$species <- "ABDU"

sum_outcomes_by_flyway_allbirds <- rbind(sum_outcomes_af_mall, sum_outcomes_mf_mall, sum_outcomes_af_abdu, sum_outcomes_mf_abdu)

# Convert to factor and specify desired order
sum_outcomes_by_flyway_allbirds$status <- factor(sum_outcomes_by_flyway_allbirds$status, levels = c("Defer", "Nested, non-incubating",
                                                                                              "Failed incubation", "Successful incubation"))

sum_outcomes_by_flyway_allbirds$flyway <- factor(sum_outcomes_by_flyway_allbirds$flyway, levels = c("AF", "MF"))
sum_outcomes_by_flyway_allbirds$species<- factor(sum_outcomes_by_flyway_allbirds$species)


## status by flyway, separate plots for each species within one figure (USED THIS ONE!!)

library(ggpubr)
library(viridis)
n_status <- viridis(4)

tiff("plots_EggLayingAndIncubationCombined_14Sept2025/SpeciesSeparatePlots_OutcomesByFlyway_AllBirds.tiff", units="in", width=7, height=7, res=300)
#png("plots_EggLayingAndIncubationCombined_14Sept2025/SpeciesSeparatePlots_OutcomesByflyway_AllBirds.png")
mall<-sum_outcomes_by_flyway_allbirds %>%
  filter(species == "MALL") %>%
  mutate(
    flyway=as.factor(flyway),
    status=as.factor(status)
  )
catsums <- aggregate(n ~ flyway, mall , FUN = sum)  
a<-
  ggplot(mall, aes(x = flyway, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=flyway, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by flyway",
    x = "Flyway",
    y = "Percent",
    fill = "Nesting Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, non-incubating" = n_status[2],
                               "Failed incubation" = n_status[3],
                               "Successful incubation" = n_status[4]))+
  
  #ggtitle("MALL") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))
abdu<-sum_outcomes_by_flyway_allbirds %>%
  filter(species == "ABDU") %>%
  mutate(
    flyway=as.factor(flyway),
    status=as.factor(status)
  ) 
catsums <- aggregate(n ~ flyway, abdu , FUN = sum)  
b<-
  ggplot(abdu,aes(x = flyway, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=flyway, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by flyway",
    x = "Flyway",
    y = "Percent",
    fill = "Nesting Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, non-incubating" = n_status[2],
                               "Failed incubation" = n_status[3],
                               "Successful incubation" = n_status[4]))+
  
  #ggtitle("ABDU") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))

ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
          labels = c("ABDU", "MALL"))
dev.off()
rm(a,b)

# summary of nesting outcomes by year (ALL BIRDS)
summary3 <- summary5[!is.na(summary5$BCR), ]

# get totals by bcr
summary3 %>% group_by(species, BCR) %>% count() %>% filter(species == "ABDU") %>% filter(n >= 10)
summary3 %>% group_by(species, BCR) %>% count() %>% filter(species == "MALL") %>% filter(n >= 10)

# abdu
n_7_abdu <- 45
n_8_abdu <- 108
n_12_abdu <- 93
n_13_abdu <- 17
n_14_abdu <- 84
n_30_abdu <- 45

# mall
n_7_mall <- 31
n_8_mall <- 41
n_12_mall <- 116
n_13_mall <- 262
n_14_mall <- 141
n_28_mall <- 71
n_30_mall <- 71

sum_outcomes_7_abdu <- summary3 %>% filter(species == "ABDU" & BCR == 7) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_7_abdu*100) %>%
  mutate(species="ABDU", BCR="7")

sum_outcomes_8_abdu <- summary3 %>% filter(species == "ABDU" & BCR == 8) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_8_abdu*100) %>%
  mutate(species="ABDU", BCR="8")

sum_outcomes_12_abdu <- summary3 %>% filter(species == "ABDU" & BCR == 12) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_12_abdu*100) %>%
  mutate(species="ABDU", BCR="12")

sum_outcomes_13_abdu <- summary3 %>% filter(species == "ABDU" & BCR == 13) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_13_abdu*100) %>%
  mutate(species="ABDU", BCR="13")

sum_outcomes_14_abdu <- summary3 %>% filter(species == "ABDU" & BCR == 14) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_14_abdu*100) %>%
  mutate(species="ABDU", BCR="14")

sum_outcomes_30_abdu <- summary3 %>% filter(species == "ABDU" & BCR == 30) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_30_abdu*100) %>%
  mutate(species="ABDU", BCR="30")

sum_outcomes_7_mall <- summary3 %>% filter(species == "MALL" & BCR == 7) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_7_mall*100) %>%
  mutate(species="MALL", BCR="7")

sum_outcomes_8_mall <- summary3 %>% filter(species == "MALL" & BCR == 8) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_8_mall*100) %>%
  mutate(species="MALL", BCR="8")

sum_outcomes_12_mall <- summary3 %>% filter(species == "MALL" & BCR == 12) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_12_mall*100) %>%
  mutate(species="MALL", BCR="12")

sum_outcomes_13_mall <- summary3 %>% filter(species == "MALL" & BCR == 13) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_13_mall*100) %>%
  mutate(species="MALL", BCR="13")

sum_outcomes_14_mall <- summary3 %>% filter(species == "MALL" & BCR == 14) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_14_mall*100) %>%
  mutate(species="MALL", BCR="14")

sum_outcomes_28_mall <- summary3 %>% filter(species == "MALL" & BCR == 28) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_28_mall*100) %>%
  mutate(species="MALL", BCR="28")

sum_outcomes_30_mall <- summary3 %>% filter(species == "MALL" & BCR == 30) %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_30_mall*100) %>%
  mutate(species="MALL", BCR="30")

sum_outcomes_by_bcr_allbirds <- rbind(sum_outcomes_7_mall, sum_outcomes_8_mall, sum_outcomes_12_mall, sum_outcomes_13_mall,
                                      sum_outcomes_14_mall, sum_outcomes_28_mall,sum_outcomes_30_mall,
                                       sum_outcomes_7_abdu, sum_outcomes_8_abdu, sum_outcomes_12_abdu, sum_outcomes_13_abdu,
                                       sum_outcomes_14_abdu, sum_outcomes_30_abdu)


# Convert to factor and specify desired order
sum_outcomes_by_bcr_allbirds$status <- factor(sum_outcomes_by_bcr_allbirds$status, levels = c("Defer", "Nested, non-incubating",
                                                                                                "Failed incubation", "Successful incubation"))
sum_outcomes_by_bcr_allbirds$BCR <- factor(sum_outcomes_by_bcr_allbirds$BCR, levels = c("7", "8", "12", "13", "14",
                                                                                              "28", "30"))
## status by year, separate plots for each species within one figure (USED THIS ONE!!)

library(ggpubr)
library(viridis)
n_status <- viridis(4)

tiff("plots_EggLayingAndIncubationCombined_14Sept2025/SpeciesSeparatePlots_OutcomesByBCR_AllBirds.tiff", units="in", width=7, height=7, res=300)
#png("plots_EggLayingAndIncubationCombined_14Sept2025/SpeciesSeparatePlots_OutcomesByYear_AllBirds.png")
mall<-sum_outcomes_by_bcr_allbirds %>%
  filter(species == "MALL") %>%
  mutate(
    BCR=as.factor(BCR),
    status=as.factor(status)
  ) 
catsums <- aggregate(n ~ BCR, mall , FUN = sum)  
a<-
  ggplot(mall, aes(x = BCR, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=BCR, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Breeding Conservation Region",
    y = "Percent",
    fill = "Nesting Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, non-incubating" = n_status[2],
                               "Failed incubation" = n_status[3],
                               "Successful incubation" = n_status[4]))+
  
  
  #ggtitle("MALL") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))
abdu<-sum_outcomes_by_bcr_allbirds %>%
  filter(species == "ABDU") %>%
  mutate(
    BCR=as.factor(BCR),
    status=as.factor(status)
  ) 
catsums <- aggregate(n ~ BCR, abdu , FUN = sum)  
b<-
  ggplot(abdu,aes(x = BCR, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=BCR, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Breeding Conservation Region",
    y = "Percent",
    fill = "Nesting Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, non-incubating" = n_status[2],
                               "Failed incubation" = n_status[3],
                               "Successful incubation" = n_status[4]))+
  
  
  #ggtitle("ABDU") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))

ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
          labels = c("ABDU", "MALL"))
dev.off()
rm(a,b)




### continuing with regular results summaries.... ####

# summary of nesting outcomes overall (ALL BIRDS)
n_total <-summary3 %>% group_by(species) %>% count() # 637 (ABDU), 1078 (MALL)
sum_outcomes_abdu <- summary3 %>% filter(species == "ABDU") %>%
  group_by(status) %>%
  summarise(n.cat=n(),
            percent=n()/n_total[1,2]*100)

sum_outcomes_mall <- summary3 %>% filter(species == "MALL") %>%
  group_by(status) %>%
  summarise(n.cat=n(),
            percent=n()/n_total[2,2]*100)

# summary of nesting outcomes by age (ALL BIRDS)
# get totals by age
summary3 %>% group_by(species, age) %>% count()

# mall
n_asy_mall <- 610
n_sy_mall <- 468
# abdu
n_asy_abdu <- 354
n_sy_abdu <- 283

sum_outcomes_sy_mall <- summary3 %>% filter(species == "MALL" & age == "SY") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_sy_mall*100)

sum_outcomes_sy_mall$age <- "SY"
sum_outcomes_sy_mall$species <- "MALL"


sum_outcomes_asy_mall <- summary3 %>% filter(species == "MALL" & age == "ASY") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_asy_mall*100)

sum_outcomes_asy_mall$age <- "ASY"
sum_outcomes_asy_mall$species <- "MALL"

sum_outcomes_sy_abdu <- summary3 %>% filter(species == "ABDU" & age == "SY") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_sy_abdu*100)

sum_outcomes_sy_abdu$age <- "SY"
sum_outcomes_sy_abdu$species <- "ABDU"

sum_outcomes_asy_abdu <- summary3 %>% filter(species == "ABDU" & age == "ASY") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_asy_abdu*100)

sum_outcomes_asy_abdu$age <- "ASY"
sum_outcomes_asy_abdu$species <- "ABDU"

sum_outcomes_by_age_allbirds <- rbind(sum_outcomes_asy_mall, sum_outcomes_sy_mall, sum_outcomes_asy_abdu, sum_outcomes_sy_abdu)

#Convert to factor and specify desired order
# # failed as separate categories
# sum_outcomes_by_age_allbirds$status <- factor(sum_outcomes_by_age_allbirds$status, levels = c("Defer","Nested, failed attempt", "Nested, non-incubating",
#                                                                                               "Nested, failed incubation", "Nested, successful incubation"))

# # failed as a single category 
# sum_outcomes_by_age_allbirds$status <- factor(sum_outcomes_by_age_allbirds$status, levels = c("Defer","Nested, failed attempt", "Nested, successful attempt"))

# failed as a single category AND brood status included
sum_outcomes_by_age_allbirds$status <- factor(sum_outcomes_by_age_allbirds$status, levels = c("Defer","Nested, failed attempt", "Hatched, failed brood", "Hatched, successful brood"))



sum_outcomes_by_age_allbirds$age <- factor(sum_outcomes_by_age_allbirds$age, levels = c("SY", "ASY"))
sum_outcomes_by_age_allbirds$species<- factor(sum_outcomes_by_age_allbirds$species)

### this has species and age by status combined on one plot... but too busy!! Didn't end up using!

# 2. Create the interaction variable for nested x-axis labels
# This combines nesting_outcome and species, separated by a delimiter (e.g., ".")
sum_outcomes_by_age_allbirds$nested_x <- interaction(sum_outcomes_by_age_allbirds$status, sum_outcomes_by_age_allbirds$species, sep = ".")

# 3. Create the grouped, stacked bar chart with nested x-axis labels
library(ggplot2)
library(ggh4x)
png("plots_EggLayingAndIncubationCombined_14Sept2025/BothSpecies_OutcomesByAge_AllBirds.png")
ggplot(sum_outcomes_by_age_allbirds, aes(x = nested_x, y = percent, fill = age)) +
  geom_bar(stat = "identity", position = "stack") + # Use "stack" position for stacked bars
  # Use ggh4x::guide_axis_nested() for nested x-axis labels
  scale_x_discrete(guide = "axis_nested") +
  labs(
    title = "Nesting Outcome by Species and Age",
    x = "Nesting Outcome and Species",
    y = "Percent",
    fill = "Age"
  ) +
  theme_minimal() +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  )
dev.off()

## status by age, separate plots for each species within one figure (USED THIS ONE!!)

library(ggpubr)
library(viridis)
n_status <- viridis(5)

tiff("plots_EggLayingAndIncubationCombined_14Sept2025/SpeciesSeparatePlots_OutcomesByAge_AllBirds_31Mar2026.tiff", units="in", width=7, height=7, res=300)
#png("plots_EggLayingAndIncubationCombined_14Sept2025/SpeciesSeparatePlots_OutcomesByAge_AllBirds.png")
mall<-sum_outcomes_by_age_allbirds %>%
  filter(species == "MALL") %>%
  mutate(
    age=as.factor(age),
    status=as.factor(status)
  )
catsums <- aggregate(n ~ age, mall , FUN = sum)  
a<-
  ggplot(mall, aes(x = age, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=age, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
labs(
  #title = "Nesting Outcome by Age",
  x = "Age",
  y = "Percent",
  fill = "Nesting Outcome"
) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, failed attempt" = n_status[2],
                               "Nested, non-incubating" = n_status[3],
                               "Nested, failed incubation" = n_status[4],
                               "Nested, successful incubation" = n_status[5]))+
  
  #ggtitle("MALL") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))
abdu<-sum_outcomes_by_age_allbirds %>%
  filter(species == "ABDU") %>%
  mutate(
    age=as.factor(age),
    status=as.factor(status)
  ) 
catsums <- aggregate(n ~ age, abdu , FUN = sum)  
b<-
  ggplot(abdu,aes(x = age, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=age, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Age",
    y = "Percent",
    fill = "Nesting Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, failed attempt" = n_status[2],
                               "Nested, non-incubating" = n_status[3],
                               "Nested, failed incubation" = n_status[4],
                               "Nested, successful incubation" = n_status[5]))+
  
  #ggtitle("ABDU") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))

ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
          labels = c("ABDU", "MALL"))
dev.off()
rm(a,b)



# old plot (quickie for first view)
png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_OutcomesByAge_AllBirds.png")
ggplot(sum_outcomes_by_age_allbirds, aes(x = status)) +
  geom_point(aes(y=percent.sy, color = "SY"), size = 4, shape=15)+#, shape = 21, color = "magenta", fill="magenta", size=4) +
  geom_point(aes(y=percent.asy, color = "ASY"), size = 4, shape=16)+#, shape = 21, color = "black" , fill="black", size=4) +
  labs(
    x = "Nesting Outcome",
    y = "Percent of Birds") +
  scale_color_manual(
    name = "Age",  # Custom legend title
    values = c("ASY" = "darkblue", "SY" = "darkred"), # Custom colors for each group
    labels = c("ASY", "SY") # Custom labels for legend entries
  ) +
  ylim(0,100)
# theme_minimal()
dev.off()

# summary of nesting outcomes by year (ALL BIRDS)
# get totals by year
summary3 %>% group_by(species, year) %>% count()

# mall
n_2022_mall <- 224
n_2023_mall <- 279
n_2024_mall <- 325
n_2025_mall <- 250

# abdu
n_2021_abdu <- 24
n_2022_abdu <- 118
n_2023_abdu <- 175
n_2024_abdu <- 229
n_2025_abdu <- 91

sum_outcomes_2021_abdu <- summary3 %>% filter(year == "2021") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2021_abdu*100) %>%
  mutate(species="ABDU", year="2021")

sum_outcomes_2022_abdu <- summary3 %>% filter(year == "2022" & species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2022_abdu*100) %>%
  mutate(species="ABDU", year="2022")

sum_outcomes_2023_abdu <- summary3  %>% filter(year == "2023"& species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2023_abdu*100) %>%
  mutate(species="ABDU", year="2023")

sum_outcomes_2024_abdu <- summary3  %>% filter(year == "2024" & species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2024_abdu*100) %>%
  mutate(species="ABDU", year="2024")

sum_outcomes_2025_abdu <- summary3  %>% filter(year == "2025" & species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2025_abdu*100) %>%
  mutate(species="ABDU", year="2025")

sum_outcomes_2022_mall <- summary3 %>% filter(year == "2022" & species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2022_mall*100) %>%
  mutate(species="MALL", year="2022")

sum_outcomes_2023_mall <- summary3  %>% filter(year == "2023"& species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2023_mall*100) %>%
  mutate(species="MALL", year="2023")

sum_outcomes_2024_mall <- summary3  %>% filter(year == "2024" & species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2024_mall*100) %>%
  mutate(species="MALL", year="2024")

sum_outcomes_2025_mall <- summary3  %>% filter(year == "2025" & species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2025_mall*100) %>%
  mutate(species="MALL", year="2025")

sum_outcomes_by_year_allbirds <- rbind(sum_outcomes_2022_mall, sum_outcomes_2023_mall, sum_outcomes_2024_mall, sum_outcomes_2025_mall,
                                      sum_outcomes_2021_abdu, sum_outcomes_2022_abdu, sum_outcomes_2023_abdu, sum_outcomes_2024_abdu,
                                      sum_outcomes_2025_abdu)

sum_outcomes_by_year_allbirds$one_hundred_minus_value <- 100-sum_outcomes_by_year_allbirds$percent


# old way when doing one species at a time
# sum_outcomes_by_year_allbirds <- merge(sum_outcomes_2021, sum_outcomes_2022, by = "status_comb")
# sum_outcomes_by_year_allbirds <- merge(sum_outcomes_by_year_allbirds, sum_outcomes_2023, by = "status_comb")
# sum_outcomes_by_year_allbirds <- merge(sum_outcomes_by_year_allbirds, sum_outcomes_2024, by = "status_comb")
# sum_outcomes_by_year_allbirds <- merge(sum_outcomes_by_year_allbirds, sum_outcomes_2025, by = "status_comb")

# Convert to factor and specify desired order
# # failed as separate categories
# sum_outcomes_by_age_allbirds$status <- factor(sum_outcomes_by_age_allbirds$status, levels = c("Defer","Nested, failed attempt", "Nested, non-incubating",
#                                                                                               "Nested, failed incubation", "Nested, successful incubation"))

# # failed as a single category
# sum_outcomes_by_year_allbirds$status <- factor(sum_outcomes_by_year_allbirds$status, levels = c("Defer","Nested, failed attempt", "Nested, successful attempt"))

# failed as a single category AND brood status
sum_outcomes_by_year_allbirds$status <- factor(sum_outcomes_by_year_allbirds$status, levels = c("Defer","Nested, failed attempt", "Hatched, failed brood", "Hatched, successful brood"))


png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_OutcomesByYear_AllBirds.png")
ggplot(sum_outcomes_by_year_allbirds, aes(x = status_comb)) +
  geom_point(aes(y=percent.2022, color = "2022"), size = 4, shape=15)+#, shape = 21, color = "magenta", fill="magenta", size=4) +
  geom_point(aes(y=percent.2023, color = "2023"), size = 4, shape=16)+#, shape = 21, color = "black" , fill="black", size=4) +
  geom_point(aes(y=percent.2024, color = "2024"), size = 4, shape=17)+#, shape = 21, color = "magenta", fill="magenta", size=4) +
  geom_point(aes(y=percent.2025, color = "2025"), size = 4, shape=18)+#, shape = 21, color = "black" , fill="black", size=4) +
  labs(
    x = "Nesting Outcome",
    y = "Percent of Birds") +
  scale_color_manual(
    name = "Year",  # Custom legend title
    values = c("2022" = "darkblue", "2023" = "darkred", "2024" = "darkgreen", "2025" = "darkorange"), # Custom colors for each group
    labels = c("2022", "2023", "2024", "2025") # Custom labels for legend entries
  ) +
  ylim(0,100)
# theme_minimal()
dev.off()


## status by year, separate plots for each species within one figure (USED THIS ONE!!)

library(ggpubr)
library(viridis)
n_status <- viridis(5)

tiff("plots_EggLayingAndIncubationCombined_14Sept2025/SpeciesSeparatePlots_OutcomesByYear_AllBirds_18Jun2026.tiff", units="in", width=7, height=7, res=300)
#png("plots_EggLayingAndIncubationCombined_14Sept2025/SpeciesSeparatePlots_OutcomesByYear_AllBirds.png")
mall<-sum_outcomes_by_year_allbirds %>%
  filter(species == "MALL") %>%
  mutate(
    year=as.factor(year),
    status=as.factor(status)
  ) 
catsums <- aggregate(n ~ year, mall , FUN = sum)  
a<-
  ggplot(mall, aes(x = year, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=year, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Year",
    y = "Percent",
    fill = "Nesting Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, failed attempt" = n_status[2],
                               "Nested, non-incubating" = n_status[3],
                               "Nested, failed incubation" = n_status[4],
                               "Nested, successful incubation" = n_status[5]))+
  
  
  #ggtitle("MALL") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))
abdu<-sum_outcomes_by_year_allbirds %>%
  filter(species == "ABDU") %>%
  mutate(
    year=as.factor(year),
    status=as.factor(status)
  ) 
catsums <- aggregate(n ~ year, abdu , FUN = sum)  
b<-
  ggplot(abdu,aes(x = year, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=year, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Year",
    y = "Percent",
    fill = "Nesting Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, failed attempt" = n_status[2],
                               "Nested, non-incubating" = n_status[3],
                               "Nested, failed incubation" = n_status[4],
                               "Nested, successful incubation" = n_status[5]))+
  
  
  #ggtitle("ABDU") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))

ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
          labels = c("ABDU", "MALL"))
dev.off()
rm(a,b)

## status by age AND year, separate plots for each species within one figure (ACTUALLY USED THIS ONE!!!! lol)

library(ggpubr)
library(viridis)
n_status <- viridis(4)

"#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"

tiff("plots_EggLayingAndIncubationCombined_14Sept2025/SpeciesSeparatePlots_OutcomesByAgeAndYear_AllBirds_18Jun2026.tiff", units="in", width=25, height=12, res=300)
#png("plots_EggLayingAndIncubationCombined_14Sept2025/SpeciesSeparatePlots_OutcomesByAge_AllBirds.png")
mall<-sum_outcomes_by_age_allbirds %>%
  filter(species == "MALL") %>%
  mutate(
    age=as.factor(age),
    status=as.factor(status)
  )
catsums <- aggregate(n ~ age, mall , FUN = sum)  
a<-
  ggplot(mall, aes(x = age, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=age, y=n, label=paste0("N = ",n), fill=NULL), position=position_fill(), vjust=-0.1, size=6) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Age",
    y = "Percent",
    fill = "Reproductive Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, failed attempt" = n_status[2],
                               "Hatched, failed brood" = n_status[3],
                               "Hatched, successful brood" = n_status[4]))+
  #ggtitle("MALL") +
  theme_classic() +
  theme(plot.margin = margin(1,0.5,0,0.1, 'cm'),
        # Set size for both axis titles 
        axis.title = element_text(size = 22),
        
        # Set size for both axis text/labels 
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 17),     # Size for labels
        legend.title = element_text(size = 19)      # Size for title
  )+
  guides(fill = guide_legend(reverse = TRUE))
abdu<-sum_outcomes_by_age_allbirds %>%
  filter(species == "ABDU") %>%
  mutate(
    age=as.factor(age),
    status=as.factor(status)
  ) 
catsums <- aggregate(n ~ age, abdu , FUN = sum)  
b<-
  ggplot(abdu,aes(x = age, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=age, y=n, label=paste0("N = ",n), fill=NULL), position=position_fill(), vjust=-0.1, size=6) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Age",
    y = "Percent",
    fill = "Reproductive Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, failed attempt" = n_status[2],
                               "Hatched, failed brood" = n_status[3],
                               "Hatched, successful brood" = n_status[4]))+
  #ggtitle("ABDU") +
  theme_classic() + 
  theme(plot.margin = margin(1,0.5,0,0.1, 'cm'),
        # Set size for both axis titles 
        axis.title = element_text(size = 22),
        
        # Set size for both axis text/labels 
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 17),     # Size for labels
        legend.title = element_text(size = 19)      # Size for title
  )+
  guides(fill = guide_legend(reverse = TRUE))

c <- ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
          labels = c("ABDU", "MALL"),
          font.label = list(size = 22, color = "black", face = "bold"))

n_status <- viridis(4)

mall<-sum_outcomes_by_year_allbirds %>%
  filter(species == "MALL") %>%
  mutate(
    year=as.factor(year),
    status=as.factor(status)
  ) 
catsums <- aggregate(n ~ year, mall , FUN = sum)  
a<-
  ggplot(mall, aes(x = year, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=year, y=n, label=paste0("N = ",n), fill=NULL), position=position_fill(), vjust=-0.1, size=6) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Year",
    y = "Percent",
    fill = "Reproductive Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, failed attempt" = n_status[2],
                               "Hatched, failed brood" = n_status[3],
                               "Hatched, successful brood" = n_status[4]))+
  
  
  #ggtitle("MALL") +
  theme_classic() +
  theme(plot.margin = margin(1,0.5,0,0.1, 'cm'),
        # Set size for both axis titles 
        axis.title = element_text(size = 22),
        
        # Set size for both axis text/labels 
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 17),     # Size for labels
        legend.title = element_text(size = 19)      # Size for title
  )+
  guides(fill = guide_legend(reverse = TRUE))
abdu<-sum_outcomes_by_year_allbirds %>%
  filter(species == "ABDU") %>%
  mutate(
    year=as.factor(year),
    status=as.factor(status)
  ) 
catsums <- aggregate(n ~ year, abdu , FUN = sum)  
b<-
  ggplot(abdu,aes(x = year, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=year, y=n, label=paste0("N = ",n), fill=NULL), position=position_fill(), vjust=-0.1, size=6) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Year",
    y = "Percent",
    fill = "Reproductive Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Defer" = n_status[1],
                               "Nested, failed attempt" = n_status[2],
                               "Hatched, failed brood" = n_status[3],
                               "Hatched, successful brood" = n_status[4]))+
  
  #ggtitle("ABDU") +
  theme_classic() +
  theme(plot.margin = margin(1,0.5,0,0.1, 'cm'),
        # Set size for both axis titles 
        axis.title = element_text(size = 22),
        
        # Set size for both axis text/labels 
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 17),     # Size for labels
        legend.title = element_text(size = 19)      # Size for title
  )+
  guides(fill = guide_legend(reverse = TRUE))

d <- ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
          labels = c("ABDU", "MALL"),
          font.label = list(size = 22, color = "black", face = "bold"))

ggarrange(c,d, ncol=2, nrow=1, common.legend = TRUE)
dev.off()
rm(a,b,c,d)

### summary of nesting outcomes - nesting birds only
# first reduce to only birds that nested
summary4 <- summary3 %>% filter(status_comb == "egg_laying_defer" |
                                  status_comb == "egg_laying_failed" |
                                  status_comb == "egg_laying_hatched" |
                                  status_comb == "egg_laying_NA")


# summary of nesting outcomes overall (nesting birds only)
n_total <- summary4 %>% group_by(species) %>% count() # 914 (MALL), 448 (ABDU)
sum_outcomes_abdu <- summary4 %>% filter(species == "ABDU") %>%
  group_by(status_comb) %>%
  summarise(n.cat=n(),
            percent=n()/n_total[1,2]*100)

sum_outcomes_mall <- summary4 %>% filter(species == "MALL") %>%
  group_by(status_comb) %>%
  summarise(n.cat=n(),
            percent=n()/n_total[2,2]*100)

# nesting outcomes by age (nesting birds only)
summary4 %>% group_by(age, status_comb) %>% count()

summary4 %>% group_by(species, age) %>% count()

# mall
n_asy_mall <- 545
n_sy_mall <- 369
# abdu
n_asy_abdu <- 268
n_sy_abdu <- 180

sum_outcomes_sy_abdu <- summary4 %>% filter(age == "SY" & species == "ABDU") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_sy_abdu*100) %>%
  mutate(species = "ABDU",
         age = "SY")

sum_outcomes_asy_abdu <- summary4 %>% filter(age == "ASY" & species == "ABDU") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_asy_abdu*100) %>%
  mutate(species = "ABDU",
         age = "ASY")

sum_outcomes_sy_mall <- summary4 %>% filter(age == "SY" & species == "MALL") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_sy_mall*100) %>%
  mutate(species = "MALL",
         age = "SY")

sum_outcomes_asy_mall <- summary4 %>% filter(age == "ASY" & species == "MALL") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_asy_mall*100) %>%
  mutate(species = "MALL",
         age = "ASY")

#sum_outcomes_by_age <- merge(sum_outcomes_sy, sum_outcomes_asy, by = "status_comb")

sum_outcomes_by_age <- rbind(sum_outcomes_asy_mall, sum_outcomes_sy_mall, sum_outcomes_asy_abdu, sum_outcomes_sy_abdu) 

png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_OutcomesByAge_NestingBirdsOnly.png")
ggplot(sum_outcomes_by_age, aes(x = status_comb)) +
  geom_point(aes(y=percent.sy, color = "SY"), size = 4, shape=15)+#, shape = 21, color = "magenta", fill="magenta", size=4) +
  geom_point(aes(y=percent.asy, color = "ASY"), size = 4, shape=16)+#, shape = 21, color = "black" , fill="black", size=4) +
  labs(
    x = "Nesting Outcome",
    y = "Percent of Birds") +
  scale_color_manual(
    name = "Age",  # Custom legend title
    values = c("ASY" = "darkblue", "SY" = "darkred"), # Custom colors for each group
    labels = c("ASY", "SY") # Custom labels for legend entries
  ) +
  ylim(0,100)
# theme_minimal()
dev.off()

# nesting outcomes by year (nesting birds only)
summary4 %>% group_by(species, year, status_comb) %>% count()

summary4 %>% group_by(species, year) %>% count()

# mall
n_2022_mall <- 186
n_2023_mall <- 234
n_2024_mall <- 286
n_2025_mall <- 208
# abdu
n_2021_abdu <- 15
n_2022_abdu <- 84
n_2023_abdu <- 125
n_2024_abdu <- 169
n_2025_abdu <- 55

sum_outcomes_2021_abdu <- summary4 %>% filter(year == "2021") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_2021_abdu*100) %>%
  mutate(species="ABDU", year="2021")

sum_outcomes_2022_abdu <- summary4 %>% filter(year == "2022" & species == "ABDU") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_2022_abdu*100) %>%
  mutate(species="ABDU", year="2022")

sum_outcomes_2023_abdu <- summary4  %>% filter(year == "2023"& species == "ABDU") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_2023_abdu*100) %>%
  mutate(species="ABDU", year="2023")

sum_outcomes_2024_abdu <- summary4  %>% filter(year == "2024" & species == "ABDU") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_2024_abdu*100) %>%
  mutate(species="ABDU", year="2024")

sum_outcomes_2025_abdu <- summary4  %>% filter(year == "2025" & species == "ABDU") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_2025_abdu*100) %>%
  mutate(species="ABDU", year="2025")

sum_outcomes_2022_mall <- summary4 %>% filter(year == "2022" & species == "MALL") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_2022_mall*100) %>%
  mutate(species="MALL", year="2022")

sum_outcomes_2023_mall <- summary4  %>% filter(year == "2023"& species == "MALL") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_2023_mall*100) %>%
  mutate(species="MALL", year="2023")

sum_outcomes_2024_mall <- summary4  %>% filter(year == "2024" & species == "MALL") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_2024_mall*100) %>%
  mutate(species="MALL", year="2024")

sum_outcomes_2025_mall <- summary4  %>% filter(year == "2025" & species == "MALL") %>%
  group_by(status_comb) %>%
  summarise(n=n(),
            percent=n()/n_2025_mall*100) %>%
  mutate(species="MALL", year="2025")

sum_outcomes_by_year <- rbind(sum_outcomes_2022_mall, sum_outcomes_2023_mall, sum_outcomes_2024_mall, sum_outcomes_2025_mall,
                                       sum_outcomes_2021_abdu, sum_outcomes_2022_abdu, sum_outcomes_2023_abdu, sum_outcomes_2024_abdu,
                                       sum_outcomes_2025_abdu)

# sum_outcomes_by_year <- merge(sum_outcomes_2021, sum_outcomes_2022, by = "status_comb")
# sum_outcomes_by_year <- merge(sum_outcomes_by_year, sum_outcomes_2023, by = "status_comb")
# sum_outcomes_by_year <- merge(sum_outcomes_by_year, sum_outcomes_2024, by = "status_comb")
# sum_outcomes_by_year <- merge(sum_outcomes_by_year, sum_outcomes_2025, by = "status_comb")

png("plots_EggLayingAndIncubationCombined_14Sept2025/MALL_OutcomesByYear_NestingBirdsOnly.png")
ggplot(sum_outcomes_by_year, aes(x = status_comb)) +
  geom_point(aes(y=percent.2022, color = "2022"), size = 4, shape=15)+#, shape = 21, color = "magenta", fill="magenta", size=4) +
  geom_point(aes(y=percent.2023, color = "2023"), size = 4, shape=16)+#, shape = 21, color = "black" , fill="black", size=4) +
  geom_point(aes(y=percent.2024, color = "2024"), size = 4, shape=17)+#, shape = 21, color = "magenta", fill="magenta", size=4) +
  geom_point(aes(y=percent.2025, color = "2025"), size = 4, shape=18)+#, shape = 21, color = "black" , fill="black", size=4) +
  labs(
    x = "Nesting Outcome",
    y = "Percent of Birds") +
  scale_color_manual(
    name = "Year",  # Custom legend title
    values = c("2022" = "darkblue", "2023" = "darkred", "2024" = "darkgreen", "2025" = "darkorange"), # Custom colors for each group
    labels = c("2022", "2023", "2024", "2025") # Custom labels for legend entries
  ) +
  ylim(0,100)
# theme_minimal()
dev.off()
