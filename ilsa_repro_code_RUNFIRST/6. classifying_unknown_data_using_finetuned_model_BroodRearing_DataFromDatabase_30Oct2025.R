
#**********************************************************************************************************************************
#**********************************************************************************************************************************
# Project: Reproductive Metrics - Machine Learning
# Date: 30 July 2025
# Author: Ilsa Griebel
# Description: Applying finetuned brood rearing models to unknown black duck/mallard data and 
#               plotting to asses function
# 1. Summarize daily metrics over two 15-day windows for input to models
# 2. Apply finetuned brood rearing models to unknown black duck/mallard data
# 3. Plot to assess how well the model is functioning 
# 4. Summarize results for manuscript
#**********************************************************************************************************************************
#**********************************************************************************************************************************
# load libraries

library(dplyr)
#library(roll)
library(zoo)
library(RPostgres)
library(lubridate)
library(stringr)


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

#### 1. Summarize daily metrics over two 15-day windows for input to algorithm ####

# load unclassified data (ACC)
breeding.metrics <- tbl(conn, "daily_odba_absx_propfly_30Jun26") %>%
  collect()

# check data
colnames(breeding.metrics)

# remove ACC daily summary metrics calculate using <25% of daily fixes
breeding.metrics <- breeding.metrics %>% filter(prop_fixes >= 0.25)

# format data (add bird_year columns, format date as a date, select columns)
breeding.metrics2 <- breeding.metrics %>% 
  mutate(birdid_year=(paste(birdid, year, sep="_")),
         date=as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::select(c("birdid_year", "date", "birdid", "year", "device_id", 
                  "mean_ODBA", "mean_abs_x", "corrected_prop_fly"
  ))

# load unclassified data (GPS)
breeding.metrics <- tbl(conn, "daily_ddist_nsd_mcp_30Jun26") %>%
  collect()

# check data
colnames(breeding.metrics)

# format data (add bird_year columns, format date as a date, select columns)
breeding.metrics <- breeding.metrics %>% 
  mutate(birdid_year=(paste(birdid, year, sep="_")),
         date=as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::select(c("birdid_year", "date", "birdid", "year", "median_ddist", "mcp75area", "mean_nsd"
  ))

colnames(breeding.metrics)
summary(breeding.metrics)

colnames(breeding.metrics2)
summary(breeding.metrics2)

# combine gps and acc data
breeding.metrics2 <- merge(breeding.metrics2, breeding.metrics, by = c("birdid_year", "date", "birdid", "year"))

colnames(breeding.metrics2)
summary(breeding.metrics2)

n2 <- unique(breeding.metrics2$birdid_year) # 1230 bird-years (MALL), 858 (ABDU)

rm(breeding.metrics)


# add incubation start and end dates
 dates<- tbl(conn, "MALL2022to2025_StartEndDates_IncubationEgg_NestLocation_30Jun26") %>% # MALL
  collect() %>%
    dplyr::select(c("birdid_year_nest", "status_comb", "inc_start", "inc_end", "succ_inc_range_start", "succ_inc_range_end")) %>%
# calculate duration of incubation
  mutate(inc_end=as.Date(inc_end),
         inc_start=as.Date(inc_start),
         succ_inc_range_start=as.Date(succ_inc_range_start),
         succ_inc_range_end=as.Date(succ_inc_range_end),
         inc_duration=as.numeric(inc_end-inc_start),
         birdid_year=substring(birdid_year_nest, 1,15)) %>% # 1,15 for band_year, 1,11 for device_year
# only keep nests with incubation duration longer than 20 days ### NO LONGER DOING!
#filter(inc_duration >= 20) %>%
  
  # only keep birds classified as egg_laying_hatched
  filter(status_comb == "egg_laying_hatched") %>%
  # remove duplicates
 dplyr:: select(-birdid_year_nest) %>%
  distinct(.keep_all = TRUE)

n3 <- unique(dates$birdid_year) # 72 black ducks, 146 mallards 

# dates contains more than one nest attempt for some birds (black ducks, n = 72 but number of rows = 81) (mall, n = 146 but number of rows = 208)
# only keep the attempt that overlaps with days classified as successful hatch
dates2 <- dates %>%
  #mutate(succ_inc_range_end=succ_inc_range_end+26) %>%
  filter(int_overlaps(interval(inc_start, inc_end), interval(succ_inc_range_start, (succ_inc_range_end+26))))

n4 <- unique(dates2$birdid_year) # 71 bird years for black ducks (3 missing dates that were classified as hatched), 138 mallards, 12 missing dates that were classified as hatched

# look at distribution of inc_duration (5 birds with duration > 33, remove as failed nests??? - MALL; 1 birds with duration > 33 (34 days) - ABDU) ## NOT GOING TO DO!
hist(dates2$inc_duration)

 
 # load dates for max absx window
 max_absx <-  tbl(conn, "max_absx_window_dates_30June26") %>%
   collect() %>% 
   filter(status_comb == "egg_laying_hatched") %>%
   dplyr::select("birdid_year", "status_comb", "succ_inc_range_start", "succ_inc_range_end") %>%
   rename(max_absx_window_start=succ_inc_range_start,
          max_absx_window_end=succ_inc_range_end) %>%
   mutate(max_absx_window_end=as.Date(max_absx_window_end),
          max_absx_window_start=as.Date(max_absx_window_start)) %>%
   mutate(max_absx_window_end=max_absx_window_end+26)
 
 n5 <- unique(max_absx$birdid_year) # 74 black ducks and 150 mallards (has all bird-years that were classified as hatched!)
 
 # merge max absx df and dates df (keeping all rows from max absx df)
 dates <- merge(max_absx, dates2, by = "birdid_year", all.x = TRUE)
 
 # if incubation dates are NA, use dates of max absx window
 dates <- dates %>% 
   mutate(inc_start=ifelse(is.na(inc_start)== TRUE, max_absx_window_start, inc_start),
          inc_end=ifelse(is.na(inc_end)== TRUE, max_absx_window_end, inc_end)) %>%
   mutate(inc_start=as.Date(inc_start),
          inc_end=as.Date(inc_end)) %>%
   mutate(inc_duration=as.numeric(inc_end-inc_start)) # recalculate inc_duration so birds using absx won't have na's

breeding.metrics2 <- merge(breeding.metrics2, dates, by = "birdid_year")

n6 <- unique(breeding.metrics2$birdid_year) # 74 bird-years (abdu), 150 bird-years (mall)

# filter to post-hatch/incubation
breeding.metrics2 <- breeding.metrics2[order(breeding.metrics2$birdid_year, breeding.metrics2$date), ]

## 2 algorithms
breeding.metrics_early <- breeding.metrics2 %>% filter(date > inc_end & date <= as.Date(inc_end)+16) ## for mall, changing to 16 and then will slice to first 15, so one bird that has one missing date will  end up with 15 days
breeding.metrics_late <- breeding.metrics2 %>% filter(date > as.Date(inc_end)+15 & date <= as.Date(inc_end)+31) ## for abdu, changing to 31 and then will slice to first 15, so that one bird that has one missing date will end up with 15 days

n7 <- unique(breeding.metrics_early$birdid_year) # 141 bird-years (MALL), 72 bird-years (ABDU)
n8 <- unique(breeding.metrics_late$birdid_year) # 134 bird-years (MALL), 71 bird-years (ABDU)

# two birds missing for black ducks.. check who it is (2167-55580_2023, no battery issues and data abruptly stops at end of incubation (July 23), 2257-60237_2024, no battery issues and data abruptly stops at end of incubation (May 29))
# 9 birds missing for mallards..  died at end of incubation: 2247-26288_2022, 2257-01860_2024, 2257-44205_2023, 2257-44605_2023, 2257-46381_2024, 2257-92236_2025, 2287-01141_2024, 2287-75622_2025,  
# data ends  abruptly: 2257-59920_2025
missing_data <- as.data.frame(setdiff(unique(breeding.metrics2$birdid_year), unique(breeding.metrics_early$birdid_year)))
colnames(missing_data)[1] <- "birdid_year"
missing_data$n <- 0

# everyone has an extra day so slicing to 15 (mallards)
breeding.metrics_early <- breeding.metrics_early %>% group_by(birdid_year) %>%
  slice_head(n = 15)

#### only keep birds with full 15 days for early period (ABDU all have exactly 15 days!) 
summary_early <- breeding.metrics_early %>% group_by(birdid_year) %>% count()
hist(summary_early$n) 

# 7 mallards and 1 black duck with less than 15 days; add as fail early birds (with incomplete data)
failed_early_incomplete_data <- summary_early %>% filter(n < 15)

# add missing data birds to failed early incomplete data df
failed_early_incomplete_data <- rbind(failed_early_incomplete_data, missing_data)

summary_early_15donly <- summary_early %>% filter(n == 15)
breeding.metrics_early <- breeding.metrics_early %>% 
  filter(birdid_year %in% summary_early_15donly$birdid_year)

#### only keep birds with full 15 days for late period (3 abdu with less than 15: 
# 2257-01809_2024 - 14d - missing one day, just add one day in for this bird!, 
# 2287-32121_2025 - 12d, died before 30d, 
# 2247-74875_20222 -2 d, died June 17 before 30d) 

# slicing to first 15 days because almost all abdu have 16 days to ensure that 2257-01809_2024 has 15 days (was missing one day of data)
breeding.metrics_late <- breeding.metrics_late %>% group_by(birdid_year) %>%
  slice_head(n = 15)

summary_late <- breeding.metrics_late %>% group_by(birdid_year) %>% count()
hist(summary_late$n)

# 6 mallards and 2 abdu with less than 15 days; should add as fail late birds (with incomplete data)
failed_late_incomplete_data <- summary_late %>% filter(n < 15)

summary_late_15donly <- summary_late %>% filter(n == 15)
breeding.metrics_late <- breeding.metrics_late %>% 
  filter(birdid_year %in% summary_late_15donly$birdid_year)

# 6 mallards and 2 black ducks in early but not in late brood rearing
setdiff(breeding.metrics_early$birdid_year, breeding.metrics_late$birdid_year)
setdiff(breeding.metrics_late$birdid_year, breeding.metrics_early$birdid_year)

# calculate mean/sd for input variables (Absx_mean + mcp_mean + ddist_mean + nsd_mean + Absx_var + fly_median) = early brood rearing
  # (mcp_mean + ddist_mean + nsd_mean + fly_median) = late brood rearing
## 2 algorithm approach
breeding.metrics_15d_early <- breeding.metrics_early %>% group_by(birdid_year) %>%
  summarize(Absx_mean=mean(mean_abs_x),
            Absx_var=sd(mean_abs_x),
            mcp_mean=mean(mcp75area),
            ddist_mean=mean(median_ddist),
            nsd_mean=mean(mean_nsd),
            fly_mean=mean(corrected_prop_fly),
            inc_duration=mean(inc_duration))
breeding.metrics_15d_late<- breeding.metrics_late %>% group_by(birdid_year) %>%
  summarize(mcp_mean=mean(mcp75area),
            ddist_mean=mean(median_ddist),
            nsd_mean=mean(mean_nsd),
            fly_mean=mean(corrected_prop_fly),
            inc_duration=mean(inc_duration))

# remove NA's
breeding.metrics_15d_early <- na.omit(breeding.metrics_15d_early)
breeding.metrics_15d_late <- na.omit(breeding.metrics_15d_late)

  
#### 2. Apply finetuned incubation model to unknwon black duck/mallard data ####
# load first algorithm
# early brood rearing : fine tuned rf model using a tune length of 30 (7 Sept 2025)
# load("rf_basemodel_EarlyBroodRearing_15dayinterval_AbsxMeanVarflyMedian_DdistMcpAndNsdMean.RData") # model_svm
# # late brood rearing : base rf model (8 Sept 2025)
# load("rf_tunelength30_LateBroodRearing_15dayinterval_flymedian_DdistMcpAndNsdMean.RData") # tuned_svm
# early brood rearing - uncalibrated ACC (30 Oct 2025)
#load("rf_basemodel_EarlyBroodRearing_15dayinterval_AbsxMeanVar_flyMedian_Ddist75McpAndNsdMean_uncalibACC.RData") # model_svm
# late brood rearing - uncalibrated ACC (30 Oct 2025)
#load("rf_basemodel_LateBroodRearing_15dayinterval_flyMedian_Ddist75McpAndNsdMean_uncalibACC.RData") # model_svm


# early brood rearing - uncalibrated ACC (30 Oct 2025)
load("rf_basemodel_EarlyBroodRearing_15dayinterval_AbsxMeanVar_flyMean_Ddist75McpAndNsdMean_uncalibACC.RData") # model_svm

# use to predict on unknown data
# early brood rearing
pred_abdu_early <- predict(model_svm, newdata = breeding.metrics_15d_early) 

# load second algorithm
# late brood rearing - uncalibrated ACC (30 Oct 2025)
load("rf_TunedModel_LateBroodRearing_15dayinterval_flyMean_Ddist75McpAndNsdMean_uncalibACC.RData") # tuned_svm
# late brood rearing
pred_abdu_late <- predict(tuned_svm, newdata = breeding.metrics_15d_late) 

# combine predictions with breeding metrics
## 2 algorithm approach
breeding.labels_early <- data.frame(status = pred_abdu_early)
breeding.metrics_15d.labeled_early <- cbind(breeding.metrics_15d_early, breeding.labels_early) 
breeding.metrics_15d.labeled_early <- breeding.metrics_15d.labeled_early %>% rename(status_early=status)

breeding.labels_late <- data.frame(status = pred_abdu_late)
breeding.metrics_15d.labeled_late <- cbind(breeding.metrics_15d_late, breeding.labels_late) 
breeding.metrics_15d.labeled_late <- breeding.metrics_15d.labeled_late %>% rename(status_late=status) 
breeding.metrics_15d.labeled <- merge(breeding.metrics_15d.labeled_early, breeding.metrics_15d.labeled_late, 
                                      by = "birdid_year") 
breeding.metrics_15d.labeled <- breeding.metrics_15d.labeled %>%
  mutate(status_early_late = paste(status_early, status_late, sep = "_"))

breeding.metrics_15d.labeled_early %>% group_by(status_early) %>% count()
breeding.metrics_15d.labeled_late %>% group_by(status_late) %>% count()
breeding.metrics_15d.labeled %>% group_by(status_early_late) %>% count()

# # how many instances of failed incubations that were subsequently classified as successful early brood? 6 mallards and 1 abdu ### NO LONGER INCLUDING FAILED INCUBATIONS
# failed_inc_succ_early_brood <- breeding.metrics_15d.labeled %>%
#   filter(status_comb.x == "egg_laying_failed") %>%
#   filter(status_early == "successful_brood_early")


# for 15-d plots, merge breeding.metrics_ early and _late with early and late labels
## 2 algorithm approach
labels_ids_early <- cbind(breeding.metrics_15d_early$birdid_year, breeding.labels_early$status)
labels_id_early <- as.data.frame(labels_ids_early) %>% rename(birdid_year=V1, status=V2) %>%
  mutate(status=ifelse(status == 1, "no brood", "successful - early brood"))
breeding.metrics_early_labeled <- merge(breeding.metrics_early, labels_id_early, by = "birdid_year")

labels_ids_late <- cbind(breeding.metrics_15d_late$birdid_year, breeding.labels_late$status)
labels_id_late <- as.data.frame(labels_ids_late) %>% rename(birdid_year=V1, status=V2) %>%
  mutate(status=ifelse(status == 1, "no brood", "successful - late brood"))
breeding.metrics_late_labeled <- merge(breeding.metrics_late, labels_id_late, by = "birdid_year")

breeding.metrics_labeled <- rbind(breeding.metrics_early_labeled, breeding.metrics_late_labeled)

# # saving old classified data when I filtered out birds without data to July 1
# temp <- tbl(conn, "abdu_broodrearing_classified") %>%
#   collect()
# dbWriteTable(conn, "abdu_broodrearing_classified_old", temp, append = TRUE, row.names = FALSE)


# save labeled data (before post-algorithm filtering)
write.csv(breeding.metrics_15d.labeled, "results/MALL_2022to2025_EarlyAndLateBroodRearingClassified_AllBirds_30June2026.csv")

#dbRemoveTable(conn, "mall_broodrearing_classified_ILSA")
dbWriteTable(conn, "mall_broodrearing_classified_30Jun26", breeding.metrics_15d.labeled, append = TRUE, row.names = FALSE)

## add missing data birds in

# first add status to missing data early dataframe
failed_early_incomplete_data$status_early <- "no_brood"
failed_early_incomplete_data$status_late <- "no_brood"
failed_early_incomplete_data$status_early_late <- "no_brood_no_brood"

# next add status to missing data late dataframe
failed_late_incomplete_data$status_early <- "successful_brood_early"
failed_late_incomplete_data$status_late <- "no_brood"
failed_late_incomplete_data$status_early_late <- "successful_brood_early_no_brood"

# combine two missing data dfs together
failed_incomplete_data <- rbind(failed_early_incomplete_data, failed_late_incomplete_data)

# add extra rows to labeled dataframe
breeding.metrics_15d.labeled_v2 <- rbind(breeding.metrics_15d.labeled, matrix(NA, nrow = nrow(failed_incomplete_data), ncol = ncol(breeding.metrics_15d.labeled), dimnames = list(NULL, names(breeding.metrics_15d.labeled))))

# loop through rows of incomplete data df and add data to labeled df
for (i in 1:nrow(failed_incomplete_data)) {
  breeding.metrics_15d.labeled_v2$birdid_year[(nrow(breeding.metrics_15d.labeled)+i)] <- failed_incomplete_data$birdid_year[i]
  breeding.metrics_15d.labeled_v2$status_early[(nrow(breeding.metrics_15d.labeled)+i)] <- failed_incomplete_data$status_early[i]
  breeding.metrics_15d.labeled_v2$status_late[(nrow(breeding.metrics_15d.labeled)+i)] <- failed_incomplete_data$status_late[i]
  breeding.metrics_15d.labeled_v2$status_early_late[(nrow(breeding.metrics_15d.labeled)+i)] <- failed_incomplete_data$status_early_late[i]
}

# # remove incubation attempts between 20 and 24 d (inclusive) that were subsequently classified as no brood #### NO LONGER DOING
# to_remove <- breeding.metrics_15d.labeled %>% filter(inc_duration.x < 25) %>%
#   filter(status_early == "no_brood") %>% filter(status_comb.x != "egg_laying_hatched")
# 
# breeding.metrics_15d.labeled <- breeding.metrics_15d.labeled %>% filter(!birdid_year %in% to_remove$birdid_year) # 21 mallards, 7 abdu

# # remove birds with incubation duration greater than 33 ### NO LONGER DOING
# to_remove <- breeding.metrics_15d.labeled %>% filter(inc_duration.y > 33) # 6 mallards, 1 abdu
# 
# breeding.metrics_15d.labeled <- breeding.metrics_15d.labeled %>% filter(!birdid_year %in% to_remove$birdid_year)

# # saving old classified data when I filtered out birds without data to July 1
# temp <- tbl(conn, "abdu_broodrearing_classified_PostAlgorithmFilteringComplete") %>%
#   collect()
# dbWriteTable(conn, "abdu_broodrearing_classified_PostAlgorithmFilteringComplete_old", temp, append = TRUE, row.names = FALSE)

# save labeled data (after post-algorithm filtering)
write.csv(breeding.metrics_15d.labeled_v2, "results/MALL_2022to2025_EarlyAndLateBroodRearingClassified_AllBirds_30Jun2026.csv")
write.csv(breeding.metrics_15d.labeled_v2, "WildBirdData_ClassifiedDatasets_22Aug2025/ABDU_2021to2025_EarlyAndLateBroodRearingClassified_AllBirds_3Apr2026.csv")

dbRemoveTable(conn, name = "mall_broodrearing_classified_IncompleteDataBirdsIncluded_ILSA")
dbWriteTable(conn, "mall_broodrearing_classified_IncompleteDataBirdIncluded_30Jun26", breeding.metrics_15d.labeled_v2, append = TRUE, row.names = FALSE)




# look at summary of results post-filtering steps
breeding.metrics_15d.labeled_v2 %>% group_by(status_early_late) %>% count()

breeding.metrics_15d.labeled <- breeding.metrics_15d.labeled_v2

## missing one bird from black ducks and one from mallards (2247-80318_2022), figure out who and why?! It was because inc_duration was NA for birds that used max ABSX window dates; FIXED!! 
setdiff(unique(max_absx$birdid_year), unique(breeding.metrics_15d.labeled$birdid_year))

#### assigning end dates for failed broods ####
# first get list of all broods that failed early and all broods that failed late
fail_early <- breeding.metrics_15d.labeled %>% filter(status_early=="no_brood") # 32 bird years
fail_late <- breeding.metrics_15d.labeled %>% filter(status_late=="no_brood") # 45 bird years

# get daily summary metrics for failed broods for only days within the window that was classified as no brood
daily_metrics_fail_early  <- breeding.metrics_early %>% filter(birdid_year %in% fail_early$birdid_year)
daily_metrics_fail_late  <- breeding.metrics_late %>% filter(birdid_year %in% fail_late$birdid_year)

# adding last five days of early brood period to late brood failures to expand the window that is searched when assigning end date
 last_five <- daily_metrics_fail_early %>% group_by(birdid_year) %>%
   slice_tail(n = 5)
 daily_metrics_fail_late <- rbind(daily_metrics_fail_late, last_five)

# load quantiles for daily summary metrics of successful brood rearing birds from training dataset
quantiles_early <- read.csv("WildBirdData_ClassifiedDatasets_22Aug2025/quantiles_of_daily_metrics_for_early_brood_rearing_TrainingBirdsOnly_5Nov2025.csv")
quantiles_late <- read.csv("WildBirdData_ClassifiedDatasets_22Aug2025/quantiles_of_daily_metrics_for_late_brood_rearing_TrainingBirdsOnly_5Nov2025.csv")

### Abbey preferred approach is 2 yes's and 85% quantile so commented out other options!

# # fail early - 80 q
# daily_metrics_fail_early_80q <- daily_metrics_fail_early %>%
#   mutate(ddist_greater_than_80q=ifelse(median_ddist > quantiles_early$median_ddist[1], "Y", "N"),
#          nsd_greater_than_80q=ifelse(mean_nsd > quantiles_early$mean_nsd[1], "Y", "N"),
#          mcp_greater_than_80q=ifelse(mcp75area > quantiles_early$mcp75area[1], "Y", "N"),
#          fly_greater_than_80q=ifelse(corrected_prop_fly > quantiles_early$corrected_prop_fly[1], "Y", "N")) %>%
#   select("birdid_year", "inc_end", "date", "ddist_greater_than_80q", "nsd_greater_than_80q", "mcp_greater_than_80q", "fly_greater_than_80q")
# 
# # count number of columns per row with Y's
# search_char <- "Y"
# daily_metrics_fail_early_80q$count_Y <- apply(daily_metrics_fail_early_80q , 1, function(row) {
#   sum(grepl(search_char, row))
# })
# 
# # filter to rows containing at least one Y
# daily_metrics_fail_early_80q <- daily_metrics_fail_early_80q %>%
#   filter(count_Y >= 1)
# 
# # find earliest day with 1 Y
# end_dates_80q_1Y_fail_early <- daily_metrics_fail_early_80q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_80q_1Y=min(date))
# 
# # filter to rows containing at least two Y's
# daily_metrics_fail_early_80q <- daily_metrics_fail_early_80q %>%
#   filter(count_Y >= 2)
# 
# # find earliest day with at least 2 Y's
# end_dates_80q_2Y_fail_early <- daily_metrics_fail_early_80q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_80q_2Y=min(date))
# 
# # filter to rows containing at least three Y's
# daily_metrics_fail_early_80q <- daily_metrics_fail_early_80q %>%
#   filter(count_Y >= 3)
# 
# # find earliest day with at least 3 Y's
# end_dates_80q_3Y_fail_early <- daily_metrics_fail_early_80q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_80q_3Y=min(date))
# 
# # filter to rows containing at least four Y's
# daily_metrics_fail_early_80q <- daily_metrics_fail_early_80q %>%
#   filter(count_Y >= 4)
# 
# # find earliest day with at least 4 Y's
# end_dates_80q_4Y_fail_early <- daily_metrics_fail_early_80q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_80q_4Y=min(date))


# fail early - 85 q
daily_metrics_fail_early_85q <- daily_metrics_fail_early %>%
  mutate(ddist_greater_than_85q=ifelse(median_ddist > quantiles_early$median_ddist[2], "Y", "N"),
         nsd_greater_than_85q=ifelse(mean_nsd > quantiles_early$mean_nsd[2], "Y", "N"),
         mcp_greater_than_85q=ifelse(mcp75area > quantiles_early$mcp75area[2], "Y", "N"),
         fly_greater_than_85q=ifelse(corrected_prop_fly > quantiles_early$corrected_prop_fly[2], "Y", "N")) %>%
  select("birdid_year", "inc_end", "date", "ddist_greater_than_85q", "nsd_greater_than_85q", "mcp_greater_than_85q", "fly_greater_than_85q")

# count number of columns per row with Y's
search_char <- "Y"
daily_metrics_fail_early_85q$count_Y <- apply(daily_metrics_fail_early_85q , 1, function(row) {
  sum(grepl(search_char, row))
})

# # filter to rows containing at least one Y
# daily_metrics_fail_early_85q <- daily_metrics_fail_early_85q %>%
#   filter(count_Y >= 1)
# 
# # find earliest day with 1 Y
# end_dates_85q_1Y_fail_early <- daily_metrics_fail_early_85q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_85q_1Y=min(date))

# filter to rows containing at least two Y's
daily_metrics_fail_early_85q <- daily_metrics_fail_early_85q %>%
  filter(count_Y >= 2)

# find earliest day with at least 2 Y's
end_dates_85q_2Y_fail_early <- daily_metrics_fail_early_85q %>% group_by(birdid_year) %>% 
  summarize(brood_end_date_85q_2Y=min(date))

# # filter to rows containing at least three Y's
# daily_metrics_fail_early_85q <- daily_metrics_fail_early_85q %>%
#   filter(count_Y >= 3)
# 
# # find earliest day with at least 3 Y's
# end_dates_85q_3Y_fail_early <- daily_metrics_fail_early_85q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_85q_3Y=min(date))
# 
# # filter to rows containing at least four Y's
# daily_metrics_fail_early_85q <- daily_metrics_fail_early_85q %>%
#   filter(count_Y >= 4)
# 
# # find earliest day with at least 4 Y's
# end_dates_85q_4Y_fail_early <- daily_metrics_fail_early_85q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_85q_4Y=min(date))

# # fail early - 90 q
# daily_metrics_fail_early_90q <- daily_metrics_fail_early %>%
#   mutate(ddist_greater_than_90q=ifelse(median_ddist > quantiles_early$median_ddist[3], "Y", "N"),
#          nsd_greater_than_90q=ifelse(mean_nsd > quantiles_early$mean_nsd[3], "Y", "N"),
#          mcp_greater_than_90q=ifelse(mcp75area > quantiles_early$mcp75area[3], "Y", "N"),
#          fly_greater_than_90q=ifelse(corrected_prop_fly > quantiles_early$corrected_prop_fly[3], "Y", "N")) %>%
#   select("birdid_year", "inc_end", "date", "ddist_greater_than_90q", "nsd_greater_than_90q", "mcp_greater_than_90q", "fly_greater_than_90q")
# 
# # count number of columns per row with Y's
# search_char <- "Y"
# daily_metrics_fail_early_90q$count_Y <- apply(daily_metrics_fail_early_90q , 1, function(row) {
#   sum(grepl(search_char, row))
# })
# 
# # filter to rows containing at least one Y
# daily_metrics_fail_early_90q <- daily_metrics_fail_early_90q %>%
#   filter(count_Y >= 1)
# 
# # find earliest day with 1 Y
# end_dates_90q_1Y_fail_early <- daily_metrics_fail_early_90q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_90q_1Y=min(date))
# 
# # filter to rows containing at least two Y's
# daily_metrics_fail_early_90q <- daily_metrics_fail_early_90q %>%
#   filter(count_Y >= 2)
# 
# # find earliest day with at least 2 Y's
# end_dates_90q_2Y_fail_early <- daily_metrics_fail_early_90q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_90q_2Y=min(date))
# 
# # filter to rows containing at least three Y's
# daily_metrics_fail_early_90q <- daily_metrics_fail_early_90q %>%
#   filter(count_Y >= 3)
# 
# # find earliest day with at least 3 Y's
# end_dates_90q_3Y_fail_early <- daily_metrics_fail_early_90q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_90q_3Y=min(date))
# 
# # filter to rows containing at least four Y's
# daily_metrics_fail_early_90q <- daily_metrics_fail_early_90q %>%
#   filter(count_Y >= 4)
# 
# # find earliest day with at least 4 Y's
# end_dates_90q_4Y_fail_early <- daily_metrics_fail_early_90q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_90q_4Y=min(date))

# # fail early - 95 q
# daily_metrics_fail_early_95q <- daily_metrics_fail_early %>%
#   mutate(ddist_greater_than_95q=ifelse(median_ddist > quantiles_early$median_ddist[4], "Y", "N"),
#          nsd_greater_than_95q=ifelse(mean_nsd > quantiles_early$mean_nsd[4], "Y", "N"),
#          mcp_greater_than_95q=ifelse(mcp75area > quantiles_early$mcp75area[4], "Y", "N"),
#          fly_greater_than_95q=ifelse(corrected_prop_fly > quantiles_early$corrected_prop_fly[4], "Y", "N")) %>%
#   select("birdid_year", "inc_end", "date", "ddist_greater_than_95q", "nsd_greater_than_95q", "mcp_greater_than_95q", "fly_greater_than_95q")
# 
# # count number of columns per row with Y's
# search_char <- "Y"
# daily_metrics_fail_early_95q$count_Y <- apply(daily_metrics_fail_early_95q , 1, function(row) {
#   sum(grepl(search_char, row))
# })
# 
# # filter to rows containing at least one Y
# daily_metrics_fail_early_95q <- daily_metrics_fail_early_95q %>%
#   filter(count_Y >= 1)
# 
# # find earliest day with 1 Y
# end_dates_95q_1Y_fail_early <- daily_metrics_fail_early_95q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_95q_1Y=min(date))
# 
# # filter to rows containing at least two Y's
# daily_metrics_fail_early_95q <- daily_metrics_fail_early_95q %>%
#   filter(count_Y >= 2)
# 
# # find earliest day with at least 2 Y's
# end_dates_95q_2Y_fail_early <- daily_metrics_fail_early_95q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_95q_2Y=min(date))
# 
# # filter to rows containing at least three Y's
# daily_metrics_fail_early_95q <- daily_metrics_fail_early_95q %>%
#   filter(count_Y >= 3)
# 
# # find earliest day with at least 3 Y's
# end_dates_95q_3Y_fail_early <- daily_metrics_fail_early_95q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_95q_3Y=min(date))
# 
# # filter to rows containing at least four Y's
# daily_metrics_fail_early_95q <- daily_metrics_fail_early_95q %>%
#   filter(count_Y >= 4)
# 
# # find earliest day with at least 4 Y's
# end_dates_95q_4Y_fail_early <- daily_metrics_fail_early_95q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_95q_4Y=min(date))

# # combine all 1 Y's into one dataframe
# end_dates_1Y_fail_early <- merge(end_dates_80q_1Y_fail_early, end_dates_85q_1Y_fail_early, by="birdid_year", all = TRUE)
# end_dates_1Y_fail_early <- merge(end_dates_1Y_fail_early, end_dates_90q_1Y_fail_early, by="birdid_year", all = TRUE)
# end_dates_1Y_fail_early <- merge(end_dates_1Y_fail_early, end_dates_95q_1Y_fail_early, by="birdid_year", all = TRUE)

# # combine all 2 Y's into one dataframe
# end_dates_2Y_fail_early <- merge(end_dates_80q_2Y_fail_early, end_dates_85q_2Y_fail_early, by="birdid_year", all = TRUE)
# end_dates_2Y_fail_early <- merge(end_dates_2Y_fail_early, end_dates_90q_2Y_fail_early, by="birdid_year", all = TRUE)
# end_dates_2Y_fail_early <- merge(end_dates_2Y_fail_early, end_dates_95q_2Y_fail_early, by="birdid_year", all = TRUE)

# # combine all 3 Y's into one dataframe
# end_dates_3Y_fail_early <- merge(end_dates_80q_3Y_fail_early, end_dates_85q_3Y_fail_early, by="birdid_year", all = TRUE)
# end_dates_3Y_fail_early <- merge(end_dates_3Y_fail_early, end_dates_90q_3Y_fail_early, by="birdid_year", all = TRUE)
# end_dates_3Y_fail_early <- merge(end_dates_3Y_fail_early, end_dates_95q_3Y_fail_early, by="birdid_year", all = TRUE)
# 
# # combine all 4 Y's into one dataframe
# end_dates_4Y_fail_early <- merge(end_dates_80q_4Y_fail_early, end_dates_85q_4Y_fail_early, by="birdid_year", all = TRUE)
# end_dates_4Y_fail_early <- merge(end_dates_4Y_fail_early, end_dates_90q_4Y_fail_early, by="birdid_year", all = TRUE)
# end_dates_4Y_fail_early <- merge(end_dates_4Y_fail_early, end_dates_95q_4Y_fail_early, by="birdid_year", all = TRUE)

# add inc_end date and brood_late_start_date 
inc_end_dates <- breeding.metrics_early %>% group_by(birdid_year) %>%
  summarise(inc_end=mean(inc_end))

inc_end_dates$brood_early_start <- inc_end_dates$inc_end + 1

end_dates_fail_early <- merge(end_dates_85q_2Y_fail_early, inc_end_dates, by = "birdid_year")
end_dates_fail_early <- end_dates_fail_early %>%
  relocate(brood_early_start, .after = birdid_year) %>%
  relocate(inc_end, .after = birdid_year)

# end_dates_1Y_fail_early <- merge(end_dates_1Y_fail_early, inc_end_dates, by = "birdid_year") 
# end_dates_1Y_fail_early <- end_dates_1Y_fail_early %>%
#   relocate(brood_early_start, .after = birdid_year) %>%
#   relocate(inc_end, .after = birdid_year)

# # merge all early brood rearing end dates
# end_dates_fail_early <- merge(end_dates_1Y_fail_early, end_dates_2Y_fail_early, by = "birdid_year", all = TRUE)
# end_dates_fail_early <- merge(end_dates_fail_early, end_dates_3Y_fail_early, by = "birdid_year", all = TRUE)
# end_dates_fail_early <- merge(end_dates_fail_early, end_dates_4Y_fail_early, by = "birdid_year", all = TRUE)

# save end_dates_early
#write.csv(end_dates_fail_early, "Compiled Datasets for Machine Learning/end_dates_for_early_broods_mall_3Apr2026.csv")
write.csv(end_dates_fail_early, "Compiled Datasets for Machine Learning/end_dates_for_early_broods_abdu_3Apr2026.csv")


# # histograms of distribution of brood days when using different quantiles
# end_dates_fail_early$brood_days_80q_2Y <- end_dates_fail_early$brood_end_date_80q_2Y - end_dates_fail_early$inc_end
# end_dates_fail_early$brood_days_85q_2Y <- end_dates_fail_early$brood_end_date_85q_2Y - end_dates_fail_early$inc_end
# end_dates_fail_early$brood_days_90q_2Y <- end_dates_fail_early$brood_end_date_90q_2Y - end_dates_fail_early$inc_end
# end_dates_fail_early$brood_days_95q_2Y <- end_dates_fail_early$brood_end_date_95q_2Y - end_dates_fail_early$inc_end
# hist(as.numeric(end_dates_fail_early$brood_days_80q_2Y))
# hist(as.numeric(end_dates_fail_early$brood_days_85q_2Y))
# hist(as.numeric(end_dates_fail_early$brood_days_90q_2Y))
# hist(as.numeric(end_dates_fail_early$brood_days_95q_2Y))

# # fail late - 80 q
# daily_metrics_fail_late_80q <- daily_metrics_fail_late %>%
#   mutate(ddist_greater_than_80q=ifelse(median_ddist > quantiles_late$median_ddist[1], "Y", "N"),
#          nsd_greater_than_80q=ifelse(mean_nsd > quantiles_late$mean_nsd[1], "Y", "N"),
#          mcp_greater_than_80q=ifelse(mcp75area > quantiles_late$mcp75area[1], "Y", "N"),
#          fly_greater_than_80q=ifelse(corrected_prop_fly > quantiles_late$corrected_prop_fly[1], "Y", "N")) %>%
#   select("birdid_year", "inc_end", "date", "ddist_greater_than_80q", "nsd_greater_than_80q", "mcp_greater_than_80q", "fly_greater_than_80q")
# 
# # count number of columns per row with Y's
# search_char <- "Y"
# daily_metrics_fail_late_80q$count_Y <- apply(daily_metrics_fail_late_80q , 1, function(row) {
#   sum(grepl(search_char, row))
# })
# 
# # filter to rows containing at least one Y
# daily_metrics_fail_late_80q <- daily_metrics_fail_late_80q %>%
#   filter(count_Y >= 1)
# 
# # find earliest day with 1 Y
# end_dates_80q_1Y_fail_late <- daily_metrics_fail_late_80q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_80q_1Y=min(date))
# 
# # filter to rows containing at least two Y's
# daily_metrics_fail_late_80q <- daily_metrics_fail_late_80q %>%
#   filter(count_Y >= 2)
# 
# # find earliest day with at least 2 Y's
# end_dates_80q_2Y_fail_late <- daily_metrics_fail_late_80q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_80q_2Y=min(date))
# 
# # filter to rows containing at least three Y's
# daily_metrics_fail_late_80q <- daily_metrics_fail_late_80q %>%
#   filter(count_Y >= 3)
# 
# # find earliest day with at least 3 Y's
# end_dates_80q_3Y_fail_late <- daily_metrics_fail_late_80q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_80q_3Y=min(date))
# 
# # filter to rows containing at least four Y's
# daily_metrics_fail_late_80q <- daily_metrics_fail_late_80q %>%
#   filter(count_Y >= 4)
# 
# # find earliest day with at least 4 Y's
# end_dates_80q_4Y_fail_late <- daily_metrics_fail_late_80q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_80q_4Y=min(date))


# fail late - 85 q
daily_metrics_fail_late_85q <- daily_metrics_fail_late %>%
  mutate(ddist_greater_than_85q=ifelse(median_ddist > quantiles_late$median_ddist[2], "Y", "N"),
         nsd_greater_than_85q=ifelse(mean_nsd > quantiles_late$mean_nsd[2], "Y", "N"),
         mcp_greater_than_85q=ifelse(mcp75area > quantiles_late$mcp75area[2], "Y", "N"),
         fly_greater_than_85q=ifelse(corrected_prop_fly > quantiles_late$corrected_prop_fly[2], "Y", "N")) %>%
  select("birdid_year", "inc_end", "date", "ddist_greater_than_85q", "nsd_greater_than_85q", "mcp_greater_than_85q", "fly_greater_than_85q")

# count number of columns per row with Y's
search_char <- "Y"
daily_metrics_fail_late_85q$count_Y <- apply(daily_metrics_fail_late_85q , 1, function(row) {
  sum(grepl(search_char, row))
})

# # filter to rows containing at least one Y
# daily_metrics_fail_late_85q <- daily_metrics_fail_late_85q %>%
#   filter(count_Y >= 1)
# 
# # find earliest day with 1 Y
# end_dates_85q_1Y_fail_late <- daily_metrics_fail_late_85q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_85q_1Y=min(date))

# filter to rows containing at least two Y's
daily_metrics_fail_late_85q <- daily_metrics_fail_late_85q %>%
  filter(count_Y >= 2)

# find earliest day with at least 2 Y's
end_dates_85q_2Y_fail_late <- daily_metrics_fail_late_85q %>% group_by(birdid_year) %>% 
  summarize(brood_end_date_85q_2Y=min(date))

# # filter to rows containing at least three Y's
# daily_metrics_fail_late_85q <- daily_metrics_fail_late_85q %>%
#   filter(count_Y >= 3)
# 
# # find earliest day with at least 3 Y's
# end_dates_85q_3Y_fail_late <- daily_metrics_fail_late_85q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_85q_3Y=min(date))
# 
# # filter to rows containing at least four Y's
# daily_metrics_fail_late_85q <- daily_metrics_fail_late_85q %>%
#   filter(count_Y >= 4)
# 
# # find earliest day with at least 4 Y's
# end_dates_85q_4Y_fail_late <- daily_metrics_fail_late_85q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_85q_4Y=min(date))

# # fail late - 90 q
# daily_metrics_fail_late_90q <- daily_metrics_fail_late %>%
#   mutate(ddist_greater_than_90q=ifelse(median_ddist > quantiles_late$median_ddist[3], "Y", "N"),
#          nsd_greater_than_90q=ifelse(mean_nsd > quantiles_late$mean_nsd[3], "Y", "N"),
#          mcp_greater_than_90q=ifelse(mcp75area > quantiles_late$mcp75area[3], "Y", "N"),
#          fly_greater_than_90q=ifelse(corrected_prop_fly > quantiles_late$corrected_prop_fly[3], "Y", "N")) %>%
#   select("birdid_year", "inc_end", "date", "ddist_greater_than_90q", "nsd_greater_than_90q", "mcp_greater_than_90q", "fly_greater_than_90q")
# 
# # count number of columns per row with Y's
# search_char <- "Y"
# daily_metrics_fail_late_90q$count_Y <- apply(daily_metrics_fail_late_90q , 1, function(row) {
#   sum(grepl(search_char, row))
# })
# 
# # filter to rows containing at least one Y
# daily_metrics_fail_late_90q <- daily_metrics_fail_late_90q %>%
#   filter(count_Y >= 1)
# 
# # find earliest day with 1 Y
# end_dates_90q_1Y_fail_late <- daily_metrics_fail_late_90q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_90q_1Y=min(date))
# 
# # filter to rows containing at least two Y's
# daily_metrics_fail_late_90q <- daily_metrics_fail_late_90q %>%
#   filter(count_Y >= 2)
# 
# # find earliest day with at least 2 Y's
# end_dates_90q_2Y_fail_late <- daily_metrics_fail_late_90q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_90q_2Y=min(date))
# 
# # filter to rows containing at least three Y's
# daily_metrics_fail_late_90q <- daily_metrics_fail_late_90q %>%
#   filter(count_Y >= 3)
# 
# # find earliest day with at least 3 Y's
# end_dates_90q_3Y_fail_late <- daily_metrics_fail_late_90q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_90q_3Y=min(date))
# 
# # filter to rows containing at least four Y's
# daily_metrics_fail_late_90q <- daily_metrics_fail_late_90q %>%
#   filter(count_Y >= 4)
# 
# # find earliest day with at least 4 Y's
# end_dates_90q_4Y_fail_late <- daily_metrics_fail_late_90q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_90q_4Y=min(date))
# 
# # fail late - 95 q
# daily_metrics_fail_late_95q <- daily_metrics_fail_late %>%
#   mutate(ddist_greater_than_95q=ifelse(median_ddist > quantiles_late$median_ddist[4], "Y", "N"),
#          nsd_greater_than_95q=ifelse(mean_nsd > quantiles_late$mean_nsd[4], "Y", "N"),
#          mcp_greater_than_95q=ifelse(mcp75area > quantiles_late$mcp75area[4], "Y", "N"),
#          fly_greater_than_95q=ifelse(corrected_prop_fly > quantiles_late$corrected_prop_fly[4], "Y", "N")) %>%
#   select("birdid_year", "inc_end", "date", "ddist_greater_than_95q", "nsd_greater_than_95q", "mcp_greater_than_95q", "fly_greater_than_95q")
# 
# # count number of columns per row with Y's
# search_char <- "Y"
# daily_metrics_fail_late_95q$count_Y <- apply(daily_metrics_fail_late_95q , 1, function(row) {
#   sum(grepl(search_char, row))
# })
# 
# # filter to rows containing at least one Y
# daily_metrics_fail_late_95q <- daily_metrics_fail_late_95q %>%
#   filter(count_Y >= 1)
# 
# # find earliest day with 1 Y
# end_dates_95q_1Y_fail_late <- daily_metrics_fail_late_95q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_95q_1Y=min(date))
# 
# # filter to rows containing at least two Y's
# daily_metrics_fail_late_95q <- daily_metrics_fail_late_95q %>%
#   filter(count_Y >= 2)
# 
# # find earliest day with at least 2 Y's
# end_dates_95q_2Y_fail_late <- daily_metrics_fail_late_95q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_95q_2Y=min(date))
# 
# # filter to rows containing at least three Y's
# daily_metrics_fail_late_95q <- daily_metrics_fail_late_95q %>%
#   filter(count_Y >= 3)
# 
# # find earliest day with at least 3 Y's
# end_dates_95q_3Y_fail_late <- daily_metrics_fail_late_95q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_95q_3Y=min(date))
# 
# # filter to rows containing at least four Y's
# daily_metrics_fail_late_95q <- daily_metrics_fail_late_95q %>%
#   filter(count_Y >= 4)
# 
# # find earliest day with at least 4 Y's
# end_dates_95q_4Y_fail_late <- daily_metrics_fail_late_95q %>% group_by(birdid_year) %>% 
#   summarize(brood_end_date_95q_4Y=min(date))
# 
# # combine all 1 Y's into one dataframe
# end_dates_1Y_fail_late <- merge(end_dates_80q_1Y_fail_late, end_dates_85q_1Y_fail_late, by="birdid_year", all = TRUE)
# end_dates_1Y_fail_late <- merge(end_dates_1Y_fail_late, end_dates_90q_1Y_fail_late, by="birdid_year", all = TRUE)
# end_dates_1Y_fail_late <- merge(end_dates_1Y_fail_late, end_dates_95q_1Y_fail_late, by="birdid_year", all = TRUE)
# 
# # combine all 2 Y's into one dataframe
# end_dates_2Y_fail_late <- merge(end_dates_80q_2Y_fail_late, end_dates_85q_2Y_fail_late, by="birdid_year", all = TRUE)
# end_dates_2Y_fail_late <- merge(end_dates_2Y_fail_late, end_dates_90q_2Y_fail_late, by="birdid_year", all = TRUE)
# end_dates_2Y_fail_late <- merge(end_dates_2Y_fail_late, end_dates_95q_2Y_fail_late, by="birdid_year", all = TRUE)
# 
# # combine all 3 Y's into one dataframe
# end_dates_3Y_fail_late <- merge(end_dates_80q_3Y_fail_late, end_dates_85q_3Y_fail_late, by="birdid_year", all = TRUE)
# end_dates_3Y_fail_late <- merge(end_dates_3Y_fail_late, end_dates_90q_3Y_fail_late, by="birdid_year", all = TRUE)
# end_dates_3Y_fail_late <- merge(end_dates_3Y_fail_late, end_dates_95q_3Y_fail_late, by="birdid_year", all = TRUE)
# 
# # combine all 4 Y's into one dataframe
# end_dates_4Y_fail_late <- merge(end_dates_80q_4Y_fail_late, end_dates_85q_4Y_fail_late, by="birdid_year", all = TRUE)
# end_dates_4Y_fail_late <- merge(end_dates_4Y_fail_late, end_dates_90q_4Y_fail_late, by="birdid_year", all = TRUE)
# end_dates_4Y_fail_late <- merge(end_dates_4Y_fail_late, end_dates_95q_4Y_fail_late, by="birdid_year", all = TRUE)

# add inc_end date and brood_late_start_date 
inc_end_dates <- breeding.metrics_late %>% group_by(birdid_year) %>%
  summarise(inc_end=mean(inc_end))

inc_end_dates$brood_late_start <- inc_end_dates$inc_end + 16

end_dates_fail_late <- merge(end_dates_85q_2Y_fail_late, inc_end_dates, by = "birdid_year") 
end_dates_fail_late <- end_dates_fail_late %>%
  relocate(brood_late_start, .after = birdid_year) %>%
  relocate(inc_end, .after = birdid_year)

# end_dates_1Y_fail_late <- merge(end_dates_1Y_fail_late, inc_end_dates, by = "birdid_year") 
# end_dates_1Y_fail_late <- end_dates_1Y_fail_late %>%
#   relocate(brood_late_start, .after = birdid_year) %>%
#   relocate(inc_end, .after = birdid_year)

# # merge all late brood rearing end dates
# end_dates_fail_late <- merge(end_dates_1Y_fail_late, end_dates_2Y_fail_late, by = "birdid_year", all = TRUE)
# end_dates_fail_late <- merge(end_dates_fail_late, end_dates_3Y_fail_late, by = "birdid_year", all = TRUE)
# end_dates_fail_late <- merge(end_dates_fail_late, end_dates_4Y_fail_late, by = "birdid_year", all = TRUE)

# save end_dates_late
#write.csv(end_dates_fail_late, "Compiled Datasets for Machine Learning/end_dates_for_late_broods_mall_last5daysOFEarlyWindowIncluded_3Apr2026.csv")
write.csv(end_dates_fail_late, "Compiled Datasets for Machine Learning/end_dates_for_late_broods_abdu_last5daysOFEarlyWindowIncluded_3Apr2026.csv")

# # histograms of distribution of brood days when using different quantiles
# end_dates_fail_late$brood_days_80q_2Y <- end_dates_fail_late$brood_end_date_80q_2Y - end_dates_fail_late$inc_end
# end_dates_fail_late$brood_days_85q_2Y <- end_dates_fail_late$brood_end_date_85q_2Y - end_dates_fail_late$inc_end
# end_dates_fail_late$brood_days_90q_2Y <- end_dates_fail_late$brood_end_date_90q_2Y - end_dates_fail_late$inc_end
# end_dates_fail_late$brood_days_95q_2Y <- end_dates_fail_late$brood_end_date_95q_2Y - end_dates_fail_late$inc_end
# hist(as.numeric(end_dates_fail_late$brood_days_80q_2Y))
# hist(as.numeric(end_dates_fail_late$brood_days_85q_2Y))
# hist(as.numeric(end_dates_fail_late$brood_days_90q_2Y))
# hist(as.numeric(end_dates_fail_late$brood_days_95q_2Y))


#### 3. Plot to assess how well the algorithm is functioning ####
library(ggpubr)
library(ggplot2)
library(grid)
summary(breeding.metrics_labeled)
breeding.metrics_labeled$status <- as.factor(breeding.metrics_labeled$status)

list_obs_final<-breeding.metrics_labeled %>%distinct(birdid, year)

###  15-day plot - 2 algorithms #### 

pdf("plots_30dayWindow_BroodRearing_5Sept2025/unknown_abdu_hatched_nests_classified_15dWindow_EarlyAndLateBroodRearing_26Nov2025.pdf",paper="a4r",width=9,height=6)
for (i in 1:nrow(list_obs_final)){
  d<-breeding.metrics_labeled %>% filter(birdid==list_obs_final[i,1] & year==list_obs_final[i,2])
  d_early <- d[1:15,]
  d_late <- d[16:30,]
  theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
  g<-ggarrange(
    ggplot(d,aes(x=date,y=median_ddist/1000,color=status))+
      geom_point() + 
      #geom_line(size=1)+
      geom_hline(yintercept = mean(d_early$median_ddist/1000, na.rm = T), color = "#21908CFF", linetype = "dashed")+
      geom_hline(yintercept = mean(d_late$median_ddist/1000, na.rm = T), color = "red", linetype = "dashed")+
      geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "dashed")+
      scale_color_manual(values = c("no brood" = "black", "successful - early brood" = "blue", "successful - late brood" = "red"))+
      scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
      labs( y = "Daily median DDIST (km"),
    ggplot(d,aes(x=date,y=mean_abs_x,color=status))+
      geom_point() +
      #geom_line()+
      geom_hline(yintercept = mean(d_early$mean_abs_x, na.rm = T), color = "#21908CFF", linetype = "dashed")+
      geom_hline(yintercept = sd(d_early$mean_abs_x, na.rm = T), color = "light#21908CFF", linetype = "dashed")+
      geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "dashed")+
      scale_color_manual(values = c("no brood" = "black", "successful - early brood" = "#21908CFF", "successful - late brood" = "red"))+
      scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
      labs( y = "Daily mean ABSX"),
    ggplot(d,aes(x=date,y=mean_nsd/(1000^2),color=status))+
      geom_point() + 
      #geom_line(size=1)+
      geom_hline(yintercept = mean(d_early$mean_nsd/(1000^2), na.rm = T), color = "#21908CFF", linetype = "dashed")+
      geom_hline(yintercept = mean(d_late$mean_nsd/(1000^2), na.rm = T), color = "red", linetype = "dashed")+
      geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "dashed")+
      scale_color_manual(values = c("no brood" = "black", "successful - early brood" = "#21908CFF", "successful - late brood" = "red"))+
      scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
      labs( y = "Daily mean NSD (km^2)"),
    ggplot(d,aes(x=date,y=mcp75area,color=status))+
      geom_point() + 
      #geom_line(size=1)+
      geom_hline(yintercept = mean(d_early$mcp75area, na.rm = T), color = "#21908CFF", linetype = "dashed")+
      geom_hline(yintercept = mean(d_late$mcp75area, na.rm = T), color = "red", linetype = "dashed")+
      geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "dashed")+
      scale_color_manual(values = c("no brood" = "black", "successful - early brood" = "#21908CFF", "successful - late brood" = "red"))+
      scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
      labs( y = "Daily area of 75% MCP"),
    ggplot(d,aes(x=date,y=corrected_prop_fly,color=status))+
      geom_point() + 
      #geom_line(size=1)+
      geom_hline(yintercept = median(d_early$corrected_prop_fly, na.rm = T), color = "#21908CFF", linetype = "dashed")+
      geom_hline(yintercept = median(d_late$corrected_prop_fly, na.rm = T), color = "red", linetype = "dashed")+
      geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "dashed")+
      scale_color_manual(values = c("no brood" = "black", "successful - early brood" = "#21908CFF", "successful - late brood" = "red"))+
      scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
      labs( y = "Daily mean proportion flying"),
    ncol=3,nrow = 2,common.legend = TRUE)
  g <- g + annotation_custom(
    grob = textGrob(paste0(list_obs_final[i, 1], " in ", list_obs_final[i, 2]), 
                    gp = gpar(fontsize = 12, fontface = "bold", col="red")),
    xmin = 0.7, xmax = Inf, ymin = 0.95, ymax = Inf  )# Adjust the positioning as needed
  print(g)
}
dev.off()
rm(g,d,i)


#### single plot for manuscript (brood rearing) ####
fledged_id <- "2137-20127"
fledged_year <- "2023"
late_id <- "2137-38410"
late_year <- "2022"
early_id <- "2167-39518"
early_year <- "2024"

# rename status to something that makes more sense for plots
library(plyr)
library(viridis)
breeding.metrics_labeled$Classification <- revalue(breeding.metrics_labeled$status, c("successful - early brood" = "Successful early brood",
                                                                                              "successful - late brood" = "Successful late brood",
                                                                                              "no brood" = "Failed brood"))

detach("package:plyr", unload = TRUE)

unique(breeding.metrics_labeled$Classification)  


setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics")
tiff("plots_30dayWindow_BroodRearing_5Sept2025/ExampleOfBroodOutcomesForManuscript_LongVersion.tiff", units="in", width=12, height=11, res=300)

d<-breeding.metrics_labeled %>% filter(birdid==fledged_id & year==fledged_year)
d_early <- d[1:15,]
d_late <- d[16:30,]
theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
f<-ggarrange(
  ggplot(d,aes(x=date,y=median_ddist/1000,color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$median_ddist/1000, na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = mean(d_late$median_ddist/1000, na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily median DDIST (km)", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=mean_abs_x,color=Classification))+
    geom_point() +
    #geom_line()+
    geom_hline(yintercept = mean(d_early$mean_abs_x, na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = sd(d_early$mean_abs_x, na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily mean ABSX", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=mean_nsd/(1000^2),color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$mean_nsd/(1000^2), na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = mean(d_late$mean_nsd/(1000^2), na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily mean NSD (km^2)", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=mcp75area,color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$mcp75area, na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = mean(d_late$mcp75area, na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily area of 75% MCP (km^2)", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=corrected_prop_fly,color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    geom_hline(yintercept = mean(d_late$corrected_prop_fly, na.rm = T), color = "black", linetype = "dotdash")+
    geom_hline(yintercept = mean(d_early$corrected_prop_fly, na.rm = T), color = "black", linetype = "dotdash")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+



    labs( y = "Daily mean proportion flying", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ncol=5,nrow = 1,common.legend = TRUE, legend="bottom")

d<-breeding.metrics_labeled %>% filter(birdid==late_id & year==late_year)
d_early <- d[1:15,]
d_late <- d[16:30,]
theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
l<-ggarrange(
  ggplot(d,aes(x=date,y=median_ddist/1000,color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$median_ddist/1000, na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = mean(d_late$median_ddist/1000, na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily median DDIST (km)", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=mean_abs_x,color=Classification))+
    geom_point() +
    #geom_line()+
    geom_hline(yintercept = mean(d_early$mean_abs_x, na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = sd(d_early$mean_abs_x, na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily mean ABSX", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=mean_nsd/(1000^2),color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$mean_nsd/(1000^2), na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = mean(d_late$mean_nsd/(1000^2), na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily mean NSD (km^2)", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=mcp75area,color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$mcp75area, na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = mean(d_late$mcp75area, na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily area of 75% MCP (km^2)", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=corrected_prop_fly,color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$corrected_prop_fly, na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = mean(d_late$corrected_prop_fly, na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily mean proportion flying", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ncol=5,nrow = 1,common.legend = TRUE, legend="bottom")

d<-breeding.metrics_labeled %>% filter(birdid==early_id & year==early_year)
d_early <- d[1:15,]
d_late <- d[16:30,]
theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
e<-ggarrange(
  ggplot(d,aes(x=date,y=median_ddist/1000,color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$median_ddist/1000, na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = mean(d_late$median_ddist/1000, na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily median DDIST (km)", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=mean_abs_x,color=Classification))+
    geom_point() +
    #geom_line()+
    geom_hline(yintercept = mean(d_early$mean_abs_x, na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = sd(d_early$mean_abs_x, na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily mean ABSX", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=mean_nsd/(1000^2),color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$mean_nsd/(1000^2), na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = mean(d_late$mean_nsd/(1000^2), na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily mean NSD (km^2)", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=mcp75area,color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$mcp75area, na.rm = T), color = "black", linetype = "dotted")+
    geom_hline(yintercept = mean(d_late$mcp75area, na.rm = T), color = "black", linetype = "dashed")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily area of 75% MCP (km^2)", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ggplot(d,aes(x=date,y=corrected_prop_fly,color=Classification))+
    geom_point() + 
    #geom_line(size=1)+
    geom_hline(yintercept = mean(d_early$corrected_prop_fly, na.rm = T), color = "black", linetype = "dotdash")+
    geom_hline(yintercept = mean(d_late$corrected_prop_fly, na.rm = T), color = "black", linetype = "dotdash")+
    geom_vline(xintercept = min(as.Date(d_late$date), na.rm = T), color = "black", linetype = "solid")+
    scale_color_manual(values = c("Failed brood" = "#440154FF", "Successful early brood" = "#21908CFF", "Successful late brood" = "#FDE725FF"))+
    scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
    labs( y = "Daily mean proportion flying", x = "Date") + theme(plot.margin = margin(2,0.5,0.1,0.1, 'cm')),
  ncol=5,nrow = 1,common.legend = TRUE, legend="bottom")

ggarrange(f,l,e, ncol=1, nrow=3,# common.legend = TRUE,
          labels = c("Overall Status: Fledged brood", "Overall Status: Late failed brood",
                     "Overall Status: Early failed brood"))
x = c(0, 0.5, 1, 0, 0.5, 1
)
y = c(0.33, 0.33, 0.33, 0.665, 0.665, 0.665
)
id = c(1,1,1,2,2,2
)
grid.polygon(x,y,id)
dev.off()

#### summarizing brood rearing results for manuscript ####
setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics") # laptop

library(ggpubr)
library(ggplot2)
library(grid)
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

# load deployment data
library(readxl)
deploy_mall <- read_excel("Deployment Files/deployments_18Sept2025.xlsx", sheet = "MALL")
deploy_abdu <- read_excel("Deployment Files/deployments_18Sept2025.xlsx", sheet = "ABDU")

# reduce deploy data to one device per year (latest deployment) and format data
deploy_mall$`Banding Date` <- as.Date(deploy_mall$`Banding Date`)
deploy_mall <- deploy_mall %>% 
  mutate(year=year(`Banding Date`)) %>% 
  group_by(year, `Transmitter Number`) %>%
  filter(`Banding Date` == max(`Banding Date`)) %>% 
  dplyr::select(c("Transmitter Number", "Band Number", "Species", "year", "Age")) %>%
  rename(device_id=`Transmitter Number`,
         band=`Band Number`,
         age=Age,
         species=Species)

deploy_abdu$`Banding Date` <- as.Date(deploy_abdu$`Banding Date`)
deploy_abdu <- deploy_abdu %>% 
  mutate(year=year(`Banding Date`)) %>% 
  group_by(year, `Transmitter Number`) %>%
  filter(`Banding Date` == max(`Banding Date`)) %>% 
  dplyr::select(c("Transmitter Number", "Band Number", "Species", "year", "Age")) %>%
  rename(device_id=`Transmitter Number`,
         band=`Band Number`,
         age=Age,
         species=Species)

deploy2 <- rbind(deploy_abdu, deploy_mall)

# merge by device (switched to band for MALL), year and KEEP ALL ROWS FROM BROOD REARING DF!
brood_rearing$band <- substr(brood_rearing$birdid_year, start = 1, stop = 10)
brood_rearing$year <- substr(brood_rearing$birdid_year, start = 12, stop = 15)

brood_rearing2 <- merge(brood_rearing, deploy2, by = c("band", "year", "species"), all.x = TRUE) %>%
  dplyr::select(c("birdid_year", "device_id", "band", "species", "year", "age", "status_early", "status_late", "status_early_late" ))

# make any age that is NA equal to ASY (NAs are because it is the second year of data from a bird, so year no longer matches banding year and device id - regardless if it was an SY or ASY at banding, it will be (or still be) an adult the next year. Could make this more precise and make ATY if ASY in previous year, but not necessary at this point... )
brood_rearing2$age[is.na(brood_rearing2$age)] <- "ASY"

# change no_brood_successful_brood_late to no_brood_no_brood
brood_rearing2 <- brood_rearing2 %>% 
  mutate(status_early_late=ifelse(status_early_late=="no_brood_successful_brood_late", "no_brood_no_brood", status_early_late))

# rename status to something that makes more sense for plots
library(plyr)
brood_rearing2$status <- revalue(brood_rearing2$status_early_late, c("successful_brood_early_no_brood" = "Failed late",
                                                                     "successful_brood_early_successful_brood_late" = "Fledged", 
                                                                     "no_brood_no_brood" = "Failed early"))

detach("package:plyr", unload = TRUE)

# summarize brood rearing outcomes by year, age and overall
# summary of brood outcomes overall - early
n_total <- brood_rearing2 %>% group_by(species) %>% count() # 150 mall, 74 abdu
sum_outcomes_abdu <- brood_rearing2 %>% filter(species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_total[1,2]*100) 

# successful early = 62.2 (first row)
100 - sum_outcomes_abdu$percent

# late brood rearing
sum_outcomes_abdu_late <- sum_outcomes_abdu[2:3,]

total <- sum(sum_outcomes_abdu_late[,2])  #46

# successful late = 28.3
sum_outcomes_abdu_late[2,2]/total

# successful to 30 d = 17.6
.622*.283

sum_outcomes_mall <- brood_rearing2 %>% filter(species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_total[2,2]*100)

# successful early = 64.7 (first row)
100 - sum_outcomes_mall$percent

# late brood rearing
sum_outcomes_mall_late <- sum_outcomes_mall[2:3,]

total <- sum(sum_outcomes_mall_late[,2]) #97

# successful late = 58.8
sum_outcomes_mall_late[2,2]/total

# successful to 30 d = 17.6
.647*.588

# summary of brood outcomes by age (ALL BIRDS)
# get totals by age - early
brood_rearing2 %>% group_by(species, age) %>% count()

#mall
n_asy_mall <- 100
n_sy_mall <- 50
#abdu
n_asy_abdu <- 56
n_sy_abdu <- 18

sum_outcomes_sy_abdu <- brood_rearing2 %>% filter(age == "SY" & species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_sy_abdu*100)  %>%
  mutate(species="ABDU",
         age="SY")

sum_outcomes_asy_abdu <- brood_rearing2 %>% filter(age == "ASY" & species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_asy_abdu*100) %>%
  mutate(species="ABDU",
         age="ASY")

sum_outcomes_sy_mall <- brood_rearing2 %>% filter(age == "SY" & species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_sy_mall*100)  %>%
  mutate(species="MALL",
         age="SY")

sum_outcomes_asy_mall <- brood_rearing2 %>% filter(age == "ASY" & species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_asy_mall*100) %>%
  mutate(species="MALL",
         age="ASY")

#sum_outcomes_by_age_allbirds <- merge(sum_outcomes_sy, sum_outcomes_asy, by = "status", all = TRUE)

sum_outcomes_by_age_allbirds <- rbind(sum_outcomes_sy_mall, sum_outcomes_asy_mall, sum_outcomes_sy_abdu, sum_outcomes_asy_abdu)


my_dataframe <- data.frame(
  status = "Fledged",
  n = 0,
  percent = 0,
  species = "ABDU",
  age = "SY"
)


sum_outcomes_by_age_allbirds <- rbind(sum_outcomes_by_age_allbirds, my_dataframe)

sum_outcomes_by_age_allbirds$success <- 100-sum_outcomes_by_age_allbirds$percent

# brood success by age - late
sum_outcomes_by_age_allbirds_late <- sum_outcomes_by_age_allbirds %>%
  filter(status != "Failed early") %>%
  dplyr::select("status", "n", "species", "age")

sum_outcomes_by_age_allbirds_late %>% group_by(species, age) %>% summarize(n_total=sum(n))

#mall
n_asy_mall <- 66
n_sy_mall <- 31
#abdu
n_asy_abdu <- 39
n_sy_abdu <- 7

sum_outcomes_sy_abdu <- sum_outcomes_by_age_allbirds_late %>% filter(age == "SY" & species == "ABDU" & status == "Fledged") %>%
  mutate(percent_fledged=n/n_sy_abdu*100
         )

sum_outcomes_asy_abdu <- sum_outcomes_by_age_allbirds_late %>% filter(age == "ASY" & species == "ABDU"& status == "Fledged") %>%
  mutate(percent_fledged=n/n_asy_abdu*100
  )

sum_outcomes_sy_mall <- sum_outcomes_by_age_allbirds_late %>% filter(age == "SY" & species == "MALL"& status == "Fledged") %>%
  mutate(percent_fledged=n/n_sy_mall*100
  )

sum_outcomes_asy_mall <- sum_outcomes_by_age_allbirds_late %>% filter(age == "ASY" & species == "MALL"& status == "Fledged") %>%
  mutate(percent_fledged=n/n_asy_mall*100
  )

#sum_outcomes_by_age_allbirds <- merge(sum_outcomes_sy, sum_outcomes_asy, by = "status", all = TRUE)

sum_outcomes_by_age_allbirds_late <- rbind(sum_outcomes_sy_mall, sum_outcomes_asy_mall, sum_outcomes_sy_abdu, sum_outcomes_asy_abdu)

## old plots for quick display of results
png("plots_EggLayingAndIncubationCombined_14Sept2025/mall_BroodRearingOutcomesByAge.png")
ggplot(sum_outcomes_by_age_allbirds, aes(x = status)) +
  geom_point(aes(y=percent.sy, color = "SY"), size = 4, shape=15)+#, shape = 21, color = "magenta", fill="magenta", size=4) +
  geom_point(aes(y=percent.asy, color = "ASY"), size = 4, shape=16)+#, shape = 21, color = "black" , fill="black", size=4) +
  labs(
    x = "Brood Rearing Outcome",
    y = "Percent of Birds") +
  scale_color_manual(
    name = "Age",  # Custom legend title
    values = c("ASY" = "dark#21908CFF", "SY" = "dark#FDE725FF"), # Custom colors for each group
    labels = c("ASY", "SY") # Custom labels for legend entries
  ) +
  ylim(0,100)
# theme_minimal()
dev.off()

## status by age, separate plots for each species within one figure (USED THIS ONE!!)
# Convert to factor and specify desi#FDE725FF order
sum_outcomes_by_age_allbirds$status <- factor(sum_outcomes_by_age_allbirds$status, levels = c("Failed early", "Failed late",
                                                                                              "Fledged"))

sum_outcomes_by_age_allbirds$age <- factor(sum_outcomes_by_age_allbirds$age, levels = c("SY", "ASY"))


library(ggpubr)
library(viridis)
n_status <- viridis(3)

tiff("plots_30dayWindow_BroodRearing_5Sept2025/SpeciesSeparatePlots_OutcomesByAge_BroodRearing.tiff", units="in", width=6, height=7, res=300)
#png("plots_30dayWindow_BroodRearing_5Sept2025/SpeciesSeparatePlots_OutcomesByAge_BroodRearing.png")
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
    fill = "Brood Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Failed early" = n_status[1],
                               "Failed late" = n_status[2],
                               "Fledged" = n_status[3]))+
  
 # ggtitle("MALL") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))

abdu<-sum_outcomes_by_age_allbirds %>%
  filter(species == "ABDU") %>%
  mutate(
    age=as.factor(age),
    status=as.factor(status)
  )
catsums <- aggregate(n ~ age, abdu , FUN = sum)
b<-
  ggplot(abdu, aes(x = age, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=age, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Age",
    y = "Percent",
    fill = "Brood Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Failed early" = n_status[1],
                               "Failed late" = n_status[2],
                               "Fledged" = n_status[3]))+
  
  #ggtitle("ABDU") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))


ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
          labels = c("ABDU", "MALL"))
dev.off()
rm(a,b)



# summary of nesting outcomes by year (ALL BIRDS)
# get totals by year - early
brood_rearing2 %>% group_by(species, year) %>% count()

#mall
n_2022_mall <- 33
n_2023_mall <- 41
n_2024_mall <- 52
n_2025_mall <- 24
#abdu
n_2021_abdu <- 4
n_2022_abdu <- 14
n_2023_abdu <- 19
n_2024_abdu <- 27
n_2025_abdu <- 10

sum_outcomes_2021_abdu <- brood_rearing2 %>% filter(year == "2021") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2021_abdu*100) %>%
  mutate(species="ABDU", year="2021")

sum_outcomes_2022_abdu <- brood_rearing2 %>% filter(year == "2022" & species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2022_abdu*100) %>%
  mutate(species="ABDU", year="2022")

sum_outcomes_2023_abdu <- brood_rearing2  %>% filter(year == "2023"& species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2023_abdu*100) %>%
  mutate(species="ABDU", year="2023")

sum_outcomes_2024_abdu <- brood_rearing2  %>% filter(year == "2024" & species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2024_abdu*100) %>%
  mutate(species="ABDU", year="2024")

sum_outcomes_2025_abdu <- brood_rearing2  %>% filter(year == "2025" & species == "ABDU") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2025_abdu*100) %>%
  mutate(species="ABDU", year="2025")

sum_outcomes_2022_mall <- brood_rearing2 %>% filter(year == "2022" & species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2022_mall*100) %>%
  mutate(species="MALL", year="2022")

sum_outcomes_2023_mall <- brood_rearing2  %>% filter(year == "2023"& species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2023_mall*100) %>%
  mutate(species="MALL", year="2023")

sum_outcomes_2024_mall <- brood_rearing2  %>% filter(year == "2024" & species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2024_mall*100) %>%
  mutate(species="MALL", year="2024")

sum_outcomes_2025_mall <- brood_rearing2  %>% filter(year == "2025" & species == "MALL") %>%
  group_by(status) %>%
  summarise(n=n(),
            percent=n()/n_2025_mall*100) %>%
  mutate(species="MALL", year="2025")

sum_outcomes_by_year_allbirds <- rbind(sum_outcomes_2022_mall, sum_outcomes_2023_mall, sum_outcomes_2024_mall, sum_outcomes_2025_mall,
                                       sum_outcomes_2021_abdu, sum_outcomes_2022_abdu, sum_outcomes_2023_abdu, sum_outcomes_2024_abdu,
                                       sum_outcomes_2025_abdu)
# sum_outcomes_by_year_allbirds <- merge(sum_outcomes_2021, sum_outcomes_2022, by = "status", all = TRUE)
# sum_outcomes_by_year_allbirds <- merge(sum_outcomes_by_year_allbirds, sum_outcomes_2023, by = "status", all = TRUE)
# sum_outcomes_by_year_allbirds <- merge(sum_outcomes_by_year_allbirds, sum_outcomes_2024, by = "status", all = TRUE)
# sum_outcomes_by_year_allbirds <- merge(sum_outcomes_by_year_allbirds, sum_outcomes_2025, by = "status", all = TRUE)

# adding data for 0's
my_dataframe <- data.frame(
  status = c("Failed late", "Fledged"),
  n = c(0, 0),
  percent = c(0, 0),
  species = c("ABDU", "ABDU"),
  year = c("2021", "2025")
)


sum_outcomes_by_year_allbirds <- rbind(sum_outcomes_by_year_allbirds, my_dataframe)

sum_outcomes_by_year_allbirds$success <- 100-sum_outcomes_by_year_allbirds$percent

# brood success by year - late
sum_outcomes_by_year_allbirds_late <- sum_outcomes_by_year_allbirds %>%
  filter(status != "Failed early") %>%
  dplyr::select("status", "n", "species", "year")

sum_outcomes_by_year_allbirds_late %>% group_by(species, year) %>% summarize(n_total=sum(n))

#mall
n_2022_mall <- 19
n_2023_mall <- 29
n_2024_mall <- 35
n_2025_mall <- 14
#abdu
n_2021_abdu <- 2
n_2022_abdu <- 9
n_2023_abdu <- 15
n_2024_abdu <- 14
n_2025_abdu <- 6

sum_outcomes_2021_abdu <- sum_outcomes_by_year_allbirds_late %>% filter(year == "2021" & species == "ABDU" & status == "Fledged") %>%
  mutate(percent_fledged=n/n_2021_abdu*100
  )

sum_outcomes_2022_abdu <- sum_outcomes_by_year_allbirds_late %>% filter(year == "2022" & species == "ABDU"& status == "Fledged") %>%
  mutate(percent_fledged=n/n_2022_abdu*100
  )

sum_outcomes_2023_abdu <- sum_outcomes_by_year_allbirds_late %>% filter(year == "2023" & species == "ABDU" & status == "Fledged") %>%
  mutate(percent_fledged=n/n_2023_abdu*100
  )

sum_outcomes_2024_abdu <- sum_outcomes_by_year_allbirds_late %>% filter(year == "2024" & species == "ABDU"& status == "Fledged") %>%
  mutate(percent_fledged=n/n_2024_abdu*100
  )

sum_outcomes_2025_abdu <- sum_outcomes_by_year_allbirds_late %>% filter(year == "2025" & species == "ABDU"& status == "Fledged") %>%
  mutate(percent_fledged=n/n_2025_abdu*100
  )


sum_outcomes_2022_mall <- sum_outcomes_by_year_allbirds_late %>% filter(year == "2022" & species == "MALL"& status == "Fledged") %>%
  mutate(percent_fledged=n/n_2022_mall*100
  )

sum_outcomes_2023_mall <- sum_outcomes_by_year_allbirds_late %>% filter(year == "2023" & species == "MALL" & status == "Fledged") %>%
  mutate(percent_fledged=n/n_2023_mall*100
  )

sum_outcomes_2024_mall <- sum_outcomes_by_year_allbirds_late %>% filter(year == "2024" & species == "MALL"& status == "Fledged") %>%
  mutate(percent_fledged=n/n_2024_mall*100
  )

sum_outcomes_2025_mall <- sum_outcomes_by_year_allbirds_late %>% filter(year == "2025" & species == "MALL"& status == "Fledged") %>%
  mutate(percent_fledged=n/n_2025_mall*100
  )

#sum_outcomes_by_age_allbirds <- merge(sum_outcomes_sy, sum_outcomes_asy, by = "status", all = TRUE)

sum_outcomes_by_year_allbirds_late <- rbind(sum_outcomes_2022_mall, sum_outcomes_2023_mall, sum_outcomes_2024_mall, sum_outcomes_2025_mall,
                                            sum_outcomes_2021_abdu, sum_outcomes_2022_abdu, sum_outcomes_2023_abdu, sum_outcomes_2024_abdu,
                                            sum_outcomes_2025_abdu)

## old plot for rapid viewing
png("plots_EggLayingAndIncubationCombined_14Sept2025/mall_BroodRearingOutcomesByYear.png")
ggplot(sum_outcomes_by_year_allbirds, aes(x = status)) +
  geom_point(aes(y=percent.2021, color = "2021"), size = 4, shape=25)+#, shape = 21, color = "magenta", fill="magenta", size=4) +
  geom_point(aes(y=percent.2022, color = "2022"), size = 4, shape=15)+#, shape = 21, color = "magenta", fill="magenta", size=4) +
  geom_point(aes(y=percent.2023, color = "2023"), size = 4, shape=16)+#, shape = 21, color = "black" , fill="black", size=4) +
  geom_point(aes(y=percent.2024, color = "2024"), size = 4, shape=17)+#, shape = 21, color = "magenta", fill="magenta", size=4) +
  geom_point(aes(y=percent.2025, color = "2025"), size = 4, shape=18)+#, shape = 21, color = "black" , fill="black", size=4) +
  labs(
    x = "Brood Rearing Outcome",
    y = "Percent of Birds") +
  scale_color_manual(
    name = "Year",  # Custom legend title
    values = c("2021" = "darkmagenta", "2022" = "dark#21908CFF", "2023" = "dark#FDE725FF", "2024" = "darkgreen", "2025" = "darkorange"), # Custom colors for each group
    labels = c("2021", "2022", "2023", "2024", "2025") # Custom labels for legend entries
  ) +
  ylim(0,100)
# theme_minimal()
dev.off()

## status by year, separate plots for each species within one figure (USED THIS ONE!!)
library(ggpubr)
library(viridis)
n_status <- viridis(3)


tiff("plots_30dayWindow_BroodRearing_5Sept2025/SpeciesSeparatePlots_OutcomesByYear_BroodRearing.tiff", units="in", width=6, height=7, res=300)

#png("plots_30dayWindow_BroodRearing_5Sept2025/SpeciesSeparatePlots_OutcomesByYear_BroodRearing.png")
mall<-sum_outcomes_by_year_allbirds %>%
  filter(species == "MALL") %>%
  mutate(
    year=as.factor(year),
    status=as.factor(status))
catsums <- aggregate(n ~ year, mall , FUN = sum)
a <-
  ggplot(mall, aes(x = year, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=year, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Year",
    y = "Percent",
    fill = "Brood Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Failed early" = n_status[1],
                               "Failed late" = n_status[2],
                               "Fledged" = n_status[3]))+
  
  
  #ggtitle("MALL") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))

abdu <-sum_outcomes_by_year_allbirds %>%
  filter(species == "ABDU") %>%
  mutate(
    year=as.factor(year),
    status=as.factor(status)
  ) 

catsums <- aggregate(n ~ year, abdu , FUN = sum)  

b <-
  ggplot(abdu, aes(x = year, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=year, y=n, label=n, fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Year",
    y = "Percent",
    fill = "Brood Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Failed early" = n_status[1],
                               "Failed late" = n_status[2],
                               "Fledged" = n_status[3]))+
  
  
  #ggtitle("ABDU") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))


ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
          labels = c("ABDU", "MALL"))
dev.off()
rm(a,b)

### combining year and age plot into one plot for manuscript
library(ggpubr)
library(viridis)
n_status <- viridis(3)

tiff("plots_30dayWindow_BroodRearing_5Sept2025/SpeciesSeparatePlots_OutcomesByAgeAndYear_BroodRearing.tiff", units="in", width=14, height=8, res=300)
#png("plots_30dayWindow_BroodRearing_5Sept2025/SpeciesSeparatePlots_OutcomesByAge_BroodRearing.png")
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
  geom_text(data = catsums, aes(x=age, y=n, label=paste0("N = ",n), fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Age",
    y = "Percent",
    fill = "Brood Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Failed early" = n_status[1],
                               "Failed late" = n_status[2],
                               "Fledged" = n_status[3]))+
  
  # ggtitle("MALL") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))

abdu<-sum_outcomes_by_age_allbirds %>%
  filter(species == "ABDU") %>%
  mutate(
    age=as.factor(age),
    status=as.factor(status)
  )
catsums <- aggregate(n ~ age, abdu , FUN = sum)
b<-
  ggplot(abdu, aes(x = age, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=age, y=n, label=paste0("N = ",n), fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Age",
    y = "Percent",
    fill = "Brood Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Failed early" = n_status[1],
                               "Failed late" = n_status[2],
                               "Fledged" = n_status[3]))+
  
  #ggtitle("ABDU") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))


c <- ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
          labels = c("ABDU", "MALL"))

mall<-sum_outcomes_by_year_allbirds %>%
  filter(species == "MALL") %>%
  mutate(
    year=as.factor(year),
    status=as.factor(status))
catsums <- aggregate(n ~ year, mall , FUN = sum)
a <-
  ggplot(mall, aes(x = year, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=year, y=n, label=paste0("N = ",n), fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Year",
    y = "Percent",
    fill = "Brood Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Failed early" = n_status[1],
                               "Failed late" = n_status[2],
                               "Fledged" = n_status[3]))+
  
  
  #ggtitle("MALL") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))

abdu <-sum_outcomes_by_year_allbirds %>%
  filter(species == "ABDU") %>%
  mutate(
    year=as.factor(year),
    status=as.factor(status)
  ) 

catsums <- aggregate(n ~ year, abdu , FUN = sum)  

b <-
  ggplot(abdu, aes(x = year, y = percent, fill = forcats::fct_rev(status))) +
  geom_bar(stat="identity", position = "fill") + # Use "stack" position for stacked bars
  geom_text(data = catsums, aes(x=year, y=n, label=paste0("N = ",n), fill=NULL), position=position_fill(), vjust=-0.05) +
  scale_y_continuous(labels= scales::percent)+
  labs(
    #title = "Nesting Outcome by Age",
    x = "Year",
    y = "Percent",
    fill = "Brood Outcome"
  ) +
  # Further customization for nested axis text appearance if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Adjust angle for readability
  ) +
  
  scale_fill_manual(values = c("Failed early" = n_status[1],
                               "Failed late" = n_status[2],
                               "Fledged" = n_status[3]))+
  
  
  #ggtitle("ABDU") +
  theme_classic() + theme(plot.margin = margin(1,0,0,0, 'cm'))


d <- ggarrange(b,a, ncol=1, nrow=2, common.legend = TRUE,
          labels = c("ABDU", "MALL"))

ggarrange(c,d, ncol=2, nrow=1, common.legend = FALSE)


dev.off()






# load start_end dates for nesting and add brood rearing, save for Brittany (ME)
dates <- read.csv("Compiled Datasets for Machine Learning/MALL_2022to2025_StartEndDatesOfIncubationLayingPlusNestLocation_MigrationFixed_NoLayDateAllowed_20Nov2025.csv")
dates <- read.csv("Compiled Datasets for Machine Learning/ABDU_2021to2025_StartEndDatesOfIncubationLayingPlusNestLocation_MigrationFixed_NoLayDateAllowed_21Nov2025.csv")
colnames(dates)
colnames(brood_rearing2)
dates2 <- merge(dates, brood_rearing2, by = "birdid_year", all.x=TRUE)
write.csv(dates2, "Compiled Datasets for Machine Learning/MALL_2022to2025_StartEndDatesOfIncubationAndLayingPlusNestLocation_PlusBroodRearing_1Dec2025.csv")
write.csv(dates2, "Compiled Datasets for Machine Learning/ABDU_2021to2025_StartEndDatesOfIncubationAndLayingPlusNestLocation_PlusBroodRearing_1Dec2025.csv")

