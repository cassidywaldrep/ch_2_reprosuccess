######################## 02_predicting_behaviours.R ##################

# Code: Taking summary statistics ACC data and predict behaviours based on ML model
# Date: 8 July 2025 
# Author: Cassidy Waldrep / Erika Garcez

# Description: 
# this code will connect to the database and pull the ACC data
# then, it will use predict to run it against the most updated ACC model
# After calculating behaviours, it will run daily time activity budgets

# Load packages an

library(tidyverse)
library(kernlab)
library(caret)
library(xgboost)
library(readxl)
library(parallel)
library(doParallel)
library(RPostgres)

# load most updated machine learning model 

ABDU_RF_OPT_G <- readRDS("ABDU_RF_OPT_G.rds")

# connect to database

conn <- dbConnect(Postgres(),
                  dbname = "acc_gps_data_mallards",
                  host = "localhost",
                  port = 5432,
                  user = "postgres",
                  password = "mallard_ducks"
)

#---------------------------------------------------------------------#
############# PULL SUMMARY STATS ACC DATA FROM DATABASE ##############
#---------------------------------------------------------------------#

# first we need to make sure there are no NAs in the data (this may not run if the data is too big)

sum_stats <- tbl(conn, "sum_stats_2022_2025") %>%
  collect() 

# run this to check for na's as well

dbGetQuery(conn, "
SELECT COUNT(*) AS total_rows,
  SUM(CASE WHEN burst IS NULL THEN 1 ELSE 0 END) AS na_burst,
  SUM(CASE WHEN device_id IS NULL THEN 1 ELSE 0 END) AS na_device_id,
  SUM(CASE WHEN bandnum IS NULL THEN 1 ELSE 0 END) AS na_bandnum,
  SUM(CASE WHEN odba IS NULL THEN 1 ELSE 0 END) AS na_odba,
  SUM(CASE WHEN vedba IS NULL THEN 1 ELSE 0 END) AS na_vedba,
  SUM(CASE WHEN \"UTC_datetime\" IS NULL THEN 1 ELSE 0 END) AS na_datetime
FROM sum_stats_acc_data_2022_sep2026;
")

# pulls the list of bands

bands <- dbGetQuery(conn, "
  SELECT DISTINCT bandnum FROM sum_stats_acc_data_2022_sep2026 ORDER BY bandnum;
") %>% 
  pull(bandnum)

# check if any NAs - on local R
# 
# sum_stats %>% 
#   filter(if_any(everything(), is.na))
# 
# # get a list of all bands
# bands <- sum_stats %>%
#   distinct(bandnum) %>%
#   pull(bandnum)


# drop our table if needed


#dbExecute(conn, 'DROP TABLE IF EXISTS modeloutput;')

#### create dummy table to parallel processing is able to work

# Pick one band to simulate structure

example_band <- bands[1]

# Pull data for one band to simulate predictions

summary_stats_example <- tbl(conn, "sum_stats_acc_data_2022_sep2026") %>%
  filter(bandnum == example_band) %>%
  collect()

# Create dummy prediction output

dummy_predictions <- predict(ABDU_RF_OPT_G, newdata = summary_stats_example) %>%
  bind_cols(summary_stats_example) %>%
  rename("behaviour" = '...1') %>%
  head(0)  # Zero rows, just the column structure

# Write dummy table to the database (overwrite if exists)

dbWriteTable(conn, "behaviormodel_2022_sep2026", dummy_predictions, overwrite = TRUE, row.names = FALSE)


# Close Connection #

dbDisconnect(conn)

#---------------------------------------------------------------------#
############# PARALLEL PROCESSING ##############
#---------------------------------------------------------------------#

# Set credential to send for each core #
db_credentials <- list(
  dbname = "acc_gps_data_mallards",
  user = "postgres",
  password = "mallard_ducks",
  host = "localhost"
)

# Set up parallel processing #
num_cores <- detectCores() - 2  # This means it will use all cores - 2 from you computer. - adjust as needed
cl <- makeCluster(num_cores)
registerDoParallel(cl)

clusterExport(cl, list("bands", "ABDU_RF_OPT_G", "db_credentials"))
clusterEvalQ(cl, { library(dplyr); library(RPostgres) })


start <- Sys.time()

# Perform the loop with parallelization #
res <- foreach(b = bands, .packages = c("dplyr", "RPostgres")) %dopar% {
  conn <- dbConnect(RPostgres::Postgres(), 
                    dbname = db_credentials$dbname, 
                    user = db_credentials$user, 
                    password = db_credentials$password, 
                    host = db_credentials$host)
  
  # Get the data for the specific band
  summary_stats_band <- tbl(conn, "sum_stats_acc_data_2022_sep2026") %>%
    filter(bandnum == b) %>%
    collect()  # Bring the specific band data into R memory for processing
  
  cat(paste0("Processing Band Number:",b), file = "/Users/cassidywaldrep/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Analyses/phd_dissertation/Chapter1_TimeActivityBudgets/logs/log_model_behaviour_17Sep2026.txt", append = TRUE)
  
  if (nrow(summary_stats_band) <= 2) {
    dbDisconnect(conn)
    return(NULL)
  }
  
  predictions <- predict(ABDU_RF_OPT_G, newdata = summary_stats_band) %>%
    bind_cols(summary_stats_band) %>%
    rename("behaviour" = '...1')
  
  dim <- dim(predictions)
  
  cat(paste0("Completed Band Number: ", b, " DIM: ", paste(dim, collapse = "x"), "\n"), 
      file = "/Users/cassidywaldrep/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Analyses/phd_dissertation/Chapter1_TimeActivityBudgets/logs/log_model_behaviour_17Sep2026.txt", 
      append = TRUE)
  
  
  # Save 
  dbWriteTable(conn, "behaviormodel_2022_sep2026", predictions, append = TRUE, row.names = FALSE)
  dbDisconnect(conn)
  
  return(NULL)  
}


# Stop cluster
stopCluster(cl) 
end <- Sys.time()

#---------------------------------------------------------------------#
############# CONFUSION MATRIX ##############
#---------------------------------------------------------------------#

# extract the confusion matrix 

conf_matrix <- confusionMatrix(ABDU_RF_OPT_G) 

# connect to database again and read in new model output

conn <- dbConnect(
  Postgres(),
  dbname = "acc_gps_data_mallards",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "mallard_ducks"
)

# if this doesn't read in then restart R and try again. 

summarized_output <- tbl(conn, "behaviormodel_2022_sep2026") %>%
  mutate(date = as_date(UTC_datetime)) %>%
  group_by(bandnum, date) %>% # grouping by day
  summarise(mean_ODBA = mean(odba), 
            num_fixes = n(), 
            prop_fixes = n()/144, 
            device_id = mean(as.numeric(device_id)), 
            prop_fly = mean(case_when(behaviour == "fly" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_feed = mean(case_when(behaviour == "feed" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_stationary = mean(case_when(behaviour == "rest" ~ 1, TRUE ~ 0), na.rm = TRUE),
            prop_preen = mean(case_when(behaviour == "preen" ~ 1, TRUE ~ 0), na.rm = TRUE),
            num_feed_fixes = sum(case_when(behaviour == "feed" ~ 1, TRUE ~ 0), na.rm = TRUE),
            num_fly_fixes = sum(case_when(behaviour == "fly" ~ 1, TRUE ~ 0), na.rm = TRUE),
            num_rest_fixes = sum(case_when(behaviour == "rest" ~ 1, TRUE ~ 0), na.rm = TRUE),
            num_preen_fixes = sum(case_when(behaviour == "preen" ~ 1, TRUE ~ 0), na.rm = TRUE)) %>%
  collect()


# Function from Resheff et al. 2022 to compute the confusion matrix correction for time budgets

#' @param conf_mat : confusion matrix --> rows are actual and columns are predicted (rf.confusion.matrix2 in code)
#' @param observed_budget : uncorrected time budget (the daily proportions for everything)
#' @return The corrected time budget

compute_correction_time_budget <- function (conf_mat, observed_budget){
  # Normalize the rows
  
  row_normalized_conf_mat <- conf_mat / rowSums(conf_mat) # shows the proportion that were given that category
  
  # Compute the inverse - transpose of the confusion matrix
  
  inv_transposed_conf <- solve(t(row_normalized_conf_mat))
  
  # See Equation (8)
  # Note: added t() to code provided in suppl. mat. to match format of data frame created above
  
  return(t(inv_transposed_conf %*% observed_budget)) # %*% is matrix multiplication
}

# run the function on your data...make sure to extract the confusion matrix table

corrected_daily_sums <- compute_correction_time_budget(conf_matrix$table, 
                                                       t(as.data.frame(summarized_output[,c("prop_feed","prop_fly", "prop_preen", "prop_stationary")])))

# rename columns

corrected_daily_sums <- as.data.frame(corrected_daily_sums) %>% 
  rename(corrected_prop_fly = fly, corrected_prop_feed = feed, 
         corrected_prop_stationary = rest, corrected_prop_preen = preen)

# combine with full data

all_daily_activity_budget <-
  
  # add corrected values to original data set 
  bind_cols(summarized_output, corrected_daily_sums) %>%
  
  # if less than 1/144 (basically, less than one the lowest amount of burst possible), make 0
  
  mutate(across(starts_with("corrected_prop_"),
                ~ if_else(. < 0.006944444, 0, .)), 
         
         # more than 1 isn't possible, so bring those values down to 1
         
         across(starts_with("corrected_prop"), 
                ~ if_else(. > 1, 1, .))) %>%
  # filter out birds that had fixes less than 80% and greater than 1% (not possible)
  filter(prop_fixes >= 0.8,
         prop_fixes <= 1.0)

# lastly, some individuals had a broken harness. we need to take any day after that out as the ACC is not reliable

masterlist <- read_excel("mall_9_17_2026.xlsx", 
                         sheet = "MALL") %>%
  janitor::clean_names() %>%
  dplyr::select(band_number, broken_harness) %>%
  filter(is.na(broken_harness) == FALSE) %>%
  mutate(date_broken = as.Date(broken_harness))

# filtering out rows after the broken date

filtered_activity <- all_daily_activity_budget %>%
  mutate(bandnum = as.character(bandnum),                      
         bandnum = str_replace(bandnum, "(?<=^.{4})", "-")) %>%
  left_join(masterlist, by = c("bandnum" = "band_number")) %>%
  
  # keep only rows before or on break date
  
  filter(is.na(date_broken) | date < date_broken) %>%
  dplyr::select(!c("broken_harness", "date_broken"))

# Write finalized daily activity budget to database

dbWriteTable(conn, "all_daily_activity_budget_2022_sep2026_80%fixes", filtered_activity, append = FALSE, overwrite = TRUE, row.names = FALSE)

all_daily_activity_budget<- tbl(conn, "all_daily_activity_budget_2022_2025_80%fixes") %>%
  collect() 
# 
# ### taking a deeper dive into the twisted harnesses
# 
# masterlist <- read_excel("data/mall_1_5_26.xlsx", 
#                          sheet = "MALL") %>%
#   janitor::clean_names() %>%
#   dplyr::select(band_number, broken_harness) %>%
#   filter(is.na(broken_harness) == FALSE) %>%
#   mutate(date_broken = as.Date(broken_harness))
# 
# # filtering out rows after the broken date
# 
# activity_broken <- all_daily_activity_budget %>%
#   mutate(bandnum = as.character(bandnum),                      
#          bandnum = str_replace(bandnum, "(?<=^.{4})", "-")) %>%
#   left_join(masterlist, by = c("bandnum" = "band_number")) %>%
#   filter(!is.na(broken_harness)) %>%
#   
#   # keep only rows before or on break date
#   
#   mutate(broken = ifelse(is.na(date_broken) | date < date_broken, "No", "Yes")) %>%
#   arrange(bandnum, date)
# 
# activity_broken %>%
#   group_by(bandnum) %>%
#   filter(broken == "Yes") %>%
#   summarize(count = n())
# 

