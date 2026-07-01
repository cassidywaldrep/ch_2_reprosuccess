
#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: Reproductive Metrics - Machine Learning
# Date: 30 July 2025
# Author: Ilsa Griebel
# Description: Applying finetuned incubation model to unknown black duck data and 
#               plotting to asses function
# 1. Summarize daily metrics over 28-day window for input to algorithm
# 2. Apply finetuned incubation model to unknwon black duck data
# 3. Plot to assess how well the algorithm is functioning 
#**********************************************************************************************************************************
#**********************************************************************************************************************************
# load libraries
library(dplyr)
#library(roll)
library(zoo)
library(geosphere)
library(RPostgres)
library(lubridate)
#
#### 1. Summarize daily metrics over 6-day window for input to algorithm ####
# connect to database
conn <- dbConnect(
  Postgres(),
  dbname = "acc_gps_data_mallards",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "mallard_ducks"
)

# load unclassified data (ACC)
breeding.metrics <- tbl(conn, "odba_absx_sunrise_and_not_sunrise_30June26") %>%
  collect()

# check data
colnames(breeding.metrics)

# remove ACC daily summary metrics calculate using <25% of daily fixes
breeding.metrics <- breeding.metrics %>%
  mutate(prop_fixes = (num_fixes_sunrise+num_fixes_not_sunrise)/144) %>%
  filter(prop_fixes >= 0.25)

# format data (add bird_year columns, format date as a date, select columns)
breeding.metrics2 <- breeding.metrics %>% 
  mutate(birdid_year=(paste(birdid, year, sep="_")),
         date=as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::select(c("birdid_year", "date", "birdid", "year", "device_id",
                  "mean_ODBA_sunrise", "mean_abs_x_sunrise", 
                  "mean_ODBA_not_sunrise","mean_abs_x_not_sunrise"
  ))

# load unclassified data (GPS)
breeding.metrics <- tbl(conn, "ddist_nsd_sunrise_hours_30June26") %>%
  collect()

# check data
colnames(breeding.metrics)

# format data (add bird_year columns, format date as a date, select columns)
breeding.metrics <- breeding.metrics %>% 
  mutate(birdid_year=(paste(birdid, year, sep="_")),
         date=as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::select(c("birdid_year", "date", "birdid", "year", "median_ddist",
                  "mean_nsd_sunrise", "mean_nsd_not_sunrise"
  ))

colnames(breeding.metrics)
summary(breeding.metrics)

colnames(breeding.metrics2)
summary(breeding.metrics2)

# combine gps and acc data
breeding.metrics2 <- merge(breeding.metrics2, breeding.metrics, by = c("birdid_year", "date", "birdid", "year"))

colnames(breeding.metrics2)
summary(breeding.metrics2)

n1 <- unique(breeding.metrics2$birdid_year) # 1234 bird-years

rm(breeding.metrics)

# create daily ratios of 1 to 5 hours relative to all other days in the year
breeding.metrics2 <- breeding.metrics2 %>%
  mutate(mean_ODBA_ratio=mean_ODBA_sunrise/mean_ODBA_not_sunrise,
         mean_abs_x_ratio=mean_abs_x_sunrise/mean_abs_x_not_sunrise,
         # median_ddist_ratio=median_ddist.x/median_ddist.y,
         mean_nsd_ratio=mean_nsd_sunrise/mean_nsd_not_sunrise
  )

colnames(breeding.metrics2)
summary(breeding.metrics2)

## remove birds without complete data to the end of July #### NO LONGER DOING!!!!!! 
# # create data frame of last day of data collection for each bird
# max_day <- breeding.metrics2 %>% group_by(birdid_year) %>%
#   summarise(max_date = max(date))
# 
# # create separate data frames for each year
# max_day_2022 <- max_day %>% filter(max_date > "2022-03-01" & max_date <= "2022-09-15")
# max_day_2023 <- max_day %>% filter(max_date > "2023-03-01" & max_date <= "2023-09-15")
# max_day_2024 <- max_day %>% filter(max_date > "2024-03-01" & max_date <= "2024-09-15")
# max_day_2025 <- max_day %>% filter(max_date > "2025-03-01" & max_date <= "2025-09-15")
# 
# # filter out birds with max_day's < July 1 (MALL)
# max_day_2022_filtered <- max_day_2022 %>%
#   filter(max_date > "2022-06-30")
# max_day_2023_filtered <- max_day_2023 %>%
#   filter(max_date > "2023-06-30")
# max_day_2024_filtered <- max_day_2024 %>%
#   filter(max_date > "2024-06-30")
# max_day_2025_filtered <- max_day_2025 %>%
#   filter(max_date > "2025-06-30")
# 
# # combine years into a single data frame
# max_day <- rbind(max_day_2022_filtered, max_day_2023_filtered, max_day_2024_filtered, max_day_2025_filtered)
# 
# rm(max_day_2022, max_day_2022_filtered, max_day_2023, max_day_2023_filtered, max_day_2024, max_day_2024_filtered,
#    max_day_2025, max_day_2025_filtered)
# 
# # only keep birds with data at least until June 30
# breeding.metrics2 <- breeding.metrics2 %>% filter(birdid_year %in% max_day$birdid_year)
# 
# n3 <- unique(breeding.metrics2$birdid_year) # 869 bird-years
# 
# rm(max_day)

# only apply egg laying algorithm to July 1
breeding.metrics2 <- breeding.metrics2 %>% 
  mutate(month=month(date)) %>%
  filter(month < 7)

breeding.metrics2 <- breeding.metrics2[order(breeding.metrics2$birdid_year, breeding.metrics2$date), ]

## also remove any birds that missing 2 or more consecutive days of data (suggestive of battery issues) -- used to be 5 days but tested with training data and now doing > 2
# add column for number of days between daily rows
breeding.metrics2 <- breeding.metrics2 %>% group_by(birdid_year) %>%
  mutate(date_diff=date-lag(date))

hist(as.numeric(breeding.metrics2$date_diff))

# create dataframe containing only birds with a date difference > 2 
greater_than_2days <- breeding.metrics2 %>% filter(date_diff>2) ### any birds missing one day, use >= 2; any birds missing two or more consecutive days, use >2
hist(as.numeric(greater_than_2days$date_diff))

n_rem<-unique(greater_than_2days$birdid_year) # should remove 79 bird-year's (now only removes 69 bird-years when not excluding birds without complete data to July 1) -- removes 105 now that removing birds missing 2 or more days

# remove birds with any date diff's > 2 
breeding.metrics2 <- breeding.metrics2 %>% filter(!birdid_year %in% greater_than_2days$birdid_year)

n4 <- unique(breeding.metrics2$birdid_year) # 778 bird-years

rm(greater_than_2days)


## only keep birds with at least 7 days of data (window size for egg laying algorithm)
# count number of days per bird_year
num_days_per_birdyear <- breeding.metrics2 %>%
  group_by(birdid_year) %>%
  summarise(n=n())

# create dataframe containing only bird-year's with less than 7 days and take a look at the distribution
less_than_7_days <- num_days_per_birdyear %>% filter(n <7) # 43 bird-years
hist(as.numeric(less_than_7_days$n))

# remove bird-years with less than 7 days  
breeding.metrics2 <- breeding.metrics2 %>% filter(!birdid_year %in% less_than_7_days$birdid_year)

n5 <- unique(breeding.metrics2$birdid_year) # 1121 bird-years (MALL), now 996 bird-years when removing birds missing 2 or more consecutive days of data

rm(less_than_7_days)


#   # from left (so window will be calculated from the date to 6 days after) ## for first 6 days egg laying and last 6 days egg laying algorithms
# breeding.metrics_6d <- breeding.metrics2 %>% group_by(birdid_year) %>%
#   mutate(ODBA_mean=rollapply(mean_ODBA_sunrise, width = 6, FUN = mean, fill = NA, align = "left"),
#          Absx_mean=rollapply(mean_abs_x_sunrise, width = 6, FUN = mean, fill = NA, align = "left"),
#          ddist_mean=rollapply(median_ddist, width = 6, FUN = mean, fill = NA, align = "left"),
#          Absx_var=rollapply(mean_abs_x_sunrise, width = 6, FUN = sd, fill = NA, align = "left"),
#          # when adding ratio variables
#          ODBA_median_ratio=rollapply(mean_ODBA_ratio, width = 6, FUN = median, fill = NA, align = "left"),
#          Absx_mean_ratio=rollapply(mean_abs_x_ratio, width = 6, FUN = mean, fill = NA, align = "left"),
#          Absx_var_ratio=rollapply(mean_abs_x_ratio, width = 6, FUN = sd, fill = NA, align = "left"),
#          nsd_median_ratio=rollapply(mean_nsd_ratio, width = 6, FUN = median, fill = NA, align = "left")
#          )

# for last 6 days egg laying + first day incubating algorithm
breeding.metrics_7d <- breeding.metrics2 %>% group_by(birdid_year) %>%
  mutate(Absx_mean=rollapply(mean_abs_x_sunrise, width = 7, FUN = mean, fill = NA, align = "left"),
         ddist_mean=rollapply(median_ddist, width = 7, FUN = mean, fill = NA, align = "left"),
         Absx_var=rollapply(mean_abs_x_sunrise, width = 7, FUN = sd, fill = NA, align = "left"),
         # when adding ratio variables
         ODBA_median_ratio=rollapply(mean_ODBA_ratio, width = 7, FUN = median, fill = NA, align = "left"),
         Absx_var_ratio=rollapply(mean_abs_x_ratio, width = 7, FUN = sd, fill = NA, align = "left")
  )

# just checking it's working how I expect!  
mean(breeding.metrics2$mean_ODBA_sunrise[1:7])
sd(breeding.metrics2$mean_abs_x_sunrise[1:7])

# remove NA's
#breeding.metrics_6d <- na.omit(breeding.metrics_6d)
breeding.metrics_7d <- na.omit(breeding.metrics_7d)

n6 <- unique(breeding.metrics_7d$birdid_year)
  
#### 2. Apply finetuned incubation model to unknown black duck/mallard data ####
# load algorithm
### these ones below include 2025 data!!
# fine tuned svm model using 1 to 5 hours post-sunrise + 3 ratio variables with 6 days egg laying only using a tune length of 25, 2025 data included (26 Sept 2025) 
#load("svm_tunelength25_egglaying_6dayEggLayingWith1to5HoursPostSunrisePlus3RatioVars_absxmeanVarRatios_odbaRatio_ddist_2025dataIncl.RData") # tuned_svm
# fine tuned rf model using 1 to 5 hours post-sunrise + 3 ratio variables with 6 days egg laying only using a tune length of 45, 2025 data included (26 Sept 2025) 
#load("rf_tunelength45_egglaying_6dayEggLayingWith1to5HoursPostSunrisePlus3RatioVars_absxmeanVarRatios_odbaRatio_ddist_2025dataIncl.RData") # tuned_rf
# fine tuned rf model after feature selection using 1 to 5 hours post-sunrise + 3 ratio variables with 6 days egg laying only using a tune length of 30, 2025 data included (26 Sept 2025) 
#load("rf_RFE_tunelength30_egglaying_6dayEggLayingWith1to5HoursPostSunrisePlus3RatioVars_absxvar_absxmeanVarRatios_odbaRatio_2025dataIncl.RData") # tuned_rf_opt
# fine tuned svm model after feature selection using 1 to 5 hours post-sunrise + 3 ratio variables with 6 days egg laying only using a tune length of 25, 2025 data included (26 Sept 2025) 
#load("svm_RFE_tunelength25_egglaying_6dayEggLayingWith1to5HoursPostSunrisePlus3RatioVars_absxvar_absxmeanRatio_2025dataIncl.RData") # tuned_svm_opt
### uncalibrated ACC
# base rf model for first 6 days of egg laying
#load("rf_basemodel_egglaying_First6daysLayingWith1to5HoursPostSunrisePlusRatioVars_odbamean_absxvar_absxmeanRatio_nsdmedianRatio_UncalibACC.RData") # model_svm
# fine-tuned rf model for last 6 days of egg laying
#load("rf_tunelength20_egglaying_Last6daysLayingWith1to5HoursPostSunrisePlusRatioVars_absxmean_absxvar_ddistmean_absxmeanVarRatio_ODBAmedianRatio_UncalibACC.RData") # tuned_svm
# base rf model for last 6 days of egg laying + 1 day incubation
load("rf_basemodel_egglaying_Last6dLayingPlus1dInc_1to5HoursPostSunrisePlusRatioVars_absxmean_absxvar_ddistmean_absxVarRatio_ODBAmedianRatio_Uncalib.Rdata") # model_svm


# use to predict on unknown data
# first 6 days of egg laying
#pred_abdu <- predict(model_svm, newdata = breeding.metrics_6d) 
# last 6 days of egg laying
#pred_abdu <- predict(tuned_svm, newdata = breeding.metrics_6d) 
# last 6 days of egg laying + 1 day incubation
pred_abdu <- predict(model_svm, newdata = breeding.metrics_7d) 



# combine predictions with breeding metrics
breeding.labels <- data.frame(status = pred_abdu)
breeding.metrics_6d.labeled <- cbind(breeding.metrics_7d, breeding.labels)

# add breeding outcome column, where status hatched > failed > defer
breeding.metrics_6d.labeled <- breeding.metrics_6d.labeled %>% 
  group_by(birdid_year) %>%
  mutate(breeding_outcome=if_else(all(status == "defer"), "defer", "nested"))# %>%
  #mutate(breeding_outcome=if_else(rep(any(status == "hatched"),n()), "hatched", breeding_outcome))

# count number of birds per category
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

summary <- breeding.metrics_6d.labeled %>% group_by(birdid_year) %>%
  summarize(breeding_outcome=get_mode(breeding_outcome))

summary %>% group_by(breeding_outcome) %>% count()

# reduce labelled data to only egg laying days
labelled.data_laying_days_only <- breeding.metrics_6d.labeled %>% filter(status == "egg_laying") %>%
  mutate(date=as.Date(date))

# remove egg laying dates after Jun 30
labelled.data_laying_days_only <- labelled.data_laying_days_only %>% 
  mutate(month=month(date)) %>%
  filter(month <= 6)

# adding column to distinguish different egg laying bouts (previously did 3 days, but now trying 6 because the window size is 6)
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

max(summary_number_per_group$count_n)

sum(summary_number_per_group$count_n)

# # save labelled data
# write.csv(breeding.metrics_6d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/WildBirdData_2021to2023_EggLayingClassifiedBy1to5HourPostSunrisePeriod6dWindowRFmodel_27Aug2025.csv")
# write.csv(breeding.metrics_6d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/WildBirdData_2021to2023_EggLayingClassified_1to5HourPostSunrisePlus3RatioVars_6dWindowSVMmodel_29Aug2025.csv")
# write.csv(breeding.metrics_6d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/WildBirdData_2021to2023_EggLayingClassified_1to5HourPostSunrisePlus3RatioVars_6dWindowSVMmodel_Filtered_8Sept2025.csv")
# # includes 2025 data
# write.csv(breeding.metrics_6d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/ABDUData_2021to2023_EggLayingClassified_1to5HourPostSunrisePlus3RatioVars_6dWindowSVM_FT_Filtered_1Oct2025.csv")
# write.csv(breeding.metrics_6d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/ABDUData_2021to2023_EggLayingClassified_1to5HourPostSunrisePlus3RatioVars_6dWindowSVM_RFE_Filtered_1Oct2025.csv")
# write.csv(breeding.metrics_6d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/ABDUData_2021to2023_EggLayingClassified_1to5HourPostSunrisePlus3RatioVars_6dWindowRF_FT_Filtered_1Oct2025.csv")
# # uncalibrated ACC
# write.csv(breeding.metrics_6d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/MALLData_2022to2025_EggLayingClassified_First6DaysLaying_RF_Filtered_18Oct2025.csv")
# write.csv(breeding.metrics_6d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/MALLData_2022to2025_EggLayingClassified_Last6DaysLaying_RF_Filtered_18Oct2025.csv")
# write.csv(breeding.metrics_6d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/MALLData_2022to2025_EggLayingClassified_Last6DaysLayingAnd1Inc_RF_Filtered_18Oct2025.csv")


# removing date_diff column that is causing issues!
breeding.metrics_6d.labeled <- breeding.metrics_6d.labeled[, -which(names(breeding.metrics_6d.labeled) == "date_diff")]
# # saving old classified data when I filtered out birds without data to July 1
# temp <- tbl(conn, "mall_egglaying_classified_last6daysAnd1inc") %>%
#   collect()
# dbWriteTable(conn, "mall_egglaying_classified_last6daysAnd1inc_old", temp, append = TRUE, row.names = FALSE)
# send labeled data to db
#dbWriteTable(conn, "mall_egglaying_classified_first6days", breeding.metrics_6d.labeled, append = TRUE, row.names = FALSE)
#dbWriteTable(conn, "mall_egglaying_classified_last6days", breeding.metrics_6d.labeled, append = TRUE, row.names = FALSE)
#dbRemoveTable(conn, "mall_egglaying_classified_last6daysAnd1inc")
dbWriteTable(conn, "mall_egglaying_classified_last6daysAnd1inc_30Jun26", breeding.metrics_6d.labeled, append = TRUE, row.names = FALSE)


# #### 3. Plot to assess how well the algorithm is functioning ####
# breeding.metrics_6d.labeled <- tbl(conn, "mall_egglaying_classified_last6daysAnd1inc_ILSA") %>%
#   collect()
# 
# library(ggpubr)
# library(ggplot2)
# library(grid)
# summary(breeding.metrics_6d.labeled)
# breeding.metrics_6d.labeled$status <- as.factor(breeding.metrics_6d.labeled$status)
# breeding.metrics_6d.labeled$breeding_outcome <- as.factor(breeding.metrics_6d.labeled$breeding_outcome)
# 
# # rename status to something that makes more sense for plots
# library(plyr)
# breeding.metrics_6d.labeled$Classification <- revalue(breeding.metrics_6d.labeled$status, c("egg_laying" = "Egg laying",
#                                                    "defer" = "Defer"))
# 
# detach("package:plyr", unload = TRUE)
# 
# unique(breeding.metrics_6d.labeled$Classification)  
# 
# viridis(4)
# 
# "#440154" "#30678D" "#36B677" "#FDE725"
# 
# "#440154FF" "#31688EFF"
# 
# #### single plot for manuscript (egg laying)
# nested_id <- "2137-20138"
# nested_year <- "2023"
# defer_id <- "2137-20138"
# defer_year <- "2024"
# 
# setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics")
# tiff("plots_6dayWindow_EggLaying_12Aug2025/ExampleOfNestedAndDeferForManuscript_LongVersion.tiff", units="in", width=20, height=12, res=300)
# 
# d<-breeding.metrics_6d.labeled %>% filter(birdid==nested_id & year==nested_year)
# theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
# g<-ggarrange(
#   ggplot(d,aes(x=date,y=ddist_mean,color=Classification))+
#     geom_point() + 
#     #geom_line(size=1)+
#     #geom_hline(yintercept = min(d$ddist_mean, na.rm = T), color = "#31688EFF", linetype = "dashed")+
#     scale_color_manual(values = c("Defer" = "#440154FF", "Egg laying" = "#36B677"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 7-day mean of median DDIST", x = "Date")+ theme(plot.margin = margin(1,0.5,0.1,0.05, 'cm')),
#   ggplot(d,aes(x=date,y=Absx_mean,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_mean, na.rm = T), color = "#36B677", linetype = "dashed")+
#     scale_color_manual(values = c("Defer" = "#440154FF", "Egg laying" = "#36B677"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 7-day mean of mean ABSX", x = "Date")+ theme(plot.margin = margin(1,0.5,0.1,0, 'cm')),
#   ggplot(d,aes(x=date,y=Absx_var,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_var, na.rm = T), color = "#36B677", linetype = "dashed")+
#     scale_color_manual(values = c("Defer" = "#440154FF", "Egg laying" = "#36B677"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 7-day SD of mean ABSX", x = "Date")+ theme(plot.margin = margin(1,0.5,0.1,0, 'cm')),
#   ggplot(d,aes(x=date,y=ODBA_median_ratio,color=Classification))+
#     geom_point() + 
#     #geom_line()+
#     #geom_hline(yintercept = min(d$ODBA_median_ratio, na.rm = T), color = "#36B677", linetype = "dashed")+
#     scale_color_manual(values = c("Defer" = "#440154FF", "Egg laying" = "#36B677"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 7-day median of mean ODBA ratio", x = "Date")+ theme(plot.margin = margin(1,0.5,0.1,0.05, 'cm')),
#   ggplot(d,aes(x=date,y=Absx_var_ratio,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_var_ratio, na.rm = T), color = "#36B677", linetype = "dashed")+
#     scale_color_manual(values = c("Defer" = "#440154FF", "Egg laying" = "#36B677"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 7-day SD of mean ABSX ratio", x = "Date")+ theme(plot.margin = margin(1,0.5,0.1,0, 'cm')),
#   ncol=5,nrow = 1,common.legend = TRUE)
# # g <- g + annotation_custom(
# #   grob = textGrob(paste0(nested_id, " in ", nested_year, ": ", d$breeding_outcome), 
# #                   gp = gpar(fontsize = 12, fontface = "bold", col="#36B677")),
# #   xmin = 0.7, xmax = Inf, ymin = 0.95, ymax = Inf  )
# e<-breeding.metrics_6d.labeled %>% filter(birdid==defer_id & year==defer_year)
# theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
# h<-ggarrange(
#   ggplot(e,aes(x=date,y=ddist_mean,color=Classification))+
#     geom_point() + 
#     #geom_line(size=1)+
#     #geom_hline(yintercept = min(d$ddist_mean, na.rm = T), color = "#36B677", linetype = "dashed")+
#     scale_color_manual(values = c("Defer" = "#440154FF", "Egg laying" = "#36B677"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 7-day mean of median DDIST", x = "Date")+ theme(plot.margin = margin(1,0.5,0.1,0.05, 'cm')),
#   ggplot(e,aes(x=date,y=Absx_mean,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_mean, na.rm = T), color = "#36B677", linetype = "dashed")+
#     scale_color_manual(values = c("Defer" = "#440154FF", "Egg laying" = "#36B677"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 7-day mean of mean ABSX", x = "Date")+ theme(plot.margin = margin(1,0.5,0.1,0, 'cm')),
#   ggplot(e,aes(x=date,y=Absx_var,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_var, na.rm = T), color = "#36B677", linetype = "dashed")+
#     scale_color_manual(values = c("Defer" = "#440154FF", "Egg laying" = "#36B677"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 7-day SD of mean ABSX", x = "Date")+ theme(plot.margin = margin(1,0.5,0.1,0, 'cm')),
#   ggplot(e,aes(x=date,y=ODBA_median_ratio,color=Classification))+
#     geom_point() + 
#     #geom_line()+
#     #geom_hline(yintercept = min(d$ODBA_median_ratio, na.rm = T), color = "#36B677", linetype = "dashed")+
#     scale_color_manual(values = c("Defer" = "#440154FF", "Egg laying" = "#36B677"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 7-day median of mean ODBA ratio", x = "Date")+ theme(plot.margin = margin(1,0.5,0.1,0.05, 'cm')),
#   ggplot(e,aes(x=date,y=Absx_var_ratio,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_var_ratio, na.rm = T), color = "#36B677", linetype = "dashed")+
#     scale_color_manual(values = c("Defer" = "#440154FF", "Egg laying" = "#36B677"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 7-day SD of mean ABSX ratio", x = "Date")+ theme(plot.margin = margin(1,0.5,0.1,0, 'cm')),
#   ncol=5,nrow = 1, legend = "none"
#   #,common.legend = TRUE
#   )
# # h <- h + annotation_custom(
# #   grob = textGrob(paste0(defer_id, " in ", defer_year, ": ", d$breeding_outcome), 
# #                   gp = gpar(fontsize = 12, fontface = "bold", col="#36B677")),
# #   xmin = 0.7, xmax = Inf, ymin = 0.95, ymax = Inf  )
# 
# ggarrange(g,h, ncol=1, nrow=2,# common.legend = TRUE,
#           labels = c("Overall Status: Nested", "Overall Status: Defer"))
# x = c(0, 0.5, 1#, 0.5, 0.5, 0.5
#       )
# y = c(0.5, 0.5, 0.5#,0, 0.5, 1
#       )
# id = c(1,1,1#,2,2,2
#        )
# grid.polygon(x,y,id)
# dev.off()
# 
# list_obs_final<-breeding.metrics_6d.labeled %>%distinct(birdid, year)
# pdf("plots_6dayWindow_EggLaying_12Aug2025/unknown_mall_removed_by_missing_data_filtering_step_Last6dEggLaying1Inc_daily_13Apr2026.pdf",paper="a4r",width=9,height=6)
# #last 6 days variables
# for (i in 1:nrow(list_obs_final)){
#   d<-breeding.metrics_6d.labeled %>% filter(birdid==list_obs_final[i,2] & year==list_obs_final[i,3])
#   theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
#   g<-ggarrange(
#     ggplot(d,aes(x=date,y=ddist_mean,color=status))+
#       geom_point() + 
#       #geom_line(size=1)+
#       geom_hline(yintercept = min(d$ddist_mean, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day mean of median DDIST"),
#     ggplot(d,aes(x=date,y=Absx_mean,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_mean, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day mean of mean ABSX"),
#    # ggplot(d,aes(x=date,y=ODBA_median,color=status))+
#     #  geom_point() + 
#       #geom_line()+
#      # geom_hline(yintercept = min(d$ODBA_median, na.rm = T), color = "red", linetype = "dashed")+
#       #scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       #scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       #labs( y = "Rolling 6-day median of mean ODBA"),
#     ggplot(d,aes(x=date,y=Absx_var,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_var, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day SD of mean ABSX"),
#     # ggplot(d,aes(x=date,y=Absx_mean_ratio,color=status))+
#     #   geom_point() +
#     #   #geom_line()+
#     #   geom_hline(yintercept = min(d$Absx_mean_ratio, na.rm = T), color = "red", linetype = "dashed")+
#     #   scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#     #   scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     #   labs( y = "Rolling 6-day mean of mean ABSX ratio"),
#     ggplot(d,aes(x=date,y=ODBA_median_ratio,color=status))+
#       geom_point() + 
#       #geom_line()+
#       geom_hline(yintercept = min(d$ODBA_median_ratio, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day median of mean ODBA ratio"),
#     ggplot(d,aes(x=date,y=Absx_var_ratio,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_var_ratio, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day SD of mean ABSX ratio"),
#     ncol=3,nrow = 2,common.legend = TRUE)
#   g <- g + annotation_custom(
#     grob = textGrob(paste0(list_obs_final[i, 2], " in ", list_obs_final[i, 3], ": ", d$breeding_outcome), 
#                     gp = gpar(fontsize = 12, fontface = "bold", col="red")),
#     xmin = 0.7, xmax = Inf, ymin = 0.95, ymax = Inf  )# Adjust the positioning as needed
#   print(g)
# }
# dev.off()
# rm(g,d,i)
# 
# #first 6 days of egg laying
# for (i in 1:nrow(list_obs_final)){
#   d<-breeding.metrics_6d.labeled %>% filter(birdid==list_obs_final[i,2] & year==list_obs_final[i,3])
#   theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
#   g<-ggarrange(
#     ggplot(d,aes(x=date,y=Absx_var,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_var, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day SD of mean ABSX"),
#     ggplot(d,aes(x=date,y=Absx_mean_ratio,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_mean_ratio, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day mean of mean ABSX ratio"),
#     ggplot(d,aes(x=date,y=Absx_var_ratio,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_var_ratio, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day SD of mean ABSX ratio"),
#     ggplot(d,aes(x=date,y=nsd_median_ratio,color=status))+
#       geom_point() + 
#       #geom_line(size=1)+
#       geom_hline(yintercept = min(d$nsd_median_ratio, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day median of mean NSD ratio"),
#     ggplot(d,aes(x=date,y=ODBA_mean,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$ODBA_mean, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day mean of mean ODBA"),
#     ncol=2,nrow = 2,common.legend = TRUE)
#   g <- g + annotation_custom(
#     grob = textGrob(paste0(list_obs_final[i, 2], " in ", list_obs_final[i, 3], ": ", d$breeding_outcome), 
#                     gp = gpar(fontsize = 12, fontface = "bold", col="red")),
#     xmin = 0.7, xmax = Inf, ymin = 0.95, ymax = Inf  )# Adjust the positioning as needed
#   print(g)
# }
# dev.off()
# 
# ### ratio!!!
# for (i in 1:nrow(list_obs_final)){
#   d<-breeding.metrics_6d.labeled %>% filter(birdid==list_obs_final[i,2] & year==list_obs_final[i,3])
#   theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
#   g<-ggarrange(
#     ggplot(d,aes(x=date,y=nsd_median,color=status))+
#       geom_point() + 
#       #geom_line(size=1)+
#       geom_hline(yintercept = min(d$nsd_median, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day median of mean NSD ratio"),
#     ggplot(d,aes(x=date,y=Absx_median,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_median, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day median of mean ABSX ratio"),
#     ggplot(d,aes(x=date,y=ODBA_median,color=status))+
#       geom_point() + 
#       #geom_line()+
#       geom_hline(yintercept = min(d$ODBA_median, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day median of mean ODBA ratio"),
#     ggplot(d,aes(x=date,y=Absx_var,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_var, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day SD of mean ABSX ratio"),
#     ncol=2,nrow = 2,common.legend = TRUE)
#   g <- g + annotation_custom(
#     grob = textGrob(paste0(list_obs_final[i, 2], " in ", list_obs_final[i, 3], ": ", d$breeding_outcome), 
#                     gp = gpar(fontsize = 12, fontface = "bold", col="red")),
#     xmin = 0.7, xmax = Inf, ymin = 0.95, ymax = Inf  )# Adjust the positioning as needed
#   print(g)
# }
# dev.off()
# 
# pdf("plots_6dayWindow_EggLaying_12Aug2025/unknown_mall_filtered_by_mig_First6dEggLaying_daily_LeftAlignRollWindow_RF_18Oct2025.pdf",paper="a4r",width=9,height=6)
# for (i in 1:nrow(list_obs_final)){
#   d<-breeding.metrics_6d.labeled %>% filter(birdid==list_obs_final[i,2] & year==list_obs_final[i,3])
#   theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
#   g<-ggarrange(
#     ggplot(d,aes(x=date,y=ddist_mean,color=status))+
#       geom_point() + 
#       #geom_line(size=1)+
#       geom_hline(yintercept = min(d$ddist_mean, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day mean of median DDIST"),
#     ggplot(d,aes(x=date,y=Absx_mean,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_mean, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day mean of mean ABSX"),
#     # ggplot(d,aes(x=date,y=ODBA_median,color=status))+
#     #  geom_point() + 
#     #geom_line()+
#     # geom_hline(yintercept = min(d$ODBA_median, na.rm = T), color = "red", linetype = "dashed")+
#     #scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#     #scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     #labs( y = "Rolling 6-day median of mean ODBA"),
#     ggplot(d,aes(x=date,y=Absx_var,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_var, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day SD of mean ABSX"),
#     ggplot(d,aes(x=date,y=Absx_mean_ratio,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_mean_ratio, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day mean of mean ABSX ratio"),
#     ggplot(d,aes(x=date,y=ODBA_median_ratio,color=status))+
#       geom_point() + 
#       #geom_line()+
#       geom_hline(yintercept = min(d$ODBA_median_ratio, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day median of mean ODBA ratio"),
#     ggplot(d,aes(x=date,y=Absx_var_ratio,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_var_ratio, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "egg_laying" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 6-day SD of mean ABSX ratio"),
#     ncol=4,nrow = 2,common.legend = TRUE)
#   g <- g + annotation_custom(
#     grob = textGrob(paste0(list_obs_final[i, 2], " in ", list_obs_final[i, 3], ": ", d$breeding_outcome), 
#                     gp = gpar(fontsize = 12, fontface = "bold", col="red")),
#     xmin = 0.7, xmax = Inf, ymin = 0.95, ymax = Inf  )# Adjust the positioning as needed
#   print(g)
# }
# dev.off()
# rm(g,d,i)

