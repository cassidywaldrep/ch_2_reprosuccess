
#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: Reproductive Metrics - Machine Learning
# Date: 30 July 2025
# Author: Ilsa Griebel
# Description: Applying finetuned incubation model to unknown black duck data and 
#               plotting to asses function
# 1. Summarize daily metrics over 28-day window for input to algorithm
# 2. Apply finetuned incubation model to unknown black duck data
# 3. Plot to assess how well the algorithm is functioning 
#**********************************************************************************************************************************
#**********************************************************************************************************************************

#### 1. Summarize daily metrics over 28-day window for input to algorithm ####
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
  dplyr::select(c("birdid_year", "date", "birdid", "year", "device_id", "mean_ODBA", "mean_abs_x"
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
# # look at distribution of max_day for each year
# hist(max_day_2022$max_date, breaks = "weeks", freq = TRUE)
# hist(max_day_2023$max_date, breaks = "weeks", freq = TRUE)
# hist(max_day_2024$max_date, breaks = "weeks", freq = TRUE)
# hist(max_day_2025$max_date, breaks = "weeks", freq = TRUE)
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
# n3 <- unique(breeding.metrics2$birdid_year) # 872 bird-years
# 
# rm(max_day)

# only apply incubation algorithm to July 31 ( ABDU and MALL) 
filter_2022 <- breeding.metrics2 %>% filter(year == "2022") %>% 
  filter(date <= "2022-07-31")
filter_2023 <- breeding.metrics2 %>% filter(year == "2023") %>% 
  filter(date <= "2023-07-31")
filter_2024 <- breeding.metrics2 %>% filter(year == "2024") %>% 
  filter(date <= "2024-07-31")
filter_2025 <- breeding.metrics2 %>% filter(year == "2025") %>% 
  filter(date <= "2025-07-31")

# max(filter_2022$date)
# max(filter_2023$date)
# max(filter_2024$date)
# max(filter_2025$date)

breeding.metrics2 <- rbind(filter_2022, filter_2023, filter_2024, filter_2025)

breeding.metrics2 <- breeding.metrics2[order(breeding.metrics2$birdid_year, breeding.metrics2$date), ]

## also remove any birds that missing 2 or more consecutive days of data (suggestive of battery issues) --- used to be 5 but tested this on training data and decided to use 1 day!!!!
# add column for number of days between daily rows
breeding.metrics2 <- breeding.metrics2 %>% group_by(birdid_year) %>%
  mutate(date_diff=date-lag(date))

hist(as.numeric(breeding.metrics2$date_diff))

# create dataframe containing only birds with a date difference > 2 
greater_than_2days <- breeding.metrics2 %>% filter(date_diff>2) # >=2, removes birds missing 1 day of data; >2, removes birds missing 2 or more consecutive days of data
hist(as.numeric(greater_than_2days$date_diff))

n_rem<-unique(greater_than_2days$birdid_year) # should remove 67 bird-year's (now 78 bird-year's for MALL) - now 102 with removing birds missing 2 or more days

# remove birds with any date diff's >= 2  
breeding.metrics2 <- breeding.metrics2 %>% filter(!birdid_year %in% greater_than_2days$birdid_year)

n4 <- unique(breeding.metrics2$birdid_year) # 805 bird-years (now 1152 bird-years with not removing birds without complete data to July 1) -- now 1117 with removing birds missing 2 or more days

rm(greater_than_2days)

## only keep birds with at least 26 days of data (window size for incubation algorithm)
# count number of days per bird_year
num_days_per_birdyear <- breeding.metrics2 %>%
  group_by(birdid_year) %>%
  summarise(n=n())

# create dataframe containing only bird-year's with less than 26 days and take a look at the distribution
less_than_26_days <- num_days_per_birdyear %>% filter(n <26) # 110 bird-years
hist(as.numeric(less_than_26_days$n))

# remove bird-years with less than 26 days  
breeding.metrics2 <- breeding.metrics2 %>% filter(!birdid_year %in% less_than_26_days$birdid_year)

n5 <- unique(breeding.metrics2$birdid_year) # 1042 bird-years (MALL), now 1007 with removing birds with more than 2 days missing

rm(less_than_26_days)

# calculate rolling mean/sd for four input variables (ODBA_mean + Absx_mean + ddist_mean + Absx_var)
  # centered (date is in the middle/centre of the 28 day window)
#breeding.metrics_28d <- breeding.metrics4 %>% group_by(birdid_year) %>%
 # mutate(ODBA_mean=roll_mean(mean_ODBA, width = 28),
  #       Absx_mean=roll_mean(mean_abs_x, width = 28),
   #      ddist_mean=roll_mean(median_ddist, width = 28),
    #     Absx_var=roll_sd(mean_abs_x, width = 28))

  # from left (so window will be calculated from the date to 26 days after)
breeding.metrics_28d <- breeding.metrics2 %>% group_by(birdid_year) %>%
  mutate(ODBA_mean=rollapply(mean_ODBA, width = 26, FUN = mean, fill = NA, align = "left"),
         Absx_mean=rollapply(mean_abs_x, width = 26, FUN = mean, fill = NA, align = "left"),
         ddist_mean=rollapply(median_ddist, width = 26, FUN = mean, fill = NA, align = "left"),
         Absx_var=rollapply(mean_abs_x, width = 26, FUN = sd, fill = NA, align = "left"))


# just checking it's working how I expect!  
mean(breeding.metrics2$mean_ODBA[1:26])
sd(breeding.metrics2$mean_abs_x[1:26])

# remove NA's
breeding.metrics_28d <- na.omit(breeding.metrics_28d)

n6 <- unique(breeding.metrics_28d$birdid_year)
  
#### 2. Apply finetuned incubation model to unknown black duck/mallard data ####
# load algorithm

# fine-tuned model using only two variables (absx mean and absx sd)
#load("svm_tuned_incubation_28dayinterval_absxmeanANDvar.RData")

# base model using 4 variables (absx mean and sd, odba mean and median ddist mean)
#load("svm_base_incubation_28dayinterval_absxmeanANDvar_odba_ddist.RData")

# new fine tuned 4 variable model using a tune length of 20 (6 Aug 2025)
#load("svm_tunelength20_incubation_28dayinterval_absxmeanANDvar_odba_ddist.RData")

# new fine tuned 2 variable model from RFE using a tune length of 30 (6 Aug 2025) - probably the same as previous two variable model??
#load("svm_tunelength30andRFE_incubation_28dayinterval_absxmeanANDvar.RData")

# base model, 4 variables, 2025 data included (25 Sept 2025)
#load("rf_basemodel_incubation_28dayinterval_absxmeanANDvar_odba_ddist_2025dataIncl.RData")

# RF base model, 4 variables, 2025 data included, ACC UNCALIBRATED!! (18 Oct 2025)
# setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics") # laptop
load("rf_basemodel_incubation_26dayinterval_absxmeanANDvar_odba_ddist_UncalibratedACC.RData") #model_svm

# use to predict on unknown data
#pred_abdu <- predict(tuned_svm, newdata = breeding.metrics_28d) # using 2 variable, finetuned model
#pred_abdu <- predict(model_svm, newdata = breeding.metrics_28d) # using 4 variable, base model
#pred_abdu <- predict(tuned_svm, newdata = breeding.metrics_28d) # using 4 variables, finetuned model with tune length of 20
#pred_abdu <- predict(tuned_svm_opt, newdata = breeding.metrics_28d) # using 2 variables from RFE, finetuned model with tune length of 30
#pred_abdu <- predict(model_svm, newdata = breeding.metrics_28d) # using 4 variables, base model, 2025 data included

pred_abdu <- predict(model_svm, newdata = breeding.metrics_28d) # using 4 variables, base model, 2025 data included, ACC UNCALIBRATED!

# combine predictions with breeding metrics
breeding.labels <- data.frame(status = pred_abdu)
breeding.metrics_28d.labeled <- cbind(breeding.metrics_28d, breeding.labels)

# add breeding outcome column, where status hatched > failed > defer
breeding.metrics_28d.labeled <- breeding.metrics_28d.labeled %>% 
  group_by(birdid_year) %>%
  mutate(breeding_outcome=if_else(all(status == "defer"), "defer", "failed")) %>%
  mutate(breeding_outcome=if_else(rep(any(status == "hatched"),n()), "hatched", breeding_outcome))

# count number of birds per category
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

summary <- breeding.metrics_28d.labeled %>% group_by(birdid_year) %>%
  summarize(breeding_outcome=get_mode(breeding_outcome))

summary %>% group_by(breeding_outcome) %>% count()

# save labeled data
# write.csv(breeding.metrics_28d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/WildBirdData_2021to2023_IncubationClassifiedBy4Var28dWindowSVMmodel_22Aug2025.csv")
# write.csv(breeding.metrics_28d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/WildBirdData_2021to2023_IncubationClassifiedBy4Var28dWindowSVMmodel_FilterByMig_8Sept2025.csv")
# write.csv(breeding.metrics_28d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/ABDUData_2021to2023_IncubationClassifiedBy4Var28dWindowRFmodel_FilterByMig_2025Incl_1Oct2025.csv")
# write.csv(breeding.metrics_28d.labeled, "WildBirdData_ClassifiedDatasets_22Aug2025/MALLData_2022to2025_IncubationClassifiedBy4Var26dWindowRFmodel_FilterByMig_UncalibACC_18Oct2025.csv")

# send labeled data to db
# removing date_diff column that is causing issues!
breeding.metrics_28d.labeled <- breeding.metrics_28d.labeled[, -which(names(breeding.metrics_28d.labeled) == "date_diff")]
# # saving old classified data when I filtered out birds without data to July 1
# temp <- tbl(conn, "mall_incubation_classified") %>%
#   collect()
# dbWriteTable(conn, "mall_incubation_classified_old", temp, append = TRUE, row.names = FALSE)
# dbRemoveTable(conn, "mall_incubation_classified")
dbWriteTable(conn, "mall_incubation_classified_30June26", breeding.metrics_28d.labeled, append = TRUE, row.names = FALSE)

inc <- tbl(conn, "mall_incubation_classified_ILSA") %>%
  collect()



inc %>%
  dplyr::select(birdid_year, breeding_outcome) %>%
  distinct() %>%
  group_by(breeding_outcome) %>%
  summarize(count = n())


# #### 3. Plot to assess how well the algorithm is functioning ####
# breeding.metrics_28d.labeled <- tbl(conn, "abdu_incubation_classified") %>%
#   collect()
# 
# 
# library(ggpubr)
# library(ggplot2)
# library(grid)
# summary(breeding.metrics_28d.labeled)
# breeding.metrics_28d.labeled$status <- as.factor(breeding.metrics_28d.labeled$status)
# breeding.metrics_28d.labeled$breeding_outcome <- as.factor(breeding.metrics_28d.labeled$breeding_outcome)
# 
# list_obs_final<-breeding.metrics_28d.labeled %>%distinct(birdid, year)
# pdf("plots_28dayWindow_21Jul2025/unknown_mall_removed_by_missing_data_filtering_step_FullTermIncubation_daily_13Apr2026.pdf",paper="a4r",width=9,height=6)
# for (i in 1:nrow(list_obs_final)){
#   d<-breeding.metrics_28d.labeled %>% filter(birdid==list_obs_final[i,2] & year==list_obs_final[i,3])
#   theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
#   g<-ggarrange(
#     ggplot(d,aes(x=date,y=ddist_mean,color=status))+
#       geom_point() + 
#       #geom_line(size=1)+
#       geom_hline(yintercept = min(d$ddist_mean, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "failed" = "blue", "hatched" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 28-day mean of median DDIST"),
#     ggplot(d,aes(x=date,y=Absx_mean,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_mean, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "failed" = "blue", "hatched" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 28-day mean of mean ABSX"),
#     ggplot(d,aes(x=date,y=ODBA_mean,color=status))+
#       geom_point() + 
#       #geom_line()+
#       geom_hline(yintercept = min(d$ODBA_mean, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "failed" = "blue", "hatched" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 28-day mean of mean ODBA"),
#     ggplot(d,aes(x=date,y=Absx_var,color=status))+
#       geom_point() +
#       #geom_line()+
#       geom_hline(yintercept = min(d$Absx_var, na.rm = T), color = "red", linetype = "dashed")+
#       scale_color_manual(values = c("defer" = "black", "failed" = "blue", "hatched" = "red"))+
#       scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#       labs( y = "Rolling 28-day SD of mean ABSX"),
#     ncol=2,nrow = 2,common.legend = TRUE)
#   g <- g + annotation_custom(
#     grob = textGrob(paste0(list_obs_final[i, 2], " in ", list_obs_final[i, 3], ": ", d$breeding_outcome), 
#                     gp = gpar(fontsize = 12, fontface = "bold", col="red")),
#     xmin = 0.7, xmax = Inf, ymin = 0.95, ymax = Inf  )# Adjust the positioning as needed
#   print(g)
# }
# dev.off()
# rm(g,d,i)
# 
# 
# #### single plot for manuscript (incubation)
# breeding.metrics_28d.labeled <- tbl(conn, "mall_incubation_classified") %>%
#   collect()
# 
# hatched_id <- "2117-05291"
# hatched_year <- "2025"
# failed_id <- "2087-64995"
# failed_year <- "2025"
# defer_id <- "2087-66898"
# defer_year <- "2025"
# 
# # rename status to something that makes more sense for plots
# library(plyr)
# breeding.metrics_28d.labeled$Classification <- revalue(breeding.metrics_28d.labeled$status, c("failed" = "Failed incubation",
#                                                                                             "defer" = "Non-incubating",
#                                                                                             "hatched" = "Successful incubation"))
# 
# detach("package:plyr", unload = TRUE)
# 
# unique(breeding.metrics_28d.labeled$Classification)  
# 
# viridis(4)
# "#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"
# 
# setwd("C:/Users/ilsag/OneDrive - University of Saskatchewan/PhD/R/data/Identifying Reproductive Metrics")
# tiff("plots_28dayWindow_21Jul2025/ExampleOfIncubationOutcomesForManuscript_longVersion.tiff", units="in", width=12, height=13, res=300)
# 
# d1<-breeding.metrics_28d.labeled %>% filter(birdid==hatched_id & year==hatched_year)
# theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
# h<-ggarrange(
#   ggplot(d1,aes(x=date,y=ddist_mean,color=Classification))+
#     geom_point() + 
#     #geom_line(size=1)+
#     #geom_hline(yintercept = min(d$ddist_mean, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day mean of median DDIST", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ggplot(d1,aes(x=date,y=Absx_mean,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_mean, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day mean of mean ABSX", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ggplot(d1,aes(x=date,y=ODBA_mean,color=Classification))+
#     geom_point() + 
#     #geom_line()+
#     #geom_hline(yintercept = min(d$ODBA_mean, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day mean of mean ODBA", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ggplot(d1,aes(x=date,y=Absx_var,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_var, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day SD of mean ABSX", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ncol=4,nrow = 1,common.legend = TRUE, legend="bottom")
# 
# d2<-breeding.metrics_28d.labeled %>% filter(birdid==failed_id & year==failed_year)
# theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
# f<-ggarrange(
#   ggplot(d2,aes(x=date,y=ddist_mean,color=Classification))+
#     geom_point() + 
#     #geom_line(size=1)+
#     #geom_hline(yintercept = min(d$ddist_mean, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day mean of median DDIST", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ggplot(d2,aes(x=date,y=Absx_mean,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_mean, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day mean of mean ABSX", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ggplot(d2,aes(x=date,y=ODBA_mean,color=Classification))+
#     geom_point() + 
#     #geom_line()+
#     #geom_hline(yintercept = min(d$ODBA_mean, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day mean of mean ODBA", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ggplot(d2,aes(x=date,y=Absx_var,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_var, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day SD of mean ABSX", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ncol=4,nrow = 1, legend = "bottom", common.legend = TRUE
#   )
# 
# d3<-breeding.metrics_28d.labeled %>% filter(birdid==defer_id & year==defer_year)
# theme_set(theme_classic() + theme(axis.text.x = element_text(angle = 90)))
# d<-ggarrange(
#   ggplot(d3,aes(x=date,y=ddist_mean,color=Classification))+
#     geom_point() + 
#     #geom_line(size=1)+
#     #geom_hline(yintercept = min(d$ddist_mean, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day mean of median DDIST", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ggplot(d3,aes(x=date,y=Absx_mean,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_mean, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day mean of mean ABSX", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ggplot(d3,aes(x=date,y=ODBA_mean,color=Classification))+
#     geom_point() + 
#     #geom_line()+
#     #geom_hline(yintercept = min(d$ODBA_mean, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day mean of mean ODBA", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ggplot(d3,aes(x=date,y=Absx_var,color=Classification))+
#     geom_point() +
#     #geom_line()+
#     #geom_hline(yintercept = min(d$Absx_var, na.rm = T), color = "#FDE725FF", linetype = "dashed")+
#     scale_color_manual(values = c("Non-incubating" = "#31688EFF", "Failed incubation" = "#35B779FF", "Successful incubation" = "#FDE725FF"))+
#     scale_x_date(date_labels = "%b %d", date_breaks = "10 day")+
#     labs( y = "Rolling 26-day SD of mean ABSX", x = "Date") + theme(plot.margin = margin(2,0.5,0.2,0.1, 'cm')),
#   ncol=4,nrow = 1,legend = "bottom", common.legend = TRUE
#   )
# 
# ggarrange(h,f,d, ncol=1, nrow=3,# common.legend = TRUE,
#           labels = c("Overall Status: Successful Incubation", "Overall Status: Failed Incubation",
#                      "Overall Status: Non-incubating"))
# x = c(0, 0.5, 1, 0, 0.5, 1
# )
# y = c(0.33, 0.33, 0.33,0.665, 0.665, 0.665
# )
# id = c(1,1,1,2,2,2
# )
# grid.polygon(x,y,id)
# dev.off()
# 
