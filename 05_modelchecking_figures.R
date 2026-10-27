######################## 04_modelchecking_figures.R ##################

# Code: FAC model checking
# Date: 5 Oct 2026
# Author: Cassidy Waldrep

# Load packages and functions

library(tidyverse)
library(nimble)
library(nimbleHMC)
library(coda)
library(MCMCvis)
library(ggdist)

#-------------------------------------------------------------------------#
###### Reading in data  #############
#-------------------------------------------------------------------------#

summary <- read_csv("results/summarystats_fac_26Oct2026_fullrun.csv")
posteriors <- read_csv("results/posteriors_fac_26Oct2026_fullrun.csv")
results <- load("results/full_fac_26OCT2026_300000_updatedsampler_adapt_alltogether.RData")

#-------------------------------------------------------------------------#
###### Checking Convergence #############
#-------------------------------------------------------------------------#

samples < results

# convert to MCMC list
samples <- list(chain1 = samples[[1]], 
                chain2 = samples[[2]], 
                chain3 = samples[[3]])

mcmc_list <- as.mcmc.list(lapply(samples, mcmc))

for(i in 1:3){
  mcmc_list[[i]] <- mcmc_list[[i]][ , which(is.na(colSums(mcmc_list[[i]])) == F)]
}

# assess convergence

rhat <- gelman.diag(mcmc_list, multivariate = F)
rhat <- unlist(rhat$psrf)
rhat[which(rhat[,1] > 1.1), ] # if nothing here, then good

# check traceplots

# MCMCtrace(mcmc_list, type = "trace",
#           filename = paste0("results/traceplots/full_fac_traceplots_21Oct2026_100000.pdf"))


#-------------------------------------------------------------------------#
###### Effect of weather on ODBA and PTF #############
#-------------------------------------------------------------------------#

odba_weather <- summary %>%
  filter(str_detect(parameter, "beta_ODBA_")) %>%
  extract(
    parameter, 
    into = c("metric", "covariate", "season"), 
    regex = "beta_([A-Z]+)_([a-zA-Z_]+)\\[([0-9]+)\\]", 
    remove = FALSE,   # Keeps the original 'parameter' column intact
    convert = TRUE    # Automatically converts season to numeric
  ) %>%
  filter(covariate %in% c("temp", "precip", "temp_precip", "freeze"),
         !parameter %in% c("beta_ODBA_freeze[2]", 
                        "beta_ODBA_freeze[3]", 
                        "beta_ODBA_freeze[4]", 
                        "beta_ODBA_freeze[5]")) %>%
  mutate(type = case_when(prop <= 0.2 & mean > 0 ~ "positive effect", 
                          prop <= 0.2 & mean <= 0 ~ "negative effect", 
                          TRUE ~ "little to no effect"), 
         season = as.factor(season))

odba_weather_posteriors <- posteriors %>%
  inner_join(odba_weather %>% dplyr::select(parameter:season, mean, type), by = "parameter") %>%
  mutate(parameter = fct_reorder(parameter, mean, .desc = TRUE)) 

param_levels <- levels(factor(odba_weather_posteriors$season))

rect_df <- data.frame(
  xmin = seq_along(param_levels) - 0.5,
  xmax = seq_along(param_levels) + 0.5,
  fill = rep(c("a", "b"), length.out = length(param_levels))
)

ggplot(data = odba_weather_posteriors) +
  geom_rect(data = rect_df, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE,
            fill = rep(c("grey93", "white"), length.out = length(param_levels))[match(rect_df$fill, c("a","b"))],
            alpha = 1) +
  geom_violin(aes(x = season, y = posterior, color = covariate, fill = covariate),
              position = position_dodge(width = 0.8),
                  scale = "width",  
                  trim = FALSE,     
                  linewidth = 0.4,
                  alpha = 0.7) +
  stat_pointinterval(aes(x = season, y = posterior, group = covariate),
    .width = c(0.66, 0.95),  # Adds both 66% (thick) and 95% (thin) interval bars
    size = 2,
    stroke = 0.3,
    color = "black",
    position = position_dodge(width = 0.8)
  ) +
  scale_fill_manual(values = c("#36382E", "#DDA77B", "#945D5E", "#76949F")) +
  scale_color_manual(values = c("#36382E", "#DDA77B", "#945D5E", "#76949F")) +
  labs(
    y = "Effect on ODBA", 
    x = "", 
    fill = "Weather"
  ) +
  ylim(-0.2, 0.2) +
  ggpubr::theme_pubr() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  theme(legend.position = c(0.5, 0.94), 
        legend.direction = "horizontal") +
  guides(color = "none") 



ptf_weather <- summary %>%
  filter(str_detect(parameter, "beta_feed_")) %>%
  extract(
    parameter, 
    into = c("metric", "covariate", "season"), 
    regex = "beta_([a-zA-Z]+)_([a-zA-Z_]+)\\[([0-9]+)\\]", 
    remove = FALSE,   # Keeps the original 'parameter' column intact
    convert = TRUE    # Automatically converts season to numeric
  ) %>%
  filter(covariate %in% c("temp", "precip", "temp_precip", "freeze"),
         !parameter %in% c("beta_feed_freeze[2]", 
                           "beta_feed_freeze[3]", 
                           "beta_feed_freeze[4]", 
                           "beta_feed_freeze[5]")) %>%
  mutate(type = case_when(prop <= 0.2 & mean > 0 ~ "positive effect", 
                          prop <= 0.2 & mean <= 0 ~ "negative effect", 
                          TRUE ~ "little to no effect"), 
         season = as.factor(season))

feed_weather_posteriors <- posteriors %>%
  inner_join(ptf_weather %>% dplyr::select(parameter:season, mean, type), by = "parameter") %>%
  mutate(parameter = fct_reorder(parameter, mean, .desc = TRUE)) 

param_levels <- levels(factor(feed_weather_posteriors$season))

rect_df <- data.frame(
  xmin = seq_along(param_levels) - 0.5,
  xmax = seq_along(param_levels) + 0.5,
  fill = rep(c("a", "b"), length.out = length(param_levels))
)

ggplot(data = feed_weather_posteriors) +
  geom_rect(data = rect_df, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE,
            fill = rep(c("grey93", "white"), length.out = length(param_levels))[match(rect_df$fill, c("a","b"))],
            alpha = 1) +
  geom_violin(aes(x = season, y = posterior, color = covariate, fill = covariate),
              position = position_dodge(width = 0.8),
              scale = "width",  
              trim = FALSE,     
              linewidth = 0.4,
              alpha = 0.7) +
  stat_pointinterval(aes(x = season, y = posterior, group = covariate),
                     .width = c(0.66, 0.95),  # Adds both 66% (thick) and 95% (thin) interval bars
                     size = 2,
                     stroke = 0.3,
                     color = "black",
                     position = position_dodge(width = 0.8)
  ) +
  scale_fill_manual(values = c("#36382E", "#DDA77B", "#945D5E", "#76949F")) +
  scale_color_manual(values = c("#36382E", "#DDA77B", "#945D5E", "#76949F")) +
  labs(
    y = "Effect on PTF", 
    x = "", 
    fill = "Weather"
  ) +
  ylim(-0.2, 0.2) +
  ggpubr::theme_pubr() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  theme(legend.position = c(0.5, 0.94), 
        legend.direction = "horizontal") +
  guides(color = "none") 
