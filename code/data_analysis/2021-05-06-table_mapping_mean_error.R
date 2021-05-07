
rm(list = ls())
library(tidyverse)
library(cowplot)
library(ggsci)
library(lubridate)
library(scales)
library(latex2exp)
options(scipen=999)


# name levels
names_levels1 <- c("MIMS", "ENMO", "MAD", "AI")

# ------------------------------------------------------------------------------
# read precomputed data 

path_tmp <- paste0(here::here(), "/results/2021-05-06-mapping_mean_error.rds")
dat_acc_agg <- readRDS(path_tmp)


# ------------------------------------------------------------------------------
# (OLD) generate tables 

# summary: ME per participant
tbl_participant_ME <- rbind(
  as.numeric(summary(dat_acc_agg$ME_MIMS)),
  as.numeric(summary(dat_acc_agg$ME_ENMO)),
  as.numeric(summary(dat_acc_agg$ME_MAD)),
  as.numeric(summary(dat_acc_agg$ME_AI))
) %>% round(1) %>%
  as.data.frame() 
names(tbl_participant_ME) <- c(names(summary(1:10)))
tbl_participant_ME <- tbl_participant_ME %>% mutate(
  name = names_levels1, .before = everything()
)
tbl_participant_ME
# View(tbl_participant_ME)


# summary: MSE per participant
tbl_participant_MSE <- rbind(
  as.numeric(summary(dat_acc_agg$MSE_MIMS)),
  as.numeric(summary(dat_acc_agg$MSE_ENMO)),
  as.numeric(summary(dat_acc_agg$MSE_MAD)),
  as.numeric(summary(dat_acc_agg$MSE_AI))
) %>% round(0) %>%
  as.data.frame() 
names(tbl_participant_MSE) <- c(names(summary(1:10)))
tbl_participant_MSE <- tbl_participant_MSE %>% mutate(
  name = names_levels1, .before = everything()
)
tbl_participant_MSE
# View(tbl_participant_MSE)



# ------------------------------------------------------------------------------
# (NEW) generate tables 

# summary: ME per participant
tbl_participant_ME <- 
  dat_acc_agg %>%
  select(starts_with("ME_")) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = paste0("ME_", names_levels1))) %>%
  arrange(name)

tbl_participant_ME_form <- 
  tbl_participant_ME %>%
  mutate(
    val_mean_f   = sprintf("%.1f", val_mean),
    val_sd_f     = sprintf("%.1f", val_sd),
    val_median_f = sprintf("%.1f", val_median),
    val_min_f    = sprintf("%.1f", val_min),
    val_max_f    = sprintf("%.1f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_A = val_mean_sd, 
         val_median_min_max_A = val_median_min_max)
tbl_participant_ME_form
# View(tbl_participant_ME_form)


# summary: MSE per participant
tbl_participant_MSE <- 
  dat_acc_agg %>%
  select(starts_with("MSE_")) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = paste0("MSE_", names_levels1))) %>%
  arrange(name)

tbl_participant_MSE_form <- 
  tbl_participant_MSE %>%
  mutate(
    val_mean_f   = sprintf("%.0f", val_mean),
    val_sd_f     = sprintf("%.0f", val_sd),
    val_median_f = sprintf("%.0f", val_median),
    val_min_f    = sprintf("%.0f", val_min),
    val_max_f    = sprintf("%.0f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_A = val_mean_sd, 
         val_median_min_max_A = val_median_min_max)
tbl_participant_MSE_form
# View(tbl_participant_MSE_form)
