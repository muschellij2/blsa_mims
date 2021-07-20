
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

path_tmp <- paste0(here::here(), "/results/2021-07-15-mapping_MPE_MAPE_mapminutelevel.rds")
dat_acc_agg <- readRDS(path_tmp)


# ------------------------------------------------------------------------------
# (NEW) generate tables 

# summary: MPE per participant
tbl_participant_MPE <- 
  dat_acc_agg %>%
  select(starts_with("MPE_")) %>%
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
  mutate(name = factor(name, levels = paste0("MPE_", names_levels1))) %>%
  arrange(name)

tbl_participant_MPE_form <- 
  tbl_participant_MPE %>%
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
tbl_participant_MPE_form
# View(tbl_participant_MPE_form)


# summary: MAPE per participant
tbl_participant_MAPE <- 
  dat_acc_agg %>%
  select(starts_with("MAPE_")) %>%
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
  mutate(name = factor(name, levels = paste0("MAPE_", names_levels1))) %>%
  arrange(name)

tbl_participant_MAPE_form <- 
  tbl_participant_MAPE %>%
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
tbl_participant_MAPE_form
# View(tbl_participant_MAPE_form)


