#' This script generates "Table 2" for the paper. 
#' These are summary of minute-level masures. 
#' 
#' Input: 
#' - /data_processed/2021-01-19-measures_masterfile_winsorized.rds


rm(list = ls())
library(tidyverse)

# ------------------------------------------------------------------------------
# read data 

# measures_masterfile -- file with minute-level PA measures 
measures_masterfile_fpath <- paste0(here::here(), "/data_processed/2021-01-19-measures_masterfile_winsorized.rds")
measures_masterfile <- readRDS(measures_masterfile_fpath)
dim(measures_masterfile)
# [1] 6147240      10
measures_masterfile %>% select(subj_id, visit_id) %>% distinct() %>% nrow()
# [1] 721
length(unique(measures_masterfile$subj_id))
# [1] 721


# ------------------------------------------------------------------------------
# table without split into two groups

mm_agg <- measures_masterfile %>%
  select(all_of(c("AC", "MIMS", "MAD", "ENMO", "AI"))) %>%
  mutate(AC_LOG = log(AC + 1)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = c("AC", "AC_LOG", "MIMS", "MAD", "ENMO", "AI"))) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup()

mm_agg_form <- 
  mm_agg %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd),
    val_median_f = sprintf("%.3f", val_median),
    val_min_f    = sprintf("%.3f", val_min),
    # val_max_f    = sprintf("% 06.3f", val_max),
    val_max_f    = sprintf("%.3f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd = val_mean_sd, 
         val_median_min_max = val_median_min_max)

View(mm_agg_form)


# ------------------------------------------------------------------------------
# table WITH split into two groups

# (1) using all day
mm_agg_A <- measures_masterfile %>%
  select(all_of(c("AC", "MIMS", "MAD", "ENMO", "AI"))) %>%
  mutate(AC_LOG = log(AC + 1)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = c("AC", "AC_LOG", "MIMS", "MAD", "ENMO", "AI"))) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup()
mm_agg_A_form <- 
  mm_agg_A %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd),
    val_median_f = sprintf("%.3f", val_median),
    val_min_f    = sprintf("%.3f", val_min),
    # val_max_f    = sprintf("% 06.3f", val_max),
    val_max_f    = sprintf("%.3f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_A = val_mean_sd, 
         val_median_min_max_A = val_median_min_max)


# (2) excluding night time 
mm_agg_B <- measures_masterfile %>%
  dplyr::filter(!(lubridate::hour(HEADER_TIME_STAMP) %in% c(23,0,1,2,3,4))) %>%
  select(all_of(c("AC", "MIMS", "MAD", "ENMO", "AI"))) %>%
  mutate(AC_LOG = log(AC + 1)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = c("AC", "AC_LOG", "MIMS", "MAD", "ENMO", "AI"))) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup()
mm_agg_B_form <- 
  mm_agg_B %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd),
    val_median_f = sprintf("%.3f", val_median),
    val_min_f    = sprintf("%.3f", val_min),
    # val_max_f    = sprintf("% 06.3f", val_max),
    val_max_f    = sprintf("%.3f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_B = val_mean_sd, 
         val_median_min_max_B = val_median_min_max)

tbl_out <- mm_agg_A_form %>% left_join(mm_agg_B_form, by = "name")

View(tbl_out)

