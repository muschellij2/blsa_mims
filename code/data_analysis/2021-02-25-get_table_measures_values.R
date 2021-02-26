#' This script generates two tables with PA minute-level summary: 
#' (A) (MAIN TEXT) summary of measurement's sum per day, per participant 
#' (B) (SUPPLEMENTARY) summary of measurement per minute, per participant 
#' 
#' Input: 
#' - /data_processed/2021-02-25-measures_masterfile_winsorized.rds


rm(list = ls())
library(tidyverse)

# ------------------------------------------------------------------------------
# read data 

# measures_masterfile -- file with minute-level PA measures 
measures_masterfile_fpath <- paste0(here::here(), "/data_processed/2021-02-25-measures_masterfile_winsorized.rds")
measures_masterfile <- readRDS(measures_masterfile_fpath)
dim(measures_masterfile)
# [1] 6147240      10
measures_masterfile %>% select(subj_id, visit_id) %>% distinct() %>% nrow()
# [1] 721
length(unique(measures_masterfile$subj_id))
# [1] 721


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --  TABLE (A)  ---------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

osm_vars <- c("AC", "AC_LOG", "MIMS", "MAD", "ENMO", "AI")

# (1) using all day
mm_agg_A <-
  measures_masterfile %>%
  mutate(AC_LOG = log(AC + 1),
         HEADER_TIME_STAMP_date = as.Date(HEADER_TIME_STAMP)) %>%
  select(all_of(c("subj_id", "HEADER_TIME_STAMP_date", "wear_flag", osm_vars))) %>%
  group_by(subj_id) %>%
  mutate(wear_flag_sum = sum(wear_flag),
         days_cnt = n_distinct(HEADER_TIME_STAMP_date),
         days_cnt_scaled = days_cnt * (wear_flag_sum / (days_cnt * 1440))) %>%
  select(-HEADER_TIME_STAMP_date, -wear_flag, -wear_flag_sum, -days_cnt) %>%
  group_by(subj_id, days_cnt_scaled) %>%
  summarise_all(sum, na.rm = TRUE) %>% # added Feb 25
  as.data.frame() %>%
  mutate(across(all_of(osm_vars)), . / days_cnt_scaled) %>%
  ungroup() %>%
  select(-c(subj_id, days_cnt_scaled)) %>%
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

# (2) excluding night time 
mm_agg_B <- measures_masterfile %>%
  dplyr::filter(!(lubridate::hour(HEADER_TIME_STAMP) %in% c(23,0,1,2,3,4))) %>%
  mutate(AC_LOG = log(AC + 1),
         HEADER_TIME_STAMP_date = as.Date(HEADER_TIME_STAMP)) %>%
  select(all_of(c("subj_id", "HEADER_TIME_STAMP_date", "wear_flag", osm_vars))) %>%
  group_by(subj_id) %>%
  mutate(wear_flag_sum = sum(wear_flag),
         days_cnt = n_distinct(HEADER_TIME_STAMP_date),
         days_cnt_scaled = days_cnt * (wear_flag_sum / (days_cnt * 1080))) %>%
  select(-HEADER_TIME_STAMP_date, -wear_flag, -wear_flag_sum, -days_cnt) %>%
  group_by(subj_id, days_cnt_scaled) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(across(all_of(osm_vars)), . / days_cnt_scaled) %>%
  ungroup() %>%
  select(-c(subj_id, days_cnt_scaled)) %>%
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
         val_mean_sd_B = val_mean_sd, 
         val_median_min_max_B = val_median_min_max)


tbl_out <- mm_agg_A_form %>% left_join(mm_agg_B_form, by = "name")
View(tbl_out)


rm(mm_agg_A, mm_agg_A_form, mm_agg_B, mm_agg_B_form)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --  TABLE (B)  ---------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

osm_vars <- c("AC", "AC_LOG", "MIMS", "MAD", "ENMO", "AI")

# (1) using all day
mm_agg_A <- 
  measures_masterfile %>%
  dplyr::filter(wear_flag == 1) %>% # added Feb 25 
  mutate(AC_LOG = log(AC + 1)) %>%
  select(all_of(osm_vars)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = osm_vars)) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value, na.rm = TRUE),
    val_sd     = sd(value, na.rm = TRUE),
    val_median = median(value, na.rm = TRUE),
    val_min    = min(value, na.rm = TRUE),
    val_max    = max(value, na.rm = TRUE)
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
mm_agg_B <- 
  measures_masterfile %>%
  dplyr::filter(!(lubridate::hour(HEADER_TIME_STAMP) %in% c(23,0,1,2,3,4))) %>%
  dplyr::filter(wear_flag == 1) %>% # added Feb 25 
  mutate(AC_LOG = log(AC + 1)) %>%
  select(all_of(osm_vars)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = osm_vars)) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value, na.rm = TRUE),
    val_sd     = sd(value, na.rm = TRUE),
    val_median = median(value, na.rm = TRUE),
    val_min    = min(value, na.rm = TRUE),
    val_max    = max(value, na.rm = TRUE)
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

