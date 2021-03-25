#' This script generates table with summary of correlations between the 
#' open-source measures. 
#' 
#' Input: 
#' - /results/2021-03-03-correlations_between_measures.rds 

rm(list = ls())
library(tidyverse)
library(data.table)

# read accelerometry measures master file
datacc_fpath <- paste0(here::here(), "/results/2021-03-03-correlations_between_measures.rds")
datacc <- readRDS(datacc_fpath)

length(unique(datacc$file_id))
# Jan 21, 2021: 721

var_names <- c(
  "AC_MIMS",
  "AC_ENMO",
  "AC_MAD",
  "AC_AI",
  "MIMS_ENMO",
  "MIMS_MAD",
  "MIMS_AI",
  "ENMO_MAD",
  "ENMO_AI",
  "MAD_AI"
)
var_labels <- sapply(var_names, function(val){
  paste0("cor(", strsplit(val, "_")[[1]][1], ", ", strsplit(val, "_")[[1]][2], ")")
})

# summarize correlations computed using all day
datacc_A <-
  datacc %>% 
  filter(from11to5_exclude == 0) %>%
  select(all_of(var_names)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = var_names, labels = var_labels)) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup()
datacc_A_form <- 
  datacc_A %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd),
    val_median_f = sprintf("%.3f", val_median),
    val_min_f    = sprintf("% 06.3f", val_min),
    val_max_f    = sprintf("% 06.3f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ",", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_A = val_mean_sd, 
         val_median_min_max_A = val_median_min_max)


# summarize correlations computed using day exclusing 11pm-5am
datacc_B <-
  datacc %>% 
  filter(from11to5_exclude == 1) %>%
  select(all_of(var_names)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = var_names, labels = var_labels)) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup()
datacc_B_form <- 
  datacc_B %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd),
    val_median_f = sprintf("%.3f", val_median),
    val_min_f    = sprintf("%.3f", val_min),
    val_max_f    = sprintf("% 06.3f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ",", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_B = val_mean_sd, 
         val_median_min_max_B = val_median_min_max)

tbl_out <- datacc_A_form %>% left_join(datacc_B_form, by = "name")
tbl_out

View(tbl_out)

# save data to file 
# fpath_tmp <- paste0(here::here(), "/results/2021-03-03-table_measures_corr.csv")
# fwrite(as.data.table(tbl_out), fpath_tmp) 



