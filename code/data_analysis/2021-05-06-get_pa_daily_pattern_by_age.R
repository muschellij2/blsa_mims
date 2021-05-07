
#' (A) Smoothed 24-hour median activity counts per minute for each age group. 
#' (B) Smoothed medians of 24-hour cumulative activity counts per day for each age group. 
#' Groups: <60-year old (green), 60- to 67-year old (red), 68- to 74-year old (blue), ≥75-year old (orange).

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
options(scipen=999)


# ------------------------------------------------------------------------------
# read data 

## read mastervisit
mastervisit_fpath <- paste0(here::here(), "/covariates/2021-01-19-mastervisit.rdata")
mastervisit <- get(load(mastervisit_fpath, ex <- new.env()), envir = ex) %>% 
  select(age = Age, subj_id = IDNo, visit_id = Visit) %>%
  distinct()
dim(mastervisit)

# read acc
fpath_tmp <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc0 <- readRDS(fpath_tmp) %>% as.data.frame()
dat_acc <- dat_acc0 %>% 
  left_join(mastervisit, by = c("subj_id", "visit_id"))
# add minute index, label
dat_acc$minute_idx   <- rep(1 : 1440, (nrow(dat_acc) / 1440))
dat_acc$minute_label <- lubridate::ymd_hms("2013-01-01 00:00:00") + lubridate::minutes(dat_acc$minute_idx - 1)
# add age categorized
# dat_acc$age_cat1  <- gtools::quantcut(dat_acc$age, 4)
cut_breaks = c(min(dat_acc$age), 59, 67, 74, max(dat_acc$age))
dat_acc$age_cat2 <- cut(dat_acc$age, breaks = cut_breaks, include.lowest = TRUE)
# look up 
# dat_acc %>% group_by(age_cat1) %>% summarise(subj_id_cnt = n_distinct(subj_id))
dat_acc %>% group_by(age_cat2) %>% summarise(subj_id_cnt = n_distinct(subj_id))
# age_cat2 subj_id_cnt
# <fct>          <int>
# 1 [22,59]          140
# 2 (59,67]          102
# 3 (67,74]          129
# 4 (74,97]          284

#' Groups: 
#' <60-year old (green), 
#' 60- to 67-year old (red), 
#' 68- to 74-year old (blue), 
#' ≥75-year old (orange).

# ------------------------------------------------------------------------------
# estimate: smoothed median 

# fitted 
dat_acc_agg_fitted <- 
  dat_acc %>% 
  select(AC, MIMS, ENMO, MAD, AI,
         AC_hat_from_MIMS, AC_hat_from_ENMO, AC_hat_from_MAD, AC_hat_from_AI, 
         age_cat = age_cat2, minute_idx, minute_label) %>%
  group_by(age_cat, minute_idx, minute_label) %>%
  summarize_all(median)
dat_acc_agg_fitted$fit_type <- "fitted"

fpath_tmp <- paste0(here::here(), "/results/2021-05-06-pa_daily_pattern_by_age.rds")
saveRDS(dat_acc_agg_fitted, fpath_tmp)

