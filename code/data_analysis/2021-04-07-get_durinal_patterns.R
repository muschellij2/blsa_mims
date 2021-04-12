
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
fpath_tmp <- paste0(here::here(), "/data_processed/2021-04-07-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc0 <- readRDS(fpath_tmp) %>% as.data.frame()
dat_acc <- dat_acc0 %>% 
  left_join(mastervisit, by = c("subj_id", "visit_id"))
# add minute index, label
dat_acc$minute_idx   <- rep(1 : 1440, (nrow(dat_acc) / 1440))
dat_acc$minute_label <- lubridate::ymd_hms("2013-01-01 00:00:00") + lubridate::minutes(dat_acc$minute_idx - 1)
# add age categorized
dat_acc$age_cat1  <- gtools::quantcut(dat_acc$age, 4)
cut_breaks = c(min(dat_acc$age), 59, 67, 74, max(dat_acc$age))
dat_acc$age_cat2 <- cut(dat_acc$age, breaks = cut_breaks, include.lowest = TRUE)
# look up 
dat_acc %>% group_by(age_cat1) %>% summarise(subj_id_cnt = n_distinct(subj_id))
dat_acc %>% group_by(age_cat2) %>% summarise(subj_id_cnt = n_distinct(subj_id))


# ------------------------------------------------------------------------------
# estimate: smoothed median 

# fitted 
dat_acc_agg_fitted <- 
  dat_acc %>% 
  select(AC, MIMS, ENMO, MAD, AI,
         AC_hat_from_MIMS, AC_hat_from_ENMO, AC_hat_from_MAD, AC_hat_from_AI, 
         age_cat = age_cat1, minute_idx, minute_label) %>%
  group_by(age_cat, minute_idx, minute_label) %>%
  summarize_all(median)
dat_acc_agg_fitted$fit_type <- "fitted"

# CI via bootstrap 
subj_id_vec <- sort(unique(dat_acc$subj_id)) 
subj_id_vec_l <- length(subj_id_vec)
dat_acc_boot_agg <- data.table()
B <- 100
for (b_tmp in 1 : B){
  print(b_tmp)
  set.seed(b_tmp)
  # sample individual's data 
  subj_id_boot <- sample(subj_id_vec, replace = TRUE, size = subj_id_vec_l)
  subj_id_boot_df <- data.frame(subj_id = subj_id_boot) %>% mutate(subj_id_boot = row_number())
  dat_acc_boot <- dat_acc %>% left_join(subj_id_boot_df, by = "subj_id") %>% 
    filter(!is.na(subj_id_boot)) %>% select(-subj_id_boot)
  # aggregate 
  dat_acc_boot_agg_b <- 
    dat_acc_boot %>% 
    select(AC, AC_hat_from_MIMS, AC_hat_from_ENMO, AC_hat_from_MAD, AC_hat_from_AI, 
           age_cat = age_cat1, minute_idx, minute_label) %>%
    group_by(age_cat, minute_idx, minute_label) %>%
    summarize_all(median)
  dat_acc_boot_agg_b$fit_type <- "boot"
  dat_acc_boot_agg_b$boot_idx <- b_tmp
  # append 
  dat_acc_boot_agg <- rbindlist(list(dat_acc_boot_agg, as.data.table(dat_acc_boot_agg_b)))
}
dat_acc_boot_agg_df <- as.data.frame(dat_acc_boot_agg)
head(dat_acc_boot_agg_df)

dat_acc_agg_CIlower <- 
  dat_acc_boot_agg_df %>%
  select(-boot_idx, -fit_type) %>%
  group_by(age_cat, minute_idx, minute_label) %>%
  summarise_all(quantile, 0.025) %>%
  as.data.frame() %>%
  ungroup() %>%
  mutate(fit_type = "CI_lower")
dat_acc_agg_CIupper <- 
  dat_acc_boot_agg_df %>%
  select(-boot_idx, -fit_type) %>%
  group_by(age_cat, minute_idx, minute_label) %>%
  summarise_all(quantile, 1-0.025) %>%
  as.data.frame() %>%
  mutate(fit_type = "CI_upper")

dat_acc_agg <- rbind(
  # dat_acc_agg_CIlower,
  # dat_acc_agg_CIupper,
  dat_acc_agg_fitted
)
head(dat_acc_agg)
table(dat_acc_agg$fit_type)


fpath_tmp <- paste0(here::here(), "/results/2021-04-07-get_durinal_patterns.rds")
saveRDS(dat_acc_agg, fpath_tmp)

