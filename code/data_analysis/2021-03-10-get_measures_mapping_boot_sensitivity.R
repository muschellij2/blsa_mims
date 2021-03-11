#!/usr/bin/env Rscript

#' Script to run sensitivity analysis of open-source measures in estimating
#' AC thresholds.
#' 
#' Notes: 
#' 
#' cd $mims
#' cd code/data_analysis
#' Rnosave 2021-03-10-get_measures_mapping_boot_sensitivity.R -t 1-3 -N JOB_sensit

# rm(list = ls())
library(tidyverse)
library(knitr)
library(gridExtra)
library(ROCR)
library(cowplot)

dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-03-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath) 
dat_acc <- dat_acc %>% dplyr::filter(wear_and_valid_flag == 1)

# current job params (specific to array job index)
job_idx = as.numeric(Sys.getenv("SGE_TASK_ID")) # job_idx <- 2
AC_thresh <- c(1, 1853, 2303)[job_idx]
message(paste0("AC_thresh = ", AC_thresh))

measure_grid_l <- 100
get_auc <- TRUE


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Define functions and objects needed to run sensitivity analysis

# function to evaluate threshold 
eval_measure_threshold <- function(AC_vec, 
                                   measure_vec, 
                                   AC_thresh,
                                   measure_thresh_grid, 
                                   measure_label,
                                   get_auc){
  
  message(measure_label)
  
  # define true AC outcomes
  true1_vec       <- as.numeric(AC_vec < AC_thresh)
  true1_which_vec <- which(AC_vec < AC_thresh)
  true0_which_vec <- which(!(AC_vec < AC_thresh))
  
  # define objects to store results 
  out_df         <- data.frame(measure_thresh_grid = measure_thresh_grid)
  out_df$out_sens = out_df$out_spec = out_df$out_accr = out_df$out_auc = NA
  
  # iterate over measure grid values
  measure_thresh_grid_l <- length(measure_thresh_grid)
  for (i in 1 : measure_thresh_grid_l){ # i <- 2
    message(i)
    est1_vec           <- as.numeric(measure_vec < measure_thresh_grid[i])
    out_df$out_sens[i] <- mean(est1_vec[true1_which_vec])
    out_df$out_spec[i] <- 1 - mean(est1_vec[true0_which_vec])
    out_df$out_accr[i] <- mean(true1_vec == est1_vec)
    if (get_auc){
      # prediction(predictions, labels, label.ordering = NULL)
      pred <- ROCR::prediction(est1_vec, true1_vec)
      auc_obj <- ROCR::performance(pred, "auc")
      out_df$out_auc[i] <- as.numeric(auc_obj@y.values)
    }
  }
  
  out_df$measure_name <- measure_label
  return(out_df)
}


# define parameters grid 
param_df <- data.frame()

# AC_thresh = 1
param_df <- rbind(param_df, data.frame(
  AC_thresh        = rep(1, 4),
  measure_name     = c("MIMS", "ENMO", "MAD", "AI"),
  measure_grid_min = rep(0, 4),
  measure_grid_max = c(1, 0.05, 0.025, 1)
))

# AC_thresh = 1853
param_df <- rbind(param_df, data.frame(
  AC_thresh        = rep(1853, 4),
  measure_name     = c("MIMS", "ENMO", "MAD", "AI"),
  measure_grid_min = c(9.5, 0.01, 0.02, 3),
  measure_grid_max = c(11.5, 0.05, 0.045, 4)
))

# AC_thresh = 2303
param_df <- rbind(param_df, data.frame(
  AC_thresh        = rep(2303, 4),
  measure_name     = c("MIMS", "ENMO", "MAD", "AI"),
  measure_grid_min = c(11.5, 0.015, 0.035, 3),
  measure_grid_max = c(13.5, 0.06, 0.055, 5)
))



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Run sensitivity analysis 

# pull parameters grid 
param_df_sub  <- param_df[param_df[, 1] == AC_thresh, ] 
if (nrow(param_df_sub) == 0) stop("nrow(param_df_sub) == 0")
grid_min <- param_df_sub %>% pull(measure_grid_min)
grid_max <- param_df_sub %>% pull(measure_grid_max)
names(grid_min) <- param_df_sub %>% pull(measure_name)
names(grid_max) <- param_df_sub %>% pull(measure_name)

# run performance stats
perf_df_MIMS <- eval_measure_threshold(
  AC_vec = dat_acc$AC, 
  measure_vec = dat_acc$MIMS, 
  AC_thresh = AC_thresh,
  measure_thresh_grid = seq(grid_min["MIMS"], grid_max["MIMS"], length.out = measure_grid_l), 
  measure_label = "MIMS",
  get_auc = get_auc
)

perf_df_ENMO <- eval_measure_threshold(
  AC_vec = dat_acc$AC, 
  measure_vec = dat_acc$ENMO, 
  AC_thresh = AC_thresh,
  measure_thresh_grid = seq(grid_min["ENMO"], grid_max["ENMO"], length.out = measure_grid_l), 
  measure_label = "ENMO",
  get_auc = get_auc
)
  
perf_df_MAD <- eval_measure_threshold(
  AC_vec = dat_acc$AC, 
  measure_vec = dat_acc$MAD, 
  AC_thresh = AC_thresh,
  measure_thresh_grid = seq(grid_min["MAD"], grid_max["MAD"], length.out = measure_grid_l), 
  measure_label = "MAD",
  get_auc = get_auc
)

perf_df_AI <- eval_measure_threshold(
  AC_vec = dat_acc$AC, 
  measure_vec = dat_acc$AI, 
  AC_thresh = AC_thresh,
  measure_thresh_grid = seq(grid_min["AI"], grid_max["AI"], length.out = measure_grid_l), 
  measure_label = "AI",
  get_auc = get_auc
)

# save results to file 
perf_df_comb <- rbind(
  perf_df_MIMS,
  perf_df_ENMO,
  perf_df_MAD,
  perf_df_AI
) %>% 
  as.data.frame() %>%
  mutate(AC_thresh = AC_thresh) 
  
out_fname <- paste0("2021-03-10-measures_mapping_sensitivity_ACthresh_", AC_thresh, ".rds")
saveRDS(perf_df_comb, paste0(here::here(), "/results/", out_fname))

message("FINISHED and saved.")

# get /dcl01/smart/data/activity/blsa_mims/results/2021-03-10-measures_mapping_sensitivity_ACthresh_1853.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/2021-03-10-measures_mapping_sensitivity_ACthresh_1853.rds
# get /dcl01/smart/data/activity/blsa_mims/results/2021-03-10-measures_mapping_sensitivity_ACthresh_1.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/2021-03-10-measures_mapping_sensitivity_ACthresh_1.rds
# get /dcl01/smart/data/activity/blsa_mims/results/2021-03-10-measures_mapping_sensitivity_ACthresh_2303.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/2021-03-10-measures_mapping_sensitivity_ACthresh_2303.rds

