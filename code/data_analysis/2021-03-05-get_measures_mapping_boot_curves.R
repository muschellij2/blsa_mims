#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#' cd $mims
#' cd code/data_analysis
#' Rnosave 2021-03-05-get_measures_mapping_boot_curves.R -l mem_free=20G,h_vmem=20G,h_stack=256M -t 1-12 -N JOB_gam_boot

library(tidyverse)
library(mgcv)
library(data.table)
options(scipen=999)

# get the params for current run 
param_row <- as.numeric(Sys.getenv("SGE_TASK_ID"))
# param_row <- 12

# pull parameters specific to this job 
B <- 2
param_grid <- expand.grid(k = c(5, 7, 10, 20, 30, 40), 
                          step2_replacement = c(1),
                          rm0AC = c(0,1),
                          capAC = c(0))
dim(param_grid)

k          <- param_grid[param_row, 1]
step2_repl <- param_grid[param_row, 2]
rm0AC      <- param_grid[param_row, 3]
capAC      <- param_grid[param_row, 4]

# define parameters-specific filename suffix 
file_suff <- paste0("_k_", k, "_step2repl_", step2_repl,  "_rm0AC_", rm0AC, "_capAC_", capAC, ".rds")
message(file_suff)

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-03-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath) 
dat_acc <- dat_acc %>% dplyr::filter(wear_and_valid_flag == 1)

subj_id_vec <- unique(dat_acc$subj_id)
subj_id_n   <- length(subj_id_vec)
AC_seq_max  <- round(max(dat_acc$AC))

# apply special conditions of current simulation setting
# if (capAC){
#   AC_seq_max <- 5000
#   dat_acc <- dat_acc %>% filter(AC <= 5000)
# }
if (rm0AC == 1){
  dat_acc <- dat_acc %>% filter(AC != 0)
}
AC_seq <- seq(from = 0, to = AC_seq_max, by = 1)
boot_newdat <- data.frame(AC = AC_seq)

set.seed(1)
# define objects to store the results 
boot_out_MIMS <- matrix(NA, nrow = length(AC_seq), ncol = B)
boot_out_ENMO <- matrix(NA, nrow = length(AC_seq), ncol = B)
boot_out_MAD  <- matrix(NA, nrow = length(AC_seq), ncol = B)
boot_out_AI   <- matrix(NA, nrow = length(AC_seq), ncol = B)

t1 <- Sys.time()
for (b_tmp in 1 : B){ # b_tmp <- 1
  
  message(paste0("b_tmp: ", b_tmp))
  boot_subj_id <- sample(subj_id_vec, size = subj_id_n, replace = TRUE)
  boot_df <- lapply(boot_subj_id, function(subj_id_tmp){ # subj_id_tmp <- boot_subj_id[1]
    # sampling with replacement individuals
    dat_acc_tmp <- dat_acc[dat_acc$subj_id == subj_id_tmp, ]
    if (step2_repl == 1) {
      # sampling with replacement within individual
      rows_sample <- sample(1 : nrow(dat_acc_tmp), size = nrow(dat_acc_tmp), replace = TRUE)
      dat_acc_tmp <- dat_acc_tmp[rows_sample, ]
    }
    as.data.table(dat_acc_tmp)
  })
  boot_df <- rbindlist(boot_df) 
  
  # MIMS 
  boot_fit <- gam(MIMS ~ s(AC, bs = "cr", k = k), data = boot_df)
  boot_out_MIMS[, b_tmp] <- predict(boot_fit, newdata = boot_newdat)
  
  # ENMO 
  boot_fit <- gam(ENMO ~ s(AC, bs = "cr", k = k), data = boot_df)
  boot_out_ENMO[, b_tmp] <- predict(boot_fit, newdata = boot_newdat)
  
  # MAD 
  boot_fit <- gam(MAD ~ s(AC, bs = "cr", k = k), data = boot_df)
  boot_out_MAD[, b_tmp] <- predict(boot_fit, newdata = boot_newdat)
  
  # AI 
  boot_fit <- gam(AI ~ s(AC, bs = "cr", k = k), data = boot_df)
  boot_out_AI[, b_tmp] <- predict(boot_fit, newdata = boot_newdat)
  
  if (b_tmp %% 100 == 0){
    # save tmp results to file 
    saveRDS(boot_out_MIMS, paste0(here::here(), "/results/2021-03-05-boot_out_MIMS", file_suff)) # typo 
    saveRDS(boot_out_ENMO, paste0(here::here(), "/results/2021-03-05-boot_out_ENMO", file_suff))
    saveRDS(boot_out_MAD,  paste0(here::here(), "/results/2021-03-05-boot_out_MAD", file_suff))
    saveRDS(boot_out_AI,   paste0(here::here(), "/results/2021-03-05-boot_out_AI", file_suff))
  }
  
}
t2 <- Sys.time()
message(t2-t1)

# save final results to file
saveRDS(boot_out_MIMS, paste0(here::here(), "/results/2021-03-05-boot_out_MIMS", file_suff)) # typo 
saveRDS(boot_out_ENMO, paste0(here::here(), "/results/2021-03-05-boot_out_ENMO", file_suff))
saveRDS(boot_out_MAD,  paste0(here::here(), "/results/2021-03-05-boot_out_MAD", file_suff))
saveRDS(boot_out_AI,   paste0(here::here(), "/results/2021-03-05-boot_out_AI", file_suff))


# "/dcl01/smart/data/activity/blsa_mims/results/2021-03-03-boot_out_MIMS_k_40_step2repl_1_rm0AC_1_capAC_0.rds"
