
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#' qrsh
#' cd /dcl01/smart/data/activity/blsa_mims
#' module load conda_R

library(tidyverse)
library(mgcv)
library(data.table)
options(scipen=999)

# get the params for current run 
arg_str <- as.character(args[1])
param_row <- as.numeric(arg_str)
# param_row <- 14

# pull parameters specific to this job 
param_grid <- expand.grid(k = c(3,5,7,10), 
                          step2_replacement = c(1),
                          rm0AC = c(0,1),
                          capAC = c(0,1))

k          <- param_grid[param_row, 1]
step2_repl <- param_grid[param_row, 2]
rm0AC      <- param_grid[param_row, 3]
capAC      <- param_grid[param_row, 4]
B <- 1000

# define parameters-specific filename suffix 
file_suff <- paste0("_k_", k, "_step2repl_", step2_repl,  "_rm0AC_", rm0AC, "_capAC_", capAC, ".rds")
message(file_suff)

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-01-19-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath)
subj_id_vec <- unique(dat_acc$subj_id)
subj_id_n <- length(subj_id_vec)
AC_seq_max <- max(dat_acc$AC)

# apply special conditions of current simulation setting
if (capAC){
  AC_seq_max <- 5000
  dat_acc <- dat_acc %>% filter(AC <= 5000)
}
if (rm0AC){
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
    saveRDS(boot_out_MIMS, paste0(here::here(), "/results/2021-02-09-boot_out_MIMS", file_suff)) # typo 
    saveRDS(boot_out_ENMO, paste0(here::here(), "/results/2021-02-09-boot_out_ENMO", file_suff))
    saveRDS(boot_out_MAD,  paste0(here::here(), "/results/2021-02-09-boot_out_MAD", file_suff))
    saveRDS(boot_out_AI,   paste0(here::here(), "/results/2021-02-09-boot_out_AI", file_suff))
  }
  
}
t2 <- Sys.time()
message(t2-t1)

# save final results to file
saveRDS(boot_out_MIMS, paste0(here::here(), "/results/2021-02-09-boot_out_MIMS", file_suff)) # typo 
saveRDS(boot_out_ENMO, paste0(here::here(), "/results/2021-02-09-boot_out_ENMO", file_suff))
saveRDS(boot_out_MAD,  paste0(here::here(), "/results/2021-02-09-boot_out_MAD", file_suff))
saveRDS(boot_out_AI,   paste0(here::here(), "/results/2021-02-09-boot_out_AI", file_suff))

