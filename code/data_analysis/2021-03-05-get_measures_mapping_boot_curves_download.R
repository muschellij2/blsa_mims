
#' This script combines results from bootstrap analysis of mapping between AC
#' and other minute-level measures into one data frame. The data frame contains
#' the following summary computed across B=1000 resamplings:
#' - mean
#' - median
#' - quantile 0.025
#' - quantile 1 - 0.025
#' separately for each (k--size of spline basis, whether resample level 1) 
#' pair of settings

rm(list = ls())

library(tidyverse)
library(data.table)
library(matrixStats)

# read file names with bootstrap results
result_fnames0  <- list.files(paste0(here::here(), "/results"), full.names = FALSE)
result_fnames   <- result_fnames0[which(grepl(pattern = "2021-03-05", result_fnames0))]
result_fnames_l <- length(result_fnames)
result_fnames_l

# define significance level used to create confidence intervals 
sign_alpha <- 0.05

# define object to store aggregated results from each file 
dat_out_list <- vector(mode = "list", length = result_fnames_l)

for (i in 1 : result_fnames_l){ # i <- 5
  print(i)
  fname_i <- result_fnames[i]
  # pull bootstrap parameters from the current file name 
  fname_i_ss     <- strsplit(fname_i, "_")[[1]]
  measure_name_i <- fname_i_ss[3]
  k_i            <- as.numeric(fname_i_ss[5])
  step2repl_i    <- as.numeric(fname_i_ss[7])
  rm0AC_i        <- as.numeric(fname_i_ss[9])
  capAC_i        <- as.numeric(gsub("\\.rds", "", fname_i_ss[11]))
  # pull bootstrap data results from the current file name
  dat_i <- readRDS(paste0(here::here(), "/results/", fname_i))
  # aggregate bootstrap data results from the current file name
  dat_i_out <- data.frame(
    val_mean   = matrixStats::rowMeans2(dat_i, na.rm = TRUE),
    val_median = matrixStats::rowMedians(dat_i, na.rm = TRUE),
    val_q025   = matrixStats::rowQuantiles(dat_i, probs = sign_alpha, na.rm = TRUE),
    val_q975   = matrixStats::rowQuantiles(dat_i, probs = (1 - sign_alpha/2), na.rm = TRUE),
    val_cnt    = apply(dat_i, 1, function(row_j) sum(!is.na(row_j)))
  )
  dat_i_out$measure_name = measure_name_i
  dat_i_out$k = k_i
  dat_i_out$step2repl = step2repl_i
  dat_i_out$rm0AC = rm0AC_i
  dat_i_out$capAC = capAC_i
  dat_out_list[[i]] <- as.data.table(dat_i_out)
}

# combine list of data tables into one data table 
dat_out_df <- rbindlist(dat_out_list) %>% as.data.frame()
nrow(dat_out_df)
# Mar 10: [1] 504160

AC_seq <- seq(from = 0, to = 15754, by = 1)
nrow(dat_out_df) / length(AC_seq)

dat_out_df$AC <- rep(AC_seq, result_fnames_l)

# save result to file 
saveRDS(dat_out_df, paste0(here::here(), "/results/2021-03-10-boot_out_all.rds")) 


# get /dcl01/smart/data/activity/blsa_mims/results/2021-03-10-boot_out_all.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/2021-03-10-boot_out_all.rds
