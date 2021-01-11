#' This script iterates over Actilife-outputed CSV files with minute-level 
#' activity counts and computes minute-level wear/non-wear flag (\code{1}/\code{0})
#' for each minute of activity counts data.
#' 
#' The algorithm uses detection algorithm closely following that of
#' Choi et al. (2011). See detailed documentation and implementation here: 
#' https://github.com/martakarass/arctools/blob/master/R/get_wear_flag.R
#' 
#' input files: /csv/
#' output files: /csv_wear_flag/
#' 
#' use: 
#' cd /dcl01/smart/data/activity/blsa_mims

rm(list = ls())

library(dplyr)
library(data.table)
library(lubridate)
library(arctools)

fpaths = sort(list.files(path = paste0(here::here(), "/csv"), full.names = TRUE, pattern = "[.]csv"))
fpaths_l <- length(fpaths)

for (fidx in 1:fpaths_l){ # fidx <- 1
  message(paste0("fidx: ", fidx))
  # ActiGraph AC file path
  fpath = fpaths[fidx]
  # output file path
  fname <- basename(fpath)
  fname_out <- gsub(pattern = ".csv", replacement = "_wearflag", fname)
  fpath_out <- paste0(here::here(), "/csv_wear_flag/", fname_out)
  # read ActiGraph AC file 
  dat <- data.table::fread(fpath) %>% as.data.frame()
  # process and save  
  dat$timestamp <- lubridate::ymd_hms(dat$timestamp)
  dat$wear_flag <- arctools::get_wear_flag(dat$vectormagnitude, nonwear_0s_minimum_window = 90)
  dat_out <- dat %>% dplyr::select(timestamp, wear_flag)
  saveRDS(object = dat_out, file = fpath_out)
}
