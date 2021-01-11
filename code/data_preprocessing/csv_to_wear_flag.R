#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

#' This script iterates over Actilife-outputed CSV files with minute-level 
#' activity counts and computes minute-level wear/non-wear flag (\code{1}/\code{0})
#' for each minute of activity counts data.
#' 
#' The algorithm uses detection algorithm closely following that of
#' Choi et al. (2011). See detailed documentation and implementation here: 
#' https://github.com/martakarass/arctools/blob/master/R/get_wear_flag.R
#' 
#' input files: csv/
#' output files: csv/

library(readr)
library(read.gt3x)
library(lubridate)
library(dplyr)
options(digits.secs = 3)

# source util functions
source(here::here("code/helper_functions.R"))

# function to get serial number
get_serial_num <- function(header) {
  hdr = strsplit(header, "---")[[1]]
  hdr = gsub("-", "", hdr)
  hdr = hdr[ !hdr %in% ""]
  ACTIGRAPH_SERIALNUM_PATTERN <- "SerialNumber:\\s*([A-Za-z0-9]+)\\s*StartTime.*"
  sn = sub(ACTIGRAPH_SERIALNUM_PATTERN, "\\1", hdr[[2]])
  return(sn)
}

# list of files with raw output from ActiLife 
fnames = sort(list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat"))
fnames_l <- length(fnames)

# define object to store results
df = data.frame(fname = basename(fnames))
df$sn = NA
df$accel_min = NA
df$accel_max = NA
df$dynamic_range_min = NA
df$dynamic_range_max = NA

# iterate over files with raw output from ActiLife 
for (fidx in 1:fnames_l){
  fname = fnames[fidx]
  message(paste0("fidx: ", fidx, ", fname: ", basename(fname)))
  # read raw accelerometry data .mat file
  acc_df = read_acc_mat(fname)
  # pull meta information about the file 
  srate = acc_df$fs
  header = acc_df$hed
  dynamic_range = get_dynamic_range(header)
  sn = get_serial_num(header)
  # pull min/max vals across all axes
  acc_df_range_xyz <- sapply(c("X", "Y", "Z"), function(r) range(acc_df$Xi[[r]]))
  acc_df_range <- range(as.vector(acc_df_range_xyz))
  # store values
  df$sn[fidx] <- sn
  df$accel_min[fidx] <- acc_df_range[1]
  df$accel_max[fidx] <- acc_df_range[2]
  df$dynamic_range_min[fidx] <- dynamic_range[1]
  df$dynamic_range_max[fidx] <- dynamic_range[2]
  rm(acc_df)
}

# save results to file 
readr::write_rds(df, here::here("results", "2021-01-11-device_and_data_range_info.rds"))
