#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

#' This script iterates over raw output from ActiLife giving acceleration 
#' measurements [g] along three orthogonal axes. It retrieves and saves 
#' device info and data range info. 
#' 
#' Notes: 
#' 
#' cd $mims
#' cd code/data_preprocessing
#' Rnosave 2021-03-23-device_and_data_range_info.R -l mem_free=30G,h_vmem=30G,h_stack=256M -N JOB_device_stats


options(digits.secs = 3)

# source util functions
source(here::here("code/helper_functions.R"))

# function to get serial number
get_serial_number <- function(header) {
  hdr = strsplit(header, "---")[[1]]
  hdr = gsub("-", "", hdr)
  hdr = hdr[ !hdr %in% ""]
  ACTIGRAPH_SERIALNUM_PATTERN <- "SerialNumber:\\s*([A-Za-z0-9]+)\\s*StartTime.*"
  sn = sub(ACTIGRAPH_SERIALNUM_PATTERN, "\\1", hdr[[2]])
  return(sn)
}

# function to get ActiLifev
get_ActiLifev <- function(header) {
  hdr = strsplit(header, "---")[[1]]
  hdr = gsub("-", "", hdr)
  hdr = hdr[ !hdr %in% ""]
  PATTERN <- ".*ActiLifev\\s*([A-Za-z0-9.]+)\\s*Firmwarev.*"
  out = sub(PATTERN, "\\1", hdr[[1]])
  return(out)
}

# function to get Firmwarev
get_Firmwarev <- function(header) {
  hdr = strsplit(header, "---")[[1]]
  hdr = gsub("-", "", hdr)
  hdr = hdr[ !hdr %in% ""]
  PATTERN <- ".*Firmwarev\\s*([A-Za-z0-9.]+)\\s*dateformat.*"
  out = sub(PATTERN, "\\1", hdr[[1]])
  return(out)
}

# function to get StartDate
get_StartDate <- function(header) {
  hdr = strsplit(header, "---")[[1]]
  hdr = gsub("-", "", hdr)
  hdr = hdr[ !hdr %in% ""]
  PATTERN <- ".*StartDate\\s*([A-Za-z0-9/]+)\\s*EpochPeriod.*"
  out = sub(PATTERN, "\\1", hdr[[2]])
  out = as.character(as.Date(out, format = "%m/%d/%Y"))
  return(out)
}

# function to get DownloadDate
get_DownloadDate <- function(header) {
  hdr = strsplit(header, "---")[[1]]
  hdr = gsub("-", "", hdr)
  hdr = hdr[ !hdr %in% ""]
  PATTERN <- ".*DownloadDate\\s*([A-Za-z0-9/]+)\\s*CurrentMemory.*"
  out = sub(PATTERN, "\\1", hdr[[2]])
  out = as.character(as.Date(out, format = "%m/%d/%Y"))
  return(out)
}


# list of files with raw output from ActiLife 
fnames = sort(list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat"))
fnames_l <- length(fnames)

# define object to store results
df = data.frame(fname = basename(fnames))
df$sn = NA
df$srate = NA
df$actilifev = NA
df$firmwarev = NA
df$startdate = NA
df$downloaddate = NA
df$accel_min = NA
df$accel_max = NA
df$dynamic_range_min = NA
df$dynamic_range_max = NA

# iterate over files with raw output from ActiLife 
for (fidx in 1:fnames_l){  # fidx <- 1
  fname = fnames[fidx]
  print(paste0("fidx: ", fidx, ", fname: ", basename(fname)))
  
  tryCatch({
    # read raw accelerometry data .mat file
    acc_df = read_acc_mat(fname)
    # pull meta information about the file 
    header = acc_df$hed
    dynamic_range = get_dynamic_range(header)
    sn = get_serial_number(header)
    # pull min/max vals across all axes
    acc_df_range_xyz <- sapply(c("X", "Y", "Z"), function(r) range(acc_df$Xi[[r]], na.rm = TRUE))
    acc_df_range <- range(as.vector(acc_df_range_xyz))
    # store values
    df$sn[fidx]           <- get_serial_number(header)
    df$srate[fidx]        <- acc_df$fs
    df$actilifev[fidx]    <- get_ActiLifev(header)
    df$firmwarev[fidx]    <- get_Firmwarev(header)
    df$startdate[fidx]    <- get_StartDate(header)
    df$downloaddate[fidx] <- get_DownloadDate(header)
    df$accel_min[fidx]    <- acc_df_range[1]
    df$accel_max[fidx]    <- acc_df_range[2]
    df$dynamic_range_min[fidx] <- dynamic_range[1]
    df$dynamic_range_max[fidx] <- dynamic_range[2]
  }, error = function(e) {
    print(e)
  })
  rm(acc_df)
  
  if (fidx %% 100 == 0) {
    # save current state of the results to file 
    saveRDS(df, paste0(here::here(), "/results/2021-03-23-device_and_data_range_info.rds"))
  }
}

# save final results to file 
saveRDS(df, paste0(here::here(), "/results/2021-03-23-device_and_data_range_info.rds"))
