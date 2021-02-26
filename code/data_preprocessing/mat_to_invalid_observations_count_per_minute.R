
#' @description 
#' This script takes raw output from ActiLife giving acceleration measurements 
#' [g] along three orthogonal axes and computes minute-level summary statistic: 
#' - valid_minute [0/1] -- a flag whether or not a minute is a valid minute 
#'   in a sense it does not contain any of the axes being fixed at consecutive
#'   values from the range. 
#' 
#' Notes: 
#' cd $mims 
#' cd code/data_preprocessing
#' Rnosave mat_to_invalid_observations_count_per_minute.R -l mem_free=30G,h_vmem=31G -N JB_invalid_obs


rm(list = ls())
library(tidyverse)
library(readr)
source(here::here("code/helper_functions.R"))
options(digits.secs = 3)

# define input file name (specific to array job index)
# ifile =  as.numeric(as.character(args[1])) # as.numeric(Sys.getenv("SGE_TASK_ID"))

fnames = sort(list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat"))

for (ifile in 1 : length(fnames)){

  message(paste0("ifile: ", ifile, " [", round(ifile / length(fnames) * 100, 2), " %]"))
  # fname = "/Users/martakaras/OneDrive - Johns Hopkins/BLSA/mat/599705WaTAS1E23150406 (2015-09-18)RAW.mat"

  fname = fnames[ifile]
  id = sub("RAW[.]mat", "", basename(fname))
  outfile <- paste0(here::here(), "/valid_minutes/", id, "_valid_minutes.rds")
  
  # read raw accelerometry data .mat file
  acc_df = read_acc_mat(fname)
  # pull meta information about the file 
  srate = acc_df$fs
  header = acc_df$hed
  dynamic_range =  get_dynamic_range(header)
  dr_maxabs <- max(abs(dynamic_range))
    
  # subset data to keep timestamp and three axes data only
  acc_df = acc_df$Xi
  acc_df = acc_df %>%
    select(HEADER_TIME_STAMP, X, Y, Z)
  stopifnot(!anyNA(acc_df))
  
  # generate "invalid subsecond-level observation" flag
  acc_df$X_diff <- c(1, diff(acc_df$X))
  acc_df$Y_diff <- c(1, diff(acc_df$Y))
  acc_df$Z_diff <- c(1, diff(acc_df$Z))
  acc_df <- mutate(acc_df, X_obs_invalid = as.numeric((X_diff == 0) & (abs(X) == dr_maxabs)))
  acc_df <- mutate(acc_df, Y_obs_invalid = as.numeric((Y_diff == 0) & (abs(Y) == dr_maxabs)))
  acc_df <- mutate(acc_df, Z_obs_invalid = as.numeric((Z_diff == 0) & (abs(Z) == dr_maxabs)))
  acc_df <- mutate(acc_df, any_obs_invalid = as.numeric((X_obs_invalid + Y_obs_invalid + Z_obs_invalid) > 0))
    
  # compute number of invalid subsecond-level observations per minute
  acc_df_agg = 
    acc_df %>% 
    mutate(HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, "1 min")) %>% 
    group_by(HEADER_TIME_STAMP) %>% 
    summarize(cnt_obs_invalid = sum(any_obs_invalid)) %>% 
    mutate(cnt_sec_invalid = round(cnt_obs_invalid / srate, 1)) %>%
    as.data.frame() 

  saveRDS(out_ALL, outfile)
  message("Saved output.")
}

