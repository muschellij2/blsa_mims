
args = commandArgs(trailingOnly = TRUE)

#' @description 
#' This script takes raw output from ActiLife giving acceleration measurements 
#' [g] along three orthogonal axes and computes minute-level summary statistic: 
#' - valid_minute [0/1] -- a flag whether or not a minute is a valid minute 
#'   in a sense it does not contain any of the axes being fixed at consecutive
#'   values from the range. 
#' 
#' Notes: 
#' qrsh 
#' cd $mims 
#' cd code/data_preprocessing
#' Rnosave mat_to_invalid_observations_count_per_minute.R -l mem_free=30G,h_vmem=31G -N JB_invalid_obs


rm(list = ls())
library(tidyverse)
library(readr)
source(here::here("code/helper_functions.R"))
options(digits.secs = 3)

# define input file name (specific to array job index)
ifile  =  as.numeric(as.character(args[1])) # as.numeric(Sys.getenv("SGE_TASK_ID"))
fnames = sort(list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat"))
fname  = fnames[ifile]
message(paste0("ifile: ", ifile, " [", round(ifile / length(fnames) * 100, 2), " %]"))

id = sub("RAW[.]mat", "", basename(fname))
outfile <- paste0(here::here(), "/flag_minutes/", id, "_flag_minutes.rds")

# read raw accelerometry data .mat file
acc_df = read_acc_mat(fname)
# pull meta information about the file 
srate = acc_df$fs
header = acc_df$hed
dynamic_range =  get_dynamic_range(header)
# get the max absolute value of dynamic range
dr_maxabs = max(abs(dynamic_range))
  
# subset data to keep timestamp and three axes data only
acc_df = acc_df$Xi
acc_df = acc_df %>%
  select(HEADER_TIME_STAMP, X, Y, Z)
stopifnot(!anyNA(acc_df))

# axis-specific flag: the value is same value as lead (one earlier) value
acc_df$X_aslead <- as.numeric(c(1, diff(acc_df$X)) == 0)
acc_df$Y_aslead <- as.numeric(c(1, diff(acc_df$Y)) == 0)
acc_df$Z_aslead <- as.numeric(c(1, diff(acc_df$Z)) == 0)
acc_df <- acc_df %>%
  mutate(
    # axis-specific flag: the value is an extreme dynamic range value
    X_spike = as.numeric(abs(X) == dr_maxabs),
    Y_spike = as.numeric(abs(Y) == dr_maxabs),
    Z_spike = as.numeric(abs(Z) == dr_maxabs),
    # axis-specific flag: the value (a) is same value as lead + (b) is an extreme dynamic range value
    X_aslead_spike =  as.numeric(X_aslead + X_spike == 2),
    Y_aslead_spike =  as.numeric(Y_aslead + Y_spike == 2),
    Z_aslead_spike =  as.numeric(Z_aslead + Z_spike == 2),
    # three axes-combined flag: any value is an extreme dynamic range value
    any_spike =  as.numeric((X_spike + Y_spike + Z_spike) > 0),
    # three axes-combined flag: any value (a) is same value as lead + (b) is an extreme dynamic range value
    any_aslead_spike =  as.numeric((X_aslead_spike + Y_aslead_spike + Z_aslead_spike) > 0)
  ) %>%
  select(-X_aslead, -Y_aslead, -Z_aslead) %>%
  select(-X_spike, -Y_spike, -Z_spike) %>%
  select(-X_aslead_spike, -Y_aslead_spike, -Z_aslead_spike) 

# compute number of invalid subsecond-level observations per minute
acc_df_agg = 
  acc_df %>% 
  mutate(HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, "1 min")) %>% 
  group_by(HEADER_TIME_STAMP) %>% 
  summarize(
    cnt_any_spike = sum(any_spike),
    cnt_any_aslead_spike = sum(any_aslead_spike)
  ) %>% 
  as.data.frame() 

saveRDS(acc_df_agg, outfile)
message("Saved output.")


