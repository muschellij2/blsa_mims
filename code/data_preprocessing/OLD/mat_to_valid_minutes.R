#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

#' @description 
#' This script takes raw output from ActiLife giving acceleration measurements 
#' [g] along three orthogonal axes and computes minute-level summary statistic: 
#' - valid_minute [0/1] -- a flag whether or not a minute is a valid minute 
#'   in a sense it does not contain any of the axes being fixed at consecutive
#'   values from the range. 
#' 
#' input files dir: /mats/
#' output files dir: /open_source_measures/
#' 
#' use:
#' qrsh -l mem_free=50G,h_vmem=50G,h_fsize=50G,h_stack=256M
#' cd /dcl01/smart/data/activity/blsa_mims
#' module load conda_R

rm(list = ls())
library(tidyverse)
library(readr)
source(here::here("code/helper_functions.R"))
options(digits.secs = 3)

# define input file name (specific to array job index)
# ifile =  as.numeric(as.character(args[1])) # as.numeric(Sys.getenv("SGE_TASK_ID"))
ifile <- 1 
fnames = sort(list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat"))
fname = fnames[ifile]
fname = "/Users/martakaras/OneDrive - Johns Hopkins/BLSA/mat/599705WaTAS1E23150406 (2015-09-18)RAW.mat"

id = sub("RAW[.]mat", "", basename(fname))

# define output file name
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

# generate "invalid observation" flag
acc_df$X_diff <- c(1, diff(acc_df$X))
acc_df$Y_diff <- c(1, diff(acc_df$Y))
acc_df$Z_diff <- c(1, diff(acc_df$Z))
acc_df <- mutate(acc_df, X_obs_invalid = as.numeric((X_diff == 0) & (abs(X) == dr_maxabs)))
acc_df <- mutate(acc_df, Y_obs_invalid = as.numeric((Y_diff == 0) & (abs(Y) == dr_maxabs)))
acc_df <- mutate(acc_df, Z_obs_invalid = as.numeric((Z_diff == 0) & (abs(Z) == dr_maxabs)))
acc_df <- mutate(acc_df, any_obs_invalid = as.numeric((X_obs_invalid + Y_obs_invalid + Z_obs_invalid) > 0))
  
sum(acc_df$any_obs_invalid)
# [1] 118123

# compute number of invalid subsecond-level observations per minute
acc_df_agg = 
  acc_df %>% 
  mutate(HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, "1 min")) %>% 
  group_by(HEADER_TIME_STAMP) %>% 
  summarize(
    cnt_obs_invalid = sum(any_obs_invalid),
  ) %>% 
  as.data.frame() 


acc_df_agg$cnt_obs_invalid[acc_df_agg$cnt_obs_invalid > 0] / srate
acc_df_agg$cnt_obs_invalid[acc_df_agg$is_minute_invalid > 0] / srate

acc_df2 <- as.data.frame(acc_df2)
acc_df2$cnt_obs_invalid[acc_df2$cnt_obs_invalid > 0] 

acc_df2 %>% filter(cnt_obs_invalid == 9600) %>% filter(any_obs_invalid > 0)


# ------------------------------------------------------------------------------
# compute open source measures (OSM)

# MIMS, AI, MAD 
# > see documentation at 
#   https://github.com/muschellij2/SummarizedActigraphy/blob/master/R/calculate_measures.R
out_ALL0 = SummarizedActigraphy::calculate_measures(
  df = acc_df, 
  fix_zeros = TRUE, 
  dynamic_range = dynamic_range,
  calculate_mims = TRUE,
  verbose = TRUE)
out_ALL0 = dplyr::rename(out_ALL0, HEADER_TIME_STAMP = time)

# # @MK on Jan 14, 2020: 
# # adding the belows line to address issue with MIMSunit package that allows 
# # negative values in the output, see issue: https://github.com/mHealthGroup/MIMSunit/issues/21
# out_ALL0$MIMS_UNIT[out_ALL0$MIMS_UNIT < 0] <- 0

# ENMO 
# > The ENMO at time t is defined as max [r(t) – 1, 0]. 
# > Further, the ENMO in a window of size H is defined as the average ENMO across 
#   the time points in that window. 
out_ENMO = acc_df %>% 
  mutate(         
    r = sqrt(X^2 + Y^2 + Z^2),
    ENMO_t = ifelse(r - 1 > 0 , r - 1, 0),
    HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, "1 min")) %>% 
  group_by(HEADER_TIME_STAMP) %>% 
  summarise(
    ENMO = mean(ENMO_t)
  )

# combine partial results into final data frame 
out_ALL = full_join(out_ALL0, out_ENMO, by = "HEADER_TIME_STAMP")

# ------------------------------------------------------------------------------
# save to file

saveRDS(out_ALL, outfile)
message("Saved output.")

