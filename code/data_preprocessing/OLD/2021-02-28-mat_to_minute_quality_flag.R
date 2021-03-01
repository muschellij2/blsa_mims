#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

#' Notes: 
#' 
#' qrsh -l mem_free=20G,h_vmem=20G,h_stack=256M
#' cd $mims
#' cd code/data_preprocessing
#' Rnosave mat_to_minute_quality_flag.R -l mem_free=20G,h_vmem=20G,h_stack=256M -t 1-1244 -tc 90 -N JOB_quality_flag
#' 
#' fname = "/Users/martakaras/OneDrive - Johns Hopkins/BLSA/mat/599705WaTAS1E23150406 (2015-09-18)RAW.mat"
#' fname = "/dcl01/smart/data/activity/blsa_mims/mats/599705WaTAS1E23150406 (2015-09-18)RAW.mat"

library(tidyverse)
library(readr)
source(here::here("code/helper_functions.R"))
options(digits.secs = 3)

# define input file name (specific to array job index)
ifile  =  as.numeric(Sys.getenv("SGE_TASK_ID"))
fnames = sort(list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat"))
fname  = fnames[ifile]
message(paste0("ifile: ", ifile, " [", round(ifile / length(fnames) * 100, 2), " %]"))

id = sub("RAW[.]mat", "", basename(fname))
outfile <- paste0(here::here(), "/quality_flag/", id, "_quality_flag.rds")

# read raw accelerometry data .mat file
acc_df = read_acc_mat(fname)
# pull meta information about the file 
srate = acc_df$fs
header = acc_df$hed
dynamic_range =  get_dynamic_range(header)
# get the max absolute value of dynamic range
g_maxabs = max(abs(dynamic_range))
  
# subset data to keep timestamp and three axes data only
acc_df = acc_df$Xi
acc_df = acc_df %>%
  select(HEADER_TIME_STAMP, X, Y, Z)
stopifnot(!anyNA(acc_df))

# axis-specific flag: the value is same value as a lag (one earlier) value
# x <- c(1,2,1,3,3,3,4,3,3)
# x_aslag <- as.numeric(c(1, diff(x)) == 0)
# x_aslead <- as.numeric(rev(c(1, diff(rev(x))) == 0))
# x_ascontig <- pmax(x_aslag, x_aslead)

# x-axis flag: as contiguous 
acc_df$X_aslag    <- as.numeric(c(1, diff(acc_df$X)) == 0)
acc_df$X_aslead   <- as.numeric(rev(c(1, diff(rev(acc_df$X))) == 0))
acc_df$X_ascontig <- pmax(acc_df$X_aslag, acc_df$X_aslead)
acc_df <- select(acc_df, -X_aslag, -X_aslead)
# y-axis flag: as contiguous 
acc_df$Y_aslag    <- as.numeric(c(1, diff(acc_df$Y)) == 0)
acc_df$Y_aslead   <- as.numeric(rev(c(1, diff(rev(acc_df$Y))) == 0))
acc_df$Y_ascontig <- pmax(acc_df$Y_aslag, acc_df$Y_aslead)
acc_df <- select(acc_df, -Y_aslag, -Y_aslead)
# z-axis flag: as contiguous 
acc_df$Z_aslag    <- as.numeric(c(1, diff(acc_df$Z)) == 0)
acc_df$Z_aslead   <- as.numeric(rev(c(1, diff(rev(acc_df$Z))) == 0))
acc_df$Z_ascontig <- pmax(acc_df$Z_aslag, acc_df$Z_aslead)
acc_df <- select(acc_df, -Z_aslag, -Z_aslead)

acc_df <- acc_df %>%
  mutate(
    # axis-specific flag: the value is an extreme dynamic range value
    X_g_maxabs = as.numeric(abs(X) == g_maxabs),
    Y_g_maxabs = as.numeric(abs(Y) == g_maxabs),
    Z_g_maxabs = as.numeric(abs(Z) == g_maxabs),
    # axis-specific flag: the value (a) is same value as lag observation + (b) is an extreme dynamic range value
    X_contig_g_maxabs =  as.numeric(X_ascontig + X_g_maxabs == 2),
    Y_contig_g_maxabs =  as.numeric(Y_ascontig + Y_g_maxabs == 2),
    Z_contig_g_maxabs =  as.numeric(Z_ascontig + Z_g_maxabs == 2),
    # three axes-combined flag: any value is an extreme dynamic range value
    anyaxis_g_maxabs =  as.numeric((X_g_maxabs + Y_g_maxabs + Z_g_maxabs) > 0),
    # three axes-combined flag: any value (a) is same value as lead + (b) is an extreme dynamic range value
    anyaxis_contig_g_maxabs =  as.numeric((X_contig_g_maxabs + Y_contig_g_maxabs + Z_contig_g_maxabs) > 0)
  ) %>%
  select(-X_ascontig, -Y_ascontig, -Z_ascontig) %>%
  select(-X_g_maxabs, -Y_g_maxabs, -Z_g_maxabs) %>%
  select(-X_contig_g_maxabs, -Y_contig_g_maxabs, -Z_contig_g_maxabs) 

# aggregate number of positive flags per minute 
acc_df_agg <- 
  acc_df %>% 
  mutate(HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, "1 min")) %>% 
  group_by(HEADER_TIME_STAMP) %>% 
  summarize(
    anyaxis_g_maxabs = sum(anyaxis_g_maxabs),
    anyaxis_contig_g_maxabs = sum(anyaxis_contig_g_maxabs)
  ) %>% 
  as.data.frame() 

saveRDS(acc_df_agg, outfile)
message("Saved output.")


