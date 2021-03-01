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

# define lower range of "near" the max absolute value of dynamic range area
g_maxabsN <- g_maxabs - 0.05
# define spike value
g_spike <- round(diff(range(dynamic_range)) * 11/12, 1)
  
# subset data to keep timestamp and three axes data only
acc_df = acc_df$Xi
acc_df = acc_df %>%
  select(HEADER_TIME_STAMP, X, Y, Z) 
stopifnot(!anyNA(acc_df))

# function to flag and per minute-aggregate a single axis of raw accelerometry data 
aggregate_axis <- function(axis_var_name){
  x <- acc_df %>% pull(get(axis_var_name))
  x_lag    <- c(1, diff(x)) 
  x_lead   <- rev(c(1, diff(rev(x)))) 
  x_aslag    <- as.numeric(x_lag == 0)
  x_aslead   <- as.numeric(x_lead == 0)
  # flag: is contiguous 
  x_ascontig <- pmax(x_aslag, x_aslead)
  # flag: is spike 
  x_spike    <- as.numeric(pmax(abs(x_lag), abs(x_lead)) >= g_spike)
  # flag: is max, is maxN (max near)
  x_g_max <- as.numeric(x == g_maxabs)
  x_g_min <- as.numeric(x == (-g_maxabs))
  x_g_maxN <- as.numeric(x >= g_maxabsN)
  x_g_minN <- as.numeric(x <= (-g_maxabsN))
  x_contig_g_max =  as.numeric(x_ascontig + x_g_max == 2)
  x_contig_g_min =  as.numeric(x_ascontig + x_g_min == 2)
  x_contig_g_maxN =  as.numeric(x_ascontig + x_g_maxN == 2)
  x_contig_g_minN =  as.numeric(x_ascontig + x_g_minN == 2)
  x_df_agg <- data.frame(
    HEADER_TIME_STAMP = acc_df %>% pull(get('HEADER_TIME_STAMP')),
    x_ascontig,
    x_spike,
    x_g_max,
    x_g_min,
    x_g_maxN,
    x_g_minN,
    x_contig_g_max,
    x_contig_g_min,
    x_contig_g_maxN,
    x_contig_g_minN
  ) %>% 
    mutate(HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, "1 min")) %>% 
    group_by(HEADER_TIME_STAMP) %>% 
    summarize_all(sum)
  names(x_df_agg) <- sapply(names(x_df_agg), function(val) gsub("x_", paste0(axis_var_name, "_"), val))
  return(x_df_agg)
}

# generate flags for each axis separately, aggregate per minute
acc_df_agg_X <- aggregate_axis("X")
acc_df_agg_Y <- aggregate_axis("Y")
acc_df_agg_Z <- aggregate_axis("Z")
# combine flags, select a subset of all axis-specific flags 
acc_df_agg <- 
  acc_df_agg_X %>% 
  inner_join(acc_df_agg_Y, by = "HEADER_TIME_STAMP") %>% 
  inner_join(acc_df_agg_Z, by = "HEADER_TIME_STAMP") %>%
  rowwise() %>%
  mutate(
    anyaxis_ascontig = max(X_ascontig, Y_ascontig, Z_ascontig), 
    anyaxis_spike = max(X_spike, Y_spike, Z_spike), 
    anyaxis_g_max = max(X_g_max, Y_g_max, Z_g_max), 
    anyaxis_g_min = max(X_g_min, Y_g_min, Z_g_min), 
    anyaxis_g_maxN = max(X_g_maxN, Y_g_maxN, Z_g_maxN), 
    anyaxis_g_minN = max(X_g_minN, Y_g_minN, Z_g_minN), 
    anyaxis_contig_g_max = max(X_contig_g_max, Y_contig_g_max, Z_contig_g_max), 
    anyaxis_contig_g_min = max(X_contig_g_min, Y_contig_g_min, Z_contig_g_min), 
    anyaxis_contig_g_maxN = max(X_contig_g_maxN, Y_contig_g_maxN, Z_contig_g_maxN), 
    anyaxis_contig_g_minN = max(X_contig_g_minN, Y_contig_g_minN, Z_contig_g_minN)
) %>% select(
  HEADER_TIME_STAMP,
  # anyaxis_ascontig, 
  anyaxis_spike,
  anyaxis_g_max,
  anyaxis_g_min,
  anyaxis_g_maxN,
  anyaxis_g_minN,
  anyaxis_contig_g_max,
  anyaxis_contig_g_min,
  anyaxis_contig_g_maxN, 
  anyaxis_contig_g_minN
) %>% ungroup() %>%
  as.data.frame()

saveRDS(acc_df_agg, outfile)
message("Saved output.")


