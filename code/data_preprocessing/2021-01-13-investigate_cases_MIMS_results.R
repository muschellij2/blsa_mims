#' This script is to reproduce suspicious result I observed with MIMS 
#' for n=3 cases; data in which these cases were indentified comes from: 
#' 
#' - one (1st) visit per participant
#' - data from valid days only (no more than 10% of non-wear)
#' 
#' input files: 
#' > /mats/ 
#' 
#' out file: 
#' > /results/2021-01-13-investigate_cases_MIMS_results.rds


rm(list = ls())
library(dplyr)
library(data.table)
library(lubridate)
library(MIMSunit)
library(readr)
library(ActivityIndex)
library(SummarizedActigraphy)
options(scipen=999)
source(here::here("code/helper_functions.R"))

packageVersion("MIMSunit")
# [1] ‘0.9.2’ (same as on CRAN as of 2021-01-13; does have)
packageVersion("SummarizedActigraphy")
# [1] ‘0.3.1’

# ------------------------------------------------------------------------------
# read data summary file and find cases with negative entries

fpath <- paste0(here::here(), "/results/2021-01-13-measures_vals_summary_per_participant.rds")
dat_summary <- readRDS(fpath)
dat_summary_sub <- 
  dat_summary %>% 
  filter(val_sum_below0 > 0) %>%
  select(file_id, validdays_cnt, measure_name, val_sum_below0) %>%
  as.data.frame()
dat_summary_sub
# file_id validdays_cnt measure_name val_sum_below0
# 1 493405WaTAS1E23150402 (2015-09-09)             3         MIMS              1
# 2 599705WaTAS1E23150406 (2015-09-18)             6         MIMS             22
# 3 612007WaTAS1H12190132 (2019-09-03)             5         MIMS             50


# ------------------------------------------------------------------------------
# rerun for one of the files 

fnames = sort(list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat"))
fname = fnames[grepl(strsplit(dat_summary_sub$file_id[3], " ")[[1]][1], fnames)]

# read raw accelerometry data .mat file
acc_df = read_acc_mat(fname)
# pull meta information about the file 
srate = acc_df$fs
header = acc_df$hed
dynamic_range =  get_dynamic_range(header)
# subset data to keep timestamp and three axes data only
acc_df = acc_df$Xi
acc_df = acc_df %>%
  select(HEADER_TIME_STAMP, X, Y, Z)
stopifnot(!anyNA(acc_df))

# compute MIMS, AI, MAD 
out_ALL0 = SummarizedActigraphy::calculate_measures(
  df = acc_df, 
  fix_zeros = TRUE, 
  dynamic_range = dynamic_range,
  calculate_mims = TRUE,
  verbose = TRUE)
out_ALL0 = dplyr::rename(out_ALL0, HEADER_TIME_STAMP = time)

