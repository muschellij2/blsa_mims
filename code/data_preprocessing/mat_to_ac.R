#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

#' @description 
#' This script takes raw output from ActiLife giving acceleration measurements 
#' [g] along three orthogonal axes and computes minute-level summary statistic: 
#' - MIMS
#' - AI
#' - MAD
#' - ENMO 
#' 
#' input files dir: /mats/
#' output files dir: /open_source_measures/
#' 
#' use:
#' qrsh -l mem_free=50G,h_vmem=50G,h_fsize=50G,h_stack=256M
#' cd /dcl01/smart/data/activity/blsa_mims
#' module load conda_R

# remotes::install_github("javybai/ActivityIndex")
# remotes::install_github("muschellij2/SummarizedActigraphy")
library(MIMSunit)
library(dplyr)
library(readr)
library(ActivityIndex)
library(SummarizedActigraphy)
message("Loaded packages.")
options(digits.secs = 3)

# source util functions
source(here::here("code/helper_functions.R"))

# define input file name (specific to array job index)
ifile =  as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifile)) {
  ifile = 1
}

fnames = sort(list.files(path = here::here("mats"), full.names = TRUE, 
                         pattern = "[.]mat"))
fname = fnames[ifile]
id = sub("RAW[.]mat", "", basename(fname))
message(fname)
# define output file name
outfile = here::here("open_measures", paste0(id, "_AChat.rds"))

if (!file.exists(outfile)) {
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
  
  # ---------------------------------------------------------------
  # compute open source measures (OSM)
  
  # AC
  acc_df = acc_df %>% 
    SummarizedActigraphy::fix_zeros(fill_in = TRUE, 
                                    by_second = FALSE)
  
  
  ac = SummarizedActigraphy::calculate_ac(
    df = acc_df)
  ac$X = ac$Y = ac$Z = NULL
  
  # ---------------------------------------------------------------
  # save to file
  
  saveRDS(ac, outfile)
  message("Saved output.")
  
}
