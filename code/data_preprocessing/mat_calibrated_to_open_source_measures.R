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
source(here::here("code/helper_functions.R"))

library(SummarizedActigraphy)
message("Loaded packages.")
options(digits.secs = 3)

# source util functions

fnames = list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat")
outfiles = here::here("open_measures", 
                      paste0(sub("RAW[.]mat", "", basename(fnames)), 
                             "_calibrated_OSM.rds"))


# define input file name (specific to array job index)
ifile =  as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifile)) {
  ifile = 1
}

fname = fnames[ifile]
id = sub("RAW[.]mat", "", basename(fname))
message(fname)
# define output file name
outfile = here::here("open_measures", paste0(id, "_calibrated_OSM.rds"))

# read raw accelerometry data .mat file
if (!file.exists(outfile)) {
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
  
  # ------------------------------------------------------------------------------
  # compute open source measures (OSM)
  
  # MIMS, AI, MAD 
  # > see documentation at 
  #   https://github.com/muschellij2/SummarizedActigraphy/blob/master/R/calculate_measures.R
  acc_df = SummarizedActigraphy::calibrate(acc_df, fix_zeros = TRUE, 
                                           round_after_calibration = TRUE)
  out = SummarizedActigraphy::calculate_measures(
    df = acc_df, 
    fix_zeros = FALSE, # already imputed!
    dynamic_range = dynamic_range,
    calculate_mims = TRUE,
    calculate_ac = TRUE,
    flag_data = FALSE,
    verbose = TRUE)
  out = dplyr::rename(out, HEADER_TIME_STAMP = time)
  attr(out, "calibration_values") = attr(acc_df, "calibration_values")
  
  # # @MK on Jan 14, 2020: 
  # # adding the below line to address issue with MIMSunit package that allows 
  # # negative values in the output, see issue: https://github.com/mHealthGroup/MIMSunit/issues/21
  # out$MIMS_UNIT[out0$MIMS_UNIT < 0] <- 0
  
  # ENMO 
  # > The ENMO at time t is defined as max [r(t) – 1, 0]. 
  # > Further, the ENMO in a window of size H is defined as the average ENMO across 
  #   the time points in that window. 
  # out_ENMO = acc_df %>% 
  #   mutate(         
  #     r = sqrt(X^2 + Y^2 + Z^2),
  #     ENMO_t = ifelse(r - 1 > 0 , r - 1, 0),
  #     HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, "1 min")) %>% 
  #   group_by(HEADER_TIME_STAMP) %>% 
  #   summarise(
  #     ENMO = mean(ENMO_t)
  #   )
  # 
  # # combine partial results into final data frame 
  # out = full_join(out, out_ENMO, by = "HEADER_TIME_STAMP")
  
  # ------------------------------------------------------------------------------
  # save to file
  
  saveRDS(out, outfile, compress = "xz")
  message("Saved output.")
} else {
  out = readr::read_rds(outfile)
  if (is.null(attr(out, "calibration_values"))) {
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
    
    # ------------------------------------------------------------------------------
    # compute open source measures (OSM)
    
    # MIMS, AI, MAD 
    # > see documentation at 
    #   https://github.com/muschellij2/SummarizedActigraphy/blob/master/R/calculate_measures.R
    acc_df = SummarizedActigraphy::calibrate(acc_df, fix_zeros = TRUE, 
                                             round_after_calibration = TRUE)
    attr(out, "calibration_values") = attr(acc_df, "calibration_values")
    saveRDS(out, outfile, compress = "xz")
  }
}
