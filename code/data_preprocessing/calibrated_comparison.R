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
library(corrr)
source(here::here("code/helper_functions.R"))

library(SummarizedActigraphy)
message("Loaded packages.")
options(digits.secs = 3)

# source util functions

fnames = list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat")
df = tibble::tibble(
  c_outfile = here::here("open_measures", 
                          paste0(sub("RAW[.]mat", "", basename(fnames)), 
                                 "_calibrated_OSM.rds")),
  outfile = here::here("open_source_measures", 
                        paste0(sub("RAW[.]mat", "", basename(fnames)), 
                               "_OSM.rds"))
)

df = df %>% 
  filter(file_exists(outfile) & file_exists(c_outfile)) 
iid = 1 

cmat = vector(mode = "list", length = nrow(df)) 
calib_mat = vector(mode = "list", length = nrow(df)) 

for (iid in seq(nrow(df))) {
  idf = df[iid, ]
  i1 = readRDS(idf$outfile)
  i2 = readRDS(idf$c_outfile)
  stopifnot(nrow(i1) == nrow(i2))
  calib = attr(i2, "calibration_values")
  calib = calib[c("scale", "offset")]
  calib_mat[[iid]] = calib
  stopifnot(all(i1$HEADER_TIME_STAMP == i2$HEADER_TIME_STAMP))
  i1 = i1 %>% 
    select(-HEADER_TIME_STAMP)
  i2 = i2 %>% 
    select(-HEADER_TIME_STAMP)  
  cc = cor(i1, i2)
  cmat[[iid]] = cc
  print(iid)
}

readr::write_rds(cmat, file = "results/calibration_correlation_matrices.R", 
                 compress = "xz")
readr::write_rds(calib_mat, file = "results/calibration_coefficients.R", 
                 compress = "xz")
# weird mims: 493405WaTAS1E23150402 (2015-09-09)_OSM.rds
enmo = sapply(cmat, function(x) x['ENMO', 'ENMO_t'])
mad = sapply(cmat, function(x) x['MAD', 'MAD'])
mims = sapply(cmat, function(x) x['MIMS_UNIT', 'MIMS_UNIT'])
ai = sapply(cmat, function(x) x['AI', 'AI'])


  
