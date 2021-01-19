#' This script produces "master file" with minute-level values 
#' of 5 measures: 
#' 
#' - AC, MIMS, MAD, ENMO, AI,
#' 
#' after filtering data to keep only: 
#' 
#' - data for participants for which both activity counts (AC) and open source
#'   measures (OSM) data exist
#' - data for participants who have >= 3 valid days
#' - data form valid days only
#' 
#' and after data winsorization. 
#' 
#' input files: 
#' - /data_processed/2021-01-19-measures_masterfile.rds
#' 
#' out file: 
#' - /data_processed/2021-01-19-measures_masterfile_winsorized.rds
#' 
#' Notes: 
#' - use: cd /dcl01/smart/data/activity/blsa_mims

rm(list = ls())
library(dplyr)
library(data.table)
library(lubridate)
library(arctools)
library(stringr)
library(here)
options(scipen=999)

# read minute-level measures master file
dat_fpath <- paste0(here::here(), "/data_processed/2021-01-19-measures_masterfile.rds")
dat <- readRDS(dat_fpath) 
dim(dat)
# Jan 18, 2021: 5791560      10
# Jan 19, 2021: 6147240      10

# read values with population-level measures that will be used for winsorizing 
fpath_tmp <- paste0(here::here(), "/results/2021-01-19-measures_vals_summary_population.rds")
wn_df <- readRDS(fpath_tmp)
str(wn_df)
wn_vec <- wn_df[, "val_qt_0.99999"]
names(wn_vec) <- wn_df[, "metric_name"]

# summary of values before winsorizing
summary(dat$AC)
summary(dat$MIMS)
summary(dat$MAD)
summary(dat$ENMO)
summary(dat$AI)

# winsorize 
dat_wn <- 
  dat %>%
  mutate(
    AC   = replace(AC,   AC   > wn_vec["AC"],   wn_vec["AC"]),
    MIMS = replace(MIMS, MIMS > wn_vec["MIMS"], wn_vec["MIMS"]),
    MAD  = replace(MAD,  MAD  > wn_vec["MAD"],  wn_vec["MAD"]),
    ENMO = replace(ENMO, ENMO > wn_vec["ENMO"], wn_vec["ENMO"]),
    AI   = replace(AI,   AI   > wn_vec["AI"],   wn_vec["AI"])
  )

summary(dat_wn$AC)
summary(dat_wn$MIMS)
summary(dat_wn$MAD)
summary(dat_wn$ENMO)
summary(dat_wn$AI)

# save as data frame
fpath_out <- paste0(here::here(), "/data_processed/2021-01-19-measures_masterfile_winsorized.rds")
saveRDS(dat_wn, fpath_out)
