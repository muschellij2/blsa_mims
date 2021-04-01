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
#' - /data_processed/2021-03-25-measures_masterfile.rds
#' 
#' out file: 
#' - /data_processed/2021-03-25-measures_masterfile_winsorized.rds
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


# ------------------------------------------------------------------------------

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath) 
dat_acc <- dat_acc %>% dplyr::filter(wear_and_valid_flag == 1)
x <- dat_acc$AC

t1 <- Sys.time()
dat_acc$AC_hat_MIMS <- MIMS_to_AC_map(dat_acc$MIMS)
dat_acc$AC_hat_ENMO <- ENMO_to_AC_map(dat_acc$ENMO)
dat_acc$AC_hat_MAD  <- MAD_to_AC_map(dat_acc$MAD)
dat_acc$AC_hat_AI   <- AI_to_AC_map(dat_acc$AI)
t2 <- Sys.time()
t2 - t1
c(
  cor(dat_acc$AC, dat_acc$AC_hat_MIMS),
  cor(dat_acc$AC, dat_acc$AC_hat_ENMO),
  cor(dat_acc$AC, dat_acc$AC_hat_MAD),
  cor(dat_acc$AC, dat_acc$AC_hat_AI)
)

t1 <- Sys.time()
dat_acc$MIMS_hat <- AC_to_MIMS_map(dat_acc$AC)
dat_acc$ENMO_hat <- AC_to_ENMO_map(dat_acc$AC)
dat_acc$MAD_hat  <- AC_to_MAD_map(dat_acc$AC)
dat_acc$AI_hat   <- AC_to_AI_map(dat_acc$AC)
t2 <- Sys.time()
t2 - t1
c(
  cor(dat_acc$MIMS, dat_acc$MIMS_hat),
  cor(dat_acc$ENMO, dat_acc$ENMO_hat),
  cor(dat_acc$MAD, dat_acc$MAD_hat),
  cor(dat_acc$AI, dat_acc$AI_hat)
)


t1 <- Sys.time()
dat_acc$MIMS_hat <- AC_to_MIMS_map(dat_acc$AC, set0_for_AC0 = FALSE)
dat_acc$ENMO_hat <- AC_to_ENMO_map(dat_acc$AC, set0_for_AC0 = FALSE)
dat_acc$MAD_hat  <- AC_to_MAD_map(dat_acc$AC, set0_for_AC0 = FALSE)
dat_acc$AI_hat   <- AC_to_AI_map(dat_acc$AC, set0_for_AC0 = FALSE)
t2 <- Sys.time()
t2 - t1
c(
  cor(dat_acc$MIMS, dat_acc$MIMS_hat),
  cor(dat_acc$ENMO, dat_acc$ENMO_hat),
  cor(dat_acc$MAD, dat_acc$MAD_hat),
  cor(dat_acc$AI, dat_acc$AI_hat)
)

