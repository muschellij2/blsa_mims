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
#' - /data_processed/2021-05-06-measures_masterfile.rds
#' 
#' out file: 
#' - /data_processed/2021-05-06-measures_masterfile_winsorized.rds
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
dat_fpath <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile.rds")
dat <- readRDS(dat_fpath) 
head(dat)

# read values with population-level measures that will be used for winsorizing 
fpath_tmp <- paste0(here::here(), "/results/2021-05-06-measures_vals_summary_population.rds")
wn_df <- readRDS(fpath_tmp)
str(wn_df)
wn_vec <- wn_df[, "val_qt_0.999"] # @MK: changed Mar 3 
names(wn_vec) <- wn_df[, "metric_name"]

# summary of values before winsorizing
summary(dat %>% filter(wear_and_valid_flag == 1) %>% pull(AC))
summary(dat %>% filter(wear_and_valid_flag == 1) %>% pull(MIMS))
summary(dat %>% filter(wear_and_valid_flag == 1) %>% pull(ENMO))
summary(dat %>% filter(wear_and_valid_flag == 1) %>% pull(MAD))
summary(dat %>% filter(wear_and_valid_flag == 1) %>% pull(AI))
summary(dat %>% filter(wear_and_valid_flag == 1) %>% pull(ENMO_nc))

# winsorize 
dat_wn <- 
  dat %>%
  mutate(
    AC   = replace(AC,   AC   > wn_vec["AC"],   wn_vec["AC"]),
    MIMS = replace(MIMS, MIMS > wn_vec["MIMS"], wn_vec["MIMS"]),
    ENMO = replace(ENMO, ENMO > wn_vec["ENMO"], wn_vec["ENMO"]),
    MAD  = replace(MAD,  MAD  > wn_vec["MAD"],  wn_vec["MAD"]),
    AI   = replace(AI,   AI   > wn_vec["AI"],   wn_vec["AI"]),
    ENMO_nc   = replace(ENMO_nc,   ENMO_nc   > wn_vec["ENMO_nc"],   wn_vec["ENMO_nc"])
  )

summary(dat_wn %>% filter(wear_and_valid_flag == 1) %>% pull(AC))
summary(dat_wn %>% filter(wear_and_valid_flag == 1) %>% pull(MIMS))
summary(dat_wn %>% filter(wear_and_valid_flag == 1) %>% pull(ENMO))
summary(dat_wn %>% filter(wear_and_valid_flag == 1) %>% pull(MAD))
summary(dat_wn %>% filter(wear_and_valid_flag == 1) %>% pull(AI))

# save as data frame
fpath_out <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized.rds")
saveRDS(dat_wn, fpath_out)

# get /dcl01/smart/data/activity/blsa_mims/data_processed/2021-05-06-measures_masterfile.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/data_processed/2021-05-06-measures_masterfile.rds
# get /dcl01/smart/data/activity/blsa_mims/data_processed/2021-05-06-measures_masterfile_winsorized.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/data_processed/2021-05-06-measures_masterfile_winsorized.rds
