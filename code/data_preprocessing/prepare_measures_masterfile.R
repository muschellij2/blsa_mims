#' This script produces "master file" with minute-level values 
#' of 5 measures: 
#' 
#' - AC, MIMS, MAD, ENMO, AI,
#' 
#' after filtering data to keep only: 
#' 
#' - data for participants for which both activity counts (AC) and open source
#'   measures (OSM) data exist
#' - data for participants who have >= 3 valid days (>=90% of wear time with valid minute flags)
#' - data form valid days only
#' - data have a matched entry with BLSA covariates file (mastervisit.rdata) 
#' 
#' input files: 
#' - /open_source_measures/
#' - /csv/ 
#' - /covariates/mastervisit.rdata
#' 
#' out file: 
#' - /data_processed/2021-05-06-measures_masterfile.rds
#' 
#' Notes: 
#' - use: cd /dcl01/smart/data/activity/blsa_mims

rm(list = ls())
library(dplyr)
library(data.table)
library(lubridate)
library(arctools)
library(stringr)
options(scipen=999)


# ------------------------------------------------------------------------------
# prepare list of file names: activity counts (AC), open source metrics (OSM)
# - one (1st) visit per participant

# define file paths
fpaths_ac    = sort(list.files(path = paste0(here::here(), "/csv"), full.names = TRUE, pattern = "[.]csv"))
fpaths_osm   = sort(list.files(path = paste0(here::here(), "/open_source_measures"), full.names = TRUE))
fpaths_osmc  = sort(list.files(path = paste0(here::here(), "/open_measures"), full.names = TRUE, pattern = "_calibrated_OSM.rds"))
fpaths_qf    = sort(list.files(path = paste0(here::here(), "/quality_flag"), full.names = TRUE))

# file paths to IDs 
id_ac   = gsub("60sec.csv", "", basename(fpaths_ac))
id_osm  = gsub("_OSM.rds", "", basename(fpaths_osm))
id_osmc = gsub("_calibrated_OSM.rds", "", basename(fpaths_osmc))
id_qf   = gsub("_quality_flag.rds", "", basename(fpaths_qf))

# define vector of file paths to open source summary measures (OSM)
fpaths_df_ac   = data.frame(file_id = id_ac, file_path_ac = fpaths_ac) %>% mutate(exists_ac = 1)
fpaths_df_osm  = data.frame(file_id = id_osm, file_path_osm = fpaths_osm) %>% mutate(exists_osm = 1)
fpaths_df_osmc = data.frame(file_id = id_osmc, file_path_osmc = fpaths_osmc) %>% mutate(exists_osmc = 1)
fpaths_df_qf   = data.frame(file_id = id_qf, file_path_qf = fpaths_qf) %>% mutate(exists_qf = 1)

fpaths_df <- fpaths_df_ac %>% 
  full_join(fpaths_df_osm, by = "file_id") %>%
  full_join(fpaths_df_osmc, by = "file_id") %>%
  full_join(fpaths_df_qf, by = "file_id") %>%
  mutate(exists_all = exists_osm * exists_osm * exists_qf * exists_osmc) %>%
  mutate(subj_id = substr(file_id, start = 1, stop = 4),
         subj_id = as.numeric(subj_id),
         visit_id = substr(file_id, start = 5, stop = 6),
         visit_id = as.numeric(visit_id))
dim(fpaths_df)
# [1] 1242   12

# filter data frame to keep only these file_id where both minute-level acc data 
# files exist, 
# filter data frame to keep only these file_id that correspond to 1st visit 
fpaths_df_1stvisit_0 <- 
  fpaths_df %>%
  filter(exists_all == 1) %>%
  group_by(subj_id) %>%
  filter(visit_id == min(visit_id))
dim(fpaths_df_1stvisit_0)
# [1] 774  12

# the below line is to discard 1 case (subject ID, visit ID)=(5614,15) 
# that has duplicated raw .mats (and corresponding AC .csv) file,
# as mentioned in email sent to Jacek and John on Jan 13, 2021 at 5:33 PM
fpaths_df_1stvisit_0 <- 
  fpaths_df_1stvisit_0 %>% 
  filter(row_number() == 1) %>%  
  ungroup() %>%
  as.data.frame()
dim(fpaths_df_1stvisit_0)
# [1] 773   10
length(unique(fpaths_df_1stvisit_0$file_id))
# [1] 773


# filter data frame to keep only these file_id for which we have a matched entry 
# with BLSA covariates file (mastervisit.rdata)

## prepare masterdemog 
masterdemog_fpath <- paste0(here::here(), "/covariates/2021-01-19-masterdemog.rdata")
masterdemog0 <- get(load(masterdemog_fpath, ex <- new.env()), envir = ex)
masterdemog <- 
  masterdemog0 %>% 
  # the below commented filters do not change the dimension of the output
  # filter(!is.na(gender)) %>%
  # filter(!is.na(FirstVisit_Age)) %>%
  # filter(!is.na(BLSA_Race)) %>%
  select(idno_masterdemog = idno) %>%
  mutate(entry_masterdemog = 1) %>%
  distinct()
dim(masterdemog0)
# [1] 3445   15
dim(masterdemog)
# [1] 3445   2

## prepare mastervisit
mastervisit_fpath <- paste0(here::here(), "/covariates/2021-01-19-mastervisit.rdata")
mastervisit0 <- get(load(mastervisit_fpath, ex <- new.env()), envir = ex) 
mastervisit <- 
  mastervisit0 %>% 
  filter(!is.na(Age)) %>%
  filter(!is.na(BMI)) %>%
  # the below commented filters do not change the dimension of the outputgiven the BMI not NA condition 
  # filter(!is.na(WtKg)) %>%
  # filter(!is.na(HtCm)) %>%
  filter(!is.na(DOV)) %>%
  select(idno_mastervisit = IDNo, visit_mastervisit = Visit) %>%
  mutate(entry_mastervisit = 1)  %>%
  distinct()
dim(mastervisit0)
# [1] 25571   113
dim(mastervisit)
# [1] 25283     3
length(unique(mastervisit$idno_mastervisit))
# [1] 3439

fpaths_df_1stvisit_1 <- 
  fpaths_df_1stvisit_0 %>% 
  left_join(masterdemog, by = c("subj_id" = "idno_masterdemog")) %>% 
  left_join(mastervisit, by = c("subj_id" = "idno_mastervisit", "visit_id" = "visit_mastervisit")) %>%
  mutate(entry_masterdemog = replace(entry_masterdemog, is.na(entry_masterdemog), 0)) %>%
  mutate(entry_mastervisit = replace(entry_mastervisit, is.na(entry_mastervisit), 0))
dim(fpaths_df_1stvisit_0)
# [1] 773   12
dim(fpaths_df_1stvisit_1)
# [1] 773  14

# filter to keep only these who has masterdemog and mastervisit datas
fpaths_df_1stvisit_2 <-  
  fpaths_df_1stvisit_1 %>% 
  filter(entry_mastervisit == 1, entry_masterdemog == 1)


# @MK: Added Mar 25, 2021
# filter to keep only these files which have sensor sampling rate 80 Hz
device_info_df  <- readRDS(paste0(here::here(), "/results/2021-03-23-device_and_data_range_info.rds"))
device_info_vec <- device_info_df %>%
  mutate(
    file_id = gsub("RAW.mat", "", fname),
    file_id_date = stringr::str_extract(string = file_id, pattern = "(?<=\\().*(?=\\))"),
    file_id_date = as.Date(file_id_date)
  ) %>% 
  filter(srate == 80) %>%
  pull(file_id) 
fpaths_df_1stvisit_3 <-  fpaths_df_1stvisit_2 %>% filter(file_id %in% device_info_vec)

# final data set
fpaths_df_1stvisit <- fpaths_df_1stvisit_3
dim(fpaths_df_1stvisit)
length(unique(fpaths_df_1stvisit$subj_id)) 
# Jan 15, 2021: 772
# Jan 18, 2021: 710
# Jan 19, 2021: 758
# Feb 25, 2021: 758
# Mar 3,  2021: 758
# Mar 25, 2021: 682
# May 6,  2021: 682


# ------------------------------------------------------------------------------
# prepare master file

out_dt_list <- vector(mode = "list", length = nrow(fpaths_df_1stvisit))
  
# iterate over participants (1st visit only)
for (i in 1:nrow(fpaths_df_1stvisit)){ # i <-  1
  print(paste0("i: ", i))
  file_id    <- fpaths_df_1stvisit[i, "file_id"]
  subj_id    <- fpaths_df_1stvisit[i, "subj_id"]
  visit_id   <- fpaths_df_1stvisit[i, "visit_id"]
  fpath_osm  <- fpaths_df_1stvisit[i, "file_path_osm"]
  fpath_osmc <- fpaths_df_1stvisit[i, "file_path_osmc"]
  fpath_ac   <- fpaths_df_1stvisit[i, "file_path_ac"]
  fpath_qf   <- fpaths_df_1stvisit[i, "file_path_qf"]
  # read quality flag minute-level data; define final flag (valid_flag variable)
  f_qf <- readRDS(fpath_qf) %>%
    mutate(valid_flag = as.numeric((anyaxis_contig_g_minN + anyaxis_g_maxN + anyaxis_spike) == 0)) %>%
    select(HEADER_TIME_STAMP, valid_flag)
  # read and process activity counts (AC) 
  f_ac <- data.table::fread(fpath_ac) %>% 
    dplyr::as_tibble() %>%
    dplyr::select(HEADER_TIME_STAMP = timestamp, AC = vectormagnitude) %>%
    dplyr::mutate(HEADER_TIME_STAMP = ymd_hms(HEADER_TIME_STAMP)) %>%
    dplyr::mutate(HEADER_TIME_STAMP_date = base::as.Date(HEADER_TIME_STAMP)) %>%
    # dplyr::mutate(HEADER_TIME_STAMP_hour = lubridate::hour(HEADER_TIME_STAMP)) %>% # 0,...,23
    # define wear/non-wear flag [1/0]
    dplyr::mutate(wear_flag = arctools::get_wear_flag(AC, nonwear_0s_minimum_window = 90)) %>%
    # define valid/invalid raw acc data flag [0/1]  
    dplyr::inner_join(f_qf, by = "HEADER_TIME_STAMP") %>%
    dplyr::mutate(wear_and_valid_flag = wear_flag * valid_flag) %>%
    dplyr::group_by(HEADER_TIME_STAMP_date) %>%
    dplyr::mutate(wear_and_valid_flag_cnt = sum(wear_and_valid_flag)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(valid_day = (wear_and_valid_flag_cnt >= (0.9 * 1440)) * 1) %>%
    dplyr::filter(valid_day == 1) 
  # determine valid days count 
  valid_day_cnt <- length(unique(f_ac$HEADER_TIME_STAMP_date))
  if (!(valid_day_cnt >= 3)) {
    message(paste0(valid_day_cnt, " valid days."))
    next
  } 
  # read open source metrics (OSM)
  f_osm <- readRDS(fpath_osm) %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(HEADER_TIME_STAMP = ymd_hms(HEADER_TIME_STAMP)) %>%
    dplyr::rename(MIMS = MIMS_UNIT) %>%
    dplyr::mutate(MIMS = replace(MIMS, MIMS < 0, NA))  %>% # added Feb 25, 2021
    dplyr::rename(ENMO_nc = ENMO)  # added May 6, 2021
  # read open source metrics, computed after calibrating the data (OSMC)
  f_osmc <- readRDS(fpath_osmc) %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(HEADER_TIME_STAMP = ymd_hms(HEADER_TIME_STAMP)) %>%
    dplyr::select(HEADER_TIME_STAMP, ENMO = ENMO_t)  # added May 6, 2021
  # combine files
  f_comb <- f_osm %>% 
    dplyr::inner_join(f_osmc, by = "HEADER_TIME_STAMP") %>%
    dplyr::inner_join(f_ac, by = "HEADER_TIME_STAMP") %>% 
    # dplyr::select(HEADER_TIME_STAMP, AC, MIMS, MAD, ENMO, AI) %>%
    dplyr::mutate(file_id = file_id, .before = everything()) %>%
    dplyr::mutate(visit_id = visit_id, .before = everything()) %>%
    dplyr::mutate(subj_id = subj_id, .before = everything())
  # append to outer file
  out_dt_list[[i]] <- f_comb
}

out_df <- do.call(rbind, out_dt_list)
out_df <- out_df %>% 
  dplyr::select(file_id, subj_id, visit_id,
                HEADER_TIME_STAMP, 
                wear_flag, valid_flag, wear_and_valid_flag,
                AC, MIMS, MAD, ENMO, AI, ENMO_nc) %>%
  as.data.frame()

dim(out_df)
# Jan 15, 2021: 6225000       9
# Jan 18, 2021: 5791560      10
# Jan 19, 2021: 6147240      
# Feb 25, 2021: 6147240      12
# Mar 25, 2021: 5566920      12 
# May 6,  2021: 5566920      13

length(unique(out_df$file_id))
# Jan 19, 2021: 721
# Feb 25, 2021: 721
# Mar 3,  2021: 721
# Mar 25, 2021: 655 
# May 6,  2021: 655

# save as data frame
# out_df_fpath <- paste0(here::here(), "/data_processed/2021-01-18-measures_masterfile.rds")
# out_df_fpath <- paste0(here::here(), "/data_processed/2021-01-19-measures_masterfile.rds")
# out_df_fpath <- paste0(here::here(), "/data_processed/2021-02-25-measures_masterfile.rds")
# out_df_fpath <- paste0(here::here(), "/data_processed/2021-03-03-measures_masterfile.rds")
# out_df_fpath <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile.rds")
out_df_fpath <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile.rds")
saveRDS(out_df, out_df_fpath)


