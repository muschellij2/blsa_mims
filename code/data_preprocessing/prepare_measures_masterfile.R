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
#' input files: 
#' - /open_source_measures/
#' - /csv/ 
#' 
#' out file: 
#' - /data_processed/2021-01-15-measures_masterfile.rds
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
fpaths_ac  = sort(list.files(path = paste0(here::here(), "/csv"), full.names = TRUE, pattern = "[.]csv"))
fpaths_osm = sort(list.files(path = paste0(here::here(), "/open_source_measures"), full.names = TRUE))

# file paths to IDs 
id_ac  = gsub("60sec.csv", "", basename(fpaths_ac))
id_osm = gsub("_OSM.rds", "", basename(fpaths_osm))

# define vector of file paths to open source summary measures (OSM)
fpaths_df_ac  = data.frame(file_id = id_ac, file_path_ac = fpaths_ac) %>% mutate(exists_ac = 1)
fpaths_df_osm = data.frame(file_id = id_osm, file_path_osm = fpaths_osm) %>% mutate(exists_osm = 1)
fpaths_df <- fpaths_df_ac %>% full_join(fpaths_df_osm, by = "file_id") %>%
  mutate(exists_all = exists_osm * exists_osm) %>%
  mutate(subj_id = substr(file_id, start = 1, stop = 4),
         subj_id = as.numeric(subj_id),
         visit_id = substr(file_id, start = 5, stop = 6),
         visit_id = as.numeric(visit_id))
# filter data frame to keep files where both minute-level data files exist 
# and is 1st visit out of these 
fpaths_df_1stvisit <- fpaths_df %>%
  filter(exists_all == 1) %>%
  group_by(subj_id) %>%
  filter(visit_id == min(visit_id)) %>%
  # the below line is to discard 1 case (subject ID, visit ID)=(5614,15) 
  # that has duplicated raw .mats (and corresponding AC .csv) file,
  # as mentioned in email sent to Jacek and John on Jan 13, 2021 at 5:33 PM
  filter(row_number() == 1) %>%  
  ungroup() %>%
  as.data.frame()
dim(fpaths_df_1stvisit)
length(unique(fpaths_df_1stvisit$subj_id)) 
# Jan 15, 2020: 772


# ------------------------------------------------------------------------------
# prepare master file

out_dt_list <- vector(mode = "list", length = nrow(fpaths_df_1stvisit))
  
# iterate over participants (1st visit only)
for (i in 1:nrow(fpaths_df_1stvisit)){
  print(paste0("i: ", i))
  file_id  <- fpaths_df_1stvisit[i, "file_id"]
  subj_id   <- fpaths_df_1stvisit[i, "subj_id"]
  visit_id  <- fpaths_df_1stvisit[i, "visit_id"]
  fpath_osm <- fpaths_df_1stvisit[i, "file_path_osm"]
  fpath_ac  <- fpaths_df_1stvisit[i, "file_path_ac"]
  # read and process activity counts (AC) 
  f_ac <- data.table::fread(fpath_ac) %>% 
    dplyr::as_tibble() %>%
    dplyr::select(HEADER_TIME_STAMP = timestamp, AC = vectormagnitude) %>%
    dplyr::mutate(HEADER_TIME_STAMP = ymd_hms(HEADER_TIME_STAMP)) %>%
    dplyr::mutate(HEADER_TIME_STAMP_date = base::as.Date(HEADER_TIME_STAMP)) %>%
    dplyr::mutate(HEADER_TIME_STAMP_hour = lubridate::hour(HEADER_TIME_STAMP)) %>% # 0,...,23
    dplyr::mutate(wear_flag = arctools::get_wear_flag(AC, nonwear_0s_minimum_window = 90)) %>%
    # dplyr::mutate(from11to5 = ifelse(HEADER_TIME_STAMP_hour %in% c(23,0,1,2,3,4), 1, 0)) %>%
    dplyr::group_by(HEADER_TIME_STAMP_date) %>%
    dplyr::mutate(wear_flag_cnt = sum(wear_flag)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(valid_day = (wear_flag_cnt >= (0.9 * 1440)) * 1) %>%
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
    dplyr::rename(MIMS = MIMS_UNIT) 
  # combine files
  f_comb <- f_osm %>% 
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
                HEADER_TIME_STAMP, wear_flag, 
                AC, MIMS, MAD, ENMO, AI) 

dim(out_df)
# Jan 15, 2021: 6225000       9
# Jan 18, 2021: 6225000       10

table(out_df$wear_flag)


# save as data frame
fout_path <- paste0(here::here(), "/data_processed/2021-01-18-measures_masterfile.rds")
saveRDS(out_df, fout_path)
