#' This script produce summaries of minute-level measures per partipant. 
#' In generating the measures, considered are only: 
#' 
#' - one (1st) visit per participant
#' - data from valid days only (no more than 10% of non-wear)
#' 
#' input files: 
#' > /open_source_measures/
#' > /csv/ 
#' 
#' out file: 
#' > /results/2021-01-13-measures_vals_summary_per_participant.rds
#' 
#' Notes: 
#' - use: cd /dcl01/smart/data/activity/blsa_mims
#' - checked a few (5) cases for which dim (activity counts df) was different than 
#'   dim (opem source measures df) and  in each case, it was a matter of one (last)
#'   minute observation 

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
id_osm = gsub("_OSM", "", basename(fpaths_osm))

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
  # as mentioned in email sent to Jace and John on Jan 13, 2021 at 5:33 PM
  filter(row_number() == 1) %>%  
  ungroup() %>%
  as.data.frame()
dim(fpaths_df_1stvisit)
length(unique(fpaths_df_1stvisit$subj_id)) 
# 772


# ------------------------------------------------------------------------------
# summarize values across participants  

# function to generate summary of values in vector: quantiles, max 
probs_vec <- c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)
get_vals_summary <- function(vec){
  out_quantiles <- quantile(vec, probs = probs_vec)
  out <- c(
    length(vec), 
    sum(vec < 0),
    sum((vec > 0 && vec <= 0.0001)),
    mean(vec),
    sd(vec),
    min(vec), 
    out_quantiles,
    max(vec)
    )
  return(out)
}

# define obejcts to store per-participant data summary results
out_nrow <- nrow(fpaths_df_1stvisit)
out_ncol <- length(get_vals_summary(1:3))
out_AC   <- matrix(nrow = out_nrow, ncol = out_ncol)
out_MIMS <- matrix(nrow = out_nrow, ncol = out_ncol)
out_MAD  <- matrix(nrow = out_nrow, ncol = out_ncol)
out_ENMO <- matrix(nrow = out_nrow, ncol = out_ncol)
out_AI   <- matrix(nrow = out_nrow, ncol = out_ncol)
out_validdays <- matrix(nrow = out_nrow, ncol = 1)

# interate over participants (1st visit only)
for (i in 1:out_nrow){
  print(paste0("i: ", i))
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
    dplyr::mutate(valid_day = (wear_flag_cnt >= (0.9 * 1440)) * 1)
  # read open source metrics (OSM)
  f_osm <- readRDS(fpath_osm) %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(HEADER_TIME_STAMP = ymd_hms(HEADER_TIME_STAMP)) %>%
    dplyr::rename(MIMS = MIMS_UNIT)
  # join the two data frames: AC, OSM
  # filter to keep data from valid day only 
  f_comb <- f_osm %>% dplyr::inner_join(f_ac, by = "HEADER_TIME_STAMP") 
  if (nrow(f_comb) != nrow(f_ac)) message("nrow(f_comb) != nrow(f_ac)")
  if (nrow(f_comb) != nrow(f_osm)) message("nrow(f_comb) != nrow(f_osm)")
  f_comb_F <- f_comb %>% filter(valid_day == 1) %>% as.data.frame()
  # if no valid days for that person
  if (nrow(f_comb_F) == 0){
    message("0 valid days")
    out_validdays[i, 1] <- 0
    next 
  }
  out_AC[i, ]   <- get_vals_summary(f_comb_F$AC)
  out_MIMS[i, ] <- get_vals_summary(f_comb_F$MIMS)
  out_MAD[i, ]  <- get_vals_summary(f_comb_F$MAD)
  out_ENMO[i, ] <- get_vals_summary(f_comb_F$ENMO)
  out_AI[i, ]   <- get_vals_summary(f_comb_F$AI)
  out_validdays[i, 1] <- length(unique(f_comb_F$HEADER_TIME_STAMP_date))
}

# append summary of 5 measures to one data frame 
out_vals <- rbind(
  out_AC, 
  out_MIMS,
  out_MAD,
  out_ENMO,
  out_AI
) %>% as.data.frame()
names(out_vals) <- c(
  "vac_cnt",
  "val_sum_below0",
  "val_sum_above0_below0.0001",
  "val_mean",
  "val_sd", 
  "val_min",
  paste0("val_quantile_", probs_vec),
  "val_max"
)

# add some meta info 
out <- data.frame(
  subj_id = rep(fpaths_df_1stvisit$subj_id, 5),
  visit_id = rep(fpaths_df_1stvisit$visit_id, 5),
  file_id = rep(fpaths_df_1stvisit$file_id, 5),
  validdays_cnt = rep(out_validdays[, 1], 5),
  measure_name = rep(c("AC", "MIMS", "MAD", "ENMO", "AI"), each = out_nrow)
) %>% cbind(out_vals)

# save as data frame
fout_path <- paste0(here::here(), "/results/2021-01-13-measures_vals_summary_per_participant.rds")
saveRDS(out, fout_path)
