#' This script produce summaries of minute-level measures per population. 
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
#' > /results/2021-01-13-measures_vals_summary_population.rds
#' 
#' use: 
#' cd /dcl01/smart/data/activity/blsa_mims

rm(list = ls())
library(dplyr)
library(data.table)
library(lubridate)
library(arctools)
library(stringr)
options(scipen=999)


# ------------------------------------------------------------------------------
# prepare list of files

# define file paths to ActiGrpah activity counts (AC), open source metrics (OSM)
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
  # as mentioned in email sent to Jace and John on Jan 13, 2021 at 5:33 PM
  filter(row_number() == 1) %>%  
  ungroup() %>%
  as.data.frame()
dim(fpaths_df_1stvisit)
length(unique(fpaths_df_1stvisit$subj_id)) 
# 772


# ------------------------------------------------------------------------------
# combine values across all participants 
#' - one (1st) visit per participant
#' - data from valid days only (no more than 10% of non-wear)

# vector with metric names
metric_name_vec <- c("AC", "MIMS", "MAD", "ENMO", "AI")

out_l <- nrow(fpaths_df_1stvisit)
out_dt_list <- vector("list", length = out_l)
for (i in 1:out_l){
  print(paste0("i: ", i))
  # iterate over participants (1st visit only)
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
  f_comb_F <- f_comb %>% filter(valid_day == 1) %>% as.data.frame()
  if (nrow(f_comb_F) > 0) {
    out_dt_list[[i]] <- as.data.table(f_comb_F[, metric_name_vec])
  }
}

# combine list into one df of values 
out_df <- rbindlist(out_dt_list) %>% as.data.frame()
dim(out_df)
# rm(out_dt_list)


# ------------------------------------------------------------------------------
# calculate the statistics

# function to generate summary of values in vector: quantiles, max 
get_vals_summary <- function(vec){
  probs_vec <- c(0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999)
  out_quantiles <- quantile(vec, probs = probs_vec)
  out <- c(length(vec), min(vec), out_quantiles,  max(vec))
  names(out) <- c("val_cnt", "val_min", paste0("val_qt_", probs_vec), "val_max")
  return(out)
}

val_summary_AC   <- get_vals_summary(out_df$AC)
val_summary_MIMS <- get_vals_summary(out_df$MIMS)
val_summary_MAD  <- get_vals_summary(out_df$MAD)
val_summary_ENMO <- get_vals_summary(out_df$ENMO)
val_summary_AI   <- get_vals_summary(out_df$AI)

# combine together 
metric_name_vec <- c("AC", "MIMS", "MAD", "ENMO", "AI")
d_val_summary_all <- rbind(
  val_summary_AC,
  val_summary_MIMS,
  val_summary_MAD,
  val_summary_ENMO,
  val_summary_AI
) %>% as.data.frame() %>%
  mutate(metric_name = metric_name_vec, .before = everything())

# save as data frame
# fout_path <- paste0(here::here(), "/results/2021-01-13-measures_vals_summary_population.rds")
fout_path <- paste0(here::here(), "/results/2021-01-15-measures_vals_summary_population.rds")
saveRDS(d_val_summary_all, fout_path)





