#' This script iterates over Actilife-outputed CSV files and computes 
#' correlation between minute-level activity counts and 
#' open-source minute-level measurements. 
#' 
#' input files: 
#' > /csv/
#' > /open_source_measures/
#' 
#' output files: 
#' > /open_source_measures_ac_corr/


rm(list = ls())
library(dplyr)
library(data.table)
library(lubridate)
library(arctools)


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
  dplyr::mutate(exists_all = exists_osm * exists_osm) %>%
  dplyr::mutate(subj_id = substr(file_id, start = 1, stop = 4),
               subj_id = as.numeric(subj_id),
               visit_id = substr(file_id, start = 5, stop = 6),
               visit_id = as.numeric(visit_id))
# filter data frame to keep files where both minute-level data files exist 
# and is 1st visit out of these 
fpaths_df_1stvisit <- fpaths_df %>%
  dplyr::filter(exists_all == 1) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::filter(visit_id == min(visit_id)) %>%
  # the below line is to discard 1 case (subject ID, visit ID)=(5614,15) 
  # that has duplicated raw .mats (and corresponding AC .csv) file,
  # as mentioned in email sent to Jacek and John on Jan 13, 2021 at 5:33 PM
  dplyr::filter(row_number() == 1) %>%  
  dplyr::ungroup() %>%
  as.data.frame()
dim(fpaths_df_1stvisit)
length(unique(fpaths_df_1stvisit$subj_id)) 


# ------------------------------------------------------------------------------

# read data to be used for windsorizing values 
wv_df_fpath <- paste0(here::here(), "/results/2021-01-13-measures_vals_summary_population.rds")
wv_df <- readRDS(wv_df_fpath) %>%
  dplyr::select(metric_name, wv = val_qt_0.99999) %>%
  as.data.frame()
wv_AC   <- wv_df[wv_df$metric_name == "AC", "wv"]
wv_MIMS <- wv_df[wv_df$metric_name == "MIMS", "wv"]
wv_MAD  <- wv_df[wv_df$metric_name == "MAD", "wv"]
wv_ENMO <- wv_df[wv_df$metric_name == "ENMO", "wv"]
wv_AI   <- wv_df[wv_df$metric_name == "AI", "wv"]
c(wv_AC, wv_MIMS, wv_MAD, wv_ENMO, wv_AI)


# ------------------------------------------------------------------------------

# function to correlation matrix' upper triangle vector from data frame
dat_to_corr_uppertri <- function(dat, corr_col_names = c("AC", "MIMS", "MAD", "ENMO", "AI")){
  m <- cor(dat[, corr_col_names])
  out <- data.frame(S1=rownames(m)[row(m)[upper.tri(m)]], 
                    S2=colnames(m)[col(m)[upper.tri(m)]], 
                    corr_val=m[upper.tri(m)]) %>%
    dplyr::mutate(S1_S2 = paste0(S1, "_", S2)) %>%
    dplyr::select(corr_val, S1_S2) %>%
    tidyr::pivot_wider(names_from = "S1_S2", values_from = "corr_val")
  return(out)
}


# ------------------------------------------------------------------------------
# compute correlations 

out_out_all <- data.frame()

for (i in 1:nrow(fpaths_df_1stvisit)){
  print(paste0("i: ", i))
  f_id      <- fpaths_df_1stvisit[i, "file_id"]
  fpath_osm <- fpaths_df_1stvisit[i, "file_path_osm"]  # path to open source metrics (OSM) file
  fpath_ac  <- fpaths_df_1stvisit[i, "file_path_ac"]   # path to ActiGraph counts (AC) file 
  # read and process activity counts (AC) 
  f_ac <- data.table::fread(fpath_ac) %>% 
    dplyr::as_tibble() %>%
    dplyr::select(HEADER_TIME_STAMP = timestamp, AC = vectormagnitude) %>%
    dplyr::mutate(HEADER_TIME_STAMP = ymd_hms(HEADER_TIME_STAMP)) %>%
    dplyr::mutate(HEADER_TIME_STAMP_date = base::as.Date(HEADER_TIME_STAMP)) %>%
    dplyr::mutate(HEADER_TIME_STAMP_hour = lubridate::hour(HEADER_TIME_STAMP)) %>% # 0,...,23
    dplyr::mutate(wear_flag = arctools::get_wear_flag(AC, nonwear_0s_minimum_window = 90)) %>%
    dplyr::mutate(from11to5 = ifelse(HEADER_TIME_STAMP_hour %in% c(23,0,1,2,3,4), 1, 0)) %>%
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
  f_comb <- f_osm %>% 
    dplyr::inner_join(f_ac, by = "HEADER_TIME_STAMP") %>%
    dplyr::filter(valid_day == 1)
  if (nrow(f_comb) == 0){
    message("0 valid days")
    next
  }
  # compute correlations: no windsorizing of values 
  out_corr_NW <- rbind(
    f_comb %>% dat_to_corr_uppertri,
    f_comb %>% filter(wear_flag == 1) %>% dat_to_corr_uppertri,
    f_comb %>% filter(from11to5 == 0) %>% dat_to_corr_uppertri,
    f_comb %>% filter(wear_flag == 1, from11to5 == 0) %>% dat_to_corr_uppertri
  )
  # compute correlations: with windsorizing 
  f_comb_WW <- f_comb %>% 
    mutate(AC = replace(AC, AC > wv_AC, wv_AC)) %>%
    mutate(MIMS = replace(MIMS, MIMS > wv_MIMS, wv_MIMS)) %>%
    mutate(MAD = replace(MAD, MAD > wv_MAD, wv_MAD)) %>%
    mutate(ENMO = replace(ENMO, ENMO > wv_ENMO, wv_ENMO)) %>%
    mutate(AI = replace(AI, AI > wv_AI, wv_AI)) 
  out_corr_WW <- rbind(
    f_comb_WW %>% dat_to_corr_uppertri,
    f_comb_WW %>% filter(wear_flag == 1) %>% dat_to_corr_uppertri,
    f_comb_WW %>% filter(from11to5 == 0) %>% dat_to_corr_uppertri,
    f_comb_WW %>% filter(wear_flag == 1, from11to5 == 0) %>% dat_to_corr_uppertri
  )
  # combine corr values 
  out_df_corr <- rbind(out_corr_NW, out_corr_WW) %>% as.data.frame()
  # define meta info df 
  out_df_i <- data.frame(
    wearflag_only = rep(c(0,1), 4),
    from11to5_excl = rep(c(0,0,1,1), 2),
    vals_windsorized = rep(c(0,1), each = 4)
  ) %>% cbind(out_df_corr) %>%
    mutate(file_id = f_id, .before = everything())  %>%
    mutate(file_idx = i, .before = everything()) 
  # append results
  out_out_all <- rbind(out_out_all, out_df_i)
}

# save as data frame
# fout_path <- paste0(here::here(), "/results/2021-01-13-osm_ac_corr.rds")
fout_path <- paste0(here::here(), "/results/2021-01-15-osm_ac_corr.rds")
saveRDS(out_out_all, fout_path)

