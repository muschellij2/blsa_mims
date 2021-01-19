#' This script  computes 
#' correlation between minute-level activity counts and 
#' open-source minute-level measurements. It uses winsorized version of masterfile
#' data with the measures.
#' 
#' input files: 
#' > /data_processed/2021-01-18-measures_masterfile_winsorized.rds
#' 
#' output files: 
#' > /results/2021-01-18-correlations_between_measures.rds


rm(list = ls())
library(dplyr)
library(data.table)
library(lubridate)

fpath_tmp <- paste0(here::here(), "/data_processed/2021-01-18-measures_masterfile_winsorized.rds")
dat <- readRDS(fpath_tmp)
str(dat)

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

# compute correlations for all day data (24h)
dat_corr_allday <- 
  dat %>% 
  dplyr::group_by(file_id) %>%
  dplyr::mutate(obs_cnt = n(), .before = dplyr::ends_with("_id")) %>%
  dplyr::ungroup() %>% 
  dplyr::nest_by(file_id, subj_id, visit_id, obs_cnt)  %>%
  dplyr::summarise(dat_to_corr_uppertri(data)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(from11to5_exclude = 0, .after = dplyr::ends_with("_id")) %>%
  as.data.frame()

# compute correlations while EXCLUDING 11pm - 5am 
dat_corr_from11to5exclude <- 
  dat %>% 
  dplyr::filter(!(lubridate::hour(HEADER_TIME_STAMP) %in% c(23,0,1,2,3,4))) %>%
  dplyr::group_by(file_id) %>%
  dplyr::mutate(obs_cnt = n(), .before = dplyr::ends_with("_id")) %>%
  dplyr::ungroup() %>% 
  dplyr::nest_by(file_id, subj_id, visit_id, obs_cnt)  %>%
  dplyr::summarise(dat_to_corr_uppertri(data)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(from11to5_exclude = 1, .after = dplyr::ends_with("_id")) %>%
  as.data.frame()

# combine data 
dat_corr_comb <- rbind(
  dat_corr_allday,
  dat_corr_from11to5exclude
)

dim(dat_corr_comb)
length(unique(dat_corr_comb$file_id))

# save as data frame
# fout_path <- paste0(here::here(), "/results/2021-01-13-osm_ac_corr.rds")
fout_path <- paste0(here::here(), "/results/2021-01-18-correlations_between_measures.rds")
saveRDS(dat_corr_comb, fout_path)

