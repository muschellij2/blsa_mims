#' This script  computes correlation between minute-level activity counts and 
#' open-source minute-level measurements. 
#' 
#' Note: 
#' - We use winsorized version of masterfile data with the measures.
#' - We use valid days only, and valid minutes within valid days. 
#' 
#' input files: 
#' > /data_processed/2021-03-25-measures_masterfile_winsorized.rds
#' 
#' output files: 
#' > /results/2021-03-25-correlations_between_measures.rds

rm(list = ls())
library(dplyr)
library(data.table)
library(lubridate)

fpath_tmp <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile_winsorized.rds")
dat <- readRDS(fpath_tmp)
dim(dat)
# Jan 18, 2021: 5791560      10
# Jan 19, 2021: 6147240      10
# Feb 22, 2021: 6147240      10
# Feb 25, 2021: 6147240      10
# Mar 3,  2021: 6147240      12
# Mar 25, 2021: 5566920      12

# function to correlation matrix' upper triangle vector from data frame
dat_to_corr_uppertri <- function(dat, corr_col_names = c("AC", "MIMS", "ENMO", "MAD", "AI")){
  m <- cor(dat[, corr_col_names], use = "pairwise.complete.obs") # added Feb 25, 2021
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
  dplyr::filter(wear_and_valid_flag == 1) %>% # added Mar 3, 2021
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
  dplyr::filter(wear_and_valid_flag == 1) %>% # # added Mar 3, 2021
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
# [1] 655

# save as data frame
fout_path <- paste0(here::here(), "/results/2021-03-25-correlations_between_measures.rds")
saveRDS(dat_corr_comb, fout_path)

# get /dcl01/smart/data/activity/blsa_mims/results/2021-03-25-correlations_between_measures.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/2021-03-25-correlations_between_measures.rds
