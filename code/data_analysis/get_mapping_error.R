
rm(list = ls())
library(tidyverse)
options(scipen=999)


# ------------------------------------------------------------------------------
# read data 

fpath_tmp <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc <- readRDS(fpath_tmp) %>% as.data.frame()
str(dat_acc)

# ------------------------------------------------------------------------------
#' PART 1
#' map minute-level values, sum to daily total, compute MPE, APE

dat_acc_F <- 
  dat_acc %>%
  select(subj_id, HEADER_TIME_STAMP_date, starts_with("AC")) %>%
  group_by(subj_id, HEADER_TIME_STAMP_date) %>%
  summarise_all(sum) %>%
  mutate(
    # percentage error
    # := difference between an experimental and theoretical value, divided by the 
    #    theoretical value, multiplied by 100 to give a percent
    PE_MIMS = 100 * (AC_hat_from_MIMS - AC)/AC,
    PE_ENMO = 100 * (AC_hat_from_ENMO - AC)/AC,
    PE_MAD  = 100 * (AC_hat_from_MAD - AC)/AC,
    PE_AI   = 100 * (AC_hat_from_AI - AC)/AC,
    # absolute percentage error
    APE_MIMS = abs(PE_MIMS),
    APE_ENMO = abs(PE_ENMO),
    APE_MAD  = abs(PE_MAD),
    APE_AI   = abs(PE_AI)
  )

dat_acc_agg <- 
  dat_acc_F %>%
  group_by(subj_id) %>%
  summarise(
    # mean percentage error (MPE)
    MPE_MIMS = mean(PE_MIMS),
    MPE_ENMO = mean(PE_ENMO),
    MPE_MAD  = mean(PE_MAD),
    MPE_AI   = mean(PE_AI),
    # mean absolute percentage error (MAPE)
    MAPE_MIMS = mean(APE_MIMS),
    MAPE_ENMO = mean(APE_ENMO),
    MAPE_MAD  = mean(APE_MAD),
    MAPE_AI   = mean(APE_AI),
    # median percentage error (MPE)
    MdPE_MIMS = median(PE_MIMS),
    MdPE_ENMO = median(PE_ENMO),
    MdPE_MAD  = median(PE_MAD),
    MdPE_AI   = median(PE_AI),
    # median absolute percentage error (MAPE)
    MdAPE_MIMS = median(APE_MIMS),
    MdAPE_ENMO = median(APE_ENMO),
    MdAPE_MAD  = median(APE_MAD),
    MdAPE_AI   = median(APE_AI),
    days_cnt = n_distinct(HEADER_TIME_STAMP_date),
    AC_sum = sum(AC),
    AC_mean = mean(AC)
)

# path_tmp <- paste0(here::here(), "/results/2021-07-30-mapping_MPE_MAPE_mapminutelevel.rds")
path_tmp <- paste0(here::here(), "/results/2022-04-23-mapping_MPE_MAPE_MdPE_MdAPE_mapminutelevel.rds")
saveRDS(dat_acc_agg, path_tmp)

