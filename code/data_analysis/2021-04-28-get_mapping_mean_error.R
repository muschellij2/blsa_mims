
rm(list = ls())
library(tidyverse)
options(scipen=999)


# ------------------------------------------------------------------------------
# read data 

fpath_tmp <- paste0(here::here(), "/data_processed/2021-04-13-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc <- readRDS(fpath_tmp) %>% as.data.frame()
str(dat_acc)


# ------------------------------------------------------------------------------
# compute bias, i.e. mean error: i-th participant mean(estimated_ij - true_ij)

dat_acc_F <- dat_acc %>%
  filter(wear_and_valid_flag == 1) %>%
  mutate(
    E_MIMS = AC_hat_from_MIMS - AC,
    E_ENMO = AC_hat_from_ENMO - AC,
    E_MAD  = AC_hat_from_MAD  - AC,
    E_AI   = AC_hat_from_AI   - AC,
    
    SE_MIMS = E_MIMS^2,
    SE_ENMO = E_ENMO^2,
    SE_MAD  = E_MAD^2,
    SE_AI   = E_AI^2
  )

dat_acc_agg <- 
  dat_acc_F %>%
  group_by(subj_id) %>%
  summarise(
    ME_MIMS = mean(E_MIMS),
    ME_ENMO = mean(E_ENMO),
    ME_MAD  = mean(E_MAD),
    ME_AI   = mean(E_AI),
    
    MSE_MIMS = mean(SE_MIMS),
    MSE_ENMO = mean(SE_ENMO),
    MSE_MAD  = mean(SE_MAD),
    MSE_AI   = mean(SE_AI),
    
    days_cnt = n_distinct(HEADER_TIME_STAMP_date),
    AC_sum = sum(AC),
    AC_mean = mean(AC)
)

path_tmp <- paste0(here::here(), "/results/2021-04-28-mapping_mean_error.rds")
saveRDS(dat_acc_agg, path_tmp)
