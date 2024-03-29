#' This script performs mappings: 
#' 
#' mapping g: {MIMS, ENMO, MAD, AI} -> AC
#' mapping f: AC -> {MIMS, ENMO, MAD, AI}

rm(list = ls())
library(tidyverse)
options(scipen=999)

# source script with mapping
source(paste0(here::here(), "/code/data_preprocessing/measures_mapping_FUNC.R"))

# read minute-level measures data (winsorized, imputed)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized_imp.rds")
dat_acc <- readRDS(dat_acc_fpath) 
nrow(dat_acc) / 1440
str(dat_acc)


# mapping g: {MIMS, ENMO, MAD, AI} -> AC
t1 <- Sys.time()
dat_acc$AC_hat_from_MIMS <- MIMS_to_AC_map(dat_acc$MIMS)
dat_acc$AC_hat_from_ENMO <- ENMO_to_AC_map(dat_acc$ENMO)
dat_acc$AC_hat_from_MAD  <- MAD_to_AC_map(dat_acc$MAD)
dat_acc$AC_hat_from_AI   <- AI_to_AC_map(dat_acc$AI)
t2 <- Sys.time()
t2 - t1
c(
  cor(dat_acc$AC, dat_acc$AC_hat_from_MIMS),
  cor(dat_acc$AC, dat_acc$AC_hat_from_ENMO),
  cor(dat_acc$AC, dat_acc$AC_hat_from_MAD),
  cor(dat_acc$AC, dat_acc$AC_hat_from_AI)
)
# earlier:
# [1] 0.9913368 0.7477549 0.9161573 0.9691873
# 2021-05-06: 
# [1] 0.9913368 0.8947565 0.9161573 0.9691873


# mapping f: AC -> {MIMS, ENMO, MAD, AI}
t1 <- Sys.time()
dat_acc$MIMS_hat_from_AC <- AC_to_MIMS_map(dat_acc$AC)
dat_acc$ENMO_hat_from_AC <- AC_to_ENMO_map(dat_acc$AC)
dat_acc$MAD_hat_from_AC  <- AC_to_MAD_map(dat_acc$AC)
dat_acc$AI_hat_from_AC   <- AC_to_AI_map(dat_acc$AC)
t2 <- Sys.time()
t2 - t1
c(
  cor(dat_acc$MIMS, dat_acc$MIMS_hat_from_AC),
  cor(dat_acc$ENMO, dat_acc$ENMO_hat_from_AC),
  cor(dat_acc$MAD, dat_acc$MAD_hat_from_AC),
  cor(dat_acc$AI, dat_acc$AI_hat_from_AC)
)
# earlier:
# [1] 0.9925881 0.7664108 0.9074925 0.9707351
# 2021-05-06: 
# [1] 0.9925881 0.8717643 0.9074925 0.9707351

# save as data frame
fpath_out <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized_imp_mapped.rds")
saveRDS(dat_acc, fpath_out)

# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/data_processed/2021-05-06-measures_masterfile_winsorized_imp_mapped.rds /dcl01/smart/data/activity/blsa_mims/data_processed/2021-05-06-measures_masterfile_winsorized_imp_mapped.rds 
