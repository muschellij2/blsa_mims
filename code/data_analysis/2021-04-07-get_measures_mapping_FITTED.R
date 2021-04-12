
rm(list = ls())
library(tidyverse)
library(mgcv)
options(scipen=999)

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath) 

# filter to keep only valid minutes data 
dat_acc <- dat_acc %>% dplyr::filter(wear_and_valid_flag == 1)
# filter to keep only valid minutes data 
dat_acc <- dat_acc %>% dplyr::filter(AC > 0)

# newdata objects 
AC_seq <- seq(from = 0, to = (1000 * 50), by = 1)
newdata <- data.frame(AC = AC_seq)

# model params
k <- 30

t1 <- Sys.time()

# MIMS 
fit_unconstr_MIMS <- gam(MIMS ~ s(AC, k = k, bs = "cr"), data = dat_acc)
newdata$MIMS_fitted <- predict(fit_unconstr_MIMS, newdata)
# ENMO 
fit_unconstr_ENMO <- gam(ENMO ~ s(AC, k = k, bs = "cr"), data = dat_acc)
newdata$ENMO_fitted <- predict(fit_unconstr_ENMO, newdata)
# MAD 
fit_unconstr_MAD <- gam(MAD ~ s(AC, k = k, bs = "cr"), data = dat_acc)
newdata$MAD_fitted <- predict(fit_unconstr_MAD, newdata)
# AI 
fit_unconstr_AI <- gam(AI ~ s(AC, k = k, bs = "cr"), data = dat_acc)
newdata$AI_fitted <- predict(fit_unconstr_AI, newdata)

t2 <- Sys.time()
t2 - t1
# Time difference of 1.989121 mins

# Save data 
fpath_tmp <- paste0(here::here(), "/results_public/2021-04-07-mapping_between_measures_FITTED.rds")
saveRDS(newdata, fpath_tmp)





