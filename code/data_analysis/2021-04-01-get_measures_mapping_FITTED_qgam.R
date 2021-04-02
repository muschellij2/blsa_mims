
#' Notes: 
#' 
#' cd $mims
#' cd code/data_analysis
#' Rnosave XXX -l mem_free=20G,h_vmem=20G,h_stack=256M -t 1-1 -N JOB_gam_boot

# https://cran.r-project.org/web/packages/qgam/vignettes/qgam.html

rm(list = ls())
library(tidyverse)
library(mgcv)
library(qgam)
library(parallel)
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
k <- 10

# MIMS 
t1 <- Sys.time()
fit_unconstr_MIMS <- qgam(MIMS ~ s(AC, k = k, bs = "cr"), data = dat_acc, 
                          qu = 0.5,
                          multicore = TRUE, 
                          ncores = parallel::detectCores() - 1)
t2 <- Sys.time()
t2 - t1
newdata$MIMS_fitted <- predict(fit_unconstr_MIMS, newdata)


# ENMO 
t1 <- Sys.time()
fit_unconstr_ENMO <- gam(ENMO ~ s(AC, k = k, bs = "cr"), data = dat_acc, 
                         qu = 0.5,
                         multicore = TRUE, 
                         ncores = parallel::detectCores() - 1)
newdata$ENMO_fitted <- predict(fit_unconstr_ENMO, newdata)
t2 <- Sys.time()
t2 - t1

# MAD 
t1 <- Sys.time()
fit_unconstr_MAD <- gam(MAD ~ s(AC, k = k, bs = "cr"), data = dat_acc, 
                        qu = 0.5,
                        multicore = TRUE, 
                        ncores = parallel::detectCores() - 1)
newdata$MAD_fitted <- predict(fit_unconstr_MAD, newdata)
t2 <- Sys.time()
t2 - t1

# AI 
t1 <- Sys.time()
fit_unconstr_AI <- gam(AI ~ s(AC, k = k, bs = "cr"), data = dat_acc, 
                       qu = 0.5,
                       multicore = TRUE, 
                       ncores = parallel::detectCores() - 1)
newdata$AI_fitted <- predict(fit_unconstr_AI, newdata)
t2 <- Sys.time()
t2 - t1


# Save data 
fpath_tmp <- paste0(here::here(), "/results_public/mapping_between_measures_FITTED_qgam.rds")
saveRDS(newdata, fpath_tmp)





