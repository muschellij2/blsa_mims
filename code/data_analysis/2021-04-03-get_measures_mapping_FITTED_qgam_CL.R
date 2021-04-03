#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

#' Notes: 
#' 
#' qacct -j jobID
#' 
#' cd $mims
#' cd code/data_analysis
#' Rnosave 2021-04-03-get_measures_mapping_FITTED_qgam_CL.R -l mem_free=50G,h_vmem=50G,h_stack=256M -t 1-4 -N JOB_qgam

library(tidyverse)
library(mgcv)
library(qgam)
library(parallel)
options(scipen=999)

idx  =  as.numeric(Sys.getenv("SGE_TASK_ID")) # idx <- 1
message(paste0("ncores = parallel::detectCores(): ", parallel::detectCores()))


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
k <- 8
knots_new <- c(1,1315,2604,3916,5285,6914,11312,15708)
ncores_tmp <- min(c(parallel::detectCores() - 1, 12))

# MIMS 
if (idx == 1){
  message(paste0("idx = ", idx))
  t1 <- Sys.time()
  fit_unconstr_MIMS <- qgam(MIMS ~ s(AC, k = k, bs = "cr"), data = dat_acc, 
                            argGam = list(knots=list(AC = knots_new)), 
                            err = 0.1, 
                            qu = 0.5,
                            multicore = TRUE, 
                            ncores = ncores_tmp)
  t2 <- Sys.time()
  message(t2 - t1)
  newdata$MIMS_fitted <- predict(fit_unconstr_MIMS, newdata)
}

# ENMO 
if (idx == 2){
  message(paste0("idx = ", idx))
  t1 <- Sys.time()
  fit_unconstr_ENMO <- qgam(ENMO ~ s(AC, k = k, bs = "cr"), data = dat_acc, 
                            argGam = list(knots=list(AC = knots_new)), 
                            err = 0.1,  
                            qu = 0.5,
                           multicore = TRUE, 
                           ncores = ncores_tmp)
  newdata$ENMO_fitted <- predict(fit_unconstr_ENMO, newdata)
  t2 <- Sys.time()
  message(t2 - t1)
}

# MAD 
if (idx == 3){
  message(paste0("idx = ", idx))
  t1 <- Sys.time()
  fit_unconstr_MAD <- qgam(MAD ~ s(AC, k = k, bs = "cr"), data = dat_acc, 
                           argGam = list(knots=list(AC = knots_new)), 
                           err = 0.1,  
                          qu = 0.5,
                          multicore = TRUE, 
                          ncores = ncores_tmp)
  newdata$MAD_fitted <- predict(fit_unconstr_MAD, newdata)
  t2 <- Sys.time()
  message(t2 - t1)
}

# AI 
if (idx == 4){
  message(paste0("idx = ", idx))
  t1 <- Sys.time()
  fit_unconstr_AI <- qgam(AI ~ s(AC, k = k, bs = "cr"), data = dat_acc, 
                          argGam = list(knots=list(AC = knots_new)), 
                          err = 0.1,  
                          qu = 0.5,
                         multicore = TRUE, 
                         ncores = ncores_tmp)
  newdata$AI_fitted <- predict(fit_unconstr_AI, newdata)
  t2 <- Sys.time()
  message(t2 - t1)
}

message("COMPLETED.")


# Save data 
fpath_tmp <- paste0(here::here(), "/results/2021-04-03-mapping_between_measures_FITTED_qgam_", idx,  ".rds")
saveRDS(newdata, fpath_tmp)






