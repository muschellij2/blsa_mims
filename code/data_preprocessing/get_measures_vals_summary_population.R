#' This script produce summaries of minute-level measures per population. 
#' It uses masterfile generated in 
#' - /code/data_preprocesing/prepare_measures_masterfile.R
#' 
#' input data file: 
#' - /data_processed/2021-02-25-measures_masterfile.rds
#' 
#' out file: 
#' - /results/2021-02-25-measures_vals_summary_population.rds
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

# function to generate summary of values in vector: quantiles, max 
get_vals_summary <- function(vec){
  probs_vec <- c(0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999)
  out_quantiles <- quantile(vec, probs = probs_vec, na.rm = TRUE)
  out <- c(length(vec), min(vec, na.rm = TRUE), out_quantiles,  max(vec, na.rm = TRUE))
  names(out) <- c("val_cnt", "val_min", paste0("val_qt_", probs_vec), "val_max")
  return(out)
}

# read minute-level measures master file
dat_fpath <- paste0(here::here(), "/data_processed/2021-02-25-measures_masterfile.rds")
dat <- readRDS(dat_fpath)
dim(dat)
# Jan 18, 2021: 5791560      10
# Feb 25, 2021: 6147240      10

val_summary_AC   <- get_vals_summary(dat$AC)
val_summary_MIMS <- get_vals_summary(dat$MIMS)
val_summary_MAD  <- get_vals_summary(dat$MAD)
val_summary_ENMO <- get_vals_summary(dat$ENMO)
val_summary_AI   <- get_vals_summary(dat$AI)

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
fout_path <- paste0(here::here(), "/results/2021-02-25-measures_vals_summary_population.rds")
saveRDS(d_val_summary_all, fout_path)





