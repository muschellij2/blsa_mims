#' This script computes summary statistics of minute-level values to explore
#' whether or not windsorizing is needed.
#' 
#' input files: 
#' > /csv/
#' > /open_source_measures/
#' 
#' out file: 
#' > /results/minute_level_metrics_summary.rds

rm(list = ls())

library(dplyr)
library(data.table)
library(lubridate)
library(arctools)
options(scipen=999)

probs_vec <- c(0.9, 0.99, 0.999, 0.9999, 0.99999, 0.999999, 0.9999999)
get_vals_summary <- function(vec){
  out_quantiles <- quantile(vec, probs = probs_vec)
  out <- c(length(vec), out_quantiles,  max(vec))
  names(out) <- c("val_cnt", paste0("val_qt_", probs_vec), "val_max")
  return(out)
}

out_matrix <- matrix()


## AC --------------------------------------------------------------------------

# define vector of file paths to ActiGrpah activity counts (AC)
fpaths_ac = sort(list.files(path = paste0(here::here(), "/csv"), full.names = TRUE, pattern = "[.]csv"))

# read values of data frame 
d_ac_list <- lapply(fpaths_ac, fread, select = c("vectormagnitude"))
d_ac_list <- rbindlist(d_ac_list)
d_ac_val <- d_ac_list$vectormagnitude; rm(d_ac_list)
d_ac_val_summary <- get_vals_summary(d_ac_val); rm(d_ac_val)
d_ac_val_summary


## OSM --------------------------------------------------------------------------

# define vector of file paths to open source summary measures (OSM)
fpaths_osm = sort(list.files(path = paste0(here::here(), "/open_source_measures"), full.names = TRUE))

# read values of data frame 
d_osm_list <- lapply(fpaths_osm, readRDS)
d_osm_list <- do.call("rbind", d_osm_list)
names(d_osm_list)

d_mims_val_summary <- get_vals_summary(d_osm_list$MIMS_UNIT)
d_mims_val_summary

d_mad_val_summary <- get_vals_summary(d_osm_list$MAD)
d_mad_val_summary

d_enmo_val_summary <- get_vals_summary(d_osm_list$ENMO)
d_enmo_val_summary

d_ai_val_summary <- get_vals_summary(d_osm_list$AI)
d_ai_val_summary


## save summary ----------------------------------------------------------------

val_summary_all <- rbind(
  d_ac_val_summary,
  d_mims_val_summary,
  d_mad_val_summary,
  d_enmo_val_summary,
  d_ai_val_summary
) %>% as.data.frame()
rownames(val_summary_all) <- NULL
val_summary_all <- cbind(
  metric_name = c("AC", "MIMS", "MAD", "ENMO", "AI"),
  val_summary_all
)

fout_path <- paste0(here::here(), "/results/minute_level_metrics_summary.rds")
saveRDS(val_summary_all, fout_path)




