
#' @description 
#' This script compares filenames we have for types of files: 
#' - .mat (raw subsecond level accelerometry data)
#' - .rds (minute-level open-source aggregates) 
#' - .rds (raw data quality check flags for minute-level data) 
#' - .csv (minute-level Actigraph AC) 

rm(list = ls())
library(tidyverse)
library(readr)

# pull file names
fnames_mat       = sort(list.files(path = here::here("mats"), full.names = FALSE, pattern = "[.]mat"))
fnames_rds_OSM   = sort(list.files(path = here::here("open_source_measures"), full.names = FALSE, pattern = "[.]rds"))
fnames_rds_OSM_C = sort(list.files(path = here::here("open_measures"), full.names = FALSE, pattern = "_calibrated_OSM.rds"))
fnames_rds_QC    = sort(list.files(path = here::here("quality_flag"), full.names = FALSE, pattern = "[.]rds"))
fnames_csv       = sort(list.files(path = here::here("csv"), full.names = FALSE))

lapply(list(fnames_mat, fnames_rds_OSM, fnames_rds_OSM_C, fnames_rds_QC, fnames_csv), length)
lapply(list(fnames_mat, fnames_rds_OSM, fnames_rds_OSM_C, fnames_rds_QC, fnames_csv), head)

fnames_mat0       <- sapply(fnames_mat, function(fname) gsub(pattern = "RAW.mat", replacement = "", fname))
fnames_rds_OSM0   <- sapply(fnames_rds_OSM, function(fname) gsub(pattern = "_OSM.rds", replacement = "", fname))
fnames_rds_OSM_C0 <- sapply(fnames_rds_OSM_C, function(fname) gsub(pattern = "_calibrated_OSM.rds", replacement = "", fname))
fnames_rds_QC0    <- sapply(fnames_rds_QC, function(fname) gsub(pattern = "_quality_flag.rds", replacement = "", fname))
fnames_csv0       <- sapply(fnames_csv, function(fname) gsub(pattern = "60sec.csv", replacement = "", fname))

fnames_mat0_df       <- data.frame(fnames = fnames_mat0, exists_mat = 1, stringsAsFactors = FALSE)
fnames_rds_OSM0_df   <- data.frame(fnames = fnames_rds_OSM0, exists_rds_OSM = 1, stringsAsFactors = FALSE)
fnames_rds_OSM_C0_df <- data.frame(fnames = fnames_rds_OSM_C0, exists_rds_OSM_C = 1, stringsAsFactors = FALSE)
fnames_rds_QC0_df    <- data.frame(fnames = fnames_rds_QC0, exists_rds_QC = 1, stringsAsFactors = FALSE)
fnames_csv0_df       <- data.frame(fnames = fnames_csv0, exists_csv = 1, stringsAsFactors = FALSE)

fnames_comb <- 
  fnames_mat0_df %>% 
  full_join(fnames_rds_OSM0_df, by = "fnames") %>% 
  full_join(fnames_rds_OSM_C0_df, by = "fnames") %>% 
  full_join(fnames_rds_QC0_df, by = "fnames") %>% 
  full_join(fnames_csv0_df, by = "fnames") %>% 
  arrange(fnames) %>%
  mutate(file_idx = row_number()) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(exist_all = exists_mat * exists_rds_OSM * exists_rds_OSM_C * exists_rds_QC * exists_csv)
dim(fnames_comb)

fnames_comb_sub0 <- fnames_comb %>% filter(exist_all == 0)
fnames_comb_sub0


