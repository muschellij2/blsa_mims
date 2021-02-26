
#' @description 
#' This script compares filenames we have for types of files: 
#' - .mat (raw subsecond level accelerometry data)
#' - .rds (minute-level open-source aggregates) 
#' - .csv (minute-level Actigraph AC) 
#' 
#' Notes: 
#' cd /dcl01/smart/data/activity/blsa_mims

rm(list = ls())
library(tidyverse)
library(readr)

# pull file names
fnames_mat = sort(list.files(path = here::here("mats"), full.names = FALSE, pattern = "[.]mat"))
fnames_rds = sort(list.files(path = here::here("open_source_measures"), full.names = FALSE, pattern = "[.]rds"))
fnames_csv = sort(list.files(path = here::here("csv"), full.names = FALSE))

lapply(list(fnames_mat, fnames_rds, fnames_csv), length)
lapply(list(fnames_mat, fnames_rds, fnames_csv), head)

fnames_mat0 <- sapply(fnames_mat, function(fname) gsub(pattern = "RAW.mat", replacement = "", fname))
fnames_rds0 <- sapply(fnames_rds, function(fname) gsub(pattern = "_OSM.rds", replacement = "", fname))
fnames_csv0 <- sapply(fnames_csv, function(fname) gsub(pattern = "60sec.csv", replacement = "", fname))

fnames_mat0_df <- data.frame(fnames = fnames_mat0, exists_mat = 1, stringsAsFactors = FALSE)
fnames_rds0_df <- data.frame(fnames = fnames_rds0, exists_rds = 1, stringsAsFactors = FALSE)
fnames_csv0_df <- data.frame(fnames = fnames_csv0, exists_csv = 1, stringsAsFactors = FALSE)
fnames_comb <- 
  fnames_mat0_df %>% 
  full_join(fnames_rds0_df, by = "fnames") %>% 
  full_join(fnames_csv0_df, by = "fnames") %>% 
  arrange(fnames) %>%
  mutate(file_idx = row_number()) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(exist_all = exists_mat * exists_rds * exists_csv)

fnames_comb_sub1 <- fnames_comb %>% filter(exist_all == 1)
fnames_comb_sub0 <- fnames_comb %>% filter(exist_all == 0)

dim(fnames_comb)

table(fnames_comb$exist_all)

fnames_comb_sub0
# <0 rows> (or 0-length row.names)



