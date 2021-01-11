
#' @description 
#' This script compares filenames we have for two types of files: .mat and .csv.
#' This is to establish what is the 
#' (a) number of .mat raw accelerometry data files which do/do not have their CSV mapping file,
#' (b) number of .CSV minute-level activity counts files which do/do not have their .mat mapping file.

rm(list = ls())
library(dplyr)
library(readr)
library(data.table)

fnames_mat = sort(list.files(path = here::here("mats"), full.names = FALSE, pattern = "[.]mat"))
fnames_csv = sort(list.files(path = here::here("csv"), full.names = FALSE))

head(fnames_mat)
head(fnames_csv)

fnames_mat0 <- sapply(fnames_mat, function(fname) gsub(pattern = "RAW.mat", replacement = "", fname))
fnames_csv0 <- sapply(fnames_csv, function(fname) gsub(pattern = "60sec.csv.gz", replacement = "", fname))

fnames_mat0_df <- data.frame(fnames = fnames_mat0, exists_mat = 1, stringsAsFactors = FALSE)
fnames_csv0_df <- data.frame(fnames = fnames_csv0, exists_csv = 1, stringsAsFactors = FALSE)
fnames_comb <- fnames_mat0_df %>% full_join(fnames_csv0_df, by = "fnames") %>% arrange(fnames)
fnames_comb[is.na(fnames_comb)] <- 0
fnames_comb <- fnames_comb %>% mutate(exist_both = exists_mat * exists_csv)

fnames_comb_sub1 <- fnames_comb %>% filter(exist_both == 1)
fnames_comb_sub0 <- fnames_comb %>% filter(exist_both == 0)

dim(fnames_comb)
# [1] 1317    4

dim(fnames_comb_sub1)
# [1] 1131    4

dim(fnames_comb_sub0)
# [1] 186   4

fnames_comb %>% select(exists_mat, exists_csv) %>% summarise_all(sum)
#   exists_mat exists_csv
# 1       1242       1206

fnames_comb_sub0 %>% select(exists_mat, exists_csv) %>% summarise_all(sum)
#   exists_mat exists_csv
# 1        111         75

# save summary to file 
fwrite(as.data.table(fnames_comb_sub0), here::here("results", "2021-01-11-exists_mat_csv_summary.csv"))

