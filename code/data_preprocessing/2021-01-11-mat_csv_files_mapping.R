
#' @description 
#' This script compares filenames we have for two types of files: .mat and .csv

rm(list = ls())
library(dplyr)
library(readr)

fnames_mat = sort(list.files(path = here::here("mats"), full.names = FALSE, pattern = "[.]mat"))
fnames_csv = sort(list.files(path = here::here("csv"), full.names = FALSE))

head(fnames_mat)
head(fnames_csv)

fnames_mat0 <- sapply(fnames_mat, function(fname) gsub(pattern = "RAW.mat", replacement = "", fname))
# fnames_csv0 <- sapply(fnames_csv, function(fname) gsub(pattern = "60sec.csv.gz", replacement = "", fname))
fnames_csv0 <- sapply(fnames_csv, function(fname) gsub(pattern = "60sec.csv", replacement = "", fname))

fnames_mat0_df <- data.frame(fnames = fnames_mat0, exists_mat = 1, stringsAsFactors = FALSE)
fnames_csv0_df <- data.frame(fnames = fnames_csv0, exists_csv = 1, stringsAsFactors = FALSE)
fnames_comb <- fnames_mat0_df %>% full_join(fnames_csv0_df, by = "fnames") %>% arrange(fnames)
fnames_comb[is.na(fnames_comb)] <- 0
fnames_comb <- fnames_comb %>% mutate(exist_both = exists_mat * exists_csv)

fnames_comb_sub1 <- fnames_comb %>% filter(exist_both == 1)
fnames_comb_sub0 <- fnames_comb %>% filter(exist_both == 0)

dim(fnames_comb)
# [1] 1242    4

table(fnames_comb$exist_both)
# 1 
# 1242 
