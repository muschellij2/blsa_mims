
rm(list = ls())
library(tidyverse)

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-01-19-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath)
names(dat_acc)
dim(dat_acc)

file_id_vec <- unique(dat_acc$file_id)



which_idx <- sapply(file_id_vec, function(x) startsWith(x, "1128"))
file_id_vec[which_idx]

mat_fnames <- list.files("/Users/martakaras/OneDrive - Johns Hopkins/BLSA/mat")
which_idx <- sapply(mat_fnames, function(x) startsWith(x, "1128"))
mat_fnames[which_idx]
