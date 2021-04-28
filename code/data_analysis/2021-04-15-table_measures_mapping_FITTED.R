
rm(list = ls())
library(data.table)
library(tidyverse)
library(cowplot)
library(ggsci)
options(scipen=999)

names_levels <- c("MIMS", "ENMO", "MAD", "AI")

# ------------------------------------------------------------------------------
# read data 

source(paste0(here::here(), "/code/data_preprocessing/measures_mapping_FUNC.R"))

fpath_tmp <- paste0(here::here(), "/results_public/mapping_between_measures_FITTED.txt")
dat_fitted <- fread(fpath_tmp) %>% as.data.frame()
dim(dat_fitted)


# ------------------------------------------------------------------------------

tbl_tmp <- rbind(
  apply(dat_fitted[1:2, ], 2, mean),
  dat_fitted[1853, ],
  dat_fitted[2303, ]
)
tbl_tmp %>% round(5)
tbl_tmp %>% round(3)

tbl_tmp <- tbl_tmp %>% round(5)
tbl_tmp

View(tbl_tmp)
