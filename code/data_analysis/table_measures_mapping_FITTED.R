
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

tbl_tmp <- dat_fitted %>% filter(AC %in% c(1853, 2860, 3940))

tbl_tmp %>% round(5)
tbl_tmp %>% round(3)

tbl_tmp <- tbl_tmp %>% round(3)
tbl_tmp

# View(tbl_tmp)
stargazer::stargazer(tbl_tmp, summary = FALSE)
