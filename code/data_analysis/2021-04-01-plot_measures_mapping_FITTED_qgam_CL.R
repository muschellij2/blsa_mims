
#' Notes: 
#' 
#' cd $mims
#' cd code/data_analysis
#' Rnosave 2021-04-01-get_measures_mapping_FITTED_qgam_CL.R -l mem_free=50G,h_vmem=50G,h_stack=256M -t 1-4 -N JOB_qgam

rm(list = ls())
library(tidyverse)
library(cowplot)
library(ggsci)
options(scipen=999)

# get /dcl01/smart/data/activity/blsa_mims/results/mapping_between_measures_FITTED_qgam_1.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/mapping_between_measures_FITTED_qgam_1.rds
# get /dcl01/smart/data/activity/blsa_mims/results/mapping_between_measures_FITTED_qgam_2.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/mapping_between_measures_FITTED_qgam_2.rds
# get /dcl01/smart/data/activity/blsa_mims/results/mapping_between_measures_FITTED_qgam_3.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/mapping_between_measures_FITTED_qgam_3.rds
# get /dcl01/smart/data/activity/blsa_mims/results/mapping_between_measures_FITTED_qgam_4.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/mapping_between_measures_FITTED_qgam_4.rds


names_levels <- c("MIMS", "ENMO", "MAD", "AI")
names_colors <- pal_futurama()(4)

# ------------------------------------------------------------------------------
# read data 

fpaths <- list.files(paste0(here::here(), "/results"), full.names = TRUE)
fpaths <- fpaths[grepl("mapping_between_measures_FITTED_qgam", fpaths)]
dat_list <- lapply(fpaths, readRDS)
dat_fitted <- 
  dat_list[[1]] %>% 
  left_join(dat_list[[2]], by = "AC") %>% 
  left_join(dat_list[[3]], by = "AC")

fpath_tmp <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(fpath_tmp) %>% as.data.frame()
dim(dat_acc)


# ------------------------------------------------------------------------------
# plot 1: main manuscript part

# AC_max <- max(dat_acc$AC)
AC_max <- 10000

# define plot data 
dat_acc_plt <- 
  dat_acc %>%
  filter(AC < AC_max) %>%
  filter(row_number() %% 500 == 0) %>%
  filter(AC > 0) %>%
  select(AC, MIMS, ENMO, MAD, AI) %>%
  pivot_longer(cols = -AC) %>%
  mutate(name_fct = factor(name, levels = names_levels))
head(dat_acc_plt)
dim(dat_acc_plt)

dat_fitted_plt <- 
  dat_fitted %>% 
  filter(AC < AC_max) %>%
  # select(AC, MIMS = MIMS_fitted, MAD = MAD_fitted, ENMO = ENMO_fitted, AI = AI_fitted) %>%
  select(AC, MAD = MAD_fitted, ENMO = ENMO_fitted, AI = AI_fitted) %>%
  pivot_longer(cols = -AC) %>%
  mutate(name_fct = factor(name, levels = names_levels))
head(dat_fitted_plt)
dim(dat_fitted_plt)

plt_list <- list()
for (i in 2 : length(names_levels)){ # i <- 1
  name_tmp  <- names_levels[i]
  color_tmp <- names_colors[i]
  dat_acc_plt_i    <- dat_acc_plt %>% filter(name_fct == name_tmp)
  dat_fitted_plt_i <- dat_fitted_plt %>% filter(name_fct == name_tmp)
  plt <- 
    ggplot(dat_acc_plt_i, aes(x = AC, y = value)) + 
    geom_point(size = 0.1, alpha = 0.1, color = color_tmp) + 
    geom_line(data = dat_fitted_plt_i, aes(x = AC, y = value, group = 1), inherit.aes = FALSE) + 
    labs(x = "ActiGraph AC", y = paste0(name_tmp, " fitted"))
  plt_list[[length(plt_list) + 1]] <- plt
}

plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

