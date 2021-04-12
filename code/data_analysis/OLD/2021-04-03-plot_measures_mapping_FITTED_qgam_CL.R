
rm(list = ls())
library(tidyverse)
library(cowplot)
library(ggsci)
options(scipen=999)

# 2021-04-03
# get /dcl01/smart/data/activity/blsa_mims/results/2021-04-03-mapping_between_measures_FITTED_qgam_1.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/2021-04-03-mapping_between_measures_FITTED_qgam_1.rds
# get /dcl01/smart/data/activity/blsa_mims/results/2021-04-03-mapping_between_measures_FITTED_qgam_2.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/2021-04-03-mapping_between_measures_FITTED_qgam_2.rds
# get /dcl01/smart/data/activity/blsa_mims/results/2021-04-03-mapping_between_measures_FITTED_qgam_3.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/2021-04-03-mapping_between_measures_FITTED_qgam_3.rds
# get /dcl01/smart/data/activity/blsa_mims/results/2021-04-03-mapping_between_measures_FITTED_qgam_4.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/results/2021-04-03-mapping_between_measures_FITTED_qgam_4.rds

names_levels <- c("MIMS", "ENMO", "MAD", "AI")
names_colors <- pal_futurama()(4)

theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=12))}
theme_set(theme_ggpr())


# ------------------------------------------------------------------------------
# read data 

fpaths <- list.files(paste0(here::here(), "/results"), full.names = TRUE)
fpaths <- fpaths[grepl("2021-04-03-mapping_between_measures_FITTED_qgam", fpaths)]
fpaths

dat_list <- lapply(fpaths, readRDS)
dat_fitted <- 
  dat_list[[1]] %>% 
  left_join(dat_list[[2]], by = "AC") %>% 
  left_join(dat_list[[3]], by = "AC") %>% 
  left_join(dat_list[[4]], by = "AC") 
head(dat_fitted, 11) %>% round(5)

# AC MIMS_fitted ENMO_fitted MAD_fitted AI_fitted
# 1   0     0.10142     0.00564    0.00492   0.34182
# 2   1     0.10804     0.00565    0.00494   0.34358
# 3   2     0.11465     0.00565    0.00495   0.34534
# 4   3     0.12127     0.00566    0.00497   0.34710
# 5   4     0.12788     0.00567    0.00499   0.34885
# 6   5     0.13450     0.00568    0.00501   0.35061
# 7   6     0.14111     0.00568    0.00503   0.35237
# 8   7     0.14773     0.00569    0.00505   0.35413
# 9   8     0.15434     0.00570    0.00507   0.35589
# 10  9     0.16095     0.00570    0.00509   0.35764
# 11 10     0.16757     0.00571    0.00510   0.35940

fpath_tmp <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(fpath_tmp) %>% as.data.frame()
dim(dat_acc) 


# ------------------------------------------------------------------------------
# plot 1: main manuscript part

# AC_max <- max(dat_acc$AC)
AC_max <- 15000

# define plot data 
dat_acc_plt <- 
  dat_acc %>%
  filter(AC < AC_max) %>%
  filter(row_number() %% 300 == 0) %>%
  filter(AC > 0) %>%
  select(AC, MIMS, ENMO, MAD, AI) %>%
  pivot_longer(cols = -AC) %>%
  mutate(name_fct = factor(name, levels = names_levels))
head(dat_acc_plt)
dim(dat_acc_plt)

dat_fitted_plt <- 
  dat_fitted %>% 
  filter(AC < AC_max) %>%
  select(AC, MIMS = MIMS_fitted, MAD = MAD_fitted, ENMO = ENMO_fitted, AI = AI_fitted) %>%
  # select(AC, MIMS = MIMS_fitted, ENMO = ENMO_fitted, AI = AI_fitted) %>%
  pivot_longer(cols = -AC) %>%
  mutate(name_fct = factor(name, levels = names_levels))
head(dat_fitted_plt)
dim(dat_fitted_plt)

plt_list <- list()
for (i in 1 : length(names_levels)){ # i <- 3
  name_tmp  <- names_levels[i]
  color_tmp <- names_colors[i]
  dat_acc_plt_i    <- dat_acc_plt %>% filter(name_fct == name_tmp)
  dat_fitted_plt_i <- dat_fitted_plt %>% filter(name_fct == name_tmp)
  plt <- 
    ggplot(dat_acc_plt_i, aes(x = AC, y = value)) + 
    geom_point(size = 0.1, alpha = 0.1, color = color_tmp) + 
    labs(x = "ActiGraph AC", y = paste0(name_tmp, " fitted")) + 
    geom_line(data = dat_fitted_plt_i, aes(x = AC, y = value, group = 1), inherit.aes = FALSE)  
  plt_list[[length(plt_list) + 1]] <- plt
}

plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-03-measures_mapping_fitted_qgam.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8) 


