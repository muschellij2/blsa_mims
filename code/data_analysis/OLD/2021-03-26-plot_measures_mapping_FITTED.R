
#' Notes: 
#' 
#' cd $mims
#' cd code/data_analysis
#' Rnosave XXX -l mem_free=20G,h_vmem=20G,h_stack=256M -t 1-12 -N JOB_gam_boot

rm(list = ls())
library(tidyverse)
library(ggsci)
library(gridExtra)
options(scipen=999)


theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank()  )}
var_names <- c("MIMS", "ENMO", "MAD", "AI")
var_colors <- c(pal_futurama()(4))
theme_set(theme_ggpr())

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath) 
dat_acc <- dat_acc %>% dplyr::filter(wear_and_valid_flag == 1)
dat_acc_sub <- dat_acc %>% filter(row_number() %% 100 == 0)
dim(dat_acc_sub)
dat_acc_sub_L <- 
  dat_acc_sub %>%
  select(AC, MIMS, MAD, ENMO, AI) %>%
  pivot_longer(cols = -c(AC)) %>%
  mutate(name_fct  = factor(name, levels = var_names[1:4])) %>%
  as.data.frame()
head(dat_acc_sub_L)

fpath_tmp <- paste0(here::here(), "/results/2021-03-26-mapping_fitted.rds")
newdata <- readRDS(fpath_tmp) %>%
  pivot_longer(cols = -c(AC)) %>%
  mutate(name = gsub("_fitted", "", name)) %>%
  mutate(name_fct  = factor(name, levels = var_names[1:4])) %>%
  as.data.frame()
head(newdata)

ggplot(dat_acc_sub_L, aes(x = AC, y = value, color = name_fct)) + 
  geom_point(alpha = 0.1, size = 0.2) + 
  facet_wrap(~name_fct, scales = "free_y") + 
  scale_color_manual(values = var_colors) + 
  geom_line(data = newdata, aes(x = AC, y = value, group = 1), color = "black")
  










