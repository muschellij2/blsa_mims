
#' Script to generate mapping between minute-level activity measure
#' 
#' Note: 
#' - To get needed files available locally, use
#'   get /dcl01/smart/data/activity/blsa_mims/data_processed/2021-01-19-measures_masterfile_winsorized.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/data_processed/2021-01-19-measures_masterfile_winsorized.rds
#'   
#' - Reference: 
#'   https://johnmuschelli.com/upper_limb_gt3x_prosthesis/thresholds.html
   

rm(list = ls())
library(tidyverse)
library(mgcv)
library(ggsci) # https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html
library(cowplot)

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-01-19-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath)
names(dat_acc)
dim(dat_acc)
length(unique(dat_acc$file_id))
length(unique(dat_acc$subj_id))


## fit and predict -------------------------------------------------------------

# MIMS ~ AC
mod <- gam(MIMS ~ s(AC, bs = "cr"), data = dat_acc)
mod_MIMS_AC_df      <- data.frame(AC = 0 : round(max(dat_acc$AC)))
mod_MIMS_AC_df$MIMS <- predict(mod, newdata = mod_MIMS_AC_df)

# ENMO ~ AC
mod <- gam(ENMO ~ s(AC, bs = "cr"), data = dat_acc)
mod_ENMO_AC_df      <- data.frame(AC = 0 : round(max(dat_acc$AC)))
mod_ENMO_AC_df$ENMO <- predict(mod, newdata = mod_ENMO_AC_df)

# MAD ~ AC 
mod <- gam(MAD ~ s(AC, bs = "cr"), data = dat_acc)
mod_MAD_AC_df      <- data.frame(AC = 0 : round(max(dat_acc$AC)))
mod_MAD_AC_df$MAD <- predict(mod, newdata = mod_MAD_AC_df)

# AI ~ AC 
mod <- gam(AI ~ s(AC, bs = "cr"), data = dat_acc)
mod_AI_AC_df      <- data.frame(AC = 0 : round(max(dat_acc$AC)))
mod_AI_AC_df$AI <- predict(mod, newdata = mod_AI_AC_df)


## generate plot ---------------------------------------------------------------

# define subset of data for the plotting purpose
k <- 100
dat_acc_sub <- dat_acc[seq(1, nrow(dat_acc), by = k), ]
dim(dat_acc_sub)

# other plotting settings
# https://rpubs.com/mclaire19/ggplot2-custom-themes
theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_bw(base_size = 14) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank()    
    )
}
theme_set(theme_ggpr())

# predefine range of x-axis corresponding to AC 
# x_lim <- c(0, round(max(dat_acc$AC)))
x_lim <- c(0, 30000)

# MIMS ~ AC
y_lim <- c(0, mod_MIMS_AC_df %>% filter(AC == x_lim[2]) %>% pull(MIMS))
plt_MIMS_AC <- 
  ggplot(dat_acc_sub, aes(x = AC, y = MIMS)) + 
  geom_point(alpha = 0.1, size = 0.5) + 
  geom_line(data = mod_MIMS_AC_df, aes(x = AC, y = MIMS), color = pal_ucscgb()(1)) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_MIMS_AC

# ENMO ~ AC
y_lim <- c(0, mod_ENMO_AC_df %>% filter(AC == x_lim[2]) %>% pull(ENMO))
plt_ENMO_AC <- 
  ggplot(dat_acc_sub, aes(x = AC, y = ENMO)) + 
  geom_point(alpha = 0.1, size = 0.5) + 
  geom_line(data = mod_ENMO_AC_df, aes(x = AC, y = ENMO), color = pal_ucscgb()(1)) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_ENMO_AC

# MAD ~ AC
y_lim <- c(0, mod_MAD_AC_df %>% filter(AC == x_lim[2]) %>% pull(MAD))
plt_MAD_AC <- 
  ggplot(dat_acc_sub, aes(x = AC, y = MAD)) + 
  geom_point(alpha = 0.1, size = 0.5) + 
  geom_line(data = mod_MAD_AC_df, aes(x = AC, y = MAD), color = pal_ucscgb()(1)) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_MAD_AC

# AI ~ AC
y_lim <- c(0, mod_AI_AC_df %>% filter(AC == x_lim[2]) %>% pull(AI))
plt_AI_AC <- 
  ggplot(dat_acc_sub, aes(x = AC, y = AI)) + 
  geom_point(alpha = 0.1, size = 0.5) + 
  geom_line(data = mod_AI_AC_df, aes(x = AC, y = AI), color = pal_ucscgb()(1)) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_AI_AC


# combined plot
plt_list <- list(plt_MIMS_AC, plt_ENMO_AC, plt_MAD_AC, plt_AI_AC)
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v")

plt_fpath <- paste0(here::here(), "/results_figures/measures_mapping.png")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 10)


## generate table with selected values -----------------------------------------

AC_vec <- c(0, 100, 1853, 1952, 2690)

tbl_out <- data.frame(
  AC = AC_vec,
  MIMS = mod_MIMS_AC_df %>% filter(AC %in% AC_vec) %>% pull(MIMS),
  ENMO = mod_ENMO_AC_df %>% filter(AC %in% AC_vec) %>% pull(ENMO),
  MAD  = mod_MAD_AC_df %>% filter(AC %in% AC_vec) %>% pull(MAD),
  AI = mod_AI_AC_df %>% filter(AC %in% AC_vec) %>% pull(AI)
) %>% round(3)

View(tbl_out)



