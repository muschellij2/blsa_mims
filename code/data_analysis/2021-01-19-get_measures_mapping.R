
#' Script to generate mapping between minute-level activity measures
#' 
#' Note: 
#' - To get needed files available locally, use
#'   get /dcl01/smart/data/activity/blsa_mims/data_processed/2021-01-19-measures_masterfile_winsorized.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/data_processed/2021-01-19-measures_masterfile_winsorized.rds
#'   
#' - We use winsorized data to make the mapping 
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


##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
## GAM fit and predict 

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
  theme_bw(base_size = 16) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank()    
    )
}
theme_set(theme_ggpr())

# predefine range of x-axis corresponding to AC 
# x_lim <- c(0, round(max(dat_acc$AC)))
# x_lim <- c(0, 30000)
x_lim <- c(0, 20000)

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
plt

plt_fpath <- paste0(here::here(), "/results_figures/measures_mapping_GAM_fit.png")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 8)
rm(plt)



##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
## Sensitivity analysis of effect of measure threshold choice in estimating if 
##minute-level AC equals 0

sum(dat_acc$AC == 0)
mean(dat_acc$AC == 0)

summary(dat_acc$MIMS)
summary(dat_acc$MAD)
summary(dat_acc$ENMO)
summary(dat_acc$AI)

dat_acc_AC0 <- dat_acc %>% filter(AC == 0)

summary(dat_acc_AC0$MIMS)
summary(dat_acc_AC0$MAD)
summary(dat_acc_AC0$ENMO)
summary(dat_acc_AC0$AI)

measure_gird_max <- c(7.5, 0.1, 0.025, 2.2)
names(measure_gird_max) <- c("MIMS", "ENMO", "MAD", "AI")

# function to compute sensitivity, specificity, accuracy 
get_performance_estimating_AC_is0 <- function(AC_vec, S2_vec, S2_grid, S2_label){
  # define vector of 0/1 where 0: (AC_vec != 0), 1: (AC_vec == 0)
  # i.e. the event (AC == 0) is a "success"
  AC_is0  <- (AC_vec == 0) * 1
  AC_which_is0 <- which(AC_vec == 0)
  AC_which_isnot0 <- which(!(AC_vec == 0))
  S2_l <- length(S2_grid)
  out_df <- data.frame(measure_grid = S2_grid)
  out_df$out_sens = out_df$out_spec = out_df$out_accr = NA
  for (i in 1:S2_l){
    print(i)
    AC_est_is0   <- (S2_vec <= S2_grid[i]) * 1
    out_df$out_sens[i] <- mean(AC_est_is0[AC_which_is0])
    out_df$out_spec[i] <- 1 - mean(AC_est_is0[AC_which_isnot0])
    out_df$out_accr[i] <- mean(AC_is0 == AC_est_is0)
  }
  out_df$measure_name <- S2_label
  return(out_df)
}

length_out <- 1000
perf_df_MIMS <- get_performance_estimating_AC_is0(
  AC_vec = dat_acc$AC, 
  S2_vec = dat_acc$MIMS, 
  S2_grid = seq(0, measure_gird_max["MIMS"], length.out = length_out), 
  S2_label = "MIMS"
  )
perf_df_ENMO <- get_performance_estimating_AC_is0(
  AC_vec = dat_acc$AC, 
  S2_vec = dat_acc$ENMO, 
  S2_grid = seq(0, measure_gird_max["ENMO"], length.out = length_out), 
  S2_label = "ENMO"
)
perf_df_MAD <- get_performance_estimating_AC_is0(
  AC_vec = dat_acc$AC, 
  S2_vec = dat_acc$MAD, 
  S2_grid = seq(0, measure_gird_max["MAD"], length.out = length_out), 
  S2_label = "MAD"
)
perf_df_AI <- get_performance_estimating_AC_is0(
  AC_vec = dat_acc$AC, 
  S2_vec = dat_acc$AI, 
  S2_grid = seq(0, measure_gird_max["AI"], length.out = length_out), 
  S2_label = "AI"
)
perf_df_comb <- rbind(
  perf_df_MIMS,
  perf_df_ENMO,
  perf_df_MAD,
  perf_df_AI
)
# fpath_out <- paste0(here::here(), "/results/2021-01-20-performance_estimating_AC_is0.rds")
# saveRDS(object = perf_df_comb, file = fpath_out)

# read precomputed results 
fpath_out <- paste0(here::here(), "/results/2021-01-20-performance_estimating_AC_is0.rds")
perf_df_comb <- readRDS(fpath_out)
perf_df_comb_long <- perf_df_comb %>% 
  pivot_longer(cols = c(out_accr,  out_spec, out_sens)) %>%
  mutate(name = factor(name, 
                       levels = c("out_accr", "out_sens", "out_spec"),
                       labels = c("accuracy", "sensitivity", "specificity")))

# MIMS 
plt_df <- perf_df_comb_long %>% filter(measure_name == "MIMS")
plt_df_whichmax_MIMS <- plt_df %>% pivot_wider(names_from = "name") %>% 
  filter(accuracy == max(accuracy)) %>% 
  filter(sensitivity == max(sensitivity)) %>% 
  filter(specificity == max(specificity)) %>% 
  filter(measure_grid == min(measure_grid)) %>% 
  pull(measure_grid)
plt_MIMS <- 
  ggplot(plt_df, aes(x = measure_grid, y = value, color = name)) + 
  geom_vline(xintercept = plt_df_whichmax_MIMS, color = pal_futurama()(3)[1], linetype = 2, alpha = 0.99) + 
  geom_line() + 
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "none") + 
  labs(x = "MIMS threshold") + 
  scale_color_futurama()
plt_MIMS

# ENMO 
plt_df <- perf_df_comb_long %>% filter(measure_name == "ENMO")
plt_df_whichmax_ENMO <- plt_df %>% pivot_wider(names_from = "name") %>% 
  filter(accuracy == max(accuracy)) %>% 
  filter(sensitivity == max(sensitivity)) %>% 
  filter(specificity == max(specificity)) %>% 
  filter(measure_grid == min(measure_grid)) %>% 
  pull(measure_grid)
plt_ENMO <- 
  ggplot(plt_df, aes(x = measure_grid, y = value, color = name)) + 
  geom_vline(xintercept = plt_df_whichmax_ENMO, color = pal_futurama()(3)[1], linetype = 2, alpha = 0.99) + 
  geom_line() + 
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "none") + 
  labs(x = "ENMO threshold") + 
  scale_color_futurama()
plt_ENMO

# MAD 
plt_df <- perf_df_comb_long %>% filter(measure_name == "MAD")
plt_df_whichmax_MAD <- plt_df %>% pivot_wider(names_from = "name") %>% 
  filter(accuracy == max(accuracy)) %>% 
  filter(sensitivity == max(sensitivity)) %>% 
  filter(specificity == max(specificity)) %>% 
  filter(measure_grid == min(measure_grid)) %>% 
  pull(measure_grid)
plt_MAD <- 
  ggplot(plt_df, aes(x = measure_grid, y = value, color = name)) + 
  geom_vline(xintercept = plt_df_whichmax_MAD, color = pal_futurama()(3)[1], linetype = 2, alpha = 0.99) + 
  geom_line() + 
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "none") + 
  labs(x = "MAD threshold") + 
  scale_color_futurama()
plt_MAD

# AI 
plt_df <- perf_df_comb_long %>% filter(measure_name == "AI")
plt_df_whichmax_AI <- plt_df %>% pivot_wider(names_from = "name") %>% 
  filter(accuracy == max(accuracy)) %>% 
  filter(sensitivity == max(sensitivity)) %>% 
  filter(specificity == max(specificity)) %>% 
  filter(measure_grid == min(measure_grid)) %>% 
  pull(measure_grid)
plt_AI <- 
  ggplot(plt_df, aes(x = measure_grid, y = value, color = name)) + 
  geom_line() + 
  geom_vline(xintercept = plt_df_whichmax_AI, color = pal_futurama()(3)[1], linetype = 2, alpha = 0.99) + 
  scale_y_continuous(limits = c(0, 1)) + 
  theme(
    legend.position = c(0.72, 0.24),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) + 
  labs(x = "AI threshold") + 
  scale_color_futurama() + 
  labs(color = "Estimating AC == 0")
plt_AI


# combined plot
plt_list <- list(plt_MIMS, plt_ENMO, plt_MAD, plt_AI)
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v")
plt

plt_fpath <- paste0(here::here(), "/results_figures/measures_mapping_performance_estimating_AC_is0.png")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 8)



##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
#' Combine GAM and above AC == 0 estimation

AC_is0_thresh <- c(
  0,
  plt_df_whichmax_MIMS,
  plt_df_whichmax_ENMO,
  plt_df_whichmax_MAD,
  plt_df_whichmax_AI
)
names(AC_is0_thresh) <- c("AC", "MIMS", "ENMO", "MAD", "AI")
AC_is0_thresh

AC_vec <- c(0, 1, 2, 10, 100, 1852, 1853, 1854, 1952, 2690)

tbl_out <- data.frame(
  AC = AC_vec,
  MIMS = mod_MIMS_AC_df %>% filter(AC %in% AC_vec) %>% pull(MIMS),
  ENMO = mod_ENMO_AC_df %>% filter(AC %in% AC_vec) %>% pull(ENMO),
  MAD  = mod_MAD_AC_df %>% filter(AC %in% AC_vec) %>% pull(MAD),
  AI = mod_AI_AC_df %>% filter(AC %in% AC_vec) %>% pull(AI)
) 

tbl_out
