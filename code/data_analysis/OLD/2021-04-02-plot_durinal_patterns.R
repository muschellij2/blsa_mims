
#' Notes: 
#' 
#' cd $mims
#' cd code/data_analysis
#' Rnosave 2021-04-01-get_measures_mapping_FITTED_qgam_CL.R -l mem_free=50G,h_vmem=50G,h_stack=256M -t 1-4 -N JOB_qgam
#' 
#' (A) Smoothed 24-hour median activity counts per minute for each age group. 
#' (B) Smoothed medians of 24-hour cumulative activity counts per day for each age group. 
#' Groups: <60-year old (green), 60- to 67-year old (red), 68- to 74-year old (blue), ≥75-year old (orange).


rm(list = ls())
library(tidyverse)
library(cowplot)
library(ggsci)
library(lubridate)
library(scales)
library(mgcv)
options(scipen=999)

names_levels <- c("MIMS", "ENMO", "MAD", "AI")
names_colors <- pal_futurama()(4)

# plot colors
age_cat_colors <- c("darkgreen", "red", "blue", "orange")
name_levels <- c("AC_hat_from_MIMS", "AC_hat_from_ENMO", "AC_hat_from_MAD", "AC_hat_from_AI")

theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=12))}
theme_set(theme_ggpr())


# ------------------------------------------------------------------------------
# read data 

## read mastervisit
mastervisit_fpath <- paste0(here::here(), "/covariates/2021-01-19-mastervisit.rdata")
mastervisit <- get(load(mastervisit_fpath, ex <- new.env()), envir = ex) %>% 
  select(age = Age, subj_id = IDNo, visit_id = Visit) %>%
  distinct()
dim(mastervisit)

# read acc
fpath_tmp <- paste0(here::here(), "/data_processed/2021-04-01-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc0 <- readRDS(fpath_tmp) %>% as.data.frame()
dat_acc <- dat_acc0 %>% 
  left_join(mastervisit, by = c("subj_id", "visit_id"))
# add minute index, label
dat_acc$minute_idx   <- rep(1 : 1440, (nrow(dat_acc) / 1440))
dat_acc$minute_label <- lubridate::ymd_hms("2013-01-01 00:00:00") + lubridate::minutes(dat_acc$minute_idx - 1)
# add age categorized
dat_acc$age_cat1  <- gtools::quantcut(dat_acc$age, 4)
cut_breaks = c(min(dat_acc$age), 59, 67, 74, max(dat_acc$age))
dat_acc$age_cat2 <- cut(dat_acc$age, breaks = cut_breaks, include.lowest = TRUE)
# look up 
dat_acc %>% group_by(age_cat1) %>% summarise(subj_id_cnt = n_distinct(subj_id))
dat_acc %>% group_by(age_cat2) %>% summarise(subj_id_cnt = n_distinct(subj_id))


# ------------------------------------------------------------------------------
# plot 1: smoothed mean 

# aggregate 
# aggregate 
W <- 60 + 30
filter_def <- seq(0, 1, length.out = W)
filter_def <- sqrt((1/2 - abs(1/2 - filter_def)))
filter_def <- filter_def / sum(filter_def)
# plot(filter_def)
# filter_def <- rep(1, W)/W

plt_df <- 
  dat_acc %>% 
  select(AC, AC_hat_from_MIMS, AC_hat_from_ENMO, AC_hat_from_MAD, AC_hat_from_AI, 
         age_cat = age_cat1, minute_idx, minute_label) %>%
  group_by(age_cat, minute_idx, minute_label) %>%
  summarize_all(mean) %>%
  group_by(age_cat) %>%
  arrange(age_cat, minute_idx, minute_label) %>%
  mutate(across(all_of(c("AC", name_levels)), function(x) stats::filter(x, filter_def, circular = TRUE))) %>%
  ungroup() 

plt_df_L <- 
  plt_df %>%
  pivot_longer(cols = -c(age_cat, minute_idx, minute_label, AC)) %>%
  mutate(name = factor(name, levels = name_levels))

plt_list <- list()
for (i in 1 : length(name_levels)){ # i <- 1
  name_i  <- name_levels[i]
  plt_df_L_i    <- plt_df_L %>% filter(name == name_i)
  plt_i <- 
    ggplot(plt_df_L_i, aes(x = minute_label, y = AC, color = age_cat, group = age_cat)) + 
    geom_line(size = 3, alpha = 0.2) + 
    scale_color_manual(values = age_cat_colors) + 
    scale_y_continuous(limits = c(0, 3000)) +
    scale_x_datetime(labels = date_format("%H:%M", tz = "UTC")) + 
    labs(x = "Time", y = "AC per minute", title = name_i) + 
    theme(legend.position = "none") + 
    geom_line(data = plt_df_L_i, aes(x = minute_label, y = value, color = age_cat, group = age_cat))
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-02-replicate_physicalcliff_mean_smoothed.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6) 



# ------------------------------------------------------------------------------
# plot 2: smoothed median 

# aggregate 
W <- 60 + 30
filter_def <- seq(0, 1, length.out = W)
filter_def <- sqrt((1/2 - abs(1/2 - filter_def)))
filter_def <- filter_def / sum(filter_def)
# plot(filter_def)
# filter_def <- rep(1, W)/W

plt_df <- 
  dat_acc %>% 
  select(AC, AC_hat_from_MIMS, AC_hat_from_ENMO, AC_hat_from_MAD, AC_hat_from_AI, 
         age_cat = age_cat1, minute_idx, minute_label) %>%
  group_by(age_cat, minute_idx, minute_label) %>%
  summarize_all(median) %>%
  group_by(age_cat) %>%
  arrange(age_cat, minute_idx, minute_label) %>%
  mutate(across(all_of(c("AC", name_levels)), function(x) stats::filter(x, filter_def, circular = TRUE))) %>%
  ungroup() 

plt_df_L <- 
  plt_df %>%
  pivot_longer(cols = -c(age_cat, minute_idx, minute_label, AC)) %>%
  mutate(name = factor(name, levels = name_levels))

plt_list <- list()
for (i in 1 : length(name_levels)){ # i <- 1
  name_i  <- name_levels[i]
  plt_df_L_i    <- plt_df_L %>% filter(name == name_i)
  plt_i <- 
    ggplot(plt_df_L_i, aes(x = minute_label, y = AC, color = age_cat, group = age_cat)) + 
    geom_line(size = 3, alpha = 0.2) + 
    scale_color_manual(values = age_cat_colors) + 
    scale_y_continuous(limits = c(0, 2500)) +
    scale_x_datetime(labels = date_format("%H:%M", tz = "UTC")) + 
    labs(x = "Time", y = paste0("Thick: ActiGraph AC per minute"),
         title  = paste0("\nThin: ", name_i)) + 
    theme(legend.position = "none") + 
    geom_line(data = plt_df_L_i, aes(x = minute_label, y = value, color = age_cat, group = age_cat))
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-02-replicate_physicalcliff_median_smoothed.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6) 



# ------------------------------------------------------------------------------
# plot 3: mean smoothed via GAM 

k <- 20

plt_df_L0 <- 
  dat_acc %>% 
  select(AC, AC_hat_from_MIMS, AC_hat_from_ENMO, AC_hat_from_MAD, AC_hat_from_AI, 
         age_cat = age_cat1, minute_idx, minute_label) %>%
  group_by(age_cat, minute_idx, minute_label) %>%
  pivot_longer(cols = -c(age_cat, minute_idx, minute_label)) 

newdata_template <- dat_acc %>% select(minute_idx, minute_label) %>% distinct()
plt_df_L1 <- data.frame()
age_cat_vec <- sort(unique(plt_df_L0$age_cat))
name_vec    <- sort(unique(plt_df_L0$name))
for (age_cat_i in age_cat_vec){ # age_cat_i <- age_cat_vec[1]; name_i <- name_vec[1]
  for (name_i in name_vec){
    message(paste0("age_cat_i: ", age_cat_i, ", name_i: ", name_i))
    fit_i_df <- plt_df_L0 %>% filter(age_cat == age_cat_i, name == name_i)
    fit_i <- gam(value ~ s(minute_idx, k = k, bs = "cr"), data = fit_i_df)
    newdata_i <- newdata_template
    newdata_i$value <- predict(fit_i, newdata_i)
    newdata_i$name <- name_i
    newdata_i$age_cat <- age_cat_i
    plt_df_L1 <- rbind(plt_df_L1, newdata_i); rm(newdata_i)
  }
}

plt_df_L <- 
  plt_df_L1 %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  pivot_longer(cols = -c(minute_idx, minute_label, age_cat, AC))

plt_list <- list()
for (i in 1 : length(name_levels)){ # i <- 1
  name_i  <- name_levels[i]
  plt_df_L_i    <- plt_df_L %>% filter(name == name_i)
  plt_i <- 
    ggplot(plt_df_L_i, aes(x = minute_label, y = AC, color = age_cat, group = age_cat)) + 
    geom_line() + 
    scale_color_manual(values = age_cat_colors) + 
    scale_y_continuous(limits = c(0, 3000)) +
    scale_x_datetime(labels = date_format("%H:%M", tz = "UTC")) + 
    labs(x = "Time", y = "AC per minute", title = name_i) + 
    theme(legend.position = "none")
  # geom_line(data = plt_df_1b, aes(x = minute_idx, y = value, color = age_cat, group = age_cat))
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

