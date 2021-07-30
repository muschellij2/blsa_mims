
#' Plot smoothed 24-hour median activity counts per minute for each age group. 
#' Summarize error between AC and measures mapped from AC. 
#' Groups: <60-year old (green), 60- to 67-year old (red), 68- to 74-year old (blue), ≥75-year old (orange).


rm(list = ls())
library(tidyverse)
library(cowplot)
library(ggsci)
library(lubridate)
library(scales)
library(latex2exp)
options(scipen=999)


# plot colors
names_levels1 <- c("AC", "MIMS", "ENMO", "MAD", "AI")
names_levels2 <- c("AC_hat_from_MIMS", "AC_hat_from_ENMO", "AC_hat_from_MAD", "AC_hat_from_AI")
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

fpath_tmp <- paste0(here::here(), "/results/2021-07-20-pa_daily_pattern_by_age.rds")
dat_acc_agg <- readRDS(fpath_tmp) %>% as.data.frame()
str(dat_acc_agg)

# smooth data 
W <- 60 + 60; W
filter_def <- seq(0, 1, length.out = W)
filter_def <- sqrt((1/2 - abs(1/2 - filter_def)))
filter_def <- filter_def / sum(filter_def)
# plot(filter_def)
# filter_def <- rep(1, W)/W

age_cat_levels <- levels(dat_acc_agg$age_cat)
age_cat_labels <- c("<60", "60-67", "68-74", "≥75")
age_cat_colors <- c("darkgreen", "red", "blue", "orange")

# ------------------------------------------------------------------------------
# plot 1: before mapping

# filter trajectories
dat_acc_agg_F <- 
  dat_acc_agg %>% 
  group_by(age_cat, fit_type) %>%
  arrange(age_cat, fit_type, minute_idx, minute_label) %>%
  mutate(across(all_of(c(names_levels1, names_levels2)), function(x) stats::filter(x, filter_def, circular = TRUE))) %>%
  ungroup() %>%
  as.data.frame()

head(dat_acc_agg)
head(dat_acc_agg_F)

plt_df <- 
  dat_acc_agg_F %>%
  pivot_longer(cols = -c(age_cat, minute_idx, minute_label, fit_type)) %>%
  mutate(age_cat_fact = factor(as.character(age_cat), levels = age_cat_levels, labels = age_cat_labels))
head(plt_df)

plt_list <- list()
for (i in 1 : length(names_levels1)){ # i <- 1
  name_i  <- names_levels1[i]
  plt_df_i    <- plt_df %>% filter(name == name_i)
  plt_i <- 
    ggplot(plt_df_i %>% filter(fit_type == "fitted"), 
           aes(x = minute_label, y = value, color = age_cat_fact, group = age_cat_fact)) + 
    geom_line(size = 1, alpha = 0.5) + 
    scale_color_manual(values = age_cat_colors) + 
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_datetime(labels = date_format("%H:%M", tz = "UTC")) + 
    # theme(legend.position = "none") + 
    labs(x = "Time", 
         y = paste0(name_i),
         title  = paste0(""),
         color = "Age:")  
  if (i == 1){
    plt_i <- plt_i + theme(legend.position = c(0.2, 0.7))
  } else {
    plt_i <- plt_i +  theme(legend.position = "none")
  }
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
  if (i == 1) plt_list[[length(plt_list) + 1]] <- (ggplot() + theme_void())
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-07-20-replicate_pa_daily_pattern_by_age_ORIG_", W, ".png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8) 



# ------------------------------------------------------------------------------
# plot 2: after mapping

# smooth data 
W <- 60 + 60; W
filter_def <- seq(0, 1, length.out = W)
filter_def <- sqrt((1/2 - abs(1/2 - filter_def)))
filter_def <- filter_def / sum(filter_def)

# filter trajectories
plt_df <- 
  dat_acc_agg %>% 
  group_by(age_cat, fit_type) %>%
  arrange(age_cat, fit_type, minute_idx, minute_label) %>%
  mutate(across(all_of(c(names_levels1, names_levels2)), function(x) stats::filter(x, filter_def, circular = TRUE))) %>%
  ungroup() %>%
  pivot_longer(cols = -c(age_cat, minute_idx, minute_label, fit_type)) %>%
  mutate(age_cat_fact = factor(as.character(age_cat), levels = age_cat_levels, labels = age_cat_labels)) %>%
  as.data.frame() 
head(plt_df)

y_lab <- c(
  "AC and $\\hat{AC}_{MIMS}$",
  "AC and $\\hat{AC}_{ENMO}$",
  "AC and $\\hat{AC}_{MAD}$",
  "AC and $\\hat{AC}_{AI}$"
)

y_lab <- c(
  "$\\hat{AC}_{MIMS}$",
  "$\\hat{AC}_{ENMO}$",
  "$\\hat{AC}_{MAD}$",
  "$\\hat{AC}_{AI}$"
)

plt_list <- list()
for (i in 1 : length(names_levels2)){ # i <- 1
  name_i  <- names_levels2[i]
  plt_i <- 
    ggplot(plt_df %>% filter(fit_type == "fitted", name == "AC"), 
           aes(x = minute_label, y = value, color = age_cat_fact, group = age_cat_fact)) + 
    geom_line(size = 1, alpha = 0.3) + 
    scale_color_manual(values = age_cat_colors) + 
    scale_y_continuous(limits = c(0, 2500)) +
    scale_x_datetime(labels = date_format("%H:%M", tz = "UTC")) + 
    labs(x = "Time", y = TeX(y_lab[i]), color = "Age:") + 
    geom_line(data = plt_df %>% filter(fit_type == "fitted", name == name_i), 
              aes(x = minute_label, y = value, 
                  color = age_cat_fact, group = age_cat_fact), 
              size = 1, linetype = "dashed") 
    if (i == 1){
    plt_i <- plt_i + theme(legend.position = c(0.2, 0.7))
  } else {
    plt_i <- plt_i +  theme(legend.position = "none")
  }
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-07-20-replicate_pa_daily_pattern_by_age_MAP_", W, ".png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6) 


# ------------------------------------------------------------------------------
# table: MAPE of differences

error_df <- 
  dat_acc_agg %>%
  select(age_cat, minute_idx, AC, starts_with("AC_hat")) %>%
  pivot_longer(cols = -c(age_cat, minute_idx, AC)) %>%
  mutate(APE0 = abs(AC - value)) %>%
  group_by(name) %>%
  summarise(MAPE0_num = sum(APE0),
            MAPE0_denom = sum(AC)) %>%
  mutate(MAPE0 = MAPE0_num / MAPE0_denom,
         MAPE0 = round(MAPE0 * 100, 1)) %>%
  ungroup() %>%
  as.data.frame()
head(error_df)








