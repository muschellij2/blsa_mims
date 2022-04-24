
rm(list = ls())
library(tidyverse)
library(ggsci)
library(cowplot)
library(latex2exp)
library(lubridate)
library(data.table)
options(scipen=999)


# ------------------------------------------------------------------------------
# read pre-computed data 

path_tmp <- paste0(here::here(), "/results/2021-05-06-astp_and_age_association.rds")
plt_df <- readRDS(path_tmp)
dim(plt_df)
head(plt_df)


# -----------------------------------------------------------------------------
# plotting utils

names_levels1 <- c("MIMS", "ENMO", "MAD", "AI")
names_colors <- pal_futurama()(4)

theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=12))}
theme_set(theme_ggpr())


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# PLOT 1: prop. active, ASTP, SATP

plt_list_all <- list()

# proportion active
plt_list <- list()
names_levels <- c("MIMS_active", "ENMO_active", "MAD_active", "AI_active")
for (i in 1 : length(names_levels)){ # i <- 1
  name_i     <- names_levels[i]
  plt_df_i   <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_lims_i <- range(c(plt_df_i$y, plt_df_i$AC_active))
  ylab_i     <- strsplit(name_i, split = "_")[[1]][1]
  color_i    <- names_colors[i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = AC_active, y = y)) + 
    geom_point(alpha = 0.15, size = 0.3, color = color_i) + 
    geom_abline(intercept = 0, slope = 1, size = 0.3) + 
    scale_x_continuous(limits = plt_lims_i) + 
    scale_y_continuous(limits = plt_lims_i) + 
    labs(y = ylab_i, 
         x = "AC", 
         title = "Perc. active")
  # plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
  plt_list_all[[length(plt_list_all) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 4, align = "v", byrow = TRUE)
plt


# ASTP
plt_list <- list()
names_levels <- c("MIMS_astp", "ENMO_astp", "MAD_astp", "AI_astp")
for (i in 1 : length(names_levels)){ # i <- 1
  name_i     <- names_levels[i]
  plt_df_i   <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_lims_i <- range(c(plt_df_i$y, plt_df_i$AC_astp))
  ylab_i     <- strsplit(name_i, split = "_")[[1]][1]
  color_i    <- names_colors[i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = AC_astp, y = y)) + 
    geom_point(alpha = 0.15, size = 0.3, color = color_i) + 
    geom_abline(intercept = 0, slope = 1, size = 0.3) + 
    scale_x_continuous(limits = plt_lims_i) + 
    scale_y_continuous(limits = plt_lims_i) + 
    labs(y = ylab_i, 
         x = "AC", 
         title = "ASTP")
  # plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
  plt_list_all[[length(plt_list_all) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 4, align = "v", byrow = TRUE)
plt


# SATP
plt_list <- list()
names_levels <- c("MIMS_satp", "ENMO_satp", "MAD_satp", "AI_satp")
for (i in 1 : length(names_levels)){ # i <- 1
  name_i     <- names_levels[i]
  plt_df_i   <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_lims_i <- range(c(plt_df_i$y, plt_df_i$AC_satp))
  ylab_i     <- strsplit(name_i, split = "_")[[1]][1]
  color_i    <- names_colors[i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = AC_satp, y = y)) + 
    geom_point(alpha = 0.15, size = 0.3, color = color_i) + 
    geom_abline(intercept = 0, slope = 1, size = 0.3) + 
    scale_x_continuous(limits = plt_lims_i) + 
    scale_y_continuous(limits = plt_lims_i) + 
    labs(y = ylab_i, 
         x = "AC", 
         title = "SATP")
  # plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
  plt_list_all[[length(plt_list_all) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 4, align = "v", byrow = TRUE)
plt


# one plot together
plt_all <- plot_grid(plotlist = plt_list_all, ncol = 4, align = "v", byrow = TRUE)
# plt_all

plt_path <- paste0(here::here(), "/results_figures/2021-05-06-activeprop_TP_comparison_across_metrics.png")
ggsave(filename = plt_path, plot = plt_all, width = 8, height = 6.2)



# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# PLOT 2: re-do plot ASTP ~ age 

names_levels <- c("MIMS_astp", "ENMO_astp", "MAD_astp", "AI_astp")
names_ylab_tex <- c(
  TeX("ASTP$_{MIMS}$"),
  TeX("ASTP$_{ENMO}$"),
  TeX("ASTP$_{MAD}$"),
  TeX("ASTP$_{AI}$")
)

plt_list <- list()
for (i in 1 : length(names_levels)){ # i <- 1
  name_i  <- names_levels[i]
  plt_df_i <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = age, y = y)) + 
    geom_point(alpha = 0.2, size = 0.5) + 
    labs(y = names_ylab_tex[i], x = "Age") + 
    geom_smooth(data = plt_df_i, aes(x = age, y = AC_astp), 
                method = 'loess', size = 0.5, color = "blue", se = FALSE) + 
    geom_smooth(method = 'loess', size = 0.5, color = "black") + 
    scale_y_continuous(limits = c(0.05, 0.5))
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}

plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-05-06-astp_and_age_association.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6.5)



# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# PLOT 3: example of individual with very different ASTP vs TP 

summary(plt_df$AC_astp - plt_df$MIMS_astp)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -0.019637  0.004162  0.008187  0.009075  0.012837  0.068560 

summary(plt_df$AC_active - plt_df$MIMS_active)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -4.12037 -0.35880  0.00000 -0.05798  0.32002  2.65108 

table(plt_df$imputed_cnt == 0)
# FALSE  TRUE 
# 138   517 


# -----------------------------------------------------------------------------
# find best participant

plt_df_sub <-
  plt_df %>%
  filter(imputed_cnt == 0) %>% 
  mutate(AC_MIMS_astp_diff = (AC_astp - MIMS_astp)) %>%
  mutate(AC_MIMS_active_diff = (AC_active - MIMS_active)) %>%
  filter(abs(AC_MIMS_active_diff) < 0.01) %>%
  filter(abs(AC_MIMS_astp_diff) == max(abs(AC_MIMS_astp_diff)))
plt_df_sub

subj_id_TMP <- 7668


# -----------------------------------------------------------------------------
# read data 

# read acc
fpath_tmp <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc <- readRDS(fpath_tmp) %>% as.data.frame() 

# read the derived mapping
fpath_tmp <- paste0(here::here(), "/results_public/mapping_between_measures_FITTED.txt")
dat_fitted <- fread(fpath_tmp) %>% as.data.frame()
# threshold for non-dominant hand 
thresh_vec <- dat_fitted %>% filter(AC == 1853) %>% unlist()
names(thresh_vec) <- gsub("_fitted", "", names(thresh_vec))
thresh_vec


# -----------------------------------------------------------------------------
# get the best time window 

dat_acc_sub <- 
  dat_acc %>% 
  filter(subj_id == subj_id_TMP) %>%
  mutate(HEADER_TIME_STAMP_hour = hour(HEADER_TIME_STAMP)) %>%
  mutate(HEADER_TIME_STAMP_date_hour = paste0(HEADER_TIME_STAMP_date, " ", HEADER_TIME_STAMP_hour))
head(dat_acc_sub)

# day-hour iteration
date_hour_vec <- unique(dat_acc_sub$HEADER_TIME_STAMP_date_hour)

diff_MIMS <- numeric()
diff_AC <- numeric()
for (i in 1: (length(date_hour_vec)-1)){ # i <- 1
  sub_i <- 
    dat_acc_sub %>%
    filter(HEADER_TIME_STAMP_date_hour %in% c(date_hour_vec[i], date_hour_vec[i+1])) 
  diff_AC[i] <- sum(abs(diff(as.numeric(sub_i$AC > thresh_vec["AC"]))))
  diff_MIMS[i] <- sum(abs(diff(as.numeric(sub_i$MIMS > thresh_vec["MIMS"]))))
}
diff_df <- data.frame(diff_MIMS = diff_MIMS, diff_AC = diff_AC, i = 1: (length(date_hour_vec)-1))


# -----------------------------------------------------------------------------
# PLOT A

diff_df %>% arrange(diff_MIMS - diff_AC) %>% head(10)

i <- 41
sub_i <- 
  dat_acc_sub %>%
  filter(HEADER_TIME_STAMP_date_hour %in% c(date_hour_vec[i], date_hour_vec[i+1])) %>%
  mutate(AC_active = as.numeric(AC > thresh_vec["AC"])) %>%
  mutate(MIMS_active = as.numeric(MIMS > thresh_vec["MIMS"])) %>%
  mutate(
    xmin = HEADER_TIME_STAMP, 
    xmax = lead(HEADER_TIME_STAMP),
    ymin = 0,
    ymax_MIMS = MIMS_active,
    ymax_AC = AC_active
  )
head(sub_i)

sum(abs(diff(as.numeric(sub_i$AC > thresh_vec["AC"]))))
sum(abs(diff(as.numeric(sub_i$MIMS > thresh_vec["MIMS"]))))

pltA1 <- 
  ggplot(sub_i, aes(x = HEADER_TIME_STAMP, y = AC)) + 
  geom_line(size = 0.3) + 
  geom_hline(yintercept = 1853, color = "magenta", alpha = 0.6, linetype = 2) + 
  labs(x = "Time", y = "AC", title = "ID=7668, Sunday") + 
  scale_y_continuous(breaks = c(0))

pltA2 <- 
  ggplot(sub_i, aes(x = HEADER_TIME_STAMP, y = MIMS)) + 
  geom_line(size = 0.3) + 
  geom_hline(yintercept = thresh_vec["MIMS"], color = "magenta", alpha = 0.6, linetype = 2) + 
  labs(x = "Time", y = "MIMS", title = "ID=7668, Sunday") + 
  scale_y_continuous(breaks = c(0))

pltA3 <- 
  ggplot(sub_i, aes(x = HEADER_TIME_STAMP, y = AC_active)) + 
  geom_step() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax_AC), 
            fill = "magenta", alpha = 0.2) + 
  labs(x = "Time", y = "AC-based\nactive bout") + 
  scale_y_continuous(breaks = c(0,1))

pltA4 <- 
  ggplot(sub_i, aes(x = HEADER_TIME_STAMP, y = MIMS_active)) + 
  geom_step() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax_MIMS), 
            fill = "magenta", alpha = 0.2) + 
  labs(x = "Time", y = "MIMS-based\nactive bout") + 
  scale_y_continuous(breaks = c(0,1))

plt_list <- list(pltA1, pltA2, pltA3, pltA4)
plt_A <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
# plt_A



# -----------------------------------------------------------------------------
# PLOT B

diff_df %>% arrange(desc(diff_MIMS - diff_AC)) %>% head(10)

i <- 63
sub_i <- 
  dat_acc_sub %>%
  filter(HEADER_TIME_STAMP_date_hour %in% c(date_hour_vec[i], date_hour_vec[i+1])) %>%
  mutate(AC_active = as.numeric(AC > thresh_vec["AC"])) %>%
  mutate(MIMS_active = as.numeric(MIMS > thresh_vec["MIMS"])) %>%
  mutate(
    xmin = HEADER_TIME_STAMP, 
    xmax = lead(HEADER_TIME_STAMP),
    ymin = 0,
    ymax_MIMS = MIMS_active,
    ymax_AC = AC_active
  )
head(sub_i)

sum(abs(diff(as.numeric(sub_i$AC > thresh_vec["AC"]))))
sum(abs(diff(as.numeric(sub_i$MIMS > thresh_vec["MIMS"]))))

pltB1 <- 
  ggplot(sub_i, aes(x = HEADER_TIME_STAMP, y = AC)) + 
  geom_line(size = 0.3) + 
  geom_hline(yintercept = 1853, color = "magenta", alpha = 0.6, linetype = 2) + 
  labs(x = "Time", y = "AC", title = "ID=7668, Monday") + 
  scale_y_continuous(breaks = c(0))

pltB2 <- 
  ggplot(sub_i, aes(x = HEADER_TIME_STAMP, y = MIMS)) + 
  geom_line(size = 0.3) + 
  geom_hline(yintercept = thresh_vec["MIMS"], color = "magenta", alpha = 0.6, linetype = 2) + 
  labs(x = "Time", y = "MIMS", title = "ID=7668, Monday") + 
  scale_y_continuous(breaks = c(0))

pltB3 <- 
  ggplot(sub_i, aes(x = HEADER_TIME_STAMP, y = AC_active)) + 
  geom_step() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax_AC), 
            fill = "magenta", alpha = 0.2, size = 0.3) + 
  labs(x = "Time", y = "AC-based\nactive bout") + 
  scale_y_continuous(breaks = c(0,1))

pltB4 <- 
  ggplot(sub_i, aes(x = HEADER_TIME_STAMP, y = MIMS_active)) + 
  geom_step() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax_MIMS), 
            fill = "magenta", alpha = 0.2, size = 0.3) + 
  labs(x = "Time", y = "MIMS-based\nactive bout") + 
  scale_y_continuous(breaks = c(0,1)) 

plt_list <- list(pltB1, pltB2, pltB3, pltB4)
plt_B <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
# plt_B


# -----------------------------------------------------------------------------
# plots combined

# plt_list_all <- list(
#   pltA1, pltA2, pltA3, pltA4,
#   ggplot() + theme_void(),
#   ggplot() + theme_void(),
#   pltB1, pltB2, pltB3, pltB4
#   )

plt_VOID <- ggplot() + theme_void()

plt_list_all <- list(
  pltA1, pltA3, plt_VOID, pltA2, pltA4,
  plt_VOID, plt_VOID, plt_VOID, plt_VOID, plt_VOID,
  pltB1, pltB3, plt_VOID, pltB2, pltB4
)
length(plt_list_all)

plt_all <- plot_grid(plotlist = plt_list_all, ncol = 3, align = "hv", byrow = FALSE, 
                     rel_heights = c(10, 5, 1.5, 10, 5),
                     rel_widths = c(10, 1.5, 10))
# plt_all

plt_path <- paste0(here::here(), "/results_figures/2021-05-06-cases_ASTP_AC_MIMS.png")
ggsave(filename = plt_path, plot = plt_all, width = 8, height = 8)


