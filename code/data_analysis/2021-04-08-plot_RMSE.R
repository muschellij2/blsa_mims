
rm(list = ls())
library(tidyverse)
library(cowplot)
library(ggsci)
library(lubridate)
library(scales)
library(latex2exp)
options(scipen=999)

# plot colors
names_levels1 <- c("MIMS", "ENMO", "MAD", "AI")
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

fpath_tmp <- paste0(here::here(), "/data_processed/2021-04-07-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc <- readRDS(fpath_tmp) %>% as.data.frame()
str(dat_acc)

dat_acc_F <- dat_acc %>%
  filter(wear_and_valid_flag == 1) %>%
  mutate(
    bias_MIMS = AC_hat_from_MIMS - AC,
    bias_ENMO = AC_hat_from_ENMO - AC,
    bias_MAD  = AC_hat_from_MAD  - AC,
    bias_AI   = AC_hat_from_AI   - AC
  )
summary(dat_acc_F$bias_MIMS)
summary(dat_acc_F$bias_ENMO)
summary(dat_acc_F$bias_MAD)
summary(dat_acc_F$bias_AI)

dat_acc_agg <- 
  dat_acc_F %>%
  group_by(subj_id) %>%
  summarise(
    RMSE_MIMS = mean(bias_MIMS^2),
    RMSE_ENMO = mean(bias_ENMO^2),
    RMSE_MAD = mean(bias_MAD^2),
    RMSE_AI = mean(bias_AI^2),
    
    RMAE_MIMS = mean(abs(bias_MIMS)),
    RMAE_ENMO = mean(abs(RMSE_ENMO)),
    RMAE_MAD  = mean(abs(RMSE_MAD)),
    RMAE_AI   = mean(abs(RMSE_AI)),
    
    BIAS_MIMS = mean((bias_MIMS)),
    BIAS_ENMO = mean((bias_ENMO)),
    BIAS_MAD  = mean((bias_MAD)),
    BIAS_AI   = mean((bias_AI)),
    
    days_cnt = n_distinct(HEADER_TIME_STAMP_date),
    AC_sum = sum(AC),
    AC_mean = mean(AC)
    )

summary(dat_acc_agg$BIAS_MIMS)
summary(dat_acc_agg$BIAS_ENMO)
summary(dat_acc_agg$BIAS_MAD)
summary(dat_acc_agg$BIAS_AI)

plt_df <- 
  dat_acc_agg %>%
  select(starts_with("RMAE"), subj_id, AC_mean) %>%
  pivot_longer(cols = -c(subj_id, AC_mean)) %>%
  mutate(name = factor(name, levels = c("RMAE_MIMS", "RMAE_ENMO", "RMAE_MAD", "RMAE_AI")))
head(plt_df)

plt <- 
  ggplot(plt_df, aes(x = AC_mean, y = value)) + 
  geom_point() + 
  facet_grid(name ~ ., scales = "free_y")
plt


plt_df <- 
  dat_acc_agg %>%
  select(starts_with("BIAS"), subj_id, AC_mean) %>%
  pivot_longer(cols = -c(subj_id, AC_mean)) %>%
  mutate(name = factor(name, levels = c("BIAS_MIMS", "BIAS_ENMO", "BIAS_MAD", "BIAS_AI")))
head(plt_df)

plt <- 
  ggplot(plt_df, aes(x = AC_mean, y = value)) + 
  geom_point(alpha = 0.2) + 
  facet_grid(name ~ ., scales = "free_y") + 
  geom_hline(yintercept = 0, color = "red")
plt



# ------------------------------------------------------------------------------
# plot 1: before mapping



fpath_tmp <- paste0(here::here(), "/data_processed/2021-04-05-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc <- readRDS(fpath_tmp) %>% as.data.frame()
str(dat_acc)

dat_acc_F <- dat_acc %>%
  filter(wear_and_valid_flag == 1) %>%
  mutate(
    bias_MIMS = AC_hat_from_MIMS - AC,
    bias_ENMO = AC_hat_from_ENMO - AC,
    bias_MAD  = AC_hat_from_MAD  - AC,
    bias_AI   = AC_hat_from_AI   - AC
  )

dat_acc_agg <- 
  dat_acc_F %>%
  group_by(subj_id) %>%
  summarise(
    RMSE_MIMS = mean(bias_MIMS^2),
    RMSE_ENMO = mean(bias_ENMO^2),
    RMSE_MAD = mean(bias_MAD^2),
    RMSE_AI = mean(bias_AI^2),
    
    RMAE_MIMS = mean(abs(bias_MIMS)),
    RMAE_ENMO = mean(abs(RMSE_ENMO)),
    RMAE_MAD  = mean(abs(RMSE_MAD)),
    RMAE_AI   = mean(abs(RMSE_AI)),
    
    BIAS_MIMS = mean((bias_MIMS)),
    BIAS_ENMO = mean((bias_ENMO)),
    BIAS_MAD  = mean((bias_MAD)),
    BIAS_AI   = mean((bias_AI)),
    
    days_cnt = n_distinct(HEADER_TIME_STAMP_date),
    AC_sum = sum(AC),
    AC_mean = mean(AC)
  )

plt_df <- 
  dat_acc_agg %>%
  select(starts_with("BIAS"), subj_id, AC_mean) %>%
  pivot_longer(cols = -c(subj_id, AC_mean)) %>%
  mutate(name = factor(name, levels = c("BIAS_MIMS", "BIAS_ENMO", "BIAS_MAD", "BIAS_AI")))
head(plt_df)

plt <- 
  ggplot(plt_df, aes(x = AC_mean, y = value)) + 
  geom_point() + 
  facet_grid(name ~ ., scales = "free_y") + 
  geom_hline(yintercept = 0, color = "red")
plt

