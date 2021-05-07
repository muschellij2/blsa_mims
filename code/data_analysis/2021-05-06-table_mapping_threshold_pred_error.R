
rm(list = ls())
library(tidyverse)
options(scipen=999)


# ------------------------------------------------------------------------------
# read data 

path_tmp <- paste0(here::here(), "/results/2021-05-06-mapping_threshold_pred_error.rds")
df_out <- readRDS(path_tmp)
head(df_out)

names_levels1 <- c("MIMS", "ENMO", "MAD", "AI")


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# TABLE 

tbl_all <- data.frame()

tbl_1 <- 
  df_out %>%
  filter(AC_thresh == 0) %>%
  select(-AC_thresh) %>%
  group_by(name, performance_measure) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = names_levels1)) %>%
  arrange(name)
tbl_1_form <- 
  tbl_1 %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")")
  ) %>%
  select(name, 
         performance_measure,
         val_mean_sd) %>%
  pivot_wider(names_from = performance_measure, values_from = val_mean_sd) %>%
  mutate(label = "AC > 0", .before = everything())
tbl_1_form
tbl_all <- rbind(tbl_all, tbl_1_form)


tbl_2 <- 
  df_out %>%
  filter(AC_thresh == 1853) %>%
  select(-AC_thresh) %>%
  group_by(name, performance_measure) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = names_levels1)) %>%
  arrange(name)
tbl_2_form <- 
  tbl_2 %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")")
  ) %>%
  select(name, 
         performance_measure,
         val_mean_sd) %>%
  pivot_wider(names_from = performance_measure, values_from = val_mean_sd) %>%
  mutate(label = "AC > 1853", .before = everything())
tbl_2_form
tbl_all <- rbind(tbl_all, tbl_2_form)


tbl_3 <- 
  df_out %>%
  filter(AC_thresh == 2303) %>%
  select(-AC_thresh) %>%
  group_by(name, performance_measure) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = names_levels1)) %>%
  arrange(name)
tbl_3_form <- 
  tbl_3 %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")")
  ) %>%
  select(name, 
         performance_measure,
         val_mean_sd) %>%
  pivot_wider(names_from = performance_measure, values_from = val_mean_sd) %>%
  mutate(label = "AC > 2303", .before = everything())
tbl_3_form
tbl_all <- rbind(tbl_all, tbl_3_form)
# View(tbl_all)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# TABLE showing overal proportion of times

path_tmp <- paste0(here::here(), "/results/2021-05-06-mapping_threshold_pred_proportion.rds")
df_out <- readRDS(path_tmp)
tbl_df <- 
  df_out %>%
  pivot_longer(cols = -AC_thresh) %>%
  pivot_wider(names_from = AC_thresh, values_from = value) %>%
  as.data.frame()
tbl_df[, 2:4] <- round(tbl_df[, 2:4] * 100, 3)
tbl_df

# View(tbl_df)













