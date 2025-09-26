
# TABLE 2 


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --   VALUES: participant's average daily sum of measures   -------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

measures_masterfile_fpath <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized_imp_mapped.rds")
measures_masterfile <- readRDS(measures_masterfile_fpath)
nrow(measures_masterfile)
# [1] 5567040      
length(unique(measures_masterfile$subj_id))
# [1] 655
nrow(measures_masterfile) / 1440
# [1] 3866


# (1) using all day ------------------------------------------------------------

osm_vars <- c("AC", "AC_LOG", "MIMS", "MAD", "ENMO", "AI")

mm_agg_A <-
  measures_masterfile %>%
  mutate(AC_LOG = log(AC + 1),
         HEADER_TIME_STAMP_date = as.Date(HEADER_TIME_STAMP)) %>%
  select(all_of(c("subj_id", "HEADER_TIME_STAMP_date", osm_vars))) %>%
  group_by(subj_id) %>%
  mutate(days_cnt = n_distinct(HEADER_TIME_STAMP_date)) %>%
  select(-HEADER_TIME_STAMP_date) %>%
  group_by(subj_id, days_cnt) %>%
  summarise_all(sum) %>% 
  as.data.frame() %>%
  mutate(across(all_of(osm_vars)), . / days_cnt) %>%
  ungroup() %>%
  select(-c(subj_id, days_cnt)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = c("AC", "AC_LOG", "MIMS", "MAD", "ENMO", "AI"))) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup() %>%
  as.data.frame()
mm_agg_A[1, 2:6] <- round(mm_agg_A[1, 2:6])

mm_agg_A_form <- 
  mm_agg_A %>%
  mutate(
    val_mean_f   = sprintf("%.1f", val_mean),
    val_sd_f     = sprintf("%.1f", val_sd),
    val_median_f = sprintf("%.1f", val_median),
    val_min_f    = sprintf("%.1f", val_min),
    val_max_f    = sprintf("%.1f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_A = val_mean_sd, 
         val_median_min_max_A = val_median_min_max)

# (2) excluding night time ------------------------------------------------------------

mm_agg_B <- 
  measures_masterfile %>%
  dplyr::filter(!(lubridate::hour(HEADER_TIME_STAMP) %in% c(23,0,1,2,3,4))) %>%
  mutate(AC_LOG = log(AC + 1),
         HEADER_TIME_STAMP_date = as.Date(HEADER_TIME_STAMP)) %>%
  select(all_of(c("subj_id", "HEADER_TIME_STAMP_date", osm_vars))) %>%
  group_by(subj_id) %>%
  mutate(days_cnt = n_distinct(HEADER_TIME_STAMP_date)) %>%
  select(-HEADER_TIME_STAMP_date) %>%
  group_by(subj_id, days_cnt) %>%
  summarise_all(sum) %>% 
  as.data.frame() %>%
  mutate(across(all_of(osm_vars)), . / days_cnt) %>%
  ungroup() %>%
  select(-c(subj_id, days_cnt)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = c("AC", "AC_LOG", "MIMS", "MAD", "ENMO", "AI"))) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup() %>%
  as.data.frame()
mm_agg_B[1, 2:6] <- round(mm_agg_B[1, 2:6])

mm_agg_B_form <- 
  mm_agg_B %>%
  mutate(
    val_mean_f   = sprintf("%.1f", val_mean),
    val_sd_f     = sprintf("%.1f", val_sd),
    val_median_f = sprintf("%.1f", val_median),
    val_min_f    = sprintf("%.1f", val_min),
    val_max_f    = sprintf("%.1f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_B = val_mean_sd, 
         val_median_min_max_B = val_median_min_max)

# combine the two
tbl_out <- mm_agg_A_form %>% left_join(mm_agg_B_form, by = "name")
tbl_out
# View(tbl_out)

tbl_out2 <- tbl_out %>% select(name, val_mean_sd_A, val_median_min_max_A)
tbl_out2 <- filter(tbl_out2, name != "AC_LOG")
row_idx <- c(
  which(tbl_out2$name == "AC"),
  which(tbl_out2$name == "MIMS"),
  which(tbl_out2$name == "ENMO"),
  which(tbl_out2$name == "MAD"),
  which(tbl_out2$name == "AI")
)
tbl_out2 <- tbl_out2[row_idx, ]
tbl_out2


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --   VALUES: participant's minute-level measures   ---------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --   unprocessed   -----------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

measures_masterfile_fpath <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile.rds")
measures_masterfile <- readRDS(measures_masterfile_fpath)
nrow(measures_masterfile)
# [1] 5566920
length(unique(measures_masterfile$subj_id))
# [1] 655
nrow(measures_masterfile) / 1440
# [1] 3865.917


# (1) using all day ------------------------------------------------------------

osm_vars <- c("AC", "AC_LOG", "MIMS", "MAD", "ENMO", "AI")

mm_agg_A <- 
  measures_masterfile %>%
  dplyr::filter(wear_and_valid_flag == 1) %>% # added Feb 25
  mutate(AC_LOG = log(AC + 1)) %>%
  select(all_of(osm_vars)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = osm_vars)) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value, na.rm = TRUE),
    val_sd     = sd(value, na.rm = TRUE),
    val_median = median(value, na.rm = TRUE),
    val_min    = min(value, na.rm = TRUE),
    val_max    = max(value, na.rm = TRUE)
  ) %>%
  ungroup()

mm_agg_A_form <- 
  mm_agg_A %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd),
    val_median_f = sprintf("%.3f", val_median),
    val_min_f    = sprintf("%.3f", val_min),
    # val_max_f    = sprintf("% 06.3f", val_max),
    val_max_f    = sprintf("%.3f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_A = val_mean_sd, 
         val_median_min_max_A = val_median_min_max)


# (2) excluding night time -----------------------------------------------------

mm_agg_B <- 
  measures_masterfile %>%
  dplyr::filter(!(lubridate::hour(HEADER_TIME_STAMP) %in% c(23,0,1,2,3,4))) %>%
  dplyr::filter(wear_and_valid_flag == 1) %>% # added Feb 25 
  mutate(AC_LOG = log(AC + 1)) %>%
  select(all_of(osm_vars)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = osm_vars)) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value, na.rm = TRUE),
    val_sd     = sd(value, na.rm = TRUE),
    val_median = median(value, na.rm = TRUE),
    val_min    = min(value, na.rm = TRUE),
    val_max    = max(value, na.rm = TRUE)
  ) %>%
  ungroup()

mm_agg_B_form <- 
  mm_agg_B %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd),
    val_median_f = sprintf("%.3f", val_median),
    val_min_f    = sprintf("%.3f", val_min),
    val_max_f    = sprintf("%.3f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_B = val_mean_sd, 
         val_median_min_max_B = val_median_min_max)

tbl_out <- mm_agg_A_form %>% left_join(mm_agg_B_form, by = "name")
View(tbl_out)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --   VALUES: participant's minute-level measures   ---------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --   processed   -----------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

measures_masterfile_fpath <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized_imp.rds")
measures_masterfile <- readRDS(measures_masterfile_fpath)
nrow(measures_masterfile)
# [1] 5567040
length(unique(measures_masterfile$subj_id))
# [1] 655
nrow(measures_masterfile) / 1440
# [1] 3866


# (1) using all day ------------------------------------------------------------

osm_vars <- c("AC", "AC_LOG", "MIMS", "MAD", "ENMO", "AI")

mm_agg_A <- 
  measures_masterfile %>%
  # dplyr::filter(wear_and_valid_flag == 1) %>% 
  mutate(AC_LOG = log(AC + 1)) %>%
  select(all_of(osm_vars)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = osm_vars)) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup()

mm_agg_A_form <- 
  mm_agg_A %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd),
    val_median_f = sprintf("%.3f", val_median),
    val_min_f    = sprintf("%.3f", val_min),
    # val_max_f    = sprintf("% 06.3f", val_max),
    val_max_f    = sprintf("%.3f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_A = val_mean_sd, 
         val_median_min_max_A = val_median_min_max)


# (2) excluding night time -----------------------------------------------------

mm_agg_B <- 
  measures_masterfile %>%
  dplyr::filter(!(lubridate::hour(HEADER_TIME_STAMP) %in% c(23,0,1,2,3,4))) %>%
  # dplyr::filter(wear_and_valid_flag == 1) %>% 
  mutate(AC_LOG = log(AC + 1)) %>%
  select(all_of(osm_vars)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = osm_vars)) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
    val_median = median(value),
    val_min    = min(value),
    val_max    = max(value)
  ) %>%
  ungroup()

mm_agg_B_form <- 
  mm_agg_B %>%
  mutate(
    val_mean_f   = sprintf("%.3f", val_mean),
    val_sd_f     = sprintf("%.3f", val_sd),
    val_median_f = sprintf("%.3f", val_median),
    val_min_f    = sprintf("%.3f", val_min),
    val_max_f    = sprintf("%.3f", val_max)
  ) %>%
  mutate(
    val_mean_sd = paste0(val_mean_f, " (", val_sd_f, ")"),
    val_median_min_max = paste0(val_median_f, " [", val_min_f, ", ", val_max_f, "]")
  ) %>%
  select(name, 
         val_mean_sd_B = val_mean_sd, 
         val_median_min_max_B = val_median_min_max)

tbl_out <- mm_agg_A_form %>% left_join(mm_agg_B_form, by = "name")
# View(tbl_out)

stargazer::stargazer(tbl_out, summary = FALSE)

rm(tbl_out2)
tbl_out2 <- tbl_out %>% select(name, val_mean_sd_A, val_median_min_max_A)
tbl_out2 <- filter(tbl_out2, name != "AC_LOG")
tbl_out2$name <- as.character(tbl_out2$name)
row_idx <- c(
  which(tbl_out2$name == "AC"),
  which(tbl_out2$name == "MIMS"),
  which(tbl_out2$name == "ENMO"),
  which(tbl_out2$name == "MAD"),
  which(tbl_out2$name == "AI")
)
tbl_out2 <- tbl_out2[row_idx, ]
stargazer::stargazer(tbl_out2, summary = FALSE)



