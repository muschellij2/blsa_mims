
rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(ggsci)
options(scipen=999)


# ------------------------------------------------------------------------------
# read data 

## read mastervisit
mastervisit_fpath <- paste0(here::here(), "/covariates/2021-01-19-mastervisit.rdata")
mastervisit <- get(load(mastervisit_fpath, ex <- new.env()), envir = ex) %>% 
  select(age = Age, subj_id = IDNo, visit_id = Visit) %>%
  distinct()
dim(mastervisit)

# read acc
fpath_tmp <- paste0(here::here(), "/data_processed/2021-04-13-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc0 <- readRDS(fpath_tmp) %>% as.data.frame()
dat_acc <- dat_acc0 %>% 
  left_join(mastervisit, by = c("subj_id", "visit_id")) %>%
  arrange(subj_id, HEADER_TIME_STAMP)
str(dat_acc)

# read the derived mapping
fpath_tmp <- paste0(here::here(), "/results_public/mapping_between_measures_FITTED.txt")
dat_fitted <- fread(fpath_tmp) %>% as.data.frame()
thresh_vec <- dat_fitted %>% filter(AC == 1853) %>% unlist()
names(thresh_vec) <- gsub("_fitted", "", names(thresh_vec))
thresh_vec


# ------------------------------------------------------------------------------
# STEP 1: compare proportion active -- overall

dat_acc %>%
  summarise(
    AC_active = mean(AC >= thresh_vec["AC"]),
    MIMS_active = mean(MIMS >= thresh_vec["MIMS"]),
    ENMO_active = mean(ENMO >= thresh_vec["ENMO"]),
    MAD_active = mean(MAD >= thresh_vec["MAD"]),
    AI_active = mean(AI >= thresh_vec["AI"])
  )
# AC_active MIMS_active ENMO_active MAD_active AI_active
# 1 0.2984911   0.2990194   0.4394849  0.3017843 0.3002229

dat_acc %>%
  filter(wear_and_valid_flag == 1) %>%
  summarise(
    AC_active = mean(AC >= thresh_vec["AC"]),
    MIMS_active = mean(MIMS >= thresh_vec["MIMS"]),
    ENMO_active = mean(ENMO >= thresh_vec["ENMO"]),
    MAD_active = mean(MAD >= thresh_vec["MAD"]),
    AI_active = mean(AI >= thresh_vec["AI"])
  )
# AC_active MIMS_active ENMO_active MAD_active AI_active
# 1 0.2983504   0.2989253   0.4393333  0.3016952 0.3001134

const <- 1.01
const_vec <- c(1, rep(const, 4))
(thresh_vec * const_vec) %>% round(3)

dat_acc %>%
  summarise(
    AC_active = mean(AC >= thresh_vec["AC"]),
    MIMS_active = mean(MIMS >= thresh_vec["MIMS"] * const),
    ENMO_active = mean(ENMO >= thresh_vec["ENMO"] * const),
    MAD_active = mean(MAD >= thresh_vec["MAD"] * const),
    AI_active = mean(AI >= thresh_vec["AI"] * const)
  )


# ------------------------------------------------------------------------------
# STEP 2: compare proportion active -- overall

df_prop_active <- 
  dat_acc %>%
  group_by(subj_id) %>%
  summarise(
    AC_active = mean(AC >= thresh_vec["AC"]),
    MIMS_active = mean(MIMS >= thresh_vec["MIMS"]),
    ENMO_active = mean(ENMO >= thresh_vec["ENMO"]),
    MAD_active = mean(MAD >= thresh_vec["MAD"]),
    AI_active = mean(AI >= thresh_vec["AI"])
  ) %>%
  as.data.frame()



plt_list <- list()
names_levels <- c("MIMS_active", "ENMO_active", "MAD_active", "AI_active")
for (i in 1 : length(names_levels)){ # i <- 1
  name_i  <- names_levels[i]
  plt_df_i <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = AC_active, y = y)) + geom_point(alpha = 0.2, size = 0.5) + 
    geom_abline(intercept = 0, slope = 1) + 
    labs(y = name_i)
  # plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-TP_check_propactive.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8)


# ------------------------------------------------------------------------------
# STEP 3: compare ASTP

get_ASTP <- function(x, thresh_tmp){
  is_active <- (x >= thresh_tmp) * 1
  rle_out <- rle(is_active)
  ## Vector of lengths of all active bouts
  actbout_len_vec  <- rle_out$lengths[which(rle_out$values == 1)]
  astp <- 1/mean(actbout_len_vec)
  return(astp)
}

plt_df <- 
  dat_acc %>%
  group_by(subj_id) %>%
  summarise(
    AC_astp = get_ASTP(AC, thresh_vec["AC"]),
    MIMS_astp = get_ASTP(MIMS, thresh_vec["MIMS"]),
    ENMO_astp = get_ASTP(ENMO, thresh_vec["ENMO"]),
    MAD_astp = get_ASTP(MAD, thresh_vec["MAD"]),
    AI_astp = get_ASTP(AI, thresh_vec["AI"])
  ) %>%
  as.data.frame()

plt_list <- list()
names_levels <- c("MIMS_astp", "ENMO_astp", "MAD_astp", "AI_astp")
for (i in 1 : length(names_levels)){ # i <- 1
  name_i  <- names_levels[i]
  plt_df_i <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = AC_astp, y = y)) + geom_point(alpha = 0.2, size = 0.5) + 
    geom_abline(intercept = 0, slope = 1) + 
    labs(y = name_i)
  # plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-TP_check_astp.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8)



# ------------------------------------------------------------------------------
# STEP 4: compare SATP

get_SATP <- function(x, thresh_tmp){
  is_active <- (x >= thresh_tmp) * 1
  rle_out <- rle(is_active)
  ## Vector of lengths of all active bouts
  nonactbout_len_vec <- rle_out$lengths[which(rle_out$values == 0)]
  satp <- 1/mean(nonactbout_len_vec)
  return(satp)
}

plt_df <- 
  dat_acc %>%
  group_by(subj_id) %>%
  summarise(
    AC_satp = get_SATP(AC, thresh_vec["AC"]),
    MIMS_satp = get_SATP(MIMS, thresh_vec["MIMS"]),
    ENMO_satp = get_SATP(ENMO, thresh_vec["ENMO"]),
    MAD_satp = get_SATP(MAD, thresh_vec["MAD"]),
    AI_satp = get_SATP(AI, thresh_vec["AI"])
  ) %>%
  as.data.frame()

plt_list <- list()
names_levels <- c("MIMS_satp", "ENMO_satp", "MAD_satp", "AI_satp")
for (i in 1 : length(names_levels)){ # i <- 1
  name_i  <- names_levels[i]
  plt_df_i <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = AC_satp, y = y)) + geom_point(alpha = 0.2, size = 0.5) + 
    geom_abline(intercept = 0, slope = 1) + 
    labs(y = name_i)
  # plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-TP_check_satp.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8)


# ------------------------------------------------------------------------------
# STEP 5: compare bout lengths

get_bout_len <- function(x, thresh_tmp){
  is_active <- (x >= thresh_tmp) * 1
  rle_out <- rle(is_active)
  ## Vector of lengths of all active bouts
  mean(rle_out$lengths)
}

plt_df <- 
  dat_acc %>%
  group_by(subj_id) %>%
  summarise(
    AC_bout_length = get_bout_len(AC, thresh_vec["AC"]),
    MIMS_bout_length = get_bout_len(MIMS, thresh_vec["MIMS"]),
    ENMO_bout_length = get_bout_len(ENMO, thresh_vec["ENMO"]),
    MAD_bout_length = get_bout_len(MAD, thresh_vec["MAD"]),
    AI_bout_length = get_bout_len(AI, thresh_vec["AI"])
  ) %>%
  as.data.frame()

plt_list <- list()
names_levels <- c("MIMS_bout_length", "ENMO_bout_length", "MAD_bout_length", "AI_bout_length")
for (i in 1 : length(names_levels)){ # i <- 1
  name_i  <- names_levels[i]
  plt_df_i <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = AC_bout_length, y = y)) + geom_point(alpha = 0.2, size = 0.5) + 
    geom_abline(intercept = 0, slope = 1) + 
    labs(y = name_i)
  # plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-TP_check_bout_length.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8)



# ------------------------------------------------------------------------------
# STEP 6: compare bout lengths, DAY ONLY

get_bout_len <- function(x, thresh_tmp){
  is_active <- (x >= thresh_tmp) * 1
  rle_out <- rle(is_active)
  ## Vector of lengths of all active bouts
  mean(rle_out$lengths)
}

plt_df <- 
  dat_acc %>%
  dplyr::filter(!(lubridate::hour(HEADER_TIME_STAMP) %in% c(21,22,23,0,1,2,3,4,5,6,7,8))) %>%
  group_by(subj_id) %>%
  summarise(
    AC_bout_length = get_bout_len(AC, thresh_vec["AC"]),
    MIMS_bout_length = get_bout_len(MIMS, thresh_vec["MIMS"]),
    ENMO_bout_length = get_bout_len(ENMO, thresh_vec["ENMO"]),
    MAD_bout_length = get_bout_len(MAD, thresh_vec["MAD"]),
    AI_bout_length = get_bout_len(AI, thresh_vec["AI"])
  ) %>%
  as.data.frame()

plt_list <- list()
names_levels <- c("MIMS_bout_length", "ENMO_bout_length", "MAD_bout_length", "AI_bout_length")
for (i in 1 : length(names_levels)){ # i <- 1
  name_i  <- names_levels[i]
  plt_df_i <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = AC_bout_length, y = y)) + geom_point(alpha = 0.2, size = 0.5) + 
    geom_abline(intercept = 0, slope = 1) + 
    labs(y = name_i)
  # plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-TP_check_bout_length_dayonly.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8)





# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# FURTHER CHECKS

names_levels <- c("MIMS_active", "ENMO_active", "MAD_active", "AI_active")
check_df <- 
  dat_acc %>%
  group_by(subj_id) %>%
  mutate(
    AC_active = (AC >= thresh_vec["AC"]) * 1,
    MIMS_active = (MIMS >= thresh_vec["MIMS"]) * 1,
    ENMO_active = (ENMO >= thresh_vec["ENMO"]) * 1,
    MAD_active = (MAD >= thresh_vec["MAD"]) * 1,
    AI_active = (AI >= thresh_vec["AI"]) * 1
  ) %>%
  select(all_of(c(names_levels, "subj_id", "AC_active"))) %>%
  as.data.frame()
head(check_df)

check_df %>% summarise_all(mean)


# ------------------------------------------------------------------------------
# (a) ACCURACY

measures_names <- c("MIMS", "ENMO", "MAD", "AI")
acc_df <- 
  check_df %>%
  mutate(
    MIMS = (AC_active == MIMS_active) * 1,
    ENMO = (AC_active == ENMO_active) * 1,
    MAD  = (AC_active == MAD_active) * 1,
    AI   = (AC_active == AI_active) * 1
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id)

plt_list <- list()
for (i in 1 : length(measures_names)){ # i <- 1
  name_i  <- measures_names[i]
  plt_df_i <- acc_df %>% filter(name == name_i)
  plt_i <- 
    ggplot(plt_df_i, aes(x = value)) + 
    geom_histogram(alpha = 0.5, fill = 'grey', color = "black", binwidth = 0.005) + 
    labs(x = paste0("accuracy: ", name_i)) + 
    scale_x_continuous(limits = c(0.7, 1.01))
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-TP_check_accuracy.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6.5) 


# ------------------------------------------------------------------------------
# (b) SENSITIVITY

measures_names <- c("MIMS", "ENMO", "MAD", "AI")
sens_df <- 
  check_df %>%
  filter(AC_active == 1) %>%
  mutate(
    MIMS = MIMS_active,
    ENMO = ENMO_active,
    MAD  = MAD_active,
    AI   = AI_active
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id)

plt_list <- list()
for (i in 1 : length(measures_names)){ # i <- 1
  name_i  <- measures_names[i]
  plt_df_i <- sens_df %>% filter(name == name_i)
  plt_i <- 
    ggplot(plt_df_i, aes(x = value)) + 
    geom_histogram(alpha = 0.5, fill = 'darkgreen', color = "black", binwidth = 0.005) + 
    labs(x = paste0("sensitivity: ", name_i)) + 
    scale_x_continuous(limits = c(0.7, 1.01))
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-TP_check_sensitivity.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6.5) 


# ------------------------------------------------------------------------------
# (c) SPECIFICITY

measures_names <- c("MIMS", "ENMO", "MAD", "AI")
spec_df <- 
  check_df %>%
  filter(AC_active == 0) %>%
  mutate(
    MIMS =  (MIMS_active == 0) * 1,
    ENMO = (ENMO_active == 0) * 1,
    MAD  = (MAD_active == 0) * 1,
    AI   = (AI_active == 0) * 1
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id)

plt_list <- list()
for (i in 1 : length(measures_names)){ # i <- 1
  name_i  <- measures_names[i]
  plt_df_i <- spec_df %>% filter(name == name_i)
  plt_i <- 
    ggplot(plt_df_i, aes(x = value)) + 
    geom_histogram(alpha = 0.5, fill = 'darkblue', color = "black", binwidth = 0.005) + 
    labs(x = paste0("specificity: ", name_i)) + 
    scale_x_continuous(limits = c(0.7, 1.01))
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-TP_check_specificity.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6.5) 




# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# REPLICATE THE RESULT FROM THE PAPER


get_ASTP <- function(x, thresh_tmp){
  is_active <- (x >= thresh_tmp) * 1
  rle_out <- rle(is_active)
  ## Vector of lengths of all active bouts
  actbout_len_vec  <- rle_out$lengths[which(rle_out$values == 1)]
  astp <- 1/mean(actbout_len_vec)
  return(astp)
}

plt_df <- 
  dat_acc %>%
  group_by(subj_id, age) %>%
  summarise(
    AC_astp = get_ASTP(AC, thresh_vec["AC"]),
    MIMS_astp = get_ASTP(MIMS, thresh_vec["MIMS"]),
    ENMO_astp = get_ASTP(ENMO, thresh_vec["ENMO"]),
    MAD_astp = get_ASTP(MAD, thresh_vec["MAD"]),
    AI_astp = get_ASTP(AI, thresh_vec["AI"])
  ) %>%
  as.data.frame()
head(plt_df)

plt_list <- list()
names_levels <- c("AC_astp", "MIMS_astp", "ENMO_astp", "MAD_astp", "AI_astp")
for (i in 1 : length(names_levels)){ # i <- 1
  name_i  <- names_levels[i]
  plt_df_i <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = age, y = y)) + 
    geom_point(alpha = 0.2, size = 0.5) + 
    labs(y = name_i) + 
    geom_smooth(method = 'loess', size = 0.5, color = "black", se = FALSE) + 
    scale_y_continuous(limits = c(0.05, 0.5))
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
  if (i == 1) plt_list[[length(plt_list) + 1]] <- (ggplot() + theme_void())
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-age_astp.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8)


# ------------------------------------------------------------------------------
# 2nd plot

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
    geom_smooth(method = 'loess', size = 0.5, color = "black", se = FALSE) + 
    scale_y_continuous(limits = c(0.05, 0.5))
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
}

plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-age_astp_COMPARE.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6.5)




# ------------------------------------------------------------------------------
# Similar, but with prop active

plt_df <- 
  dat_acc %>%
  group_by(subj_id, age) %>%
  summarise(
    AC_active = mean(AC > thresh_vec["AC"]),
    MIMS_active = mean(MIMS > thresh_vec["MIMS"]),
    ENMO_active = mean(ENMO > thresh_vec["ENMO"]),
    MAD_active = mean(MAD > thresh_vec["MAD"]),
    AI_active = mean(AI > thresh_vec["AI"])
  ) %>%
  as.data.frame()
head(plt_df)

plt_list <- list()
names_levels <- c("AC_active", "MIMS_active", "ENMO_active", "MAD_active", "AI_active")
for (i in 1 : length(names_levels)){ # i <- 1
  name_i  <- names_levels[i]
  plt_df_i <- plt_df
  plt_df_i$y <- plt_df[, name_i]
  plt_i <- 
    ggplot(plt_df_i, aes(x = age, y = y)) + 
    geom_point(alpha = 0.2, size = 0.5) + 
    labs(y = name_i) + 
    geom_smooth(method = 'loess', size = 0.5, color = "black") + 
    scale_y_continuous(limits = c(0.05, 0.5))
  plt_i
  plt_list[[length(plt_list) + 1]] <- plt_i
  if (i == 1) plt_list[[length(plt_list) + 1]] <- (ggplot() + theme_void())
}
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-age_activeprop.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8)











