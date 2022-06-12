
rm(list = ls())
library(tidyverse)
library(data.table)
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
fpath_tmp <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc0 <- readRDS(fpath_tmp) %>% as.data.frame()
dat_acc <- dat_acc0 %>% 
  left_join(mastervisit, by = c("subj_id", "visit_id")) %>%
  arrange(subj_id, HEADER_TIME_STAMP)
str(dat_acc)

# read the derived mapping
fpath_tmp <- paste0(here::here(), "/results_public/mapping_between_measures_FITTED.txt")
dat_fitted <- fread(fpath_tmp) %>% as.data.frame()


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# PERFORMANCE METRICS OF PREDICTING AC > something

df_out <- data.frame()

names_levels <- c("MIMS_pred", "ENMO_pred", "MAD_pred", "AI_pred")
measures_names <- c("MIMS", "ENMO", "MAD", "AI")


# ------------------------------------------------------------------------------
# PART (1): AC_thresh = 0

AC_thresh <- 0

thresh_vec_A <- dat_fitted %>% filter(AC == 0) %>% unlist()
thresh_vec_B <- dat_fitted %>% filter(AC == (AC_thresh+1)) %>% unlist()
thresh_vec   <- (thresh_vec_A + thresh_vec_B)/2
names(thresh_vec) <- gsub("_fitted", "", names(thresh_vec))
thresh_vec
thresh_vec %>% round(4)

check_df <- 
  dat_acc %>%
  filter(wear_and_valid_flag == 1) %>%
  group_by(subj_id) %>%
  mutate(
    AC_pred   = as.numeric(AC > 0),
    MIMS_pred = as.numeric(MIMS > thresh_vec["MIMS"]),
    ENMO_pred = as.numeric(ENMO > thresh_vec["ENMO"]),
    MAD_pred  = as.numeric(MAD > thresh_vec["MAD"]),
    AI_pred   = as.numeric(AI > thresh_vec["AI"])
  ) %>%
  select(all_of(c(names_levels, "subj_id", "AC_pred"))) %>%
  as.data.frame()
head(check_df)

# ACCURACY
acc_df <- 
  check_df %>%
  mutate(
    MIMS = (AC_pred == MIMS_pred) * 1,
    ENMO = (AC_pred == ENMO_pred) * 1,
    MAD  = (AC_pred == MAD_pred) * 1,
    AI   = (AC_pred == AI_pred) * 1
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "accuracy") %>%
  mutate(AC_thresh = AC_thresh) 

# SENSITIVITY
sens_df <- 
  check_df %>%
  filter(AC_pred == 1) %>%
  mutate(
    MIMS = MIMS_pred,
    ENMO = ENMO_pred,
    MAD  = MAD_pred,
    AI   = AI_pred
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "sensitivity") %>%
  mutate(AC_thresh = AC_thresh) 

# SPECIFICITY
spec_df <- 
  check_df %>%
  filter(AC_pred == 0) %>%
  mutate(
    MIMS =  (MIMS_pred == 0) * 1,
    ENMO = (ENMO_pred == 0) * 1,
    MAD  = (MAD_pred == 0) * 1,
    AI   = (AI_pred == 0) * 1
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "specificity") %>%
  mutate(AC_thresh = AC_thresh) 

# append results
df_out <- rbind(df_out, acc_df); rm(acc_df); dim(df_out)
df_out <- rbind(df_out, spec_df); rm(spec_df); dim(df_out)
df_out <- rbind(df_out, sens_df); rm(sens_df); dim(df_out)


# ------------------------------------------------------------------------------
# PART (2): AC_thresh = 1853

AC_thresh <- 1853

thresh_vec <- dat_fitted %>% filter(AC == AC_thresh) %>% unlist()
names(thresh_vec) <- gsub("_fitted", "", names(thresh_vec))
thresh_vec
thresh_vec %>% round(4)


check_df <- 
  dat_acc %>%
  filter(wear_and_valid_flag == 1) %>%
  group_by(subj_id) %>%
  mutate(
    AC_pred   = as.numeric(AC >= thresh_vec["AC"]),
    MIMS_pred = as.numeric(MIMS >= thresh_vec["MIMS"]),
    ENMO_pred = as.numeric(ENMO >= thresh_vec["ENMO"]),
    MAD_pred  = as.numeric(MAD >= thresh_vec["MAD"]),
    AI_pred   = as.numeric(AI >= thresh_vec["AI"])
  ) %>%
  select(all_of(c(names_levels, "subj_id", "AC_pred"))) %>%
  as.data.frame()
head(check_df)

# ACCURACY
acc_df <- 
  check_df %>%
  mutate(
    MIMS = (AC_pred == MIMS_pred) * 1,
    ENMO = (AC_pred == ENMO_pred) * 1,
    MAD  = (AC_pred == MAD_pred) * 1,
    AI   = (AC_pred == AI_pred) * 1
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "accuracy") %>%
  mutate(AC_thresh = AC_thresh) 

# SENSITIVITY
sens_df <- 
  check_df %>%
  filter(AC_pred == 1) %>%
  mutate(
    MIMS = MIMS_pred,
    ENMO = ENMO_pred,
    MAD  = MAD_pred,
    AI   = AI_pred
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "sensitivity") %>%
  mutate(AC_thresh = AC_thresh) 

sens_df %>% group_by(name) %>% summarise(value = mean(value))
# 1 AI    0.933
# 2 ENMO  0.819
# 3 MAD   0.879
# 4 MIMS  0.968

# SPECIFICITY
spec_df <- 
  check_df %>%
  filter(AC_pred == 0) %>%
  mutate(
    MIMS =  (MIMS_pred == 0) * 1,
    ENMO = (ENMO_pred == 0) * 1,
    MAD  = (MAD_pred == 0) * 1,
    AI   = (AI_pred == 0) * 1
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "specificity") %>%
  mutate(AC_thresh = AC_thresh) 

# append results
df_out <- rbind(df_out, acc_df); rm(acc_df); dim(df_out)
df_out <- rbind(df_out, spec_df); rm(spec_df); dim(df_out)
df_out <- rbind(df_out, sens_df); rm(sens_df); dim(df_out)




# ------------------------------------------------------------------------------
# PART (3): AC_thresh = 2860

AC_thresh <- 2860

thresh_vec <- dat_fitted %>% filter(AC == AC_thresh) %>% unlist()
names(thresh_vec) <- gsub("_fitted", "", names(thresh_vec))
thresh_vec
thresh_vec %>% round(4)

check_df <- 
  dat_acc %>%
  filter(wear_and_valid_flag == 1) %>%
  group_by(subj_id) %>%
  mutate(
    AC_pred   = as.numeric(AC >= thresh_vec["AC"]),
    MIMS_pred = as.numeric(MIMS >= thresh_vec["MIMS"]),
    ENMO_pred = as.numeric(ENMO >= thresh_vec["ENMO"]),
    MAD_pred  = as.numeric(MAD >= thresh_vec["MAD"]),
    AI_pred   = as.numeric(AI >= thresh_vec["AI"])
  ) %>%
  select(all_of(c(names_levels, "subj_id", "AC_pred"))) %>%
  as.data.frame()
head(check_df)

# ACCURACY
acc_df <- 
  check_df %>%
  mutate(
    MIMS = (AC_pred == MIMS_pred) * 1,
    ENMO = (AC_pred == ENMO_pred) * 1,
    MAD  = (AC_pred == MAD_pred) * 1,
    AI   = (AC_pred == AI_pred) * 1
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "accuracy") %>%
  mutate(AC_thresh = AC_thresh) 

# SENSITIVITY
sens_df <- 
  check_df %>%
  filter(AC_pred == 1) %>%
  mutate(
    MIMS = MIMS_pred,
    ENMO = ENMO_pred,
    MAD  = MAD_pred,
    AI   = AI_pred
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "sensitivity") %>%
  mutate(AC_thresh = AC_thresh) 

# SPECIFICITY
spec_df <- 
  check_df %>%
  filter(AC_pred == 0) %>%
  mutate(
    MIMS =  (MIMS_pred == 0) * 1,
    ENMO = (ENMO_pred == 0) * 1,
    MAD  = (MAD_pred == 0) * 1,
    AI   = (AI_pred == 0) * 1
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "specificity") %>%
  mutate(AC_thresh = AC_thresh) 

# append results
df_out <- rbind(df_out, acc_df); rm(acc_df); dim(df_out)
df_out <- rbind(df_out, spec_df); rm(spec_df); dim(df_out)
df_out <- rbind(df_out, sens_df); rm(sens_df); dim(df_out)
# [1] 23580     5


# ------------------------------------------------------------------------------
# PART (4): AC_thresh = 3940

AC_thresh <- 3940

thresh_vec <- dat_fitted %>% filter(AC == AC_thresh) %>% unlist()
names(thresh_vec) <- gsub("_fitted", "", names(thresh_vec))
thresh_vec
thresh_vec %>% round(4)

check_df <- 
  dat_acc %>%
  filter(wear_and_valid_flag == 1) %>%
  group_by(subj_id) %>%
  mutate(
    AC_pred   = as.numeric(AC >= thresh_vec["AC"]),
    MIMS_pred = as.numeric(MIMS >= thresh_vec["MIMS"]),
    ENMO_pred = as.numeric(ENMO >= thresh_vec["ENMO"]),
    MAD_pred  = as.numeric(MAD >= thresh_vec["MAD"]),
    AI_pred   = as.numeric(AI >= thresh_vec["AI"])
  ) %>%
  select(all_of(c(names_levels, "subj_id", "AC_pred"))) %>%
  as.data.frame()
head(check_df)

# ACCURACY
acc_df <- 
  check_df %>%
  mutate(
    MIMS = (AC_pred == MIMS_pred) * 1,
    ENMO = (AC_pred == ENMO_pred) * 1,
    MAD  = (AC_pred == MAD_pred) * 1,
    AI   = (AC_pred == AI_pred) * 1
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "accuracy") %>%
  mutate(AC_thresh = AC_thresh) 

# SENSITIVITY
sens_df <- 
  check_df %>%
  filter(AC_pred == 1) %>%
  mutate(
    MIMS = MIMS_pred,
    ENMO = ENMO_pred,
    MAD  = MAD_pred,
    AI   = AI_pred
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "sensitivity") %>%
  mutate(AC_thresh = AC_thresh) 

# SPECIFICITY
spec_df <- 
  check_df %>%
  filter(AC_pred == 0) %>%
  mutate(
    MIMS =  (MIMS_pred == 0) * 1,
    ENMO = (ENMO_pred == 0) * 1,
    MAD  = (MAD_pred == 0) * 1,
    AI   = (AI_pred == 0) * 1
  ) %>%
  select(all_of(c(measures_names, "subj_id"))) %>%
  group_by(subj_id) %>% 
  summarise_all(mean) %>%
  pivot_longer(cols = -subj_id) %>%
  mutate(performance_measure = "specificity") %>%
  mutate(AC_thresh = AC_thresh) 

# append results
df_out <- rbind(df_out, acc_df); rm(acc_df); dim(df_out)
df_out <- rbind(df_out, spec_df); rm(spec_df); dim(df_out)
df_out <- rbind(df_out, sens_df); rm(sens_df); dim(df_out)
# [1] 31440     5


# ------------------------------------------------------------------------------
# SAVE TO FILE 

path_tmp <- paste0(here::here(), "/results/2022-04-23-mapping_threshold_pred_error.rds")
saveRDS(df_out, path_tmp)
rm(df_out)
