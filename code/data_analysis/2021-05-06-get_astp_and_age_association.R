
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
fpath_tmp <- paste0(here::here(), "/data_processed/2021-05-06-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc0 <- readRDS(fpath_tmp) %>% as.data.frame()
dat_acc <- dat_acc0 %>% 
  left_join(mastervisit, by = c("subj_id", "visit_id")) %>%
  arrange(subj_id, HEADER_TIME_STAMP)
str(dat_acc)

# read the derived mapping
fpath_tmp <- paste0(here::here(), "/results_public/mapping_between_measures_FITTED.txt")
dat_fitted <- fread(fpath_tmp) %>% as.data.frame()
# threshold for non-dominant hand 
thresh_vec <- dat_fitted %>% filter(AC == 1853) %>% unlist()
names(thresh_vec) <- gsub("_fitted", "", names(thresh_vec))
thresh_vec
#            AC          MIMS          ENMO           MAD            AI 
# 1853.00000000   10.55761277    0.02208213    0.03902071    3.62010700 



# # get the thresholds from sensitivity analysis 
# dat_acc$AC_active <- as.numeric(dat_acc$AC > 1853)
# optimize_accuracy <- function(measure_grid, measure_name){
#   acc_grid <- numeric()
#   for (i in 1:length(measure_grid)){
#     print(i)
#     dat_acc$measure_active <- as.numeric(dat_acc[, measure_name] > measure_grid[i])
#     acc_grid[i] <- mean(dat_acc$AC_active == dat_acc$measure_active)
#   }
#   accur_df <- data.frame(accuracy = acc_grid, measure_values = measure_grid) %>%
#     arrange(desc(accuracy))
#   return(accur_df)
# }
# 
# MIMS_opt <- optimize_accuracy(seq(10, 11, by = 0.001), "MIMS")
# # ENMO_opt <- optimize_accuracy(seq(0.018, 0.036, length.out=100), "ENMO")
# # MAD_opt  <- optimize_accuracy(seq(0.035, 0.045, length.out=100), "MAD")
# # AI_opt   <- optimize_accuracy(seq(3.52, 3.72, length.out=100), "AI")
# 
# MIMS_opt %>% head(5)
# # ENMO_opt %>% head(5)
# # MAD_opt  %>% head(5)
# # AI_opt   %>% head(5)



# ------------------------------------------------------------------------------
# functions to compute TP metrics

get_ASTP <- function(x, thresh_tmp){
  is_active <- as.numeric(x >= thresh_tmp) 
  rle_out <- rle(is_active)
  ## Vector of lengths of all active bouts
  actbout_len_vec  <- rle_out$lengths[which(rle_out$values == 1)]
  astp <- 1/mean(actbout_len_vec)
  return(astp)
}

get_SATP <- function(x, thresh_tmp){
  is_active <- as.numeric(x >= thresh_tmp) 
  rle_out <- rle(is_active)
  ## Vector of lengths of all active bouts
  nonactbout_len_vec <- rle_out$lengths[which(rle_out$values == 0)]
  satp <- 1/mean(nonactbout_len_vec)
  return(satp)
}

# all type bout length
get_bout_len <- function(x, thresh_tmp){
  is_active <- as.numeric(x >= thresh_tmp)
  rle_out <- rle(is_active)
  mean(rle_out$lengths)
}



# ------------------------------------------------------------------------------
# compute metrics 

# STEP 1: proportion active 
df_perc_active <- 
  dat_acc %>%
  filter(wear_and_valid_flag == 1) %>%
  group_by(subj_id) %>% 
  summarise(
    AC_active   = mean(AC >= thresh_vec["AC"]),
    MIMS_active = mean(MIMS >= thresh_vec["MIMS"]),
    ENMO_active = mean(ENMO >= thresh_vec["ENMO"]),
    MAD_active  = mean(MAD >= thresh_vec["MAD"]),
    AI_active   = mean(AI >= thresh_vec["AI"])
  ) %>%
  mutate(
    AC_active   = AC_active * 100,
    MIMS_active = MIMS_active * 100,
    ENMO_active = ENMO_active * 100,
    MAD_active  = MAD_active * 100,
    AI_active   = AI_active * 100
  )

# STEP 2: active to sedentary TP
df_ASTP <- 
  dat_acc %>%
  group_by(subj_id) %>%
  summarise(
    AC_astp   = get_ASTP(AC, thresh_vec["AC"]),
    MIMS_astp = get_ASTP(MIMS, thresh_vec["MIMS"]),
    ENMO_astp = get_ASTP(ENMO, thresh_vec["ENMO"]),
    MAD_astp  = get_ASTP(MAD, thresh_vec["MAD"]),
    AI_astp   = get_ASTP(AI, thresh_vec["AI"])
  ) %>%
  as.data.frame()


# STEP 3: sedentary to active TP
df_SATP <- 
  dat_acc %>%
  group_by(subj_id) %>%
  summarise(
    AC_satp   = get_SATP(AC, thresh_vec["AC"]),
    MIMS_satp = get_SATP(MIMS, thresh_vec["MIMS"]),
    ENMO_satp = get_SATP(ENMO, thresh_vec["ENMO"]),
    MAD_satp  = get_SATP(MAD, thresh_vec["MAD"]),
    AI_satp   = get_SATP(AI, thresh_vec["AI"])
  ) %>%
  as.data.frame()


# STEP 4: averag bout length 
df_bout_len <- 
  dat_acc %>%
  group_by(subj_id) %>%
  summarise(
    AC_bout_length   = get_bout_len(AC, thresh_vec["AC"]),
    MIMS_bout_length = get_bout_len(MIMS, thresh_vec["MIMS"]),
    ENMO_bout_length = get_bout_len(ENMO, thresh_vec["ENMO"]),
    MAD_bout_length  = get_bout_len(MAD, thresh_vec["MAD"]),
    AI_bout_length   = get_bout_len(AI, thresh_vec["AI"])
  ) %>%
  as.data.frame()


# STEP 5: add age and meta data 
df_age <- 
  dat_acc %>%
  group_by(subj_id, age) %>%
  summarise(imputed_cnt = sum(wear_and_valid_flag == 0))

# merge 
df_out <- 
  df_age %>%
  left_join(df_perc_active, by = "subj_id") %>%
  left_join(df_ASTP, by = "subj_id") %>%
  left_join(df_SATP, by = "subj_id") %>%
  left_join(df_bout_len, by = "subj_id") %>%
  as.data.frame()
dim(df_out)
head(df_out)

# save results
path_tmp <- paste0(here::here(), "/results/2021-05-06-astp_and_age_association.rds")
saveRDS(df_out, path_tmp)
rm(df_out)









