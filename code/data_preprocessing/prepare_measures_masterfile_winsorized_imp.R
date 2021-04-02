#' This script performs data imputation for winsorized masterfile via FPCA . 

rm(list = ls())
library(dplyr)
library(data.table)
library(lubridate)
library(refund)
options(scipen=999)

# read minute-level measures master file
dat_fpath <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_fpath) 
dat_acc <- dat_acc %>% 
  mutate(HEADER_TIME_STAMP_date = as.Date(HEADER_TIME_STAMP)) %>%
  as.data.frame() 

length(unique(dat_acc$subj_id))
dim(dat_acc)
nrow(dat_acc) / 1440


# ------------------------------------------------------------------------------
#' create "extended" data frame of the above where each participant-day present
#' in above data also has full grid of minutes (1440)

# create vector of unique hms's
hms_vec <- 
  dat_acc %>%
  group_by(subj_id, HEADER_TIME_STAMP_date) %>%
  mutate(cnt = n()) %>%
  filter(cnt == 1440) %>%
  ungroup() %>%
  arrange(subj_id, HEADER_TIME_STAMP) %>%
  filter(row_number() <= 1440) %>%
  mutate(HEADER_TIME_STAMP_hms = substr(as.character(HEADER_TIME_STAMP), 12, 19)) %>%
  pull(HEADER_TIME_STAMP_hms) 
hms_vec

# create full table
hms_df = data.frame(HEADER_TIME_STAMP_date_hms = hms_vec)
dat_acc_ext0 <- 
  dat_acc %>% 
  select(
    subj_id, 
    file_id,
    visit_id,
    HEADER_TIME_STAMP_date,
  ) %>%
  distinct() %>%
  merge(hms_df, all = TRUE)
# construct the timestamp
dat_acc_ext <- 
  dat_acc_ext0 %>%
  mutate(HEADER_TIME_STAMP_char = paste0(HEADER_TIME_STAMP_date, " ", HEADER_TIME_STAMP_date_hms)) %>%
  arrange(subj_id, HEADER_TIME_STAMP_date, HEADER_TIME_STAMP_date_hms)
dat_acc_ext <- dat_acc_ext %>% 
  mutate(HEADER_TIME_STAMP = lubridate::ymd_hms(HEADER_TIME_STAMP_char))

# join with the acc data 
dat_acc_ext <- 
  dat_acc_ext %>% 
  select(subj_id, file_id, visit_id, HEADER_TIME_STAMP_date, HEADER_TIME_STAMP) %>%
  left_join(dat_acc, by = c("subj_id", "file_id", "visit_id", "HEADER_TIME_STAMP_date", "HEADER_TIME_STAMP")) %>%
  arrange(subj_id, HEADER_TIME_STAMP)

# add info these added are not valid minutes
dat_acc_ext$wear_flag[is.na(dat_acc_ext$wear_flag)] <- 0
dat_acc_ext$valid_flag[is.na(dat_acc_ext$valid_flag)] <- 0
dat_acc_ext$wear_and_valid_flag[is.na(dat_acc_ext$wear_and_valid_flag)] <- 0

# check if the result as expected
dat_acc_ext %>% 
  group_by(subj_id, HEADER_TIME_STAMP_date) %>%
  summarise(cnt = n()) %>%
  pull(cnt) %>%
  table()

nrow(dat_acc_ext) / 1440


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# do the imputation via FPCA

# length of one functional observation
d <- 1440

# vector of valid/invalid [1/0] dlags for minute level data 
wear_and_valid_flag <- dat_acc_ext[, "wear_and_valid_flag"]

# AC 
# define Y vector of data, arranged ascending by (subj_id, HEADER_TIME_STAMP) 
Y_vec <- dat_acc_ext[, "AC"]
# replace Y with NAs where observation is invalid
Y_vec[wear_and_valid_flag == 0] <- NA
# reshape log-transform Y into 
# (n \times d) matrix where each row of which is one functional observation;
# note some contain missing data 
Y_mat <- matrix(Y_vec, byrow = TRUE, ncol = d)
# run FPCA on functional observations 
# Y_mat is (n \times d) matrix where each row of which is one functional observation
fpca_fit <- refund::fpca.face(Y_mat)
Yhat_mat <- fpca_fit$Yhat
dat_acc_ext$AC_FPCAhat <- as.vector(t(Yhat_mat))
summary(dat_acc_ext$AC_FPCAhat)
summary(dat_acc_ext$AC)

# MIMS 
Y_vec <- dat_acc_ext[, "MIMS"]
Y_vec[wear_and_valid_flag == 0] <- NA
Y_mat <- matrix(Y_vec, byrow = TRUE, ncol = d)
fpca_fit <- refund::fpca.face(Y_mat)
Yhat_mat <- fpca_fit$Yhat
dat_acc_ext$MIMS_FPCAhat <- as.vector(t(Yhat_mat))
summary(dat_acc_ext$MIMS_FPCAhat)
summary(dat_acc_ext$MIMS)

# ENMO 
Y_vec <- dat_acc_ext[, "ENMO"]
Y_vec[wear_and_valid_flag == 0] <- NA
Y_mat <- matrix(Y_vec, byrow = TRUE, ncol = d)
fpca_fit <- refund::fpca.face(Y_mat)
Yhat_mat <- fpca_fit$Yhat
dat_acc_ext$ENMO_FPCAhat <- as.vector(t(Yhat_mat))
summary(dat_acc_ext$ENMO_FPCAhat)
summary(dat_acc_ext$ENMO)

# MAD 
Y_vec <- dat_acc_ext[, "MAD"]
Y_vec[wear_and_valid_flag == 0] <- NA
Y_mat <- matrix(Y_vec, byrow = TRUE, ncol = d)
fpca_fit <- refund::fpca.face(Y_mat)
Yhat_mat <- fpca_fit$Yhat
dat_acc_ext$MAD_FPCAhat <- as.vector(t(Yhat_mat))
summary(dat_acc_ext$MAD_FPCAhat)
summary(dat_acc_ext$MAD)

# AI 
Y_vec <- dat_acc_ext[, "AI"]
Y_vec[wear_and_valid_flag == 0] <- NA
Y_mat <- matrix(Y_vec, byrow = TRUE, ncol = d)
fpca_fit <- refund::fpca.face(Y_mat)
Yhat_mat <- fpca_fit$Yhat
dat_acc_ext$AI_FPCAhat <- as.vector(t(Yhat_mat))
summary(dat_acc_ext$AI_FPCAhat)
summary(dat_acc_ext$AI)


# ------------------------------------------------------------------------------
# winsorize the FPCAhat values and cap with 0 from the bottom

# AC 
val_max_tmp <- max(dat_acc_ext$AC, na.rm = TRUE)
dat_acc_ext$AC_FPCAhat[dat_acc_ext$AC_FPCAhat > val_max_tmp] <- val_max_tmp
dat_acc_ext$AC_FPCAhat[dat_acc_ext$AC_FPCAhat < 0] <- 0
# MIMS 
val_max_tmp <- max(dat_acc_ext$MIMS, na.rm = TRUE)
dat_acc_ext$MIMS_FPCAhat[dat_acc_ext$MIMS_FPCAhat > val_max_tmp] <- val_max_tmp
dat_acc_ext$MIMS_FPCAhat[dat_acc_ext$MIMS_FPCAhat < 0] <- 0
# ENMO 
val_max_tmp <- max(dat_acc_ext$ENMO, na.rm = TRUE)
dat_acc_ext$ENMO_FPCAhat[dat_acc_ext$ENMO_FPCAhat > val_max_tmp] <- val_max_tmp
dat_acc_ext$ENMO_FPCAhat[dat_acc_ext$ENMO_FPCAhat < 0] <- 0
# MAD 
val_max_tmp <- max(dat_acc_ext$MAD, na.rm = TRUE)
dat_acc_ext$MAD_FPCAhat[dat_acc_ext$MAD_FPCAhat > val_max_tmp] <- val_max_tmp
dat_acc_ext$MAD_FPCAhat[dat_acc_ext$MAD_FPCAhat < 0] <- 0
# AI 
val_max_tmp <- max(dat_acc_ext$AI, na.rm = TRUE)
dat_acc_ext$AI_FPCAhat[dat_acc_ext$AI_FPCAhat > val_max_tmp] <- val_max_tmp
dat_acc_ext$AI_FPCAhat[dat_acc_ext$AI_FPCAhat < 0] <- 0

summary(dat_acc_ext$AC);   summary(dat_acc_ext$AC_FPCAhat)
summary(dat_acc_ext$MIMS); summary(dat_acc_ext$MIMS_FPCAhat)
summary(dat_acc_ext$ENMO); summary(dat_acc_ext$ENMO_FPCAhat)
summary(dat_acc_ext$MAD);  summary(dat_acc_ext$MAD_FPCAhat)
summary(dat_acc_ext$AI);   summary(dat_acc_ext$AI_FPCAhat)


# ------------------------------------------------------------------------------
# construct the imputed variables  

rm(wear_and_valid_flag)
dat_acc_ext <- 
  dat_acc_ext %>%
  mutate(
    AC_imp     = ifelse(wear_and_valid_flag == 1, AC, AC_FPCAhat),
    MIMS_imp   = ifelse(wear_and_valid_flag == 1, MIMS, MIMS_FPCAhat),
    ENMO_imp   = ifelse(wear_and_valid_flag == 1, ENMO, ENMO_FPCAhat),
    MAD_imp    = ifelse(wear_and_valid_flag == 1, MAD, MAD_FPCAhat),
    AI_imp     = ifelse(wear_and_valid_flag == 1, AI, AI_FPCAhat)
  ) 

# make the checks it is what we wanted 
tmp_flag1 <- dat_acc_ext %>% filter(wear_and_valid_flag == 1)
dim(tmp_flag1)
all(tmp_flag1$AC   == tmp_flag1$AC_imp)
all(tmp_flag1$MIMS == tmp_flag1$MIMS_imp)
all(tmp_flag1$ENMO == tmp_flag1$ENMO_imp)
all(tmp_flag1$MAD  == tmp_flag1$MAD_imp)
all(tmp_flag1$AI   == tmp_flag1$AI_imp)

tmp_flag0 <- dat_acc_ext %>% filter(wear_and_valid_flag == 0) %>%
  arrange(AC)
dim(tmp_flag0)
all(tmp_flag0$AC_FPCAhat   == tmp_flag0$AC_imp)
all(tmp_flag0$MIMS_FPCAhat == tmp_flag0$MIMS_imp)
all(tmp_flag0$ENMO_FPCAhat == tmp_flag0$ENMO_imp)
all(tmp_flag0$MAD_FPCAhat  == tmp_flag0$MAD_imp)
all(tmp_flag0$AI_FPCAhat   == tmp_flag0$AI_imp)

# look up the one with the highest number flagges
subj_id_sub <- 
  tmp_flag0 %>% group_by(subj_id) %>% summarise(cnt = n()) %>% 
  filter(cnt > 200) %>%
  arrange(desc(cnt)) %>%
  pull(subj_id)
for (subj_id_tmp in subj_id_sub){
  plt_df <- dat_acc_ext %>% filter(subj_id == subj_id_tmp)
  vmax <- max(c(plt_df$AC, plt_df$AC_FPCAhat))
  plot(plt_df$AC, col = "grey", type = "l", main = "subj_id_tmp", ylim = c(0, vmax))
  lines(plt_df$AC_FPCAhat, col = "blue", type = "l")
  points(plt_df$wear_and_valid_flag * vmax, col = "red")
}

for (subj_id_tmp in subj_id_sub){
  plt_df <- dat_acc_ext %>% filter(subj_id == subj_id_tmp)
  vmax <- max(c(plt_df$ENMO, plt_df$ENMO_FPCAhat))
  plot(plt_df$ENMO, col = "grey", type = "l", main = "subj_id_tmp", ylim = c(0, vmax))
  lines(plt_df$ENMO_FPCAhat, col = "blue", type = "l")
  points(plt_df$wear_and_valid_flag * vmax, col = "brown")
}
  
  
# ------------------------------------------------------------------------------
# define and save final file 

str(dat_acc_ext)
dat_acc_F <- 
  dat_acc_ext %>%
  select(-c(AC, MIMS, MAD, ENMO, AI)) %>%
  select(-ends_with("_FPCAhat")) %>%
  arrange(subj_id, HEADER_TIME_STAMP)
head(dat_acc_F)
names(dat_acc_F) <- gsub("_imp", "", names(dat_acc_F))
head(dat_acc_F)

nrow(dat_acc_F) / d

# save as data frame
fpath_out <- paste0(here::here(), "/data_processed/2021-04-01-measures_masterfile_winsorized_imp.rds")
saveRDS(dat_acc_F, fpath_out)

# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/data_processed/2021-04-01-measures_masterfile_winsorized_imp.rds /dcl01/smart/data/activity/blsa_mims/data_processed/2021-04-01-measures_masterfile_winsorized_imp.rds 


