
#' Script to generate population-level day trajectory to allow to impute 
#' missing values data. 

rm(list = ls())
library(tidyverse)
library(refund)
library(lubridate)

#' refund::fpca.face
#' 
#' - an n × d matrix Y, each row of which is one functional observation, 
#'   with missing values allowed; or
#'   
#' - a data frame ydata, with columns 
#'   '.id' (which curve the point belongs to, say i), 
#'   '.index' (function argument such as time point t), 
#'   '.value' (observed function value Yi(t)).


# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-03-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath) %>% 
  mutate(HEADER_TIME_STAMP_date = as.Date(HEADER_TIME_STAMP)) %>%
  as.data.frame() 
dat_acc <- dat_acc %>% arrange(subj_id, HEADER_TIME_STAMP)
dim(dat_acc)
# 6147240


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# define "extended" data frame of the above, which includes full grid of minutes
# for each participant-day

# create vector of unique minutes
hms_vec <- 
  dat_acc %>%
  group_by(subj_id, HEADER_TIME_STAMP_date) %>%
  mutate(cnt = n()) %>%
  filter(cnt == 1440) %>%
  ungroup() %>%
  filter(row_number() <= 1440) %>%
  mutate(HEADER_TIME_STAMP_hms = strftime(HEADER_TIME_STAMP, format="%H:%M:%S")) %>%
  pull(HEADER_TIME_STAMP_hms)
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

dat_acc_ext <- 
  dat_acc_ext0 %>%
  mutate(HEADER_TIME_STAMP = lubridate::ymd_hms(paste0(HEADER_TIME_STAMP_date, " ", HEADER_TIME_STAMP_date_hms))) %>%
  select(-HEADER_TIME_STAMP_date_hms)

dat_acc_ext <- 
  dat_acc_ext %>% 
  select(subj_id, file_id, visit_id, HEADER_TIME_STAMP_date, HEADER_TIME_STAMP) %>%
  left_join(dat_acc, by = c("subj_id", "file_id", "visit_id", "HEADER_TIME_STAMP_date", "HEADER_TIME_STAMP")) %>%
  arrange(subj_id, HEADER_TIME_STAMP)

# add info these added are not valid minutes
dat_acc_ext$wear_flag[is.na(dat_acc_ext$wear_flag)] <- 0
dat_acc_ext$wear_and_valid_flag[is.na(dat_acc_ext$wear_and_valid_flag)] <- 0

# check if the result as expected
dat_acc_ext %>% 
  group_by(subj_id, HEADER_TIME_STAMP_date) %>%
  summarise(cnt = n()) %>%
  pull(cnt) %>%
  table()

d <- 1440
nrow(dat_acc_ext) / d

# n <- 100
# d <- 1440
# Y_orig <- 1 : (n * d) 
# Y_orig_mat <- matrix(Y_orig, byrow = TRUE, ncol = d)
# Y_orig_mat_back <- as.vector(t(Y_orig_mat))



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# impute data 

# length of one functional observation
d <- 1440

# vector of valid/invalid [1/0] dlags for minute level data 
wear_and_valid_flag <- dat_acc_ext[, "wear_and_valid_flag"]

# AC 
Y_tmp <- "AC"
# define Y vector of data, arranged ascending by (subj_id, HEADER_TIME_STAMP) 
Y_vec <- dat_acc_ext[, Y_tmp]
# replace Y with NAs where observation is invalid
Y_vec[wear_and_valid_flag == 0] <- NA
# log-transform Y 
Yl_vec <- log(Y_vec + 1)
# reshape log-transform Y into 
# (n \times d) matrix where each row of which is one functional observation;
# note some contain missing data 
Yl_mat <- matrix(Yl_vec, byrow = TRUE, ncol = d)
Yl_inx_na <- which(is.na(Yl_mat))

# run FPCA on functional observations 
# Yl_mat is (n \times d) matrix where each row of which is one functional observation
# fpca_out <- refund::fpca.face(Yl_mat, center = TRUE, knots = 35, pve = 0.95)
t1 <- Sys.time()
fpca_fit <- refund::fpca.face(Yl_mat)
Sys.time() - t1
fpca_fit_Yhat <- fpca_fit$Yhat

# # exemplary participant-day: plot 1 
# plot(Yl_mat[1, ], main = "Yl_mat[1, ]", ylab = "log(AC+1)")
# plot(fpca_fit_Yhat[1, ], main = "fpca_fit_Yhat[1, ]", ylab = "log(AC+1)")
# # exemplary participant-day: plot 2 
# plot(Yl_mat[91, ], main = "Yl_mat[91, ]", ylab = "log(AC+1)")
# plot(fpca_fit_Yhat[91, ], main = "fpca_fit_Yhat[91, ]", ylab = "log(AC+1)")



pmax(0,fpca_fit$Yhat[inx_na])

# get estimated mean function
Y_mu <- fpca_out$mu

plot(fpca_out$mu, type = "l", main = "fpca_out$mu", ylab ="Y=log(AC+1)")
plot(matrixStats::colMeans2(Yl_mat, na.rm = TRUE), type = "l", main = "colMeans(Y_mat)", ylab ="Y=log(AC+1)")



# replace missing values in log-transform Y matrix with population mean 
for (i in 1 : nrow(Y_transf_mat)){ # i <- 1
  print(i)
  obs_i <- Y_transf_mat[i, ]
  obs_i_whichNA <- which(is.na(obs_i))
  if (length(obs_i_whichNA) > 0){
    print(paste0("length(obs_i_whichNA): ", length(obs_i_whichNA)))
    obs_i[obs_i_whichNA] <- Y_mu[Y_mu]
    Y_transf_mat[i, ] <- obs_i
  }
}








Y_transf_mat_agg <- matrixStats::colMeans2(Y_transf_mat, na.rm = TRUE)
Y_mat_agg        <- matrixStats::colMeans2(Y_mat, na.rm = TRUE)
plot(Y_transf_mat_agg, main = "Y_transf_mat_agg", type = "l", ylim = c(0, max(Y_transf_mat_agg)))
plot(Y_mat_agg, main = "Y_transf_mat_agg", type = "l", ylim = c(0, max(Y_mat_agg)))
