
#' Notes: 
#' 
#' cd $mims
#' cd code/data_analysis
#' Rnosave XXX -l mem_free=20G,h_vmem=20G,h_stack=256M -t 1-12 -N JOB_gam_boot

rm(list = ls())
library(tidyverse)
library(mgcv)
library(data.table)
library(pracma)
library(quadprog)
options(scipen=999)


# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath) 
dat_acc <- dat_acc %>% dplyr::filter(wear_and_valid_flag == 1)

# data objects 
subj_id_vec <- unique(dat_acc$subj_id)
subj_id_n   <- length(subj_id_vec)
AC_seq_max  <- round(max(dat_acc$AC))

# newdata objects 
AC_seq <- seq(from = 0, to = AC_seq_max, by = 1)
newdata <- data.frame(AC = AC_seq)
N <- nrow(dat_acc)

# model params
k <- 20


# ------------------------------------------------------------------------------
# read data with threshold 0 sensitivity mapping 

thresh_df <- readRDS(paste0(here::here(), "/results/2021-03-25-measures_mapping_sensitivity_ACthresh_1.rds"))
thresh_df <- thresh_df %>% 
  group_by(measure_name) %>%
  filter(out_accr == max(out_accr)) %>%
  filter(measure_thresh_grid == max(measure_thresh_grid)) %>%
  ungroup() %>%
  as.data.frame()
thresh_vec <- thresh_df$measure_thresh_grid
names(thresh_vec) <- thresh_df$measure_name

  
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# GET MAPPING 

# MIMS 

x0 <- 0
y0 <- as.numeric(thresh_vec["MIMS"])

# make augumented data set
dat_acc_AUGUM <- dat_acc
dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1, ], dat_acc_AUGUM)
dat_acc_AUGUM$AC[1]   <- x0
dat_acc_AUGUM$MIMS[1] <- y0

# get regular spline fit
fit_unconstr <- gam(MIMS ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)

# explicitly construct smooth term's design matrix
sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
# get monotonicity contraint 
F_mat <- mono.con(sm$xp)

G <- list(
  X = sm$X,
  C = matrix(0, 0, 0),      # [0 x 0] matrix (no equality constraints)
  sp = fit_unconstr$sp,     # smoothing parameter estimates (taken from unconstrained model)
  p = sm$xp,                # array of feasible initial parameter estimates
  y = dat_acc_AUGUM$MIMS,           
  w = c(1e12, 1:N * 0 + 1)   # weights for data (put very hight weight on augumenting point)    
)
G$Ain <- F_mat$A    # matrix for the inequality constraints
G$bin <- F_mat$b    # vector for the inequality constraints
G$S   <- sm$S       # list of penalty matrices; The first parameter it penalizes is given by off[i]+1
G$off <- 0          # Offset values locating the elements of M$S in the correct location within each penalty coefficient matrix.  (Zero offset implies starting in first location)

# fit spline (using smoothing parameter estimates from unconstrained fit)
pcls_out <- pcls(G)

# predict 
newdata$MIMS_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out

table(sign(diff(newdata$MIMS_fitted)))
plot(newdata$MIMS_fitted, type = "l")
head(newdata)
# AC MIMS_fitted
# 1  0   0.5999987
# 2  1   0.6061496
# 3  2   0.6123006
# 4  3   0.6184515
# 5  4   0.6246024
# 6  5   0.6307533


# ------------------------------------------------------------------------------
# ENMO 

x0 <- 0
y0 <- as.numeric(thresh_vec["ENMO"])
y0

# make augumented data set
dat_acc_AUGUM <- dat_acc
dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1, ], dat_acc_AUGUM)
dat_acc_AUGUM$AC[1]   <- x0
dat_acc_AUGUM$ENMO[1] <- y0

# get regular spline fit
fit_unconstr <- gam(ENMO ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)

# explicitly construct smooth term's design matrix
sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
# get monotonicity contraint 
F_mat <- mono.con(sm$xp)

G <- list(
  X = sm$X,
  C = matrix(0, 0, 0),      # [0 x 0] matrix (no equality constraints)
  sp = fit_unconstr$sp,     # smoothing parameter estimates (taken from unconstrained model)
  p = sm$xp,                # array of feasible initial parameter estimates
  y = dat_acc_AUGUM$ENMO,           
  w = c(1e12, 1:N * 0 + 1)   # weights for data (put very hight weight on augumenting point)    
)
G$Ain <- F_mat$A    # matrix for the inequality constraints
G$bin <- F_mat$b    # vector for the inequality constraints
G$S   <- sm$S       # list of penalty matrices; The first parameter it penalizes is given by off[i]+1
G$off <- 0          # Offset values locating the elements of M$S in the correct location within each penalty coefficient matrix.  (Zero offset implies starting in first location)

# fit spline (using smoothing parameter estimates from unconstrained fit)
pcls_out <- pcls(G)

# predict 
newdata$ENMO_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out

table(sign(diff(newdata$ENMO_fitted)))
plot(newdata$ENMO_fitted, type = "l")


# ------------------------------------------------------------------------------
# MAD 

x0 <- 0
y0 <- as.numeric(thresh_vec["MAD"])
y0

# make augumented data set
dat_acc_AUGUM <- dat_acc
dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1, ], dat_acc_AUGUM)
dat_acc_AUGUM$AC[1]   <- x0
dat_acc_AUGUM$MAD[1] <- y0

# get regular spline fit
fit_unconstr <- gam(MAD ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)

# explicitly construct smooth term's design matrix
sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
# get monotonicity contraint 
F_mat <- mono.con(sm$xp)

G <- list(
  X = sm$X,
  C = matrix(0, 0, 0),      # [0 x 0] matrix (no equality constraints)
  sp = fit_unconstr$sp,     # smoothing parameter estimates (taken from unconstrained model)
  p = sm$xp,                # array of feasible initial parameter estimates
  y = dat_acc_AUGUM$MAD,           
  w = c(1e12, 1:N * 0 + 1)   # weights for data (put very hight weight on augumenting point)    
)
G$Ain <- F_mat$A    # matrix for the inequality constraints
G$bin <- F_mat$b    # vector for the inequality constraints
G$S   <- sm$S       # list of penalty matrices; The first parameter it penalizes is given by off[i]+1
G$off <- 0          # Offset values locating the elements of M$S in the correct location within each penalty coefficient matrix.  (Zero offset implies starting in first location)

# fit spline (using smoothing parameter estimates from unconstrained fit)
pcls_out <- pcls(G)

# predict 
newdata$MAD_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out

table(sign(diff(newdata$MAD_fitted)))
plot(newdata$MAD_fitted, type = "l")


# ------------------------------------------------------------------------------
# AI 

x0 <- 0
y0 <- as.numeric(thresh_vec["AI"])
y0

# make augumented data set
dat_acc_AUGUM <- dat_acc
dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1, ], dat_acc_AUGUM)
dat_acc_AUGUM$AC[1]   <- x0
dat_acc_AUGUM$AI[1] <- y0

# get regular spline fit
fit_unconstr <- gam(AI ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)

# explicitly construct smooth term's design matrix
sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
# get monotonicity contraint 
F_mat <- mono.con(sm$xp)

G <- list(
  X = sm$X,
  C = matrix(0, 0, 0),      # [0 x 0] matrix (no equality constraints)
  sp = fit_unconstr$sp,     # smoothing parameter estimates (taken from unconstrained model)
  p = sm$xp,                # array of feasible initial parameter estimates
  y = dat_acc_AUGUM$AI,           
  w = c(1e12, 1:N * 0 + 1)   # weights for data (put very hight weight on augumenting point)    
)
G$Ain <- F_mat$A    # matrix for the inequality constraints
G$bin <- F_mat$b    # vector for the inequality constraints
G$S   <- sm$S       # list of penalty matrices; The first parameter it penalizes is given by off[i]+1
G$off <- 0          # Offset values locating the elements of M$S in the correct location within each penalty coefficient matrix.  (Zero offset implies starting in first location)

# fit spline (using smoothing parameter estimates from unconstrained fit)
pcls_out <- pcls(G)

# predict 
newdata$AI_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out

table(sign(diff(newdata$AI_fitted)))
plot(newdata$AI_fitted, type = "l")


# ------------------------------------------------------------------------------
# Save data 

fpath_tmp <- paste0(here::here(), "/results/2021-03-26-mapping_fitted.rds")
saveRDS(newdata, fpath_tmp)





