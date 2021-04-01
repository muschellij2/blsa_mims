#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#' cd $mims
#' cd code/data_analysis
#' Rnosave 2021-03-05-get_measures_mapping_boot_curves.R -l mem_free=20G,h_vmem=20G,h_stack=256M -t 1-12 -N JOB_gam_boot

rm(list = ls())
library(tidyverse)
library(mgcv)
library(data.table)
library(pracma)
library(quadprog)
options(scipen=999)

theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank())}
theme_set(theme_ggpr())


# ------------------------------------------------------------------------------
# READ DATA 

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-03-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath) 
dat_acc <- dat_acc %>% dplyr::filter(wear_and_valid_flag == 1)

subj_id_vec <- unique(dat_acc$subj_id)
subj_id_n   <- length(subj_id_vec)
AC_seq_max  <- round(max(dat_acc$AC))

AC_seq <- seq(from = 0, to = AC_seq_max, by = 1)
boot_newdat <- data.frame(AC = AC_seq)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# MIMS 

k <- 20

t1 <- Sys.time()
boot_fit_cr <- gam(MIMS ~ s(AC, bs = "cr", k = k), data = dat_acc)
t2 <- Sys.time()
t2 - t1
# Time difference of 30.53414 secs
boot_newdat$y_pred_fit0 <- predict(boot_fit_cr, newdata = boot_newdat)


# SO -- approach #1  -----------------------------------------------------------

# make augumented data set
dat_acc_AUGUM <- dat_acc
dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1, ], dat_acc_AUGUM)
dat_acc_AUGUM$MIMS[1] <- 0
dat_acc_AUGUM$AC[1]   <- 0
N <- nrow(dat_acc)

# Show regular spline fit (and save fitted object)
f.ug <- gam(MIMS ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)

# explicitly construct smooth term's design matrix
sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
F <- mono.con(sm$xp)

G <- list(
  X=sm$X,
  C=matrix(0,0,0),  # [0 x 0] matrix (no equality constraints)
  sp=f.ug$sp,       # smoothing parameter estimates (taken from unconstrained model)
  p=sm$xp,          # array of feasible initial parameter estimates
  y=dat_acc_AUGUM$MIMS,           
  w= c(1e8, 1:N * 0 + 1)  # weights for data    
)
G$Ain <- F$A        # matrix for the inequality constraints
G$bin <- F$b        # vector for the inequality constraints
G$S <- sm$S         # list of penalty matrices; The first parameter it penalizes is given by off[i]+1
G$off <- 0          # Offset values locating the elements of M$S in the correct location within each penalty coefficient matrix.  (Zero offset implies starting in first location)

t1 <- Sys.time()
p <- pcls(G);       # fit spline (using smoothing parameter estimates from unconstrained fit)
t2 <- Sys.time()
t2 - t1
# Time difference of 1.000476 mins

# predict 
boot_newdat$y_pred_fit1 <- Predict.matrix(sm, data.frame(AC = boot_newdat$AC)) %*% p
# plot
plot(y_pred_fit0 ~ AC, data = boot_newdat, type = "l", col = 2, lwd = 2)
lines(y_pred_fit1 ~ AC, data = boot_newdat, col = 4, lwd = 2)
head(boot_newdat, 20)
#    AC y_pred_fit0  y_pred_fit1
# 1   0  0.02097162 0.0004965719
# 2   1  0.02952459 0.0091333284
# 3   2  0.03807754 0.0177700625
# 4   3  0.04663044 0.0264067515
# 5   4  0.05518328 0.0350433729
# 6   5  0.06373603 0.0436799043
# 7   6  0.07228867 0.0523163231
# 8   7  0.08084118 0.0609526068
# 9   8  0.08939354 0.0695887328
# 10  9  0.09794573 0.0782246787
# 11 10  0.10649772 0.0868604219

boot_newdat[1850:1855, ]
# AC y_pred_fit0 y_pred_fit1
# 1850 1849    10.52934    10.52834
# 1851 1850    10.53401    10.53302
# 1852 1851    10.53869    10.53770
# 1853 1852    10.54336    10.54238
# 1854 1853    10.54803    10.54706
# 1855 1854    10.55271    10.55174

table(sign(diff(boot_newdat$y_pred_fit1)))





# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ENMO 

k <- 20

t1 <- Sys.time()
boot_fit_cr <- gam(ENMO ~ s(AC, bs = "cr", k = k), data = dat_acc)
t2 <- Sys.time()
t2 - t1
# Time difference of 30.53414 secs
boot_newdat$y_pred_fit0 <- predict(boot_fit_cr, newdata = boot_newdat)


# SO -- approach #1  -----------------------------------------------------------

# make augumented data set
dat_acc_AUGUM <- dat_acc
dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1, ], dat_acc_AUGUM)
dat_acc_AUGUM$ENMO[1] <- 0
dat_acc_AUGUM$AC[1]   <- 0
N <- nrow(dat_acc)

# Show regular spline fit (and save fitted object)
f.ug <- gam(ENMO ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)

# explicitly construct smooth term's design matrix
sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
F <- mono.con(sm$xp)

G <- list(
  X=sm$X,
  C=matrix(0,0,0),  # [0 x 0] matrix (no equality constraints)
  sp=f.ug$sp,       # smoothing parameter estimates (taken from unconstrained model)
  p=sm$xp,          # array of feasible initial parameter estimates
  y=dat_acc_AUGUM$ENMO,           
  w= c(1e8, 1:N * 0 + 1)  # weights for data    
)
G$Ain <- F$A        # matrix for the inequality constraints
G$bin <- F$b        # vector for the inequality constraints
G$S <- sm$S         # list of penalty matrices; The first parameter it penalizes is given by off[i]+1
G$off <- 0          # Offset values locating the elements of M$S in the correct location within each penalty coefficient matrix.  (Zero offset implies starting in first location)

t1 <- Sys.time()
p <- pcls(G);       # fit spline (using smoothing parameter estimates from unconstrained fit)
t2 <- Sys.time()
t2 - t1
# Time difference of 1.000476 mins

# predict 
boot_newdat$y_pred_fit1 <- Predict.matrix(sm, data.frame(AC = boot_newdat$AC)) %*% p
# plot
plot(y_pred_fit0 ~ AC, data = boot_newdat, type = "l", col = 2, lwd = 2)
lines(y_pred_fit1 ~ AC, data = boot_newdat, col = 4, lwd = 2)

plot(y_pred_fit1 ~ AC, data = boot_newdat, type = "l", col = 4, lwd = 2)

head(boot_newdat, 20)
# AC y_pred_fit0  y_pred_fit1
# 1   0  0.01345137 0.0003250029
# 2   1  0.01344505 0.0003618134
# 3   2  0.01343873 0.0003986236
# 4   3  0.01343241 0.0004354335
# 5   4  0.01342610 0.0004722427
# 6   5  0.01341978 0.0005090511
# 7   6  0.01341346 0.0005458585
# 8   7  0.01340714 0.0005826646
# 9   8  0.01340082 0.0006194692
# 10  9  0.01339451 0.0006562722
# 11 10  0.01338819 0.0006930733
# 12 11  0.01338188 0.0007298723
# 13 12  0.01337557 0.0007666691
# 14 13  0.01336925 0.0008034633
# 15 14  0.01336294 0.0008402548
# 16 15  0.01335663 0.0008770434
# 17 16  0.01335033 0.0009138288
# 18 17  0.01334402 0.0009506109
# 19 18  0.01333772 0.0009873895
# 20 19  0.01333141 0.0010241643

boot_newdat[1850:1855, ]
# AC y_pred_fit0 y_pred_fit1
# 1850 1849  0.02229392  0.02184039
# 1851 1850  0.02230183  0.02185068
# 1852 1851  0.02230973  0.02186098
# 1853 1852  0.02231763  0.02187129
# 1854 1853  0.02232553  0.02188162
# 1855 1854  0.02233343  0.02189197

table(sign(diff(boot_newdat$y_pred_fit1)))
