#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#' cd $mims
#' cd code/data_analysis
#' Rnosave 2021-03-05-get_measures_mapping_boot_curves.R -l mem_free=20G,h_vmem=20G,h_stack=256M -t 1-12 -N JOB_gam_boot

rm(list = ls())
library(tidyverse)
library(mgcv)
library(data.table)
# https://stats.stackexchange.com/questions/197509/how-to-smooth-data-and-force-monotonicity
# library(scam)
options(scipen=999)

theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank()    
    )
}
theme_set(theme_ggpr())

k  <- 20

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
# KNOTS SELECTION

# approach #1 

# MIMS 

# fit model
boot_fit_cr <- gam(MIMS ~ s(AC, bs = "cr", k = k), data = dat_acc)
# predict vals
boot_newdat$y_pred_knots_def <- predict(boot_fit_cr, newdata = boot_newdat)
# get knots locations
knots_val = boot_fit_cr$smooth[[1]]$xp
# plot
ggplot(boot_newdat, aes(x = AC, y = y_pred_knots_def)) + 
  geom_line(size = 1, alpha = 0.7) + 
  scale_y_continuous(limits = c(0, 65)) + 
  labs(x = "AC", y = "MIMS fitted") + 
  geom_vline(xintercept = knots_val, linetype = 2, size = 0.3) 
  

# fit model
boot_fit_cr <- gam(ENMO ~ s(AC, bs = "cr", k = k), data = dat_acc)
# predict vals
boot_newdat$y_pred_knots_def <- predict(boot_fit_cr, newdata = boot_newdat)
# get knots locations
knots_val = boot_fit_cr$smooth[[1]]$xp
# plot
ggplot(boot_newdat, aes(x = AC, y = y_pred_knots_def)) + 
  geom_line(size = 1, alpha = 0.7) + 
  # scale_y_continuous(limits = c(0, 65)) + 
  labs(x = "AC", y = "ENMO fitted") + 
  geom_vline(xintercept = knots_val, linetype = 2, size = 0.3) 


# -----------------------------------------------------
# approach #2

sm        <- smoothCon(s(AC, k = 19, bs = "cr"), data = dat_acc)[[1]]
xp_new    <- sort(c(sm$xp, c(12000)))
knots_new <- data.frame(AC = xp_new)


# MIMS 
# fit model
boot_fit_cr <- gam(MIMS ~ s(AC, bs = "cr", k = 20), knots = knots_new, data = dat_acc)
# predict vals
boot_newdat$y_pred_knots_def <- predict(boot_fit_cr, newdata = boot_newdat)
# get knots locations
knots_val = boot_fit_cr$smooth[[1]]$xp
# plot
ggplot(boot_newdat, aes(x = AC, y = y_pred_knots_def)) + 
  geom_line(size = 1, alpha = 0.7) + 
  scale_y_continuous(limits = c(0, 65)) + 
  labs(x = "AC", y = "MIMS fitted") + 
  geom_vline(xintercept = knots_val, linetype = 2, size = 0.3) 


# fit model
boot_fit_cr <- gam(ENMO ~ s(AC, bs = "cr", k = 20), knots = knots_new, data = dat_acc)
# predict vals
boot_newdat$y_pred_knots_def <- predict(boot_fit_cr, newdata = boot_newdat)
# get knots locations
knots_val = boot_fit_cr$smooth[[1]]$xp
# plot
ggplot(boot_newdat, aes(x = AC, y = y_pred_knots_def)) + 
  geom_line(size = 1, alpha = 0.7) + 
  # scale_y_continuous(limits = c(0, 65)) + 
  labs(x = "AC", y = "ENMO fitted") + 
  geom_vline(xintercept = knots_val, linetype = 2, size = 0.3) 



# -----------------------------------------------------
# approach #3

sm        <- smoothCon(s(AC, k = 17, bs = "cr"), data = dat_acc)[[1]]
xp_new    <- sort(c(sm$xp, c(10000, 12000, 14000)))
knots_new <- data.frame(AC = xp_new)


# MIMS 
# fit model
boot_fit_cr <- gam(MIMS ~ s(AC, bs = "cr", k = 20), knots = knots_new, data = dat_acc)
# predict vals
boot_newdat$y_pred_knots_def <- predict(boot_fit_cr, newdata = boot_newdat)
# get knots locations
knots_val = boot_fit_cr$smooth[[1]]$xp
# plot
ggplot(boot_newdat, aes(x = AC, y = y_pred_knots_def)) + 
  geom_line(size = 1, alpha = 0.7) + 
  scale_y_continuous(limits = c(0, 65)) + 
  labs(x = "AC", y = "MIMS fitted") + 
  geom_vline(xintercept = knots_val, linetype = 2, size = 0.3) 


# fit model
boot_fit_cr <- gam(ENMO ~ s(AC, bs = "cr", k = 20), knots = knots_new, data = dat_acc)
# predict vals
boot_newdat$y_pred_knots_def <- predict(boot_fit_cr, newdata = boot_newdat)
# get knots locations
knots_val = boot_fit_cr$smooth[[1]]$xp
# plot
ggplot(boot_newdat, aes(x = AC, y = y_pred_knots_def)) + 
  geom_line(size = 1, alpha = 0.7) + 
  # scale_y_continuous(limits = c(0, 65)) + 
  labs(x = "AC", y = "ENMO fitted") + 
  geom_vline(xintercept = knots_val, linetype = 2, size = 0.3) 



# -----------------------------------------------------
# approach #2b

sm0        <- smoothCon(s(AC, k = 19, bs = "cr"), data = dat_acc)[[1]]
xp_new    <- sort(c(sm0$xp[-c(2,3,5)], c(12000)))
knots_new <- data.frame(AC = xp_new)

# ENMO
# fit model
boot_fit_cr <- gam(ENMO ~ s(AC, bs = "cr", k = 17), knots = knots_new, data = dat_acc)

# predict vals
boot_newdat <- data.frame(AC = AC_seq)
boot_newdat$y_pred <- predict(boot_fit_cr, newdata = boot_newdat)
# get knots locations
knots_val = boot_fit_cr$smooth[[1]]$xp
# plot
ggplot(boot_newdat, aes(x = AC, y = y_pred)) + 
  geom_line(size = 1, alpha = 0.7) + 
  # scale_y_continuous(limits = c(0, 65)) + 
  labs(x = "AC", y = "ENMO fitted") + 
  geom_vline(xintercept = knots_val, linetype = 2, size = 0.3) 


boot_newdat <- data.frame(AC = AC_seq)
# boot_newdat <- data.frame(AC = seq(0, 5000, by = 0.5))
boot_newdat$y_pred <- predict(boot_fit_cr, newdata = boot_newdat)

sign(diff(boot_newdat$y_pred))


# -----------------------------------------------------
# plot histogram of the data 

ggplot(dat_acc, aes(x = AC)) + 
  geom_histogram(fill = "grey", color = "black", alpha = 0.5,
                 binwidth = 100) + 
  labs(x = "", y = "")

ggplot(dat_acc %>% filter(AC > 0), 
       aes(x = AC)) + 
  geom_histogram(fill = "grey", color = "black", alpha = 0.5,
                 binwidth = 100, size = 0.1) + 
  labs(x = "AC (excluding vals AC=0)", y = "")




# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# KNOTS SELECTION + prediction


f.ug <- gam(MIMS ~ s(AC, k=10, bs="cr"), dat = dat_acc)

# Create Design matrix, constraints etc. for monotonic spline....
sm <- smoothCon(s(AC,k=10,bs="cr"),dat_acc,knots=NULL)[[1]]
F <- mono.con(sm$xp);   # get constraints

G <- list(X=sm$X,
          C=matrix(0,0,0,0),
          sp=f.ug$sp,
          p=sm$xp,
          y=dat_acc$MIMS,
          w = dat_acc$MIMS*0+1)         # A vector of weights for the data
G$Ain <- F$A                 # Matrix for the inequality constraints A_in p > b.
G$bin <- F$b                 # vector in the inequality constraints
G$S   <- sm$S                # A list of penalty matrices
G$off <- 0



# The function returns an array containing the estimated parameter vector.
t1 <- Sys.time()
p <- pcls(G);  # fit spline (using s.p. from unconstrained fit)
t2 <- Sys.time()
t2 - t1



