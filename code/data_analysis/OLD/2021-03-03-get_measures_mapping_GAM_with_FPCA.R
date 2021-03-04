
#' Script to generate mapping between minute-level activity measures. 
#' The proposed approach is as follows: 
#' 
#' 1. Fit GAM model yi = f(xi) + εi, i = 1,..., N (assume data independence; k=10)
#' 2. FPCA on residuals 
#' - consider residuals from the above model 
#' - get i-th individual-specific GAM fit of residuals: residj = f(xj) + εj, j = 1,..., n_i  
#' - get i-th individual-specific GAM-fitted values of residual for equally spaced X domain
#' - use all individual-specific GAM-fitted curves in FPCA
#' - pull i.e. 1-2 first FPCs 
#' 3. Fit yij = f(xij) + resid_FPC1_i(xij) + resid_FPC2_i(xij) + εij 
#' 
#' Input: 
#' - /data_processed/2021-03-03-measures_masterfile_winsorized.rds
#' 
#' Note: 
#' - To get needed files available locally, use
#'   get /dcl01/smart/data/activity/blsa_mims/data_processed/2021-03-03-measures_masterfile_winsorized.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/data_processed/2021-03-03-measures_masterfile_winsorized.rds

   

rm(list = ls())
library(tidyverse)
library(mgcv)
library(ggsci) # https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html
library(cowplot)
library(fdapace)

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-03-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath)
dat_acc <- filter(dat_acc, wear_and_valid_flag == 1)
dat_acc <- mutate(dat_acc, subj_id_F = as.factor(subj_id))
dat_acc <- as.data.frame(dat_acc)

names(dat_acc)
dim(dat_acc)
length(unique(dat_acc$file_id))
length(unique(dat_acc$subj_id))

# meta summaries: subj ID 
subj_id_vec   <- unique(dat_acc$subj_id)
subj_id_vec_l <- length(subj_id_vec)
subj_id_vec_F <- unique(dat_acc$subj_id_F)
# AC sequence
AC_seq   <- seq(from = 0, to = round(max(dat_acc$AC)), by = 1)
AC_seq_l <- length(AC_seq)


##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
##  MIMS

k_indep <- 10
k_resid <- 10

# fit GAM model to all data assuming independence 
t1 <- Sys.time()
fit_MIMS <- mgcv::bam(MIMS ~ s(AC, bs = "cr", k = k_indep), data = dat_acc, method = "fREML")
t2 <- Sys.time(); (t2 - t1)
# Time difference of 14.81344 secs
plot(fit_MIMS)
length(fit_MIMS$residuals)
dat_acc$MIMS_resid <- fit_MIMS$residuals

# generate one residual fit per individual
# Lt <- lapply(1 : subj_id_vec_l, function(i) return(AC_seq))
# Ln <- 1000
Lt <- vector("list", length = subj_id_vec_l)
Ly <- vector("list", length = subj_id_vec_l)
AC_seq <- seq(0, round(max(dat_acc$AC)))
newdata <- data.frame(AC = AC_seq)
for (i in 1 : subj_id_vec_l){ # i <- 1
  print(i)
  subj_id_i <- subj_id_vec[i]
  dat_acc_i <- dat_acc[dat_acc$subj_id == subj_id_i, ]
  fit_resid_i <- gam(MIMS_resid ~ s(AC, bs = "cr", k = k_resid), data = dat_acc_i)
  # Lt[[i]] <- seq(0, max(dat_acc_i$AC), length.out = Ln)
  Lt[[i]] <- AC_seq
  Ly[[i]] <- predict(fit_resid_i, newdata = newdata)
}

# fit FPCA on residuals
t1 <- Sys.time()
FPCA_out <- FPCA(Ly, Lt)
t2 <- Sys.time()
t2 - t1
# Time difference of 2.098218 mins

# get mapping from x_ij to FPC_1, 
# use mapping from x_ij to FPC_1, to get FPC_1 covariate in the data set 
dim(FPCA_out$phi)
FPCA_out_df <-  data.frame(x = FPCA_out$workGrid, y = FPCA_out$phi[, 1])
plot(FPCA_out_df, main = "MIMS_resid_FPC1")
FPCA_map <- gam(y ~ s(x, bs = "cr", k = 50), data = FPCA_out_df)
dat_acc$MIMS_resid_FPC1 <- predict(FPCA_map, newdata = data.frame(x = dat_acc$AC))

# fit model for few individuals
t1 <- Sys.time()
fit_MIMS_bam <- 
  mgcv::bam(MIMS ~ s(AC, bs = "cr", k = 10) + s(subj_id_F, by = MIMS_resid_FPC1, bs = "re"),
            method = "fREML", discrete = TRUE, data = dat_acc)
t2 <- Sys.time()
t2 - t1
plot(fit_MIMS_bam, main = "plot(fit_MIMS_bam)")

library(tidymv)

plot_smooths(
  model = fit_MIMS_bam,
  series = AC
)


fit_MIMS_gamm_n50$coefficients




# ------------------------------------------------------------------------------

t1 <- Sys.time()
fit_MIMS_gamm_n5 <- gamm(MIMS ~ s(AC, bs = "cr", k = 10), 
                          data = dat_acc %>% filter(subj_id_F %in% subj_id_vec_F[1:5]),
                          random = list(subj_id_F = ~ 0 + MIMS_resid_FPC1)
)
t2 <- Sys.time()
t2 - t1
# n=10
# Time difference of XXX mins

plt_df <- data.frame(x = c(5, 10, 20),
                     y = c(10.71879, 18.58492, 35.4154))

ggplot(plt_df, aes(x = x, y = y)) + 
  geom_line(linetype = 2, alpha = 0.7) + 
  geom_point(size = 3) + 
  labs(x = "n subjects used", 
       y = "gamm() execution time [minutes]") + 
  scale_x_continuous(limits = c(0, 20)) + 
  scale_y_continuous(limits = c(0, NA)) + 
  theme_grey(base_size = 20)











t1 <- Sys.time()
fit_MIMS_gamm20 <- gamm(MIMS ~ s(AC, bs = "cr", k = 10), 
                      data = dat_acc %>% filter(subj_id_F %in% subj_id_vec_F[1:20]),
                      random = list(subj_id_F = ~ 0 + MIMS_resid_FPC1)
)
t2 <- Sys.time()
t2 - t1




