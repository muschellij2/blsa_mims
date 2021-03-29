
#' Notes: 
#' 
#' cd $mims
#' cd code/data_analysis
#' Rnosave 2021-03-29-get_measures_mapping_FITTED_sensitivity.R -l mem_free=20G,h_vmem=20G,h_stack=256M -t 1-12 -N JOB_fit_sens

rm(list = ls())
library(tidyverse)
library(mgcv)
library(data.table)
# library(pracma)
# library(quadprog)
options(scipen=999)


# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath) 
dat_acc <- dat_acc %>% dplyr::filter(wear_and_valid_flag == 1)

# data objects 
subj_id_vec <- unique(dat_acc$subj_id)
subj_id_n   <- length(subj_id_vec)
AC_seq_max  <- round(max(dat_acc$AC))

# read data with thresholds sensitivity mapping 
thresh_df_all <- data.frame()
fnames <- list.files(paste0(here::here(), "/results/"), full.names = TRUE)
fnames <- fnames[grepl("2021-03-25-measures_mapping_sensitivity", fnames)]
thresh_df <- do.call("rbind", lapply(fnames, readRDS)) 
thresh_df <- thresh_df %>% 
  group_by(measure_name, AC_thresh) %>%
  filter(out_accr == max(out_accr)) %>%
  filter(measure_thresh_grid == max(measure_thresh_grid)) %>%
  ungroup() %>%
  select(AC_thresh, measure_name, measure_thresh_grid) %>%
  as.data.frame()

# model params
k <- 20
AC_seq <- seq(from = 0, to = AC_seq_max, by = 1)
N <- nrow(dat_acc)
weight_grid <- c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# GET MAPPING -- use only one anchor  

# preapre data frame to store results 
out_df <- data.frame()

for (w_tmp in weight_grid){ # w_tmp <- weight_grid[1]
  
  message(paste0("w cnt = 1,  w value = ", w_tmp))
  
  newdata <- data.frame(AC = AC_seq)

  # ------------------------------------------------------------------------------
  # MIMS 
  x0 <- 1
  y0 <- thresh_df %>% filter(AC_thresh == x0, measure_name == "MIMS") %>% pull(measure_thresh_grid)
  
  dat_acc_AUGUM <- dat_acc
  dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1, ], dat_acc_AUGUM)
  dat_acc_AUGUM$AC[1]   <- x0
  dat_acc_AUGUM$MIMS[1] <- y0
  fit_unconstr <- gam(MIMS ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)
  sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
  F_mat <- mono.con(sm$xp)
  G <- list(
    X = sm$X,
    C = matrix(0, 0, 0),      
    sp = fit_unconstr$sp,     
    p = sm$xp,                
    y = dat_acc_AUGUM$MIMS,           
    w = c(w_tmp, 1:N * 0 + 1)     
  )
  G$Ain <- F_mat$A    
  G$bin <- F_mat$b    
  G$S   <- sm$S       
  G$off <- 0          
  pcls_out <- pcls(G)
  newdata$MIMS_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out
  
  # ------------------------------------------------------------------------------
  # ENMO 
  
  x0 <- 1
  y0 <- thresh_df %>% filter(AC_thresh == x0, measure_name == "ENMO") %>% pull(measure_thresh_grid)
  
  dat_acc_AUGUM <- dat_acc
  dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1, ], dat_acc_AUGUM)
  dat_acc_AUGUM$AC[1]   <- x0
  dat_acc_AUGUM$ENMO[1] <- y0
  fit_unconstr <- gam(ENMO ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)
  sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
  F_mat <- mono.con(sm$xp)
  G <- list(
    X = sm$X,
    C = matrix(0, 0, 0),      
    sp = fit_unconstr$sp,     
    p = sm$xp,                
    y = dat_acc_AUGUM$ENMO,           
    w = c(w_tmp, 1:N * 0 + 1)     
  )
  G$Ain <- F_mat$A    
  G$bin <- F_mat$b    
  G$S   <- sm$S       
  G$off <- 0          
  pcls_out <- pcls(G)
  newdata$ENMO_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out
  
  # ------------------------------------------------------------------------------
  # MAD 
  
  x0 <- 1
  y0 <- thresh_df %>% filter(AC_thresh == x0, measure_name == "MAD") %>% pull(measure_thresh_grid)
  
  dat_acc_AUGUM <- dat_acc
  dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1, ], dat_acc_AUGUM)
  dat_acc_AUGUM$AC[1]   <- x0
  dat_acc_AUGUM$MAD[1] <- y0
  fit_unconstr <- gam(MAD ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)
  sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
  F_mat <- mono.con(sm$xp)
  G <- list(
    X = sm$X,
    C = matrix(0, 0, 0),      
    sp = fit_unconstr$sp,     
    p = sm$xp,                
    y = dat_acc_AUGUM$MAD,           
    w = c(w_tmp, 1:N * 0 + 1)     
  )
  G$Ain <- F_mat$A    
  G$bin <- F_mat$b    
  G$S   <- sm$S       
  G$off <- 0          
  pcls_out <- pcls(G)
  newdata$MAD_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out
  
  # ------------------------------------------------------------------------------
  # AI 
  
  x0 <- 1
  y0 <- thresh_df %>% filter(AC_thresh == x0, measure_name == "AI") %>% pull(measure_thresh_grid)
  
  dat_acc_AUGUM <- dat_acc
  dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1, ], dat_acc_AUGUM)
  dat_acc_AUGUM$AC[1]   <- x0
  dat_acc_AUGUM$AI[1] <- y0
  fit_unconstr <- gam(AI ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)
  sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
  F_mat <- mono.con(sm$xp)
  G <- list(
    X = sm$X,
    C = matrix(0, 0, 0),      
    sp = fit_unconstr$sp,     
    p = sm$xp,                
    y = dat_acc_AUGUM$AI,           
    w = c(w_tmp, 1:N * 0 + 1)     
  )
  G$Ain <- F_mat$A    
  G$bin <- F_mat$b    
  G$S   <- sm$S       
  G$off <- 0          
  pcls_out <- pcls(G)
  newdata$AI_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out
  
  # ------------------------------------------------------------------------------
  # append and save current condition
  newdata$w_value <- w_tmp
  newdata$w_cnt   <- 1
  out_df <- rbind(out_df, newdata)
  rm(newdata)
  fpath_tmp <- paste0(here::here(), "/results/2021-03-29-mapping_fitted_sensitivity_1weight.rds")
  saveRDS(out_df, fpath_tmp)
}

rm(out_df)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# GET MAPPING -- use three anchors  

out_df <- data.frame()

for (w_tmp in weight_grid){ # w_tmp <- weight_grid[1]
  
  message(paste0("w cnt = 3,  w value = ", w_tmp))
  
  newdata <- data.frame(AC = AC_seq)
  
  # ------------------------------------------------------------------------------
  # MIMS 
  x0 <- c(1, 1853, 2303)
  y0 <- thresh_df %>% filter(AC_thresh == x0, measure_name == "MIMS") %>% 
    arrange(AC_thresh) %>%
    pull(measure_thresh_grid)
  
  dat_acc_AUGUM <- dat_acc
  dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1:3, ], dat_acc_AUGUM)
  dat_acc_AUGUM$AC[1:3]   <- x0
  dat_acc_AUGUM$MIMS[1:3] <- y0
  fit_unconstr <- gam(MIMS ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)
  sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
  F_mat <- mono.con(sm$xp)
  G <- list(
    X = sm$X,
    C = matrix(0, 0, 0),      
    sp = fit_unconstr$sp,     
    p = sm$xp,                
    y = dat_acc_AUGUM$MIMS,           
    w = c(rep(w_tmp, 3), 1:N * 0 + 1)     
  )
  G$Ain <- F_mat$A    
  G$bin <- F_mat$b    
  G$S   <- sm$S       
  G$off <- 0          
  pcls_out <- pcls(G)
  newdata$MIMS_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out
  
  # ------------------------------------------------------------------------------
  # ENMO 
  x0 <- c(1, 1853, 2303)
  y0 <- thresh_df %>% filter(AC_thresh == x0, measure_name == "ENMO") %>% 
    arrange(AC_thresh) %>%
    pull(measure_thresh_grid)
  
  dat_acc_AUGUM <- dat_acc
  dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1:3, ], dat_acc_AUGUM)
  dat_acc_AUGUM$AC[1:3]   <- x0
  dat_acc_AUGUM$ENMO[1:3] <- y0
  fit_unconstr <- gam(ENMO ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)
  sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
  F_mat <- mono.con(sm$xp)
  G <- list(
    X = sm$X,
    C = matrix(0, 0, 0),      
    sp = fit_unconstr$sp,     
    p = sm$xp,                
    y = dat_acc_AUGUM$ENMO,           
    w = c(rep(w_tmp, 3), 1:N * 0 + 1)     
  )
  G$Ain <- F_mat$A    
  G$bin <- F_mat$b    
  G$S   <- sm$S       
  G$off <- 0          
  pcls_out <- pcls(G)
  newdata$ENMO_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out
  
  # ------------------------------------------------------------------------------
  # MAD 
  x0 <- c(1, 1853, 2303)
  y0 <- thresh_df %>% filter(AC_thresh == x0, measure_name == "MAD") %>% 
    arrange(AC_thresh) %>%
    pull(measure_thresh_grid)
  
  dat_acc_AUGUM <- dat_acc
  dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1:3, ], dat_acc_AUGUM)
  dat_acc_AUGUM$AC[1:3]   <- x0
  dat_acc_AUGUM$MAD[1:3] <- y0
  fit_unconstr <- gam(MAD ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)
  sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
  F_mat <- mono.con(sm$xp)
  G <- list(
    X = sm$X,
    C = matrix(0, 0, 0),      
    sp = fit_unconstr$sp,     
    p = sm$xp,                
    y = dat_acc_AUGUM$MAD,           
    w = c(rep(w_tmp, 3), 1:N * 0 + 1)     
  )
  G$Ain <- F_mat$A    
  G$bin <- F_mat$b    
  G$S   <- sm$S       
  G$off <- 0          
  pcls_out <- pcls(G)
  newdata$MAD_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out
  
  # ------------------------------------------------------------------------------
  # AI 
  x0 <- c(1, 1853, 2303)
  y0 <- thresh_df %>% filter(AC_thresh == x0, measure_name == "AI") %>% 
    arrange(AC_thresh) %>%
    pull(measure_thresh_grid)
  
  dat_acc_AUGUM <- dat_acc
  dat_acc_AUGUM <- rbind(dat_acc_AUGUM[1:3, ], dat_acc_AUGUM)
  dat_acc_AUGUM$AC[1:3]   <- x0
  dat_acc_AUGUM$AI[1:3] <- y0
  fit_unconstr <- gam(AI ~ s(AC, k = k, bs = "cr"), data = dat_acc_AUGUM)
  sm <- smoothCon(s(AC, k = k, bs = "cr"), dat_acc_AUGUM, knots = NULL)[[1]]
  F_mat <- mono.con(sm$xp)
  G <- list(
    X = sm$X,
    C = matrix(0, 0, 0),      
    sp = fit_unconstr$sp,     
    p = sm$xp,                
    y = dat_acc_AUGUM$AI,           
    w = c(rep(w_tmp, 3), 1:N * 0 + 1)     
  )
  G$Ain <- F_mat$A    
  G$bin <- F_mat$b    
  G$S   <- sm$S       
  G$off <- 0          
  pcls_out <- pcls(G)
  newdata$AI_fitted <- Predict.matrix(sm, data.frame(AC = newdata$AC)) %*% pcls_out
  
  
  # ------------------------------------------------------------------------------
  # append and save current condition
  newdata$w_value <- w_tmp
  newdata$w_cnt   <- 3
  out_df <- rbind(out_df, newdata)
  rm(newdata)
  fpath_tmp <- paste0(here::here(), "/results/2021-03-29-mapping_fitted_sensitivity_3weight.rds")
  saveRDS(out_df, fpath_tmp)
}








