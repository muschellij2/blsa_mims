
#' Script reads pre-computed GAM fitting results and uses it for implementing 
#' the mappings: 
#' 
#' mapping g: {MIMS, ENMO, MAD, AI} -> AC
#' mapping f: AC -> {MIMS, ENMO, MAD, AI}

library(tidyverse)
library(data.table)

# read data with GAM-fitted values
fpath_tmp <- "https://raw.githubusercontent.com/muschellij2/blsa_mims/master/results_public/mapping_between_measures_FITTED.txt"
dat_fitted <- fread(fpath_tmp) 


# ------------------------------------------------------------------------------
# mapping g: {MIMS, ENMO, MAD, AI} -> AC

# define data tables based on pre-computed values for fast reference mapping
# MIMS 
MIMS_to_AC_dt <- dat_fitted %>% select(AC, merge = MIMS_fitted) %>% as.data.table()
setkeyv(MIMS_to_AC_dt, c("merge"))
# ENMO
ENMO_to_AC_dt <- dat_fitted %>% select(AC, merge = ENMO_fitted) %>% as.data.table()
setkeyv(ENMO_to_AC_dt, c("merge"))
# MAD 
MAD_to_AC_dt <- dat_fitted %>% select(AC, merge = MAD_fitted) %>% as.data.table()
setkeyv(MAD_to_AC_dt, c("merge"))
# AI 
AI_to_AC_dt <- dat_fitted %>% select(AC, merge = AI_fitted) %>% as.data.table()
setkeyv(AI_to_AC_dt, c("merge"))

# define mapping functions
# MIMS
MIMS_to_AC_map <- function(x){
  x_dt = data.table(MIMS = x, obs_id = 1 : length(x))
  x_dt[, merge := MIMS]; setkeyv(x_dt, c("merge"))
  x_dt_merged = MIMS_to_AC_dt[x_dt, roll = 'nearest']
  x_dt_merged = x_dt_merged[order(obs_id), ]
  out <- x_dt_merged[, get(names(x_dt_merged)[1])]
  return(out)
}

# ENMO
ENMO_to_AC_map <- function(x){
  x_dt = data.table(ENMO = x, obs_id = 1 : length(x))
  x_dt[, merge := ENMO]; setkeyv(x_dt, c("merge"))
  x_dt_merged = ENMO_to_AC_dt[x_dt, roll = 'nearest']
  x_dt_merged = x_dt_merged[order(obs_id), ]
  out <- x_dt_merged[, get(names(x_dt_merged)[1])]
  return(out)
}

# MAD
MAD_to_AC_map <- function(x){
  x_dt = data.table(MAD = x, obs_id = 1 : length(x))
  x_dt[, merge := MAD]; setkeyv(x_dt, c("merge"))
  x_dt_merged = MAD_to_AC_dt[x_dt, roll = 'nearest']
  x_dt_merged = x_dt_merged[order(obs_id), ]
  out <- x_dt_merged[, get(names(x_dt_merged)[1])]
  return(out)
}

# AI
AI_to_AC_map <- function(x){
  x_dt = data.table(AI = x, obs_id = 1 : length(x))
  x_dt[, merge := AI]; setkeyv(x_dt, c("merge"))
  x_dt_merged = AI_to_AC_dt[x_dt, roll = 'nearest']
  x_dt_merged = x_dt_merged[order(obs_id), ]
  out <- x_dt_merged[, get(names(x_dt_merged)[1])]
  return(out)
}


# ------------------------------------------------------------------------------
# mapping f: AC -> {MIMS, ENMO, MAD, AI}

# define data tables based on pre-computed values for fast reference mapping
# MIMS 
AC_to_MIMS_dt <- dat_fitted %>% select(MIMS_fitted, merge = AC) %>% as.data.table()
setkeyv(AC_to_MIMS_dt, c("merge"))
# ENMO 
AC_to_ENMO_dt <- dat_fitted %>% select(ENMO_fitted, merge = AC) %>% as.data.table()
setkeyv(AC_to_ENMO_dt, c("merge"))
# MAD 
AC_to_MAD_dt <- dat_fitted %>% select(MAD_fitted, merge = AC) %>% as.data.table()
setkeyv(AC_to_MAD_dt, c("merge"))
# AI 
AC_to_AI_dt <- dat_fitted %>% select(AI_fitted, merge = AC) %>% as.data.table()
setkeyv(AC_to_AI_dt, c("merge"))

# define mapping functions
# MIMS
AC_to_MIMS_map <- function(x, set0_for_AC0 = TRUE){
  x_dt = data.table(AC = x, obs_id = 1 : length(x))
  x_dt[, merge := AC]; setkeyv(x_dt, c("merge"))
  x_dt_merged = AC_to_MIMS_dt[x_dt, roll = 'nearest']
  x_dt_merged = x_dt_merged[order(obs_id), ]
  out <- x_dt_merged[, get(names(x_dt_merged)[1])]
  if (set0_for_AC0) out[x == 0] <- 0
  return(out)
}

# ENMO
AC_to_ENMO_map <- function(x, set0_for_AC0 = TRUE){
  x_dt = data.table(AC = x, obs_id = 1 : length(x))
  x_dt[, merge := AC]; setkeyv(x_dt, c("merge"))
  x_dt_merged = AC_to_ENMO_dt[x_dt, roll = 'nearest']
  x_dt_merged = x_dt_merged[order(obs_id), ]
  out <- x_dt_merged[, get(names(x_dt_merged)[1])]
  if (set0_for_AC0) out[x == 0] <- 0
  return(out)
}

# MAD
AC_to_MAD_map <- function(x, set0_for_AC0 = TRUE){
  x_dt = data.table(AC = x, obs_id = 1 : length(x))
  x_dt[, merge := AC]; setkeyv(x_dt, c("merge"))
  x_dt_merged = AC_to_MAD_dt[x_dt, roll = 'nearest']
  x_dt_merged = x_dt_merged[order(obs_id), ]
  out <- x_dt_merged[, get(names(x_dt_merged)[1])]
  if (set0_for_AC0) out[x == 0] <- 0
  return(out)
}

# AI
AC_to_AI_map <- function(x, set0_for_AC0 = TRUE){
  x_dt = data.table(AC = x, obs_id = 1 : length(x))
  x_dt[, merge := AC]; setkeyv(x_dt, c("merge"))
  x_dt_merged = AC_to_AI_dt[x_dt, roll = 'nearest']
  x_dt_merged = x_dt_merged[order(obs_id), ]
  out <- x_dt_merged[, get(names(x_dt_merged)[1])]
  if (set0_for_AC0) out[x == 0] <- 0
  return(out)
}
