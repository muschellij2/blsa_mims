
#' Script to make EDA of mapping between minute-level activity measures
#' 
#' Note: 
#' - To get needed files available locally, use
#'   get /dcl01/smart/data/activity/blsa_mims/data_processed/2021-01-19-measures_masterfile_winsorized.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/data_processed/2021-01-19-measures_masterfile_winsorized.rds
#'   
#' - We use winsorized data to make the mapping 
#'   
#' - Reference: 
#'   https://johnmuschelli.com/upper_limb_gt3x_prosthesis/thresholds.html
   

rm(list = ls())
library(tidyverse)
library(mgcv)
library(ggsci) # https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html
library(cowplot)
library(data.table)
library(fcr)

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-01-19-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath)
names(dat_acc)
dim(dat_acc)
length(unique(dat_acc$file_id))
# Jan 21, 2021: 721
length(unique(dat_acc$subj_id))


##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
## GAM fit and predict 

AC_seq <- seq(from = 0, to = round(max(dat_acc$AC)), by = 1)

# MIMS ~ AC
mod_MIMS_AC <- gam(MIMS ~ s(AC, bs = "cr"), data = dat_acc)
mod_MIMS_AC_df      <- data.frame(AC = AC_seq)
mod_MIMS_AC_df$MIMS <- predict(mod_MIMS_AC, newdata = mod_MIMS_AC_df)
length(mod_MIMS_AC$coefficients)


##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
## Andrew's model

## The mean model in order of the formula presented below:
##     E[Y_{ij}] = f_0(t_{ij}) + f_1(t_{ij})Male_i + f_2(t_{ij})\tilde{WAZ}_{ij}
## note that we do not specify any random effects. That is done internally by fcr()
subj_id_vec <- unique(dat_acc$subj_id)



k <- 12 # number of interior knots for fpca (results in k + 3 basis functions)
K <- 15 # dimenson of smooth for time varying coefficients

# Make data subset 
data <- dat_acc %>% filter(subj_id %in% subj_id_vec[1:10])
data$X <- data$AC
data$Y       <- data$MIMS
data$subj    <- factor(data$subj_id)

data <- data %>% select(X, Y, subj)
data <- as.data.frame(data) 
saveRDS(data, "/Users/martakaras/Desktop/fcr_example_data.rds")


data <- as.data.frame(data) 

# new values of the functional domanin to predict
# tnew <- sort(unique(data$AC))
tnew <- 0 : 20000

t1 <- Sys.time()
fit <- fcr(formula = Y ~ s(argvals, k = K, bs = "ps"),
           argvals = "argvals", subj = "subj", data = data, use_bam = TRUE, 
           argvals.new = tnew,
           face.args = list(knots = k, pve = 0.99))
t2 <- Sys.time()
t2 - t1





###########################################
## Step 1: Smooth time-varying covariate ##
###########################################
dat.waz <- data.frame("y" = dat_acc$AC, "subj" = factor(dat_acc$subj_id), argvals = dat_acc$AC)
t1 <- Sys.time()
fit.waz <- face.sparse(dat.waz, newdata = dat.waz, knots = k, argvals.new = tnew)
dat_acc$wazPred <- fit.waz$y.pred
t2 <- Sys.time()
t2 - t1

#####################
## Step 2: Fit fcr ##
#####################

K <- 15 # dimenson of smooth for time varying coefficients

## The mean model in order of the formula presented below:
##     E[Y_{ij}] = f_0(t_{ij}) + f_1(t_{ij})Male_i + f_2(t_{ij})\tilde{WAZ}_{ij}
## note that we do not specify any random effects. That is done internally by fcr()
fit <- fcr(formula = Y ~ s(argvals, k=K, bs="ps") + 
             # s(argvals, by=Male, k=K, bs="ps") + 
             s(argvals, by=wazPred, bs="ps"),
           argvals = "argvals", subj="subj", data=data, use_bam=TRUE, argvals.new=tnew,
           face.args = list(knots=k, pve=0.99))











## generate plot ---------------------------------------------------------------

# define subset of data for the plotting purpose
k <- 100
dat_acc_sub <- dat_acc[seq(1, nrow(dat_acc), by = k), ]
dim(dat_acc_sub)

# other plotting settings
# https://rpubs.com/mclaire19/ggplot2-custom-themes
theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_bw(base_size = 16) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank()    
    )
}
theme_set(theme_ggpr())

# predefine range of x-axis corresponding to AC 
# x_lim <- c(0, round(max(dat_acc$AC)))
# x_lim <- c(0, 30000)
x_lim <- c(0, 20000)

# MIMS ~ AC
y_lim <- c(0, mod_MIMS_AC_df %>% filter(AC == x_lim[2]) %>% pull(MIMS))
plt_MIMS_AC <- 
  ggplot(dat_acc_sub, aes(x = AC, y = MIMS)) + 
  geom_point(alpha = 0.1, size = 0.5) + 
  geom_line(data = mod_MIMS_AC_df, aes(x = AC, y = MIMS), color = pal_ucscgb()(1)) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_MIMS_AC

# ENMO ~ AC
y_lim <- c(0, mod_ENMO_AC_df %>% filter(AC == x_lim[2]) %>% pull(ENMO))
plt_ENMO_AC <- 
  ggplot(dat_acc_sub, aes(x = AC, y = ENMO)) + 
  geom_point(alpha = 0.1, size = 0.5) + 
  geom_line(data = mod_ENMO_AC_df, aes(x = AC, y = ENMO), color = pal_ucscgb()(1)) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_ENMO_AC

# MAD ~ AC
y_lim <- c(0, mod_MAD_AC_df %>% filter(AC == x_lim[2]) %>% pull(MAD))
plt_MAD_AC <- 
  ggplot(dat_acc_sub, aes(x = AC, y = MAD)) + 
  geom_point(alpha = 0.1, size = 0.5) + 
  geom_line(data = mod_MAD_AC_df, aes(x = AC, y = MAD), color = pal_ucscgb()(1)) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_MAD_AC

# AI ~ AC
y_lim <- c(0, mod_AI_AC_df %>% filter(AC == x_lim[2]) %>% pull(AI))
plt_AI_AC <- 
  ggplot(dat_acc_sub, aes(x = AC, y = AI)) + 
  geom_point(alpha = 0.1, size = 0.5) + 
  geom_line(data = mod_AI_AC_df, aes(x = AC, y = AI), color = pal_ucscgb()(1)) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_AI_AC


# combined plot
plt_list <- list(plt_MIMS_AC, plt_ENMO_AC, plt_MAD_AC, plt_AI_AC)
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v")
plt

# plt_fpath <- paste0(here::here(), "/results_figures/measures_mapping_GAM_fit.png")
# save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 8)
# rm(plt)


##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
##  ----------------------------------------------------------------------------
## Get individual-specific GAM fits

subj_id_vec <- unique(dat_acc$subj_id)
AC_seq <- seq(from = 0, to = round(max(dat_acc$AC)), by = 100)

# MIMS ~ AC
mod_MIMS_AC_dt_list <- lapply(subj_id_vec, function(subj_id_tmp){
  dat_acc_tmp <- dat_acc %>% filter(subj_id == subj_id_tmp)
  mod_MIMS_AC <- gam(MIMS ~ s(AC, bs = "cr"), data = dat_acc_tmp)
  mod_MIMS_AC_df      <- data.frame(AC = AC_seq)
  mod_MIMS_AC_df$MIMS <- predict(mod_MIMS_AC, newdata = mod_MIMS_AC_df)
  mod_MIMS_AC_df$subj_id <- subj_id_tmp
  return(as.data.table(mod_MIMS_AC_df))
})
mod_MIMS_AC_df <- rbindlist(mod_MIMS_AC_dt_list) %>% as.data.frame()
mod_MIMS_AC_df_agg <- mod_MIMS_AC_df %>% group_by(AC) %>% summarise(MIMS = median(MIMS))

# ENMO ~ AC
mod_ENMO_AC_dt_list <- lapply(subj_id_vec, function(subj_id_tmp){
  dat_acc_tmp <- dat_acc %>% filter(subj_id == subj_id_tmp)
  mod_ENMO_AC <- gam(ENMO ~ s(AC, bs = "cr"), data = dat_acc_tmp)
  mod_ENMO_AC_df      <- data.frame(AC = AC_seq)
  mod_ENMO_AC_df$ENMO <- predict(mod_ENMO_AC, newdata = mod_ENMO_AC_df)
  mod_ENMO_AC_df$subj_id <- subj_id_tmp
  return(as.data.table(mod_ENMO_AC_df))
})
mod_ENMO_AC_df <- rbindlist(mod_ENMO_AC_dt_list) %>% as.data.frame()
mod_ENMO_AC_df_agg <- mod_ENMO_AC_df %>% group_by(AC) %>% summarise(ENMO = median(ENMO))

# MAD ~ AC
mod_MAD_AC_dt_list <- lapply(subj_id_vec, function(subj_id_tmp){
  dat_acc_tmp <- dat_acc %>% filter(subj_id == subj_id_tmp)
  mod_MAD_AC <- gam(MAD ~ s(AC, bs = "cr"), data = dat_acc_tmp)
  mod_MAD_AC_df      <- data.frame(AC = AC_seq)
  mod_MAD_AC_df$MAD <- predict(mod_MAD_AC, newdata = mod_MAD_AC_df)
  mod_MAD_AC_df$subj_id <- subj_id_tmp
  return(as.data.table(mod_MAD_AC_df))
})
mod_MAD_AC_df <- rbindlist(mod_MAD_AC_dt_list) %>% as.data.frame()
mod_MAD_AC_df_agg <- mod_MAD_AC_df %>% group_by(AC) %>% summarise(MAD = median(MAD))

# AI ~ AC
mod_AI_AC_dt_list <- lapply(subj_id_vec, function(subj_id_tmp){
  dat_acc_tmp <- dat_acc %>% filter(subj_id == subj_id_tmp)
  mod_AI_AC <- gam(AI ~ s(AC, bs = "cr"), data = dat_acc_tmp)
  mod_AI_AC_df      <- data.frame(AC = AC_seq)
  mod_AI_AC_df$AI <- predict(mod_AI_AC, newdata = mod_AI_AC_df)
  mod_AI_AC_df$subj_id <- subj_id_tmp
  return(as.data.table(mod_AI_AC_df))
})
mod_AI_AC_df <- rbindlist(mod_AI_AC_dt_list) %>% as.data.frame()
mod_AI_AC_df_agg <- mod_AI_AC_df %>% group_by(AC) %>% summarise(AI = median(AI))


##  ----------------------------------------------------------------------------
# plot

# common x lim 
x_lim <- c(0, 20000)

# MIMS ~ AC
y_lim <- c(0, 92.05009)
plt_MIMS_AC <- 
  ggplot(mod_MIMS_AC_df, aes(x = AC, y = MIMS, group = subj_id)) + 
  geom_line(alpha = 0.3, size = 0.1) + 
  geom_line(data = mod_MIMS_AC_df_agg,  aes(x = AC, y = MIMS, group = 1),
            color = "magenta", size = 1, inherit.aes = FALSE) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_MIMS_AC

# ENMO ~ AC
y_lim <- c(0, 0.5389027)
plt_ENMO_AC <- 
  ggplot(mod_ENMO_AC_df, aes(x = AC, y = ENMO, group = subj_id)) + 
  geom_line(alpha = 0.3, size = 0.1) + 
  geom_line(data = mod_ENMO_AC_df_agg,  aes(x = AC, y = ENMO, group = 1),
            color = "magenta", size = 1, inherit.aes = FALSE) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_ENMO_AC

# MAD ~ AC
y_lim <- c(0, 0.5909187)
plt_MAD_AC <- 
  ggplot(mod_MAD_AC_df, aes(x = AC, y = MAD, group = subj_id)) + 
  geom_line(alpha = 0.3, size = 0.1) + 
  geom_line(data = mod_MAD_AC_df_agg,  aes(x = AC, y = MAD, group = 1),
            color = "magenta", size = 1, inherit.aes = FALSE) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_MAD_AC

# AI ~ AC
y_lim <- c(0, 39.76357)
plt_AI_AC <- 
  ggplot(mod_AI_AC_df, aes(x = AC, y = AI, group = subj_id)) + 
  geom_line(alpha = 0.3, size = 0.1) + 
  geom_line(data = mod_AI_AC_df_agg,  aes(x = AC, y = AI, group = 1),
            color = "magenta", size = 1, inherit.aes = FALSE) + 
  scale_x_continuous(limits = x_lim) + 
  scale_y_continuous(limits = y_lim) 
plt_AI_AC


# combined plot
plt_list <- list(plt_MIMS_AC, plt_ENMO_AC, plt_MAD_AC, plt_AI_AC)
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v")
plt

plt_fpath <- paste0(here::here(), "/results_figures/measures_mapping_GAM_fit_subjectcurves.png")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 8)
rm(plt)



