
#' This script contains model selection and model fit for a GAM model where: 
#' - outcome Y -- individual-specific correlation values between the measures 
#' - covariates X -- age, sex, BMI


rm(list = ls())
library(tidyverse)
library(mgcv)
options(scipen = 999)


# ------------------------------------------------------------------------------
# read data 

# read accelerometry measures master file
acc_corr_fpath <- paste0(here::here(), "/results/2021-03-25-correlations_between_measures.rds")
acc_corr <- readRDS(acc_corr_fpath)
# get unique pairs (subj_id, visit_id)
acc_corr_meta <- acc_corr %>% select(subj_id, visit_id) %>% distinct()

# mastervisit
mastervisit_fpath <- paste0(here::here(), "/covariates/2021-01-19-mastervisit.rdata")
mastervisit <- get(load(mastervisit_fpath, ex <- new.env()), envir = ex) %>%
  select(subj_id = IDNo, visit_id = Visit, age = Age, bmi = BMI)
# masterdemog 
masterdemog_fpath <- paste0(here::here(), "/covariates/2021-01-19-masterdemog.rdata")
masterdemog <- get(load(masterdemog_fpath, ex <- new.env()), envir = ex) %>%
  select(subj_id = idno, sex) %>%
  mutate(sex = factor(sex, levels = c(0,1), labels = c("Female", "Male")))

# combine data sets
dat_comb <- 
  acc_corr %>%
  inner_join(mastervisit, by = c("subj_id", "visit_id")) %>%
  inner_join(masterdemog, by = c("subj_id")) 
dat <- dat_comb %>% filter(from11to5_exclude == 0)
str(dat)
dim(dat)
# [1] 655  18


# ------------------------------------------------------------------------------
# format data variables

# define corr names values 
var_names <- c(
  "AC_MIMS",
  "AC_ENMO",
  "AC_MAD",
  "AC_AI",
  "MIMS_ENMO",
  "MIMS_MAD",
  "MIMS_AI",
  "ENMO_MAD",
  "ENMO_AI",
  "MAD_AI"
)
# define corr names levels, labels
name_levels  <- var_names[1:4]
name_labels  <- sapply(name_levels, function(val) paste0(str_split(val, "_")[[1]], collapse = ", "))
name_labels  <- paste0("(", name_labels, ")")
name_labels2 <- paste0("corr", name_labels)
name_labels3 <- paste0("y=corr", name_labels)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# MODEL SELECTION

# GAM: y ~ s(age) + s(bmi) + ti(age, bmi)
get_fit1 <- function(i, use_rho_to_z = TRUE){ # i <- 1; use_rho_to_z = TRUE
  y_lab_tmp <- var_names[i]
  print(y_lab_tmp)
  dat$y <- dat[, y_lab_tmp]
  fit_tmp <- gam(y ~ s(age) + s(bmi) + ti(age, bmi), 
                 data = dat, method = "REML")
  print(summary(fit_tmp))
}
get_fit1(1)
get_fit1(2)
get_fit1(3)
get_fit1(4)


# GAM: y ~ s(age) + s(bmi)
get_fit2 <- function(i, use_rho_to_z = TRUE){
  y_lab_tmp <- var_names[i]
  print(y_lab_tmp)
  dat$y <- dat[, y_lab_tmp]
  fit_tmp <- gam(y ~ s(age) + s(bmi),
                 data = dat, method = "REML")
  print(summary(fit_tmp))
}
get_fit2(1)
get_fit2(2)
get_fit2(3)
get_fit2(4)

# Compare different fits with sex added: GCV.Cp 
compare_fit_gcv.ubre <- function(i){
  y_lab_tmp <- var_names[i]
  print(y_lab_tmp)
  dat$y <- dat[, y_lab_tmp]
  gcv.ubre_vec <- numeric()
  # #1
  fit_tmp <- gam(y ~ s(age) + s(bmi), data = dat)
  gcv.ubre_vec <- c(gcv.ubre_vec, fit_tmp$gcv.ubre)
  # #2
  fit_tmp <- gam(y ~ s(age) + s(bmi) + sex, data = dat)
  gcv.ubre_vec <- c(gcv.ubre_vec, fit_tmp$gcv.ubre)
  # #3
  fit_tmp <- gam(y ~ s(age, by = sex) + s(bmi), data = dat)
  gcv.ubre_vec <- c(gcv.ubre_vec, fit_tmp$gcv.ubre)
  # #4
  fit_tmp <- gam(y ~ s(age) + s(bmi, by = sex), data = dat)
  gcv.ubre_vec <- c(gcv.ubre_vec, fit_tmp$gcv.ubre)
  # #5
  fit_tmp <- gam(y ~ s(age, by = sex) + s(bmi, by = sex), data = dat)
  gcv.ubre_vec <- c(gcv.ubre_vec, fit_tmp$gcv.ubre)
  # #6
  fit_tmp <- gam(y ~ s(age) + bmi + sex, data = dat)
  gcv.ubre_vec <- c(gcv.ubre_vec, fit_tmp$gcv.ubre)
  # #7
  fit_tmp <- gam(y ~ s(age, by = sex) + bmi, data = dat)
  gcv.ubre_vec <- c(gcv.ubre_vec, fit_tmp$gcv.ubre)
  # #8
  fit_tmp <- gam(y ~ s(age) + sex, data = dat)
  gcv.ubre_vec <- c(gcv.ubre_vec, fit_tmp$gcv.ubre)
  # #9
  fit_tmp <- gam(y ~ s(age, by = sex), data = dat)
  gcv.ubre_vec <- c(gcv.ubre_vec, fit_tmp$gcv.ubre)
  out_df <- data.frame(gcv_ubre = gcv.ubre_vec) %>%
    mutate(model_idx = row_number()) %>%
    arrange(gcv_ubre) %>%
    mutate(gcv_ubre_min = row_number()) %>%
    arrange(model_idx) %>%
    mutate(y_lab = y_lab_tmp, .before = everything())
  return(out_df)
}
df <- rbind(
  compare_fit_gcv.ubre(1),
  compare_fit_gcv.ubre(2),
  compare_fit_gcv.ubre(3),
  compare_fit_gcv.ubre(4)
)
df
#      y_lab   gcv_ubre model_idx gcv_ubre_min
# 8  AC_MIMS 0.00003482122         8            1
# 20  AC_MAD 0.00111231561         2            1
# 20  AC_MAD 0.00111231561         2            1
# 33   AC_AI 0.00028861442         6            1



# ------------------------------------------------------------------------------
# model fit

# define objects to store effect results
model_fitted_smooth_df_all <- data.frame()
model_fit_summary          <- list()

for (i in 1:4){ # i <- 3
  
  # define the outcome Y (current open-source measure name)
  y_lab_tmp <- var_names[i]; print(y_lab_tmp)
  dat$y     <- dat[, y_lab_tmp]

  # fit model 
  fit_i    <- gam(y ~ s(age) + s(bmi) + sex, data = dat, method = "REML")
  fit_i_ss <- summary(fit_i)
  
  # append model summary
  model_fit_summary[[i]] <- fit_i_ss

  # define fitted values shift  
  shift_intercept <- coef(fit_i)["(Intercept)"]
  shift_sex       <- mean(as.numeric(dat$sex == "Male") * coef(fit_i)["sexMale"])
  shift           <- shift_intercept + shift_sex
  
  # get model coefficient of smooth effect: (1) age 
  plot_obj_i <- (plot(fit_i, shift = 0, se = TRUE, seWithMean = FALSE, select = 1))[[1]]
  fit_coef_smooth_i_AGE <- data.frame(
    effect_var_value = plot_obj_i$x, 
    coef_fitted = plot_obj_i$fit, 
    coef_lwr = plot_obj_i$fit - plot_obj_i$se,
    coef_upr = plot_obj_i$fit + plot_obj_i$se
  ) %>% 
    mutate(
      y_fitted_s = coef_fitted + shift,
      y_lwr_s    = coef_lwr + shift,
      y_upr_s    = coef_upr + shift,
      effect_var_name = "age",
      name = y_lab_tmp
    )
  # summary(plot_obj_i$fit[, 1])
  
  # get model coefficient of smooth effect: (2) BMI 
  rm(plot_obj_i)
  plot_obj_i <- (plot(fit_i, shift = 0, se = TRUE, seWithMean = FALSE, select = 2))[[2]]
  fit_coef_smooth_i_BMI <- data.frame(
    effect_var_value = plot_obj_i$x, 
    coef_fitted = plot_obj_i$fit, 
    coef_lwr = plot_obj_i$fit - plot_obj_i$se,
    coef_upr = plot_obj_i$fit + plot_obj_i$se
  ) %>% 
    mutate(
      y_fitted_s = coef_fitted + shift,
      y_lwr_s    = coef_lwr + shift,
      y_upr_s    = coef_upr + shift,
      effect_var_name = "bmi",
      name = y_lab_tmp
    )
  # summary(plot_obj_i$fit[, 1])
  fit_coef_smooth_i <- rbind(
    fit_coef_smooth_i_AGE,
    fit_coef_smooth_i_BMI
  )
  
  # append data frame 
  model_fitted_smooth_df_all <- rbind(model_fitted_smooth_df_all, fit_coef_smooth_i)
}



# ------------------------------------------------------------------------------
# save data 

path_tmp <- paste0(here::here(), "/results/measures_corr_model_fitted_smooth_df.rds")
saveRDS(model_fitted_smooth_df_all, path_tmp)

path_tmp <- paste0(here::here(), "/results/measures_corr_model_fit_summary.rds")
saveRDS(model_fit_summary, path_tmp)
