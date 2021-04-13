#' This script contains model selection and model fit for a GAm model where: 
#' - outcome Y -- individual-specific correlation values between the measures 
#' - covariates X -- age, sex, BMI
#' 

rm(list = ls())
library(tidyverse)
library(gridExtra)
library(ggsci)
library(mgcv)
options(scipen = 999)


# function to transform correlation values rho into Fisher z-transformed values
rho_to_z <- function (rho){
  zval <- 0.5 * log((1 + rho)/(1 - rho))
  return(zval)
}
# function to transform Fisher z-transformed values into correlation values rho
z_to_rho <- function (z){
  rhoval <- (exp(2 * z) - 1)/(1 + exp(2 * z))
  return(rhoval)
}


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
dim(dat)
str(dat)
# [1] 655  18


# ------------------------------------------------------------------------------
# format data variables

# corr names values 
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
# corr names levels, labels
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
  if (use_rho_to_z) dat$y <- rho_to_z(dat$y)
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
  if (use_rho_to_z) dat$y <- rho_to_z(dat$y)
  fit_tmp <- gam(y ~ s(age) + s(bmi),
                 data = dat, method = "REML")
  print(summary(fit_tmp))
}
get_fit2(1)
get_fit2(2)
get_fit2(3)
get_fit2(4)

# Compare different fits with sex added: GCV.Cp 
compare_fit_gcv.ubre <- function(i, use_rho_to_z = TRUE){
  y_lab_tmp <- var_names[i]
  print(y_lab_tmp)
  dat$y <- dat[, y_lab_tmp]
  if (use_rho_to_z) dat$y <- rho_to_z(dat$y)
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
#' - For 3/4 measures, the lowest gcv is for model 2: 
#'   fit_tmp <- gam(y ~ s(age) + s(bmi) + sex, data = dat)
#'   
#' - For 1/4 measures, the lowest gcv is for model 6: 
#'   fit_tmp <- gam(y ~ s(age) + bmi + sex, data = dat)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# MODEL FINAL FIT -- sumamrize estimates for (A) age

use_rho_to_z = TRUE

model_fitted_smooth_df_all <- data.frame()
model_coef_fixed_df_all    <- data.frame()

for (i in 1:4){ # i <- 3
  
  # define the outcome Y (current open-source measure name)
  y_lab_tmp <- var_names[i]
  print(y_lab_tmp)
  dat$y <- dat[, y_lab_tmp]
  if (use_rho_to_z) dat$y <- rho_to_z(dat$y)
  
  # fit model 
  fit_i <- gam(y ~ s(age) + s(bmi) + sex, data = dat, method = "REML")
  fit_i_ss <- summary(fit_i)
  
  # get model coefficients of fixed effects
  fit_coef_fixed_i <- data.frame(
    coef_name = names(fit_i_ss$p.coeff),
    coef_est  = fit_i_ss$p.coeff,
    coef_pval = round(fit_i_ss$p.pv, 5)
  ) %>% mutate(
    # convert coefficient back from z-Fisher transformed outcome scale to original outcome scale
    coef_est_rho = z_to_rho(coef_est), .after = coef_est,
    coef_est_rho = round(coef_est_rho, 5)
  )
  # add name for the outcome Y (current open-source measure name)
  fit_coef_fixed_i$name   <- y_lab_tmp
  model_coef_fixed_df_all <- rbind(model_coef_fixed_df_all, fit_coef_fixed_i)
  
  # get model coefficient of smooth effect 
  # - seWithMean = TRUE yields the component smooths are shown with confidence 
  #   intervals that include the uncertainty about the overall mean.
  plot_obj_i <- (plot(fit_i, shift = 0, seWithMean = TRUE, select = 1))[[1]]
  fit_coef_smooth_i <- data.frame(
    age = plot_obj_i$x, 
    coef_fitted = plot_obj_i$fit, 
    coef_lwr = plot_obj_i$fit - plot_obj_i$se,
    coef_upr = plot_obj_i$fit + plot_obj_i$se
  ) %>% mutate(
    # convert back to rho 
    coef_fitted_rho = z_to_rho(coef_fitted),
    coef_lwr_rho = z_to_rho(coef_lwr),
    coef_upr_rho = z_to_rho(coef_upr)
  ) 
  
  # shift model coefficient of smooth effect by estimates of fixed effects 
  # (intercept, sex) to get values on the fitted outcome scale 
  shift_intercept <- coef(fit_i)["(Intercept)"]
  shift_sex       <- mean(as.numeric(dat$sex == "Male") * coef(fit_i)["sexMale"])
  shift           <- shift_intercept + shift_sex
  fit_coef_smooth_i <- 
    fit_coef_smooth_i %>%
    mutate(
      y_fitted_s = coef_fitted + shift,
      y_lwr_s    = coef_lwr + shift,
      y_upr_s    = coef_upr + shift, 
      # convert back from z-Fisher transformed outcome scale to original outcome scale
      y_fitted_s_rho = z_to_rho(y_fitted_s),
      y_lwr_s_rho    = z_to_rho(y_lwr_s),
      y_upr_s_rho    = z_to_rho(y_upr_s),   
      name           = y_lab_tmp
    )
  model_fitted_smooth_df_all <- rbind(model_fitted_smooth_df_all, fit_coef_smooth_i)
}

# mutate df factor
model_fitted_smooth_df_all <- 
  model_fitted_smooth_df_all %>%
  mutate(name_fct = factor(name, levels = name_levels, labels = name_labels3)) 



# MODEL FINAL FIT -- PLOT ------------------------------------------------------

theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank()    
    )
}
theme_set(theme_ggpr())

# data frame with data points for the plot
plt_df_points <- dat %>%
  select(all_of(c(var_names[1:4], "age", "bmi", "sex"))) %>%
  pivot_longer(cols = -c(age, bmi, sex)) %>%
  mutate(name_fct = factor(name, levels = name_levels, labels = name_labels3)) 

# Plot: Age effect on y=z(corr(M1,M2))
ggplot(model_fitted_smooth_df_all, aes(x = age, y = coef_fitted)) + 
  geom_ribbon(aes(ymin = coef_lwr, ymax = coef_upr), alpha = 0.2) + 
  geom_line() + 
  facet_wrap(~ name_fct, ncol = 2) + 
  theme(legend.position = "none") + 
  labs(y = "Age effect on y (=z(corr))", x = "Age")

# Plot: Age effect on y=corr(M1,M2)
ggplot(model_fitted_smooth_df_all, aes(x = age, y = coef_fitted_rho)) + 
  geom_ribbon(aes(ymin = coef_lwr_rho, ymax = coef_upr_rho), alpha = 0.2) + 
  geom_line() + 
  facet_wrap(~ name_fct, ncol = 2) + 
  theme(legend.position = "none") + 
  scale_color_futurama() + 
  # labs(y = "Age effect on y\n(shifted)", x = "Age")  + 
  labs(y = "Age effect on y", x = "Age")

# Plot: Age effect on y=corr(M1,M2), shifted
plt3 <- 
  ggplot(model_fitted_smooth_df_all, aes(x = age, y = y_fitted_s_rho)) + 
  geom_point(data = plt_df_points, aes(x = age, y = value, color = name_fct), alpha = 0.1) +
  geom_ribbon(aes(ymin = y_lwr_s_rho, ymax = y_upr_s_rho), alpha = 0.2) + 
  geom_line() + 
  facet_wrap(~ name_fct, ncol = 2) + 
  theme(legend.position = "none") + 
  scale_color_futurama() + 
  scale_y_continuous(limits = c(0.5, 1)) + 
  labs(y = "Age effect on y (shifted)", x = "Age")
plt3


# save to file 
plt_path <- paste0(here::here(), "/results_figures/measures_corr_model_age_effect_twosmooths.png")
ggsave(filename = plt_path, plot = plt3, width = 8, height = 6.4)

rm(model_fitted_smooth_df_all)


# MODEL FINAL FIT -- SUMMARIZE COEFS -------------------------------------------

rownames(model_coef_fixed_df_all) <- NULL
model_coef_fixed_df_all
rm(model_coef_fixed_df_all)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# MODEL FINAL FIT -- sumamrize estimates for (B) bmi

use_rho_to_z = TRUE

model_fitted_smooth_df_all <- data.frame()
model_coef_fixed_df_all    <- data.frame()

for (i in 1:4){ # i <- 3
  
  # define the outcome Y (current open-source measure name)
  y_lab_tmp <- var_names[i]
  print(y_lab_tmp)
  dat$y <- dat[, y_lab_tmp]
  if (use_rho_to_z) dat$y <- rho_to_z(dat$y)
  
  # fit model 
  fit_i <- gam(y ~ s(age) + s(bmi) + sex, data = dat, method = "REML")
  fit_i_ss <- summary(fit_i)
  
  # get model coefficients of fixed effects
  fit_coef_fixed_i <- data.frame(
    coef_name = names(fit_i_ss$p.coeff),
    coef_est  = fit_i_ss$p.coeff,
    coef_pval = round(fit_i_ss$p.pv, 5)
  ) %>% mutate(
    # convert coefficient back from z-Fisher transformed outcome scale to original outcome scale
    coef_est_rho = z_to_rho(coef_est), .after = coef_est,
    coef_est_rho = round(coef_est_rho, 5)
  )
  # add name for the outcome Y (current open-source measure name)
  fit_coef_fixed_i$name   <- y_lab_tmp
  model_coef_fixed_df_all <- rbind(model_coef_fixed_df_all, fit_coef_fixed_i)
  
  # get model coefficient of smooth effect 
  # - seWithMean = TRUE yields the component smooths are shown with confidence 
  #   intervals that include the uncertainty about the overall mean.
  plot_obj_i <- (plot(fit_i, shift = 0, seWithMean = TRUE, select = 1))[[2]]
  fit_coef_smooth_i <- data.frame(
    bmi = plot_obj_i$x, 
    coef_fitted = plot_obj_i$fit, 
    coef_lwr = plot_obj_i$fit - plot_obj_i$se,
    coef_upr = plot_obj_i$fit + plot_obj_i$se
  ) %>% mutate(
    # convert back to rho 
    coef_fitted_rho = z_to_rho(coef_fitted),
    coef_lwr_rho = z_to_rho(coef_lwr),
    coef_upr_rho = z_to_rho(coef_upr)
  ) 
  
  # shift model coefficient of smooth effect by estimates of fixed effects 
  # (intercept, sex) to get values on the fitted outcome scale 
  shift_intercept <- coef(fit_i)["(Intercept)"]
  shift_sex       <- mean(as.numeric(dat$sex == "Male") * coef(fit_i)["sexMale"])
  shift           <- shift_intercept + shift_sex
  fit_coef_smooth_i <- 
    fit_coef_smooth_i %>%
    mutate(
      y_fitted_s = coef_fitted + shift,
      y_lwr_s    = coef_lwr + shift,
      y_upr_s    = coef_upr + shift, 
      # convert back from z-Fisher transformed outcome scale to original outcome scale
      y_fitted_s_rho = z_to_rho(y_fitted_s),
      y_lwr_s_rho    = z_to_rho(y_lwr_s),
      y_upr_s_rho    = z_to_rho(y_upr_s),   
      name           = y_lab_tmp
    )
  model_fitted_smooth_df_all <- rbind(model_fitted_smooth_df_all, fit_coef_smooth_i)
}

# mutate df factor
model_fitted_smooth_df_all <- 
  model_fitted_smooth_df_all %>%
  mutate(name_fct = factor(name, levels = name_levels, labels = name_labels3)) 



# MODEL FINAL FIT -- PLOT ------------------------------------------------------

# Plot: BMI effect on y=z(corr(M1,M2))
ggplot(model_fitted_smooth_df_all, aes(x = bmi, y = coef_fitted)) + 
  geom_ribbon(aes(ymin = coef_lwr, ymax = coef_upr), alpha = 0.2) + 
  geom_line() + 
  facet_wrap(~ name_fct, ncol = 2) + 
  theme(legend.position = "none") + 
  labs(y = "BMI effect on y (=z(corr))", x = "BMI")

# Plot: BMI effect on y=corr(M1,M2)
ggplot(model_fitted_smooth_df_all, aes(x = bmi, y = coef_fitted_rho)) + 
  geom_ribbon(aes(ymin = coef_lwr_rho, ymax = coef_upr_rho), alpha = 0.2) + 
  geom_line() + 
  facet_wrap(~ name_fct, ncol = 2) + 
  theme(legend.position = "none") + 
  scale_color_futurama() + 
  # labs(y = "BMI effect on y\n(shifted)", x = "BMI")  + 
  labs(y = "BMI effect on y", x = "BMI")

# Plot: BMI effect on y=corr(M1,M2), shifted
plt3 <- 
  ggplot(model_fitted_smooth_df_all, aes(x = bmi, y = y_fitted_s_rho)) + 
  geom_point(data = plt_df_points, aes(x = bmi, y = value, color = name_fct), alpha = 0.1) +
  geom_ribbon(aes(ymin = y_lwr_s_rho, ymax = y_upr_s_rho), alpha = 0.2) + 
  geom_line() + 
  facet_wrap(~ name_fct, ncol = 2) + 
  theme(legend.position = "none") + 
  scale_color_futurama() + 
  scale_y_continuous(limits = c(0.5, 1)) + 
  labs(y = "BMI effect on y (shifted)", x = "BMI")
plt3


# save to file 
plt_path <- paste0(here::here(), "/results_figures/measures_corr_model_bmi_effect_twosmooths.png")
ggsave(filename = plt_path, plot = plt3, width = 8, height = 6.4)

rm(model_fitted_smooth_df_all)
rm(plt_df_points)


# MODEL FINAL FIT -- SUMMARIZE COEFS -------------------------------------------

rownames(model_coef_fixed_df_all) <- NULL
model_coef_fixed_df_all
