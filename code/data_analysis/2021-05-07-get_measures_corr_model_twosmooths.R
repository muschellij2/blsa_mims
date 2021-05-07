
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
acc_corr_fpath <- paste0(here::here(), "/results/2021-05-06-correlations_between_measures.rds")
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


# ------------------------------------------------------------------------------
# MIMS 

y_lab_tmp <- "AC_MIMS"
dat$y <- dat[, y_lab_tmp]
summary(gam(y ~ sex + bmi + s(bmi, bs="tp", m=c(2,0)), data = dat))
summary(gam(y ~ sex + bmi + s(bmi, bs="tp", m=c(2,0)) + s(age, bs="tp"), data = dat))
# => BMI smooth not needed
summary(gam(y ~ sex + bmi + age + s(age, bs="tp", m=c(2,0)), data = dat))
summary(gam(y ~ sex + bmi + age, data = dat))
# => age smooth not needed

fit_MIMS <- gam(y ~ sex + bmi + age, data = dat)
summary(fit_MIMS)


# ------------------------------------------------------------------------------
# ENMO 

y_lab_tmp <- "AC_ENMO"
dat$y <- dat[, y_lab_tmp]
summary(gam(y ~ sex + bmi + s(bmi, bs="tp", m=c(2,0)), data = dat))
summary(gam(y ~ sex + bmi + s(bmi, bs="tp", m=c(2,0)) + s(age, bs="tp"), data = dat))
# => BMI smooth not needed
summary(gam(y ~ sex + bmi + age + s(age, bs="tp", m=c(2,0)), data = dat))
# => age smooth needed
summary(gam(y ~ sex + bmi + s(age, bs="cr"), data = dat))

fit_ENMO <- gam(y ~ sex + bmi + s(age, bs="cr"), data = dat)
summary(fit_ENMO)


# ------------------------------------------------------------------------------
# MAD 

y_lab_tmp <- "AC_MAD"
dat$y <- dat[, y_lab_tmp]
summary(gam(y ~ sex + bmi + s(bmi, bs="tp", m=c(2,0)), data = dat))
summary(gam(y ~ sex + bmi + s(bmi, bs="tp", m=c(2,0)) + s(age, bs="tp"), data = dat))
# => BMI smooth not needed
summary(gam(y ~ sex + bmi + age + s(age, bs="tp", m=c(2,0)), data = dat))
summary(gam(y ~ sex + bmi + age, data = dat))
# => age smooth not needed

fit_MAD <- gam(y ~ sex + bmi + age, data = dat)
summary(fit_MAD)


# ------------------------------------------------------------------------------
# AI 

y_lab_tmp <- "AC_AI"
dat$y <- dat[, y_lab_tmp]
summary(gam(y ~ sex + bmi + s(bmi, bs="tp", m=c(2,0)), data = dat))
summary(gam(y ~ sex + bmi + s(bmi, bs="tp", m=c(2,0)) + s(age, bs="tp"), data = dat))
# => BMI smooth not needed
summary(gam(y ~ sex + bmi + age + s(age, bs="tp", m=c(2,0)), data = dat))
summary(gam(y ~ sex + bmi + age, data = dat))
# => age smooth not needed

fit_AI <- gam(y ~ sex + bmi + age, data = dat)
summary(fit_AI)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# MODEL FITTED VALUES

# store all model summaries
model_fit_summary <- lapply(list(fit_MIMS, fit_ENMO, fit_MAD, fit_AI), summary)

# define objects to store estimated effect 
model_fitted_smooth_df_all <- data.frame()


# ------------------------------------------------------------------------------
# MIMS 

y_lab_tmp <- "AC_MIMS"
fit_i <- fit_MIMS
fit_i_ss <- summary(fit_i)

# define fitted values shift  
shift_intercept <- coef(fit_i)["(Intercept)"]
shift_sex       <- mean(as.numeric(dat$sex == "Male") * coef(fit_i)["sexMale"])
shift_bmi       <- mean(dat$bmi * coef(fit_i)["bmi"])
shift_age       <- mean(dat$age * coef(fit_i)["age"])

# get model coefficient of smooth effect: (1) age 
shift <- shift_intercept + shift_sex + shift_bmi
fit_coef_smooth_i_AGE <- data.frame(
  effect_var_value = dat$age,
  coef_fitted = coef(fit_i)["age"] * dat$age
  ) %>%
  mutate(
    coef_lwr = coef_fitted - fit_i_ss$p.table[4, 2],
    coef_upr = coef_fitted + fit_i_ss$p.table[4, 2],
    y_fitted_s = coef_fitted + shift,
    y_lwr_s    = coef_lwr + shift,
    y_upr_s    = coef_upr + shift,
    effect_var_name = "age",
    name = y_lab_tmp
  )
  
# get model coefficient of smooth effect: (2) BMI 
shift <- shift_intercept + shift_sex + shift_age
fit_coef_smooth_i_BMI <- data.frame(
  effect_var_value = dat$bmi,
  coef_fitted = coef(fit_i)["bmi"] * dat$bmi
) %>%
  mutate(
    coef_lwr = coef_fitted - fit_i_ss$p.table[3, 2],
    coef_upr = coef_fitted + fit_i_ss$p.table[3, 2],
    y_fitted_s = coef_fitted + shift,
    y_lwr_s    = coef_lwr + shift,
    y_upr_s    = coef_upr + shift,
    effect_var_name = "bmi",
    name = y_lab_tmp
  )
 
# append results 
fit_coef_smooth_i <- rbind(fit_coef_smooth_i_AGE, fit_coef_smooth_i_BMI)
model_fitted_smooth_df_all <- rbind(model_fitted_smooth_df_all, fit_coef_smooth_i)


# ------------------------------------------------------------------------------
# ENMO 

y_lab_tmp <- "AC_ENMO"
fit_i <- fit_ENMO
fit_i_ss <- summary(fit_i); fit_i_ss

# define fitted values shift  
shift_intercept <- coef(fit_i)["(Intercept)"]
shift_sex       <- mean(as.numeric(dat$sex == "Male") * coef(fit_i)["sexMale"])
shift_bmi       <- mean(dat$bmi * coef(fit_i)["bmi"])
# shift_age       <- mean(dat$age * coef(fit_i)["age"])

# get model coefficient of smooth effect: (1) age 
shift <- shift_intercept + shift_sex + shift_bmi
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

# get model coefficient of smooth effect: (2) BMI 
shift <- shift_intercept + shift_sex 
fit_coef_smooth_i_BMI <- data.frame(
  effect_var_value = dat$bmi,
  coef_fitted = coef(fit_i)["bmi"] * dat$bmi
) %>%
  mutate(
    coef_lwr = coef_fitted - fit_i_ss$p.table[3, 2],
    coef_upr = coef_fitted + fit_i_ss$p.table[3, 2],
    y_fitted_s = coef_fitted + shift,
    y_lwr_s    = coef_lwr + shift,
    y_upr_s    = coef_upr + shift,
    effect_var_name = "bmi",
    name = y_lab_tmp
  )

# append results 
fit_coef_smooth_i <- rbind(fit_coef_smooth_i_AGE, fit_coef_smooth_i_BMI)
model_fitted_smooth_df_all <- rbind(model_fitted_smooth_df_all, fit_coef_smooth_i)


# ------------------------------------------------------------------------------
# MAD 

y_lab_tmp <- "AC_MAD"
fit_i <- fit_MAD
fit_i_ss <- summary(fit_i)

# define fitted values shift  
shift_intercept <- coef(fit_i)["(Intercept)"]
shift_sex       <- mean(as.numeric(dat$sex == "Male") * coef(fit_i)["sexMale"])
shift_bmi       <- mean(dat$bmi * coef(fit_i)["bmi"])
shift_age       <- mean(dat$age * coef(fit_i)["age"])

# get model coefficient of smooth effect: (1) age 
shift <- shift_intercept + shift_sex + shift_bmi
fit_coef_smooth_i_AGE <- data.frame(
  effect_var_value = dat$age,
  coef_fitted = coef(fit_i)["age"] * dat$age
) %>%
  mutate(
    coef_lwr = coef_fitted - fit_i_ss$p.table[4, 2],
    coef_upr = coef_fitted + fit_i_ss$p.table[4, 2],
    y_fitted_s = coef_fitted + shift,
    y_lwr_s    = coef_lwr + shift,
    y_upr_s    = coef_upr + shift,
    effect_var_name = "age",
    name = y_lab_tmp
  )

# get model coefficient of smooth effect: (2) BMI 
shift <- shift_intercept + shift_sex + shift_age
fit_coef_smooth_i_BMI <- data.frame(
  effect_var_value = dat$bmi,
  coef_fitted = coef(fit_i)["bmi"] * dat$bmi
) %>%
  mutate(
    coef_lwr = coef_fitted - fit_i_ss$p.table[3, 2],
    coef_upr = coef_fitted + fit_i_ss$p.table[3, 2],
    y_fitted_s = coef_fitted + shift,
    y_lwr_s    = coef_lwr + shift,
    y_upr_s    = coef_upr + shift,
    effect_var_name = "bmi",
    name = y_lab_tmp
  )

# append results 
fit_coef_smooth_i <- rbind(fit_coef_smooth_i_AGE, fit_coef_smooth_i_BMI)
model_fitted_smooth_df_all <- rbind(model_fitted_smooth_df_all, fit_coef_smooth_i)



# ------------------------------------------------------------------------------
# AI 

y_lab_tmp <- "AC_AI"
fit_i <- fit_AI
fit_i_ss <- summary(fit_i)

# define fitted values shift  
shift_intercept <- coef(fit_i)["(Intercept)"]
shift_sex       <- mean(as.numeric(dat$sex == "Male") * coef(fit_i)["sexMale"])
shift_bmi       <- mean(dat$bmi * coef(fit_i)["bmi"])
shift_age       <- mean(dat$age * coef(fit_i)["age"])

# get model coefficient of smooth effect: (1) age 
shift <- shift_intercept + shift_sex + shift_bmi
fit_coef_smooth_i_AGE <- data.frame(
  effect_var_value = dat$age,
  coef_fitted = coef(fit_i)["age"] * dat$age
) %>%
  mutate(
    coef_lwr = coef_fitted - fit_i_ss$p.table[4, 2],
    coef_upr = coef_fitted + fit_i_ss$p.table[4, 2],
    y_fitted_s = coef_fitted + shift,
    y_lwr_s    = coef_lwr + shift,
    y_upr_s    = coef_upr + shift,
    effect_var_name = "age",
    name = y_lab_tmp
  )

# get model coefficient of smooth effect: (2) BMI 
shift <- shift_intercept + shift_sex + shift_age
fit_coef_smooth_i_BMI <- data.frame(
  effect_var_value = dat$bmi,
  coef_fitted = coef(fit_i)["bmi"] * dat$bmi
) %>%
  mutate(
    coef_lwr = coef_fitted - fit_i_ss$p.table[3, 2],
    coef_upr = coef_fitted + fit_i_ss$p.table[3, 2],
    y_fitted_s = coef_fitted + shift,
    y_lwr_s    = coef_lwr + shift,
    y_upr_s    = coef_upr + shift,
    effect_var_name = "bmi",
    name = y_lab_tmp
  )

# append results 
fit_coef_smooth_i <- rbind(fit_coef_smooth_i_AGE, fit_coef_smooth_i_BMI)
model_fitted_smooth_df_all <- rbind(model_fitted_smooth_df_all, fit_coef_smooth_i)


# ------------------------------------------------------------------------------
# save data 

path_tmp <- paste0(here::here(), "/results/2021-05-07-measures_corr_model_fitted_smooth_df.rds")
saveRDS(model_fitted_smooth_df_all, path_tmp)

path_tmp <- paste0(here::here(), "/results/2021-05-07-measures_corr_model_fit_summary.rds")
saveRDS(model_fit_summary, path_tmp)
