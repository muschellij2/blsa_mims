
#' This script contains model fit where: 
#' - outcome Y -- individual-specific correlation values between the measures 
#' - covariates X -- age, sex, BMI
#' 
#' Here, only the subset of the youngest participants are used.


rm(list = ls())
library(tidyverse)
library(mgcv)
library(broom)
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

mean(dat$age <= 65)
# [1] 0.319084

# generate subset of participants
dat <- dat %>% filter(age <= 65)
dim(dat)
# [1] 209  18

# ------------------------------------------------------------------------------
# format data variables

# define corr names values 
name_levels <- c(
  "AC_MIMS",
  "AC_ENMO",
  "AC_MAD",
  "AC_AI"
)
# define corr names levels, labels
name_labels  <- sapply(name_levels, function(val) paste0(str_split(val, "_")[[1]], collapse = ", "))
name_labels  <- paste0("(", name_labels, ")")
name_labels2 <- paste0("corr", name_labels)
name_labels3 <- paste0("y=corr", name_labels)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# MODEL FIT (marginal, conditional)

# MIMS 
y_lab_tmp <- "AC_MIMS"
dat$y <- dat[, y_lab_tmp]
fit_MIMS_marg <- lm(y ~ 1, data = dat)
fit_MIMS_cond <- lm(y ~ sex + bmi + age, data = dat)

# ENMO 
y_lab_tmp <- "AC_ENMO"
dat$y <- dat[, y_lab_tmp]
fit_ENMO_marg <- lm(y ~ 1, data = dat)
fit_ENMO_cond <- lm(y ~ sex + bmi + age, data = dat)

# MAD 
y_lab_tmp <- "AC_MAD"
dat$y <- dat[, y_lab_tmp]
fit_MAD_marg <- lm(y ~ 1, data = dat)
fit_MAD_cond <- lm(y ~ sex + bmi + age, data = dat)

# AI 
y_lab_tmp <- "AC_AI"
dat$y <- dat[, y_lab_tmp]
fit_AI_marg <- lm(y ~ 1, data = dat)
fit_AI_cond <- lm(y ~ sex + bmi + age, data = dat)


# ------------------------------------------------------------------------------
# MODEL FIT SUMMARY 

# marginal
summary(fit_MIMS_marg)
summary(fit_ENMO_marg)
summary(fit_MAD_marg)
summary(fit_AI_marg)

# conditional
summary(fit_MIMS_cond)
summary(fit_ENMO_cond)
summary(fit_MAD_cond)
summary(fit_AI_cond)



# ------------------------------------------------------------------------------
# TABLE 3 -- part B (correlations conditionally on age, sex, BMI)

fit_MIMS_marg_t <- tidy(fit_MIMS_marg) %>% mutate(label = name_levels[1], .before = everything())
fit_ENMO_marg_t <- tidy(fit_ENMO_marg) %>% mutate(label = name_levels[2], .before = everything())
fit_MAD_marg_t  <- tidy(fit_MAD_marg) %>% mutate(label = name_levels[3], .before = everything())
fit_AI_marg_t   <- tidy(fit_AI_marg) %>% mutate(label = name_levels[4], .before = everything())
fit_all_marg_tidy <- rbind(
  fit_MIMS_marg_t,
  fit_ENMO_marg_t,
  fit_MAD_marg_t,
  fit_AI_marg_t
)

fit_MIMS_cond_t <- tidy(fit_MIMS_cond) %>% mutate(label = name_levels[1], .before = everything())
fit_ENMO_cond_t <- tidy(fit_ENMO_cond) %>% mutate(label = name_levels[2], .before = everything())
fit_MAD_cond_t  <- tidy(fit_MAD_cond) %>% mutate(label = name_levels[3], .before = everything())
fit_AI_cond_t   <- tidy(fit_AI_cond) %>% mutate(label = name_levels[4], .before = everything())
fit_all_cond_tidy <- rbind(
  fit_MIMS_cond_t,
  fit_ENMO_cond_t,
  fit_MAD_cond_t,
  fit_AI_cond_t
)

table_df_marg <- 
  fit_all_marg_tidy %>%
  # filter(term %in% c("sexMale", "bmi", "age")) %>%
  mutate(estimate_f = sprintf("%.3f", estimate)) %>%
  mutate(stderror_f = sprintf("%.4f", std.error)) %>%
  mutate(res_f = paste0(estimate_f, " (", stderror_f, ")")) %>%
  mutate(res_f = paste0(res_f, ifelse(p.value < 0.05 & term != "(Intercept)", "*", " "))) %>%
  select(label, term, res_f) %>%
  pivot_wider(names_from = term, values_from = res_f) %>%
  select(label, intercept_m = `(Intercept)`)
table_df_marg 

table_df_cond <- 
  fit_all_cond_tidy %>%
  # filter(term %in% c("sexMale", "bmi", "age")) %>%
  mutate(estimate_f = sprintf("%.4f", estimate)) %>%
  mutate(stderror_f = sprintf("%.4f", std.error)) %>%
  mutate(res_f = paste0(estimate_f, " (", stderror_f, ")")) %>%
  mutate(res_f = paste0(res_f, ifelse(p.value < 0.05 & term != "(Intercept)", "*", " "))) %>%
  select(label, term, res_f) %>%
  pivot_wider(names_from = term, values_from = res_f) %>%
  select(label, intercept_c = `(Intercept)`, age, bmi, sexMale)
table_df_cond 

table_df <- 
  table_df_marg %>%
  left_join(table_df_cond, by = "label")
table_df %>%
  as.data.frame()

# View(tbl_out)
stargazer::stargazer(table_df, summary = FALSE)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# MODEL PLOT

head(dat)

summary(fit_MIMS_cond)
summary(fit_ENMO_cond)
summary(fit_MAD_cond)
summary(fit_AI_cond)



