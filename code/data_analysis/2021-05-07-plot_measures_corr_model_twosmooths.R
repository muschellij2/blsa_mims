
#' This script contains model selection and model fit for a GAM model where: 
#' - outcome Y -- individual-specific correlation values between the measures 
#' - covariates X -- age, sex, BMI


rm(list = ls())
library(tidyverse)
library(ggsci)
library(cowplot)
options(scipen = 999)

# ------------------------------------------------------------------------------
# read data 

# read accelerometry measures master file
acc_corr_fpath <- paste0(here::here(), "/results/2021-05-06-correlations_between_measures.rds")
acc_corr <- readRDS(acc_corr_fpath)
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
df_points <- 
  acc_corr %>%
  inner_join(mastervisit, by = c("subj_id", "visit_id")) %>%
  inner_join(masterdemog, by = c("subj_id")) %>% 
  filter(from11to5_exclude == 0)

# precomputed GAM fitted values
path_tmp <- paste0(here::here(), "/results/2021-05-07-measures_corr_model_fitted_smooth_df.rds")
df_fitted <- readRDS(path_tmp) 

# precomputed GAM model summary
path_tmp <- paste0(here::here(), "/results/2021-05-07-measures_corr_model_fit_summary.rds")
fit_summary_list <- readRDS(path_tmp)


# ------------------------------------------------------------------------------
# format data variables

# define corr names values 
names_levels <- c(
  "AC_MIMS",
  "AC_ENMO",
  "AC_MAD",
  "AC_AI"
)
# define corr names levels, labels
names_labels  <- sapply(names_levels, function(val) paste0(str_split(val, "_")[[1]], collapse = ", "))
names_labels  <- paste0("(", names_labels, ")")
names_labels2 <- paste0("corr", names_labels)
names_labels3 <- paste0("y = corr", names_labels)
names_colors <- pal_futurama()(4)

# data frame with data points for the plot
plt_df_points <-
  df_points %>%
  select(all_of(c(names_levels, "age", "bmi", "sex"))) %>%
  pivot_longer(cols = -c(age, bmi, sex)) %>%
  mutate(name_fct = factor(name, levels = names_levels, labels = names_labels3)) 

plt_df_fitted <- 
  df_fitted %>%
  mutate(name_fct = factor(name, levels = names_levels, labels = names_labels3)) 


theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank())}
theme_set(theme_ggpr())


# ------------------------------------------------------------------------------
# Plot: (1) age 

plt_list <- list()
for (i in 1 : length(names_levels)){ # i <- 1
  name_i  <- names_levels[i]
  name_label_i  <- names_labels3[i]
  color_i <- names_colors[i]
  plt_df_points_i <- plt_df_points %>% filter(name == name_i)
  plt_df_fitted_i <- plt_df_fitted %>% filter(name == name_i)
  plt <- 
    ggplot(plt_df_fitted_i %>% filter(effect_var_name == "age"), 
           aes(x = effect_var_value, y = y_fitted_s)) + 
    geom_point(data = plt_df_points_i, aes(x = age, y = value), alpha = 0.1, color = color_i) +
    geom_ribbon(aes(ymin = y_lwr_s, ymax = y_upr_s), alpha = 0.2) + 
    geom_line() + 
    theme(legend.position = "none") + 
    scale_color_futurama() + 
    scale_y_continuous(limits = c(0.5, 1)) + 
    labs(x = "Age",
         y = name_label_i)
  plt_list[[length(plt_list) + 1]] <- plt
}

plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-05-07-measures_corr_model_age_effect_twosmooths.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6.5) 


# ------------------------------------------------------------------------------
# Plot: (2) BMI 

plt_list <- list()
for (i in 1 : length(names_levels)){ # i <- 1
  name_i  <- names_levels[i]
  name_label_i  <- names_labels3[i]
  color_i <- names_colors[i]
  plt_df_points_i <- plt_df_points %>% filter(name == name_i)
  plt_df_fitted_i <- plt_df_fitted %>% filter(name == name_i)
  plt <- 
    ggplot(plt_df_fitted_i %>% filter(effect_var_name == "bmi"), 
           aes(x = effect_var_value, y = y_fitted_s)) + 
    geom_point(data = plt_df_points_i, 
               aes(x = bmi, y = value), alpha = 0.1, color = color_i) +
    geom_ribbon(aes(ymin = y_lwr_s, ymax = y_upr_s), alpha = 0.2) + 
    geom_line() + 
    theme(legend.position = "none") + 
    scale_color_futurama() + 
    scale_y_continuous(limits = c(0.5, 1)) + 
    labs(x = "BMI",
         y = name_label_i)
  plt_list[[length(plt_list) + 1]] <- plt
}

plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-05-07-measures_corr_model_bmi_effect_twosmooths.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6.5) 


# ------------------------------------------------------------------------------
# summayr model fit

# "AC_MIMS",
# "AC_ENMO",
# "AC_MAD",
# "AC_AI",

fit_summary_list[[1]]
fit_summary_list[[2]]
fit_summary_list[[3]]
fit_summary_list[[4]]


