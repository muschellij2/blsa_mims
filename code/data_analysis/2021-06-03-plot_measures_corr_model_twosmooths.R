
#' This script contains model selection and model fit for a GAM model where: 
#' - outcome Y -- individual-specific correlation values between the measures 
#' - covariates X -- age, sex, BMI


rm(list = ls())
library(tidyverse)
library(ggsci)
library(cowplot)
options(scipen = 999)


# ------------------------------------------------------------------------------
# print model summaries 

# read precomputed GAM model summary
path_tmp <- paste0(here::here(), "/results/2021-05-07-measures_corr_model_fit_summary.rds")
fit_summary_list <- readRDS(path_tmp)

fit_summary_list[[1]]
fit_summary_list[[2]]
fit_summary_list[[3]]
fit_summary_list[[4]]


# ------------------------------------------------------------------------------
# get precomputed GAM model

path_tmp <- paste0(here::here(), "/results/2021-06-03-measures_corr_model_fit.rds")
model_fits <- readRDS(path_tmp)
fit_i <- model_fits$AC_ENMO
plot_obj_i <- (plot(fit_i, shift = 0, se = TRUE, seWithMean = FALSE, select = 1))[[1]]
plt_df <- data.frame(
  effect_var_value = plot_obj_i$x, 
  coef_fitted = plot_obj_i$fit, 
  coef_lwr = plot_obj_i$fit - plot_obj_i$se,
  coef_upr = plot_obj_i$fit + plot_obj_i$se
)

theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank())}
theme_set(theme_ggpr())

plt <- 
  ggplot(plt_df, 
         aes(x = effect_var_value, y = coef_fitted)) + 
  geom_ribbon(aes(ymin = coef_lwr, ymax = coef_upr), alpha = 0.2) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = 2, color = "grey60", size = 0.3) + 
  theme(legend.position = "none") + 
  labs(x = "Age",
       y = "Effect on y=corr(AC, ENMO)")
plt

plt_path <- paste0(here::here(), "/results_figures/2021-06-03-measures_corr_model_age_effect_AC_ENMO.png")
ggsave(filename = plt_path, plot = plt, width = 8/2, height = 3) 

