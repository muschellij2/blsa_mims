
rm(list = ls())
library(tidyverse)
library(cowplot)
library(ggsci)
library(lubridate)
library(scales)
library(latex2exp)
options(scipen=999)

# plot colors
names_levels1 <- c("MIMS", "ENMO", "MAD", "AI")
names_levels2 <- c("AC_hat_from_MIMS", "AC_hat_from_ENMO", "AC_hat_from_MAD", "AC_hat_from_AI")
names_colors <- pal_futurama()(4)

theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=12))}
theme_set(theme_ggpr())


# ------------------------------------------------------------------------------
# read precomputed data 

path_tmp <- paste0(here::here(), "/results/2021-07-15-mapping_MPE_MAPE_mapminutelevel.rds")
dat_acc_agg <- readRDS(path_tmp)
head(dat_acc_agg)


# ------------------------------------------------------------------------------
# PLOT 1

names_levels <- c("MPE_MIMS", "MPE_ENMO", "MPE_MAD", "MPE_AI")
names_ylab_tex <- c(
  TeX("MPE of $\\hat{TAC}_{MIMS}$"),
  TeX("MPE of $\\hat{TAC}_{ENMO}$"),
  TeX("MPE of $\\hat{TAC}_{MAD}$"),
  TeX("MPE of $\\hat{TAC}_{AI}$")
)

plt_df <- 
  dat_acc_agg %>%
  select(starts_with("MPE_"), subj_id, AC_mean) %>%
  pivot_longer(cols = -c(subj_id, AC_mean)) %>%
  as.data.frame() %>%
  mutate(name_fct = factor(name, levels = names_levels, labels = names_ylab_tex))
head(plt_df)

plt_list <- list()
for (i in 1 : length(names_levels)){ # i <- 1
  name_tmp  <- names_levels[i]
  ylab_tmp  <- names_ylab_tex[i]
  color_tmp <- names_colors[i]
  plt_df_i    <- plt_df %>% filter(name == name_tmp)
  plt <- 
    ggplot(plt_df_i, aes(x = AC_mean, y = value)) +
    geom_point(alpha = 0.1, color = color_tmp) +
    scale_y_continuous(limits = 50 * c(-1, 1)) +
    # geom_smooth() + 
    geom_hline(yintercept = 0, linetype = 2) + 
    # facet_grid(rows = vars(name), labeller = label_parsed, switch = "both") + 
    labs(x = "Mean TAC", y = ylab_tmp) 
  plt
  plt_list[[length(plt_list) + 1]] <- plt
}

plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "vh", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-07-15-measures_mapping_error_MPE.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6) 

