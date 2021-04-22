
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
# read data 

fpath_tmp <- paste0(here::here(), "/data_processed/2021-04-13-measures_masterfile_winsorized_imp_mapped.rds")
dat_acc <- readRDS(fpath_tmp) %>% as.data.frame()
str(dat_acc)


# ------------------------------------------------------------------------------
# compute bias, i.e. mean error: i-th participant mean(estimated_ij - true_ij)

dat_acc_F <- dat_acc %>%
  filter(wear_and_valid_flag == 1) %>%
  mutate(
    E_MIMS = AC_hat_from_MIMS - AC,
    E_ENMO = AC_hat_from_ENMO - AC,
    E_MAD  = AC_hat_from_MAD  - AC,
    E_AI   = AC_hat_from_AI   - AC,
    
    SE_MIMS = E_MIMS^2,
    SE_ENMO = E_ENMO^2,
    SE_MAD  = E_MAD^2,
    SE_AI   = E_AI^2
  )

dat_acc_agg <- 
  dat_acc_F %>%
  group_by(subj_id) %>%
  summarise(
    ME_MIMS = mean(E_MIMS),
    ME_ENMO = mean(E_ENMO),
    ME_MAD  = mean(E_MAD),
    ME_AI   = mean(E_AI),
    
    MSE_MIMS = mean(SE_MIMS),
    MSE_ENMO = mean(SE_ENMO),
    MSE_MAD  = mean(SE_MAD),
    MSE_AI   = mean(SE_AI),
    
    days_cnt = n_distinct(HEADER_TIME_STAMP_date),
    AC_sum = sum(AC),
    AC_mean = mean(AC)
)


# ------------------------------------------------------------------------------
# generate tables

# summary: ME per participant
tbl_participant_ME <- rbind(
  as.numeric(summary(dat_acc_agg$ME_MIMS)),
  as.numeric(summary(dat_acc_agg$ME_ENMO)),
  as.numeric(summary(dat_acc_agg$ME_MAD)),
  as.numeric(summary(dat_acc_agg$ME_AI))
) %>% round(1) %>%
  as.data.frame() 
names(tbl_participant_ME) <- c(names(summary(1:10)))
tbl_participant_ME <- tbl_participant_ME %>% mutate(
  name = names_levels1, .before = everything()
)
tbl_participant_ME
View(tbl_participant_ME)


# summary: MSE per participant
tbl_participant_MSE <- rbind(
  as.numeric(summary(dat_acc_agg$MSE_MIMS)),
  as.numeric(summary(dat_acc_agg$MSE_ENMO)),
  as.numeric(summary(dat_acc_agg$MSE_MAD)),
  as.numeric(summary(dat_acc_agg$MSE_AI))
) %>% round(0) %>%
  as.data.frame() 
names(tbl_participant_MSE) <- c(names(summary(1:10)))
tbl_participant_MSE <- tbl_participant_MSE %>% mutate(
  name = names_levels1, .before = everything()
)
tbl_participant_MSE
View(tbl_participant_MSE)



# ------------------------------------------------------------------------------
# generate plot 

names_levels <- c("ME_MIMS", "ME_ENMO", "ME_MAD", "ME_AI")
names_ylab <- c(
  "mean $(\\hat{AC}_{MIMS} - AC)$",
  "mean $(\\hat{AC}_{ENMO} - AC)$",
  "mean $(\\hat{AC}_{MAD} - AC)$",
  "mean $(\\hat{AC}_{AI} - AC)$"
)
names_ylab_tex <- c(
  TeX("mean error $\\hat{AC}_{MIMS}$"),
  TeX("mean error $\\hat{AC}_{ENMO}$"),
  TeX("mean error $\\hat{AC}_{MAD}$"),
  TeX("mean error $\\hat{AC}_{AI}$")
)
names_ylab_tex <- c(
  TeX("ME of $\\hat{AC}_{MIMS}$"),
  TeX("ME of $\\hat{AC}_{ENMO}$"),
  TeX("ME of $\\hat{AC}_{MAD}$"),
  TeX("ME of $\\hat{AC}_{AI}$")
)

plt_df <- 
  dat_acc_agg %>%
  select(starts_with("ME_"), subj_id, AC_mean) %>%
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
    scale_y_continuous(limits = c(-2000, 2000)) + 
    # geom_smooth() + 
    geom_hline(yintercept = 0, linetype = 2) + 
    # facet_grid(rows = vars(name), labeller = label_parsed, switch = "both") + 
    labs(x = "Mean AC", y = ylab_tmp) 
  plt
  plt_list[[length(plt_list) + 1]] <- plt
}

plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "v", byrow = TRUE)
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-21-measures_mapping_ME.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6.5) 

