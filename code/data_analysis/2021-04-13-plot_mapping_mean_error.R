
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
    err_MIMS = AC_hat_from_MIMS - AC,
    err_ENMO = AC_hat_from_ENMO - AC,
    err_MAD  = AC_hat_from_MAD  - AC,
    err_AI   = AC_hat_from_AI   - AC
  )

dat_acc_agg <- 
  dat_acc_F %>%
  group_by(subj_id) %>%
  summarise(
    # RMSE_MIMS = mean(bias_MIMS^2),
    # RMSE_ENMO = mean(bias_ENMO^2),
    # RMSE_MAD = mean(bias_MAD^2),
    # RMSE_AI = mean(bias_AI^2),
    #
    # RMAE_MIMS = mean(abs(bias_MIMS)),
    # RMAE_ENMO = mean(abs(RMSE_ENMO)),
    # RMAE_MAD  = mean(abs(RMSE_MAD)),
    # RMAE_AI   = mean(abs(RMSE_AI)),
    err_MIMS = mean((err_MIMS)),
    err_ENMO = mean((err_ENMO)),
    err_MAD  = mean((err_MAD)),
    err_AI   = mean((err_AI)),
    days_cnt = n_distinct(HEADER_TIME_STAMP_date),
    AC_sum = sum(AC),
    AC_mean = mean(AC)
)

# ------------------------------------------------------------------------------
# generate tables

# summary: error across all partipants' minutes 
tbl_minute <- rbind(
  as.numeric(summary(dat_acc_F$err_MIMS)),
  as.numeric(summary(dat_acc_F$err_ENMO)),
  as.numeric(summary(dat_acc_F$err_MAD)),
  as.numeric(summary(dat_acc_F$err_AI))
) %>% round(1) %>%
  as.data.frame() 
names(tbl_minute) <- c(names(summary(1:10)))
tbl_minute <- tbl_minute %>% mutate(
  name = names_levels1, .before = everything()
)
tbl_minute


# summary: average error per participant 
tbl_participant <- rbind(
  as.numeric(summary(dat_acc_agg$err_MIMS)),
  as.numeric(summary(dat_acc_agg$err_ENMO)),
  as.numeric(summary(dat_acc_agg$err_MAD)),
  as.numeric(summary(dat_acc_agg$err_AI))
) %>% round(1) %>%
  as.data.frame() 
names(tbl_participant) <- c(names(summary(1:10)))
tbl_participant <- tbl_participant %>% mutate(
  name = names_levels1, .before = everything()
)
tbl_participant
  

# ------------------------------------------------------------------------------
# generate plot 

names_levels <- c("err_MIMS", "err_ENMO", "err_MAD", "err_AI")
names_ylab <- c(
  "mean $(\\hat{AC}_{MIMS} - AC)$",
  "mean $(\\hat{AC}_{ENMO} - AC)$",
  "mean $(\\hat{AC}_{MAD} - AC)$",
  "mean $(\\hat{AC}_{AI} - AC)$"
)
names_ylab_tex <- c(
  TeX("mean $(\\hat{AC}_{MIMS} - AC)$"),
  TeX("mean $(\\hat{AC}_{ENMO} - AC)$"),
  TeX("mean $(\\hat{AC}_{MAD} - AC)$"),
  TeX("mean $(\\hat{AC}_{AI} - AC)$")
)

plt_df <- 
  dat_acc_agg %>%
  select(starts_with("err"), subj_id, AC_mean) %>%
  pivot_longer(cols = -c(subj_id, AC_mean)) %>%
  as.data.frame() %>%
  mutate(name = factor(name, levels = names_levels, labels = names_ylab_tex))
head(plt_df)

plt <-
  ggplot(plt_df, aes(x = AC_mean, y = value)) +
  geom_point(alpha = 0.1) +
  scale_y_continuous(limits = c(-2000, 2000)) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  facet_grid(rows = vars(name), labeller = label_parsed, switch = "both") + 
  labs(x = "mean AC", y = "") + 
  theme(strip.text.y = element_text(size = 10))
plt

plt_path <- paste0(here::here(), "/results_figures/2021-04-13-measures_mapping_mean_error.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 6.5) 

