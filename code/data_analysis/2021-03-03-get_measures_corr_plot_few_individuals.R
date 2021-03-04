#' This script generates plot of minute-level values for a few selected 
#' individuals to demonstrate examples 
#' of extreme / median values of correlations between the 
#' open-source measures. 
#' 
#' Input: 
#' - /data_processed/2021-03-03-measures_masterfile_winsorized.rds
#' - /results/2021-03-03-correlations_between_measures.rds

rm(list = ls())
library(tidyverse)
library(gridExtra)
library(ggsci)


# ------------------------------------------------------------------------------
# read data 

# read accelerometry measures master file
acc_corr_fpath <- paste0(here::here(), "/results/2021-03-03-correlations_between_measures.rds")
acc_corr <- readRDS(acc_corr_fpath)
dim(acc_corr)
# [1] 1442   15

masterfile_path <- paste0(here::here(), "/data_processed/2021-03-03-measures_masterfile_winsorized.rds")
masterfile <- readRDS(masterfile_path) %>% as.data.frame()
dim(masterfile)
# [1] 6147240      10

# masterfile_uw_path <- paste0(here::here(), "/data_processed/2021-03-03-measures_masterfile.rds")
# masterfile_uw <- readRDS(masterfile_uw_path) %>% as.data.frame()
# dim(masterfile_uw)

# winsorizing values file
fpath_tmp <- paste0(here::here(), "/results/2021-03-03-measures_vals_summary_population.rds")
wn_df <- readRDS(fpath_tmp)
str(wn_df)
wn_vec <- wn_df[, "val_qt_0.999"] # @MK: changed Mar 3 
names(wn_vec) <- wn_df[, "metric_name"]

# define correlation variable names, labels 
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
var_labels <- sapply(var_names, function(val){
  paste0("cor(", strsplit(val, "_")[[1]][1], ", ", strsplit(val, "_")[[1]][2], ")")
})


acc_corr_long <-
  acc_corr %>% 
  filter(from11to5_exclude == 0) %>%
  select(all_of(c(var_names[1:4], "subj_id"))) %>%
  pivot_longer(cols = -subj_id) %>%
  as.data.frame()
corr_subj_id_sub_vec <- c(
  acc_corr_long %>% filter(name == "AC_MIMS") %>% filter(value == min(value)) %>% pull(subj_id),
  acc_corr_long %>% filter(name == "AC_ENMO") %>% filter(value == min(value)) %>% pull(subj_id),
  acc_corr_long %>% filter(name == "AC_MAD")   %>% filter(value == min(value)) %>% pull(subj_id)
) 
corr_subj_id_sub <- expand.grid(
  subj_id = corr_subj_id_sub_vec,
  name = var_names[1:4], 
  stringsAsFactors = FALSE
) %>%
  mutate(subj_id_fct  = factor(subj_id, levels = corr_subj_id_sub_vec)) %>%
  mutate(name_fct  = factor(name, levels = var_names[1:4])) %>%
  arrange(name_fct, subj_id_fct) %>%
  as.data.frame()

corr_subj_id_sub_vec
str(corr_subj_id_sub)


# ------------------------------------------------------------------------------
# PLOT 1: minute-level data scatterplot

plt_colors <- pal_futurama()(4); names(plt_colors) <- var_names[1:4]

theme_ggpr <- function(){ 
  font <- "Arial"  
  theme_minimal(base_size = 12) %+replace%    
    theme(panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank()    
    )
}
theme_set(theme_ggpr())

plt_list <- list()
for (i in 1 : nrow(corr_subj_id_sub)){ # i <- 1
  print(paste0("i: ", i))
  name_i       <- corr_subj_id_sub[i, "name"]
  subj_id_i    <- corr_subj_id_sub[i, "subj_id"]
  v2_i         <- strsplit(name_i, "_")[[1]][2]
  plt_df_i   <- 
    masterfile %>% 
    filter(subj_id == subj_id_i) %>% 
    filter(wear_and_valid_flag == 1)
  plt_df_i$x <- plt_df_i[, "AC"]
  plt_df_i$y <- plt_df_i[, v2_i]
  plt_color_i  <- plt_colors[name_i]
  ylim_max_i   <- wn_vec[gsub("AC_", "", name_i)]
  plt <- 
    ggplot(plt_df_i, aes(x = x, y = y)) + 
    geom_point(alpha = 0.1, color = plt_color_i, size = 0.3) + 
    # geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.3) +
    labs(x = "AC", y = v2_i, 
         title = paste0("ID=", subj_id_i, ", corr(AC,", v2_i, ")=", 
                        round(cor(plt_df_i$x, plt_df_i$y, use = "pairwise.complete.obs"), 3))) + 
    scale_x_continuous(limits = c(0, wn_vec["AC"])) + 
    scale_y_continuous(limits = c(0, ylim_max_i)) + 
    theme(plot.title = element_text(size = 10))
  plt_list[[length(plt_list) + 1]] <- plt
}

# do.call("grid.arrange", c(plt_list, ncol = 3))

plt <- do.call("arrangeGrob", c(plt_list, ncol = 3)) 
plt_path <- paste0(here::here(), "/results_figures/measures_corr_plot_few_scatterplot.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8) 




# ------------------------------------------------------------------------------
# PLOT 2: minute-level data ts

names_vals <- c("MIMS", "ENMO", "MAD", "AI", "AC")
names_colors <- c(pal_futurama()(4), "black")

# for a trick to keep y-axis values the same across three participants plots 
wn_vec_df <- data.frame(name = names(wn_vec), value_yaxis_max = wn_vec)

plt_list <- list()
for (i in 1 : length(corr_subj_id_sub_vec)){ # i <- 1
  print(paste0("i: ", i))
  subj_id_i <- corr_subj_id_sub_vec[i]
  plt_df_i  <- 
    masterfile %>% 
    filter(subj_id == subj_id_i) %>% 
    filter(wear_and_valid_flag == 1) %>%
    select(all_of(c("HEADER_TIME_STAMP", names_vals))) %>%
    pivot_longer(cols = -HEADER_TIME_STAMP) %>%
    mutate(name_fct = factor(name, levels = names_vals))
  plt_df_i_yaxis <- 
    plt_df_i %>% 
    group_by(name, name_fct) %>%
    filter(HEADER_TIME_STAMP == min(HEADER_TIME_STAMP)) %>%
    left_join(wn_vec_df, by = "name") %>%
    ungroup()
  plt <- 
    ggplot(plt_df_i, aes(x = HEADER_TIME_STAMP, y = value, color = name_fct)) + 
    geom_point(data = plt_df_i_yaxis, aes(x = HEADER_TIME_STAMP,
                                          y = value_yaxis_max, color = name_fct), alpha = 0.01) +
    geom_point(alpha = 0.1, size = 0.3) + 
    labs(title = paste0("ID=", subj_id_i), 
         y = "", x = "") + 
    theme(plot.title = element_text(size = 10)) + 
    facet_grid(name_fct ~ ., scales = "free") + 
    scale_color_manual(values = names_colors) + 
    scale_y_continuous(limits = c(0, NA)) +
    theme(legend.position = "none")
  plt
  plt_list[[length(plt_list) + 1]] <- plt
}

# do.call("grid.arrange", c(plt_list, ncol = 3))

plt <- do.call("arrangeGrob", c(plt_list, ncol = 3)) 
plt_path <- paste0(here::here(), "/results_figures/measures_corr_plot_few_ts.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8) 
