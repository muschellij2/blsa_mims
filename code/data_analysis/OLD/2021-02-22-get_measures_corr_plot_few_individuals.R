#' This script generates plot of minute-level values for a few selected 
#' individuals to demonstrate examples 
#' of extreme / median values of correlations between the 
#' open-source measures. 
#' 
#' Input: 
#' - /data_processed/2021-01-19-measures_masterfile_winsorized.rds
#' - /results/2021-02-22-correlations_between_measures.rds

rm(list = ls())
library(tidyverse)
library(gridExtra)
library(ggsci)


# ------------------------------------------------------------------------------
# read data 

# read accelerometry measures master file
acc_corr_fpath <- paste0(here::here(), "/results/2021-02-22-correlations_between_measures.rds")
acc_corr <- readRDS(acc_corr_fpath)
dim(acc_corr)
# [1] 1442   15

masterfile_path <- paste0(here::here(), "/data_processed/2021-01-19-measures_masterfile_winsorized.rds")
masterfile <- readRDS(masterfile_path) %>% as.data.frame()
dim(masterfile)
# [1] 6147240      10

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

# generate subsets of min, median, max values per each pair of correlation measures
# acc_corr_long <-
#   acc_corr %>% 
#   filter(from11to5_exclude == 0) %>%
#   select(all_of(c(var_names[1:4], "subj_id"))) %>%
#   pivot_longer(cols = -subj_id) %>% 
#   group_by(name)
# corr_subj_id_sub <- rbind(
#   acc_corr_long %>% filter(value == min(value)) %>% mutate(agg_func = "min"),
#   acc_corr_long %>% filter(value == median(value)) %>% mutate(agg_func = "med"),
#   acc_corr_long %>% filter(value == max(value)) %>% mutate(agg_func = "max")
# ) %>%
#   mutate(agg_func = factor(agg_func, levels = c("min", "med", "max"))) %>%
#   mutate(name_fct  = factor(name, levels = var_names[1:4])) %>%
#   arrange(name_fct, agg_func) %>%
#   as.data.frame()
# corr_subj_id_sub

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
plt_ylim_max <- c(160, 1.2, 1, 80); names(plt_ylim_max) <- var_names[1:4]

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
  plt_df_i   <- masterfile %>% filter(subj_id == subj_id_i) 
  plt_df_i$x <- plt_df_i[, "AC"]
  plt_df_i$y <- plt_df_i[, v2_i]
  plt_color_i  <- plt_colors[name_i]
  ylim_max_i   <- plt_ylim_max[name_i]
  plt <- 
    ggplot(plt_df_i, aes(x = x, y = y)) + 
    geom_point(alpha = 0.1, color = plt_color_i, size = 0.3) + 
    # geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.3) +
    labs(x = "AC", y = v2_i, 
         title = paste0("ID=", subj_id_i, ", corr(AC,", v2_i, ")=", round(cor(plt_df_i$x, plt_df_i$y), 3))) + 
    scale_x_continuous(limits = c(0, 20000)) + 
    scale_y_continuous(limits = c(0, NA)) + 
    theme(plot.title = element_text(size = 10))
  plt_list[[length(plt_list) + 1]] <- plt
}

do.call("grid.arrange", c(plt_list, ncol = 3))

plt <- do.call("arrangeGrob", c(plt_list, ncol = 3)) 
plt_path <- paste0(here::here(), "/results_figures/measures_corr_plot_few_scatterplot.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8) 




# ------------------------------------------------------------------------------
# PLOT 2: minute-level data ts

names_vals <- c("MIMS", "ENMO", "MAD", "AI", "AC")
names_colors <- c(pal_futurama()(4), "black")

plt_list <- list()
for (i in 1 : length(corr_subj_id_sub_vec)){ # i <- 1
  print(paste0("i: ", i))
  subj_id_i <- corr_subj_id_sub_vec[i]
  plt_df_i  <- 
    masterfile %>% 
    filter(subj_id == subj_id_i) %>%
    select(all_of(c("HEADER_TIME_STAMP", names_vals))) %>%
    pivot_longer(cols = -HEADER_TIME_STAMP) %>%
    mutate(name_fct = factor(name, levels = names_vals))
  plt <- 
    ggplot(plt_df_i, aes(x = HEADER_TIME_STAMP, y = value, color = name_fct)) + 
    geom_point(alpha = 0.1, size = 0.3) + 
    labs(title = paste0("ID=", subj_id_i), 
         y = "", x = "") + 
    theme(plot.title = element_text(size = 10)) + 
    facet_grid(name_fct ~ ., scales = "free_y") + 
    scale_color_manual(values = names_colors) + 
    theme(legend.position = "none")
  plt
  plt_list[[length(plt_list) + 1]] <- plt
}

do.call("grid.arrange", c(plt_list, ncol = 3))

plt <- do.call("arrangeGrob", c(plt_list, ncol = 3)) 
plt_path <- paste0(here::here(), "/results_figures/measures_corr_plot_few_ts.png")
ggsave(filename = plt_path, plot = plt, width = 8, height = 8) 



# ------------------------------------------------------------------------------
# (1) investigate closely the case of ID = 6120

subj_id_tmp <- 6120

masterfile_sub <- masterfile %>% filter(subj_id == subj_id_tmp)

masterfile_sub %>% filter(ENMO > 4)
# file_id subj_id visit_id   HEADER_TIME_STAMP wear_flag      AC MIMS
# 1  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:10:00         1 5806.45    0
# 2  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:11:00         0    0.00    0
# 3  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:12:00         0    0.00    0
# 4  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:13:00         0    0.00    0
# 5  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:14:00         0    0.00    0
# 6  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:15:00         0    0.00    0
# 7  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:16:00         0    0.00    0

file_id_tmp <- masterfile_sub[1, "file_id"]
raw_mat_fname <- paste0(file_id_tmp, "RAW.mat")
raw_mat_fpath <- paste0("/Users/martakaras/OneDrive - Johns Hopkins/BLSA/mat/", raw_mat_fname)

source(here::here("code/helper_functions.R"))

# read raw accelerometry data .mat file
acc_df = read_acc_mat(raw_mat_fpath)
# pull meta information about the file 
srate = acc_df$fs
header = acc_df$hed
dynamic_range =  get_dynamic_range(header)
# subset data to keep timestamp and three axes data only
acc_df = acc_df$Xi
acc_df = acc_df %>%
  select(HEADER_TIME_STAMP, X, Y, Z) %>%
  mutate(HEADER_TIME_STAMP_FLOOR = lubridate::floor_date(HEADER_TIME_STAMP, "1 min"))
# compute minute-level ENMO
out_ENMO = acc_df %>% 
  mutate(         
    r = sqrt(X^2 + Y^2 + Z^2),
    ENMO_t = ifelse(r - 1 > 0 , r - 1, 0)) %>% 
  group_by(HEADER_TIME_STAMP_FLOOR) %>% 
  mutate(
    ENMO = mean(ENMO_t)
  ) %>%
  ungroup() 

tmp <- out_ENMO %>% filter(Y == -8)

rbind(head(tmp), tail(tmp))

# # A tibble: 662,400 x 8
# HEADER_TIME_STAMP        X     Y     Z HEADER_TIME_STAMP_FLOOR     r ENMO_t  ENMO
# <dttm>               <dbl> <dbl> <dbl> <dttm>                  <dbl>  <dbl> <dbl>
# 1 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 2 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 3 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 4 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 5 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 6 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 7 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 8 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 9 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 10 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00



# ------------------------------------------------------------------------------
# (2) investigate closely the case of ID = 5997

subj_id_tmp <- 5997

masterfile_sub <- masterfile %>% filter(subj_id == subj_id_tmp)


masterfile_sub %>%
  group_by(aa = as.Date(HEADER_TIME_STAMP)) %>%
  summarise(corr = cor(AC, MIMS))

cor(masterfile_sub$MIMS, masterfile_sub$AC)
cor(masterfile_sub$MIMS, masterfile_sub$AC)

masterfile_sub %>% filter(MIMS == 0, AC > 0)
# file_id subj_id visit_id   HEADER_TIME_STAMP wear_flag      AC MIMS
# 1  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:10:00         1 5806.45    0
# 2  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:11:00         0    0.00    0
# 3  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:12:00         0    0.00    0
# 4  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:13:00         0    0.00    0
# 5  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:14:00         0    0.00    0
# 6  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:15:00         0    0.00    0
# 7  612007WaTAS1H12190132 (2019-09-03)    6120        7 2019-08-23 23:16:00         0    0.00    0

file_id_tmp <- masterfile_sub[1, "file_id"]
raw_mat_fname <- paste0(file_id_tmp, "RAW.mat")
raw_mat_fpath <- paste0("/Users/martakaras/OneDrive - Johns Hopkins/BLSA/mat/", raw_mat_fname)

source(here::here("code/helper_functions.R"))

# read raw accelerometry data .mat file
acc_df = read_acc_mat(raw_mat_fpath)
# pull meta information about the file 
srate = acc_df$fs
header = acc_df$hed
dynamic_range =  get_dynamic_range(header)
# subset data to keep timestamp and three axes data only
acc_df = acc_df$Xi
acc_df = acc_df %>%
  select(HEADER_TIME_STAMP, X, Y, Z) %>%
  mutate(HEADER_TIME_STAMP_FLOOR = lubridate::floor_date(HEADER_TIME_STAMP, "1 min"))
# compute minute-level ENMO
out_ENMO = acc_df %>% 
  mutate(         
    r = sqrt(X^2 + Y^2 + Z^2),
    ENMO_t = ifelse(r - 1 > 0 , r - 1, 0)) %>% 
  group_by(HEADER_TIME_STAMP_FLOOR) %>% 
  mutate(
    ENMO = mean(ENMO_t)
  ) %>%
  ungroup() 

tmp <- out_ENMO %>% filter(ENMO > 6)
tmp

tmp <- out_ENMO %>% filter(Y == -8)

rbind(head(tmp), tail(tmp))

# # A tibble: 662,400 x 8
# HEADER_TIME_STAMP        X     Y     Z HEADER_TIME_STAMP_FLOOR     r ENMO_t  ENMO
# <dttm>               <dbl> <dbl> <dbl> <dttm>                  <dbl>  <dbl> <dbl>
# 1 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 2 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 3 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 4 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 5 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 6 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 7 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 8 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 9 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00
# 10 2019-08-23 23:11:00 -0.141    -8 0.109 2019-08-23 23:11:00      8.00   7.00  7.00




# ------------------------------------------------------------------------------
# compare values with and without non-wear

names_vals <- c("MIMS", "ENMO", "MAD", "AI", "AC")
names_colors <- c(pal_futurama()(4), "black")

masterfile_plt <- 
  masterfile %>% 
  select(all_of(c("wear_flag", names_vals))) %>%
  pivot_longer(cols = -wear_flag)

masterfile_plt_agg <- 
  masterfile_plt %>% 
  dplyr::group_by(wear_flag, name) %>% 
  dplyr::summarise(
    value_min = min(value),
    value_mean = mean(value),
    value_med = median(value),
    value_max = max(value),
    value_qt0.99 = quantile(value, probs = 0.99)
  ) %>%
  as.data.frame()
masterfile_plt_agg

vec <- masterfile %>% 
  filter(wear_flag == 0) %>%
  pull(ENMO)

tail(sort(vec), 100)

masterfile_sub %>% 
  select(all_of(c("wear_flag", names_vals))) %>%
  pivot_longer(cols = -wear_flag) %>% 
  dplyr::group_by(wear_flag, name) %>% 
  dplyr::summarise(
    value_min = min(value),
    value_mean = mean(value),
    value_med = median(value),
    value_max = max(value)
  ) %>%
  as.data.frame()



