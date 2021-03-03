#' This script generates a summary of quality flag generated basen on raw 
#' accelerometry data checks. 
#' 
#' qrsh -l mem_free=10G,h_vmem=10G -now n
#' qrsh -l mem_free=30G,h_vmem=30G,h_stack=256M -now n

rm(list = ls())
library(tidyverse)
library(data.table)
library(lubridate)
library(arctools)
library(stringr)
library(janitor)
options(scipen=999)

# read the files mapping
fnames_qf <- sort(list.files(path = paste0(here::here(), "/quality_flag"), full.names = TRUE, pattern = "[.]rds"))
dat_qf_list <- out_dt_list <- vector(mode = "list", length = length(fnames_qf))
for (i in 1 : length(fnames_qf)){ # i <- 1
  print(i)
  fname_i   <- fnames_qf[i]
  file_id_i <- gsub(pattern = "_quality_flag.rds", replacement = "", basename(fname_i)) 
  dat_qf_i  <- readRDS(fname_i) %>% mutate(file_id = file_id_i, .before = everything())
  dat_qf_list[[i]] <- as.data.table(dat_qf_i)
}
dat_qf <- rbindlist(dat_qf_list) %>% as.data.frame()                        
dim(dat_qf)
head(dat_qf)


# define a conservative flag
dat_qf <- mutate(dat_qf, invalid_flag = as.numeric(
  anyaxis_contig_g_minN + anyaxis_g_maxN + anyaxis_spike > 0
))


# summary: (1) all participants & minutes --------------------------------------
dat_qf_agg_1 <- 
  dat_qf %>% 
  select(-c(file_id, HEADER_TIME_STAMP)) %>%
  summarize(across(.cols = everything(),
                   .fns = list(minutes = ~sum(.x > 0)),
                   # .fns = list(minute_cnt = ~sum(.x > 0),
                   #             minute_sum = ~sum(.x)),
                   .names = "{gsub('anyaxis_', '', col)}_{.fn}")) %>%
  as.data.frame()
dat_qf_agg_1$all_minutes = nrow(dat_qf)
dat_qf_agg_1


# summary: (2) masterfile participants & minutes -------------------------------

# read "old" (before quality checks) masterfile file 
masterfile_fpath <- paste0(here::here(), "/data_processed/2021-02-25-measures_masterfile.rds")
masterfile0 <- readRDS(masterfile_fpath)
masterfile <- masterfile0 %>%
  left_join(dat_qf, by = c("file_id", "HEADER_TIME_STAMP"))
dim(masterfile)

dat_qf_agg_2 <- 
  masterfile %>% 
  select(c(contains("anyaxis"), contains("invalid"))) %>%
  summarize(across(.cols = everything(),
                   .fns = list(minutes = ~sum(.x > 0)),
                   # .fns = list(minute_cnt = ~sum(.x > 0),
                   #             minute_sum = ~sum(.x)),
                   .names = "{gsub('anyaxis_', '', col)}_{.fn}")) %>%
  as.data.frame()
dat_qf_agg_2$all_minutes = nrow(masterfile)
dat_qf_agg_2


# summary: (3) masterfile participants & minutes within wear minutes  ----------

dat_qf_agg_3 <- 
  masterfile %>% 
  filter(wear_flag == 1) %>%
  select(c(contains("anyaxis"), contains("invalid"))) %>%
  summarize(across(.cols = everything(),
                   .fns = list(minutes = ~sum(.x > 0)),
                   # .fns = list(minute_cnt = ~sum(.x > 0),
                   #             minute_sum = ~sum(.x)),
                   .names = "{gsub('anyaxis_', '', col)}_{.fn}")) %>%
  as.data.frame()
dat_qf_agg_3$all_minutes = nrow(masterfile %>% filter(wear_flag == 1))
dat_qf_agg_3

dat_qf_agg_comb <- rbind(
  dat_qf_agg_1,
  dat_qf_agg_2,
  dat_qf_agg_3
) %>% 
  mutate(invalid_flag_prop = round(invalid_flag_minutes / all_minutes, 5), .before = all_minutes) %>%
  mutate(invalid_flag_pct = invalid_flag_prop * 100, .before = all_minutes)
dat_qf_agg_comb
names(dat_qf_agg_comb) <- sapply(names(dat_qf_agg_comb), function(val) gsub("_minutes", "", val))



# ------------------------------------------------------------------------------
# define final flag 

dat_summary <- function(df_tmp){
  df_tmp_sum_l <- lapply(df_tmp, summary)
  df_tmp_sum_l2 <- lapply(df_tmp_sum_l, function(x) c(x, 0)[1:7])
  df_tmp_sum_df <- do.call(rbind, df_tmp_sum_l2) %>% as.data.frame()
  colnames(df_tmp_sum_df) <- c(names(summary(1:2)), "na_cnt")
  df_tmp_sum_df <- df_tmp_sum_df %>% round(3)
  df_tmp_sum_df <- mutate(df_tmp_sum_df, measure = rownames(df_tmp_sum_df), .before = everything())
  rownames(df_tmp_sum_df) <- NULL
  df_tmp_sum_df <- clean_names(df_tmp_sum_df)
  return(df_tmp_sum_df)
}
vars <- c("AC", "MIMS", "ENMO", "MAD", "AI")

df_tmp_1 <- masterfile %>% 
  select(all_of(vars))%>%
  dat_summary()

df_tmp_2 <- masterfile %>% 
  filter(wear_flag == 1) %>%
  select(all_of(vars))%>%
  dat_summary()

df_tmp_3 <- masterfile %>% 
  filter(wear_flag == 1, invalid_flag == 0) %>%
  select(all_of(vars))%>%
  dat_summary()


# more conservative 
df_tmp_1
#   measure min x1st_qu  median     mean  x3rd_qu       max na_cnt
# 1      AC   0   0.000 461.620 1538.965 2407.470 64406.140      0
# 2    MIMS   0   0.000   3.627    7.899   13.084   419.185     73
# 3    ENMO   0   0.003   0.018    0.026    0.037    10.352      0
# 4     MAD   0   0.000   0.013    0.034    0.048     5.121      0
# 5      AI   0   0.000   1.241    2.913    4.534   144.272      0

df_tmp_2
#   measure min x1st_qu  median     mean  x3rd_qu       max na_cnt
# 1      AC   0   0.000 464.780 1541.109 2411.780 64406.140      0
# 2    MIMS   0   0.000   3.646    7.910   13.104   419.185     24
# 3    ENMO   0   0.003   0.018    0.026    0.037    10.352      0
# 4     MAD   0   0.000   0.014    0.034    0.049     5.121      0
# 5      AI   0   0.000   1.247    2.917    4.541   144.272      0

df_tmp_3
#   measure min x1st_qu  median     mean  x3rd_qu       max na_cnt
# 1      AC   0   0.000 464.450 1539.666 2410.400 64406.140      0
# 2    MIMS   0   0.000   3.644    7.903   13.098   334.645      0
# 3    ENMO   0   0.003   0.018    0.026    0.037     3.546      0
# 4     MAD   0   0.000   0.014    0.033    0.048     2.651      0
# 5      AI   0   0.000   1.247    2.914    4.539   144.272      0


# less conservative: 1st three conditions of NHANES
df_tmp_3
#   measure min x1st_qu  median     mean  x3rd_qu       max na_cnt
# 1      AC   0   0.000 464.510 1539.883 2410.640 64406.140      0
# 2    MIMS   0   0.000   3.644    7.904   13.099   334.645      0
# 3    ENMO   0   0.003   0.018    0.026    0.037     3.546      0
# 4     MAD   0   0.000   0.014    0.034    0.049     2.651      0
# 5      AI   0   0.000   1.247    2.915    4.539   144.272      0
