#' This script generates a summary of quality flag generated basen on raw 
#' accelerometry data checks. 
#' 
#' qrsh -l mem_free=30G,h_vmem=30G,h_stack=256M

rm(list = ls())
library(tidyverse)
library(data.table)
library(lubridate)
library(arctools)
library(stringr)
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


# summary: (1) all participants & minutes --------------------------------------
dat_qf %>% summarize(
  any_anyaxis_g_maxabs = sum(anyaxis_g_maxabs > 0),
  any_anyaxis_contig_g_maxabs = sum(anyaxis_contig_g_maxabs > 0),
  sum_anyaxis_g_maxabs = sum(anyaxis_g_maxabs),
  sum_anyaxis_contig_g_maxabs = sum(anyaxis_contig_g_maxabs)
)
# any_anyaxis_g_maxabs any_anyaxis_contig_g_maxabs sum_anyaxis_g_maxabs
# 1                  865                         514               783704
# sum_anyaxis_contig_g_maxabs
# 1                      783210


# summary: (2) masterfile participants & minutes -------------------------------

# read "old" (before quality checks) masterfile file 
masterfile_fpath <- paste0(here::here(), "/data_processed/2021-02-25-measures_masterfile.rds")
masterfile <- readRDS(masterfile_fpath) %>%
  left_join(dat_qf, by = c("file_id", "HEADER_TIME_STAMP"))
dim(masterfile)

masterfile %>% 
  mutate(HEADER_TIME_STAMP_date = as.Date(HEADER_TIME_STAMP)) %>%
  select(subj_id, HEADER_TIME_STAMP_date) %>%
  distinct() %>%
  dim()
# [1] 4269    2

masterfile %>% 
  summarize(
    any_anyaxis_g_maxabs = sum(anyaxis_g_maxabs > 0),
    any_anyaxis_contig_g_maxabs = sum(anyaxis_contig_g_maxabs > 0),
    sum_anyaxis_g_maxabs = sum(anyaxis_g_maxabs),
    sum_anyaxis_contig_g_maxabs = sum(anyaxis_contig_g_maxabs)
) %>% 
  as.data.frame()  
# minutes_any_spike minutes_any_aslead_spike minutes_sum_spike
# 1               337                      146            208099
# minutes_sum_aslead_spike
# 1                   207490


# summary: (3) masterfile participants & minutes within wear time --------------
masterfile %>% 
  filter(wear_flag == 1) %>%
  summarize(
    any_anyaxis_g_maxabs = sum(anyaxis_g_maxabs > 0),
    any_anyaxis_contig_g_maxabs = sum(anyaxis_contig_g_maxabs > 0),
    sum_anyaxis_g_maxabs = sum(anyaxis_g_maxabs),
    sum_anyaxis_contig_g_maxabs = sum(anyaxis_contig_g_maxabs)
  ) %>% 
  as.data.frame()  
# any_anyaxis_g_maxabs any_anyaxis_contig_g_maxabs sum_anyaxis_g_maxabs
# 1                  288                          97               119899
# sum_anyaxis_contig_g_maxabs
# 1                      119609


df_tmp <- masterfile %>% filter(wear_flag == 1) 
summary(df_tmp$AC)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0   464.8  1541.1  2411.8 64406.1 
summary(df_tmp$ENMO)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00000  0.00283  0.01796  0.02607  0.03708 10.35156 


df_tmp <- masterfile %>% filter(wear_flag == 1, anyaxis_contig_g_maxabs == 0) 
summary(df_tmp$ENMO)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0   464.7  1541.0  2411.6 64406.1 
summary(df_tmp$ENMO)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00283 0.01796 0.02602 0.03708 5.63296 

df_tmp <- masterfile %>% filter(wear_flag == 1, anyaxis_g_maxabs == 0) 
summary(df_tmp$AC)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0   464.7  1540.7  2411.3 64406.1 
summary(df_tmp$ENMO)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.002829 0.017958 0.026014 0.037075 5.632961 





