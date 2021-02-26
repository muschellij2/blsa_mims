#' This script is to summarize negative MIMS results.


rm(list = ls())
library(tidyverse)
library(lubridate) 

# pull file names
fnames_rds = sort(list.files(path = here::here("open_source_measures"), full.names = TRUE, pattern = "[.]rds"))
fnames_csv = sort(list.files(path = here::here("csv"), full.names = TRUE))

lapply(list(fnames_rds, fnames_csv), length)
lapply(list(fnames_rds, fnames_csv), head)

# vector to store raw data results
fnames_rds0 <- sapply(fnames_rds, function(fname) gsub(pattern = "_OSM.rds", replacement = "", basename(fname)))
fnames_csv0 <- sapply(fnames_csv, function(fname) gsub(pattern = "60sec.csv", replacement = "", basename(fname)))
fnames_df <- data.frame(
  fname_rds = fnames_rds, 
  fname_csv = fnames_csv,
  fname_rds0 = fnames_rds0,
  fname_csv0 = fnames_csv0) %>%
  mutate(rds0_csv0 = (fname_rds0 == fname_csv0))
table(fnames_df$rds0_csv0)

# search for MIMS < 0 
out_df <- data.frame()
for (i in 1 : nrow(fnames_df)){
  message(i)
  fname_rds_i <- fnames_df$fname_rds[i]
  fname_csv_i <- fnames_df$fname_csv[i]
  file_id_i   <- fnames_df$fname_csv0[i]
  rds_i <- readRDS(fname_rds_i)
  if (any(rds_i$MIMS_UNIT < 0)){
    csv_i <- 
      data.table::fread(fname_csv_i) %>% as.data.frame() %>%
      dplyr::select(HEADER_TIME_STAMP = timestamp, AC = vectormagnitude) %>%
      dplyr::mutate(HEADER_TIME_STAMP = ymd_hms(HEADER_TIME_STAMP)) 
    dat_i <- 
      rds_i %>% 
      full_join(csv_i, by = "HEADER_TIME_STAMP") %>% 
      select(HEADER_TIME_STAMP, MIMS_UNIT, AC) %>%
      mutate(file_id = file_id_i,
             file_idx = i,
             obs_idx = row_number()) %>%
      filter(MIMS_UNIT < 0)
    print(paste0("appending df i: ", i, ", nrow(dat_i): ", nrow(dat_i)))
    out_df <- rbind(out_df, dat_i)
  }
}

out_df <- as.data.frame(out_df)
out_df

# summarize MIMS < 0 
dim(out_df)
out_df_agg <-
  out_df %>% 
  group_by(file_idx, file_id) %>%
  summarize(
    cnt = n(),
    obs_idx_min = min(obs_idx),
    obs_idx_max = max(obs_idx),
    AC_mean = mean(AC, na.rm = TRUE),
    AC_sd = sd(AC, na.rm = TRUE),
    AC_median = median(AC, na.rm = TRUE),
    AC_min = min(AC, na.rm = TRUE),
    AC_max = max(AC, na.rm = TRUE)
  ) %>%
  as.data.frame()
out_df_agg

out_df_agg %>% filter(AC_min != Inf)
#   file_idx                            file_id cnt obs_idx_min obs_idx_max
# 1      551 493405WaTAS1E23150402 (2015-09-09)   1        6027        6027
# 2      839 599705WaTAS1E23150406 (2015-09-18)  22        6982        7008
# 3      904 612007WaTAS1H12190132 (2019-09-03) 312        2111        2422
#       AC_mean      AC_sd AC_median   AC_min   AC_max
# 1 21843.09000         NA 21843.090 21843.09 21843.09
# 2 10040.86591 12631.3694  4449.265  1427.00 42895.36
# 3    18.61042   328.7255     0.000     0.00  5806.45
