#' This script generates "Table 1" for the paper. 
#' 
#' It only summarizes (participant, visit) pairs for which we have data in 
#' the masterfile.
#' 
#' Input: 
#' > /data_processed/2021-01-18-measures_masterfile.rds


rm(list = ls())
library(tidyverse)

# read accelerometry measures master file
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-01-18-measures_masterfile.rds")
dat_acc <- readRDS(dat_acc_fpath)
dat_acc_id <- dat_acc %>%
  select(file_id, subj_id, visit_id) %>%
  distinct()
str(dat_acc_id)

# read demographics data masterfile 
dat_demog_fpath <- paste0(here::here(), "/covariates/mastervisit.rdata")
mastervisit <- get(load(dat_demog_fpath, ex <- new.env()), envir = ex)
mastervisit <- 
  mastervisit0 %>%
  select(idno, visit, dov) %>% 
  mutate(mastervisit_entry = 1) 

dat_comb_nan <- 
  dat_acc_id %>%
  left_join(mastervisit, by = c("subj_id" = "idno", "visit_id" = "visit")) %>% 
  filter(is.na(mastervisit_entry)) %>%
  select(file_id, subj_id, visit_id) %>%
  mutate(file_id_date = as.Date(str_sub(file_id,-11,-2)), .after = "file_id") %>%
  arrange(file_id_date) %>% 
  as.data.frame()

range(mastervisit$dov, na.rm = TRUE)
# [1] "1958-02-06" "2019-06-25"

file_id_date_vec <- 
  dat_comb_nan %>%
  filter(subj_id != 7518) %>% 
  pull(file_id_date) 
range(file_id_date_vec)
# [1] "2019-08-02" "2020-03-18"

# check for whom I can pull data from previous visit 
dat_comb_nan_test <- 
  dat_comb_nan %>%
  left_join(mastervisit, by = c("subj_id" = "idno")) %>%
  mutate(visit = replace(visit, is.na(visit), -1)) %>%
  group_by(file_id) %>% 
  filter(abs(visit_id - visit) == min(abs(visit_id - visit))) %>% 
  arrange(visit) %>%
  as.data.frame()
sum(dat_comb_nan_test$visit > 0)
    

%>% 
  filter(is.na(mastervisit_entry)) %>%
  select(file_id, subj_id, visit_id) %>%
  mutate(file_id_date = as.Date(str_sub(file_id,-11,-2)), .after = "file_id") %>%
  arrange(file_id_date) %>% 
  
  There is also one 1st accelerometry visit data file (751801WaTAS1D46140224 (2017-09-19)); I see no entry for 7518 participant ID in mastervisit.rdata despite this visit is from 2017. 





