#' This script generates "Table 2" for the paper. 
#' These are summary of 
#' 
#' It only summarizes (participant, visit) pairs for which we have data in 
#' the masterfile.
#' 
#' Input: 
#' > /data_processed/2021-01-18-measures_masterfile.rds
#' 
#' Notes: 
#' - use: cd /dcl01/smart/data/activity/blsa_mims

# get /dcl01/smart/data/activity/blsa_mims/covariates/mastervisit.rdata /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/mastervisit.rdata
# get /dcl01/smart/data/activity/blsa_mims/covariates/mastervisit.sas7bdat /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/mastervisit.sas7bdat

# get /dcl01/smart/data/activity/blsa_mims/covariates/masterdemog_no\ dob.rdata  /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/masterdemog_no\ dob.rdata 

# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-mastervisit.rdata /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-mastervisit.rdata
# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-mastervisit.sas7bdat /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-mastervisit.sas7bdat

# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-masterdemog.rdata /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-masterdemog.rdata
# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-masterdemog.dta /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-masterdemog.dta

# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-blsa_interview_mdhx_teleform.rdata /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-blsa_interview_mdhx_teleform.rdata
# put /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/covariates/2021-01-19-blsa_interview_mdhx_teleform.dta /dcl01/smart/data/activity/blsa_mims/covariates/2021-01-19-blsa_interview_mdhx_teleform.dta

# get /dcl01/smart/data/activity/blsa_mims/data_processed/2021-01-18-measures_masterfile.rds /Users/martakaras/Dropbox/_PROJECTS/blsa_mims/data_processed/2021-01-18-measures_masterfile.rds


rm(list = ls())
library(tidyverse)
library(haven)
library(utils)

# ------------------------------------------------------------------------------
# read data 

# read accelerometry measures master file
datacc_fpath <- paste0(here::here(), "/data_processed/2021-01-18-measures_masterfile.rds")
datacc <- readRDS(datacc_fpath)
datacc_id <- datacc %>%
  select(file_id, subj_id, visit_id) %>%
  distinct() %>% 
  as.data.frame() %>% 
  mutate(entry_accdata = 1) 
str(datacc_id)

# read mastervisit
mastervisit_fpath <- paste0(here::here(), "/covariates/2021-01-19-mastervisit.rdata")
mastervisit <- get(load(mastervisit_fpath, ex <- new.env()), envir = ex) %>%
  rename(idno = IDNo, visit = Visit, dov = DOV)
mastervisit_id <- 
  mastervisit %>%
  select(idno, visit, dov) %>% 
  mutate(entry_mastervisit = 1) 
dim(mastervisit)
# [1] 25571   113
dim(mastervisit_id)
# [1] 25571     4

# read masterdemog 
masterdemog_fpath <- paste0(here::here(), "/covariates/2021-01-19-masterdemog.rdata")
masterdemog <- get(load(masterdemog_fpath, ex <- new.env()), envir = ex) 
masterdemog_id <- 
  masterdemog %>%
  select(idno) %>% 
  mutate(entry_masterdemog = 1) 
dim(masterdemog)
# [1] 3445   15
dim(masterdemog_id)
# [1] 3445   2

# read interview data 
# read interview 
interview_fpath <- paste0(here::here(), "/covariates/2021-01-19-blsa_interview_mdhx_teleform.dta")
interview <- haven::read_dta(interview_fpath)
# list attributes 
attributes(interview[[1]])
# produce the meta info data 
interview_meta <- data.frame(
  var_name = names(interview),
  var_label = unlist(lapply(interview, function(ll){val <- attr(ll, "label"); ifelse(is.null(val), NA, val)}))
)
rownames(interview_meta) <- NULL
# produce the meta info data 
utils::capture.output(
  interview_meta,
  file = paste0(here::here(), "/covariates/interview_meta.txt")
)




# read interview 
interview_fpath <- paste0(here::here(), "/covariates/2021-01-19-blsa_interview_mdhx_teleform.rdata")
interview <- get(load(interview_fpath, ex <- new.env()), envir = ex) 
interview_id <- 
  interview  %>%
  select(idno, visit) %>% 
  mutate(entry_interview = 1) 

# combine unique pairs (subj_id, visit_id) between acc data measures_masterfile and 
# (a) mastervisit,
# (b) masterdemog
# NOTE: the match should be the case given how the measures_masterfile was constructed;
# this is just to double-check
dat_comb <- 
  datacc_id %>%
  left_join(mastervisit_id, by = c("subj_id" = "idno", "visit_id" = "visit")) %>%
  left_join(masterdemog_id, by = c("subj_id" = "idno")) %>%
  left_join(interview_id,   by = c("subj_id" = "idno", "visit_id" = "visit")) 
  
dim(datacc_id)
dim(dat_comb)
any(is.na(dat_comb$dov))
any(is.na(dat_comb$entry_mastervisit))
any(is.na(dat_comb$entry_masterdemog))
any(is.na(dat_comb$entry_interview))



# ------------------------------------------------------------------------------
# construct Table 1:
# 

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
    

# -----------------------------------------------------------------------------

# old mastervisit 
mastervisit_fpath <- paste0(here::here(), "/covariates/mastervisit.rdata")
mastervisit <- get(load(mastervisit_fpath, ex <- new.env()), envir = ex)
dim(mastervisit)
range(mastervisit$dov, na.rm = TRUE)

# new mastervisit 
mastervisit_fpath <- paste0(here::here(), "/covariates/2021-01-19-mastervisit.rdata")
mastervisit2 <- get(load(mastervisit_fpath, ex <- new.env()), envir = ex)
dim(mastervisit2)
range(mastervisit2$DOV, na.rm = TRUE)

# SAS
mastervisit_fpath <- paste0(here::here(), "/covariates/2021-01-19-mastervisit.sas7bdat")
mastervisit2b <- sas7bdat::read.sas7bdat(mastervisit_fpath)

mastervisit2b_colinfo <- attr(mastervisit2b, "column.info")

names(mastervisit2b_colinfo[[1]])
mastervisit2b_meta <- data.frame(
  var_name = sapply(mastervisit2b_colinfo, function(ll) ll$name),
  var_label = sapply(mastervisit2b_colinfo, function(ll) ll$label)
)

utils::capture.output(
  mastervisit2b_meta,
  file = paste0(here::here(), "/covariates/mastervisit_meta.txt")
)

# -----------------------------------------------------------------------------

# old mastervisit 
masterdemog_fpath <- paste0(here::here(), "/covariates/masterdemog_no dob.rdata")
masterdemog <- get(load(masterdemog_fpath, ex <- new.env()), envir = ex)
dim(masterdemog)
names(masterdemog)

masterdemog_meta <- data.frame(
  var_name = names(masterdemog)
)
range(masterdemog$LastVisit_Date)
# [1] "1958-02-06" "2019-06-25"

utils::capture.output(
  masterdemog_meta,
  file = paste0(here::here(), "/covariates/masterdemog_meta.txt")
)


