
rm(list = ls())
library(tidyverse)
options(digits.secs = 3)

# read data 
dat_meta   <- readRDS(paste0(here::here(), "/results/2021-03-23-device_and_data_range_info.rds"))
masterfile <- readRDS(paste0(here::here(), "/data_processed/2021-03-03-measures_masterfile.rds"))

# format data
masterfile <- masterfile %>% select(file_id, subj_id, visit_id) %>% distinct()
dat_meta <- dat_meta %>%
  mutate(
    file_id = gsub("RAW.mat", "", fname),
    file_id_date = stringr::str_extract(string = file_id, pattern = "(?<=\\().*(?=\\))"),
    file_id_date = as.Date(file_id_date)
  )

head(dat_meta)
head(masterfile)

dim(dat_meta)
dim(masterfile)


# --------------------------------------
# set 1: masterfile subset only 

dat_meta_sub <- dat_meta %>% inner_join(masterfile, by = "file_id")
dim(dat_meta_sub)
str(dat_meta_sub)

table(dat_meta_sub$srate)
# 30  80 
# 66 655 
plot(dat_meta_sub$file_id_date, dat_meta_sub$srate, xlab = "file date", ylab = "raw data frequency [Hz]")

table(dat_meta_sub$dynamic_range_min)
table(dat_meta_sub$dynamic_range_max)
summary(dat_meta_sub$accel_min)
summary(dat_meta_sub$accel_max)

table(dat_meta_sub$actilifev)
table(dat_meta_sub$firmwarev)

sort(dat_meta_sub$sn)

