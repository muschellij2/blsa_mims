
rm(list = ls())
library(tidyverse)
library(data.table)
library(mgcv)
options(scipen=999)

# read minute-level measures data (winsorized)
dat_acc_fpath <- paste0(here::here(), "/data_processed/2021-03-25-measures_masterfile_winsorized.rds")
dat_acc <- readRDS(dat_acc_fpath) 

# filter to keep only valid minutes data 
dat_acc <- dat_acc %>% dplyr::filter(wear_and_valid_flag == 1)
# filter to keep only valid minutes data 
dat_acc <- dat_acc %>% dplyr::filter(AC > 0)

# newdata objects 
AC_seq <- seq(from = 0, to = (1000 * 50), by = 1)
newdata <- data.frame(AC = AC_seq)


dat_acc_aggmean <- 
  dat_acc %>%
  mutate(AC = round(AC)) %>%
  group_by(AC) %>%
  summarise(
    MIMS = mean(MIMS)
  )

dat_acc_aggmedian <- 
  dat_acc %>%
  mutate(AC = round(AC)) %>%
  group_by(AC) %>%
  summarise(
    MIMS = median(MIMS)
  )

dat_acc_aggmean %>% filter(AC == 1853) %>% pull(MIMS) %>% unlist()
ggplot(dat_acc_aggmean, aes(x = AC, y = MIMS)) + 
  geom_point(size = 0.2, alpha = 0.2) + 
  geom_line() + 
  geom_vline(xintercept = 1853, linetype = 2) + 
  geom_hline(yintercept = dat_acc_aggmean %>% filter(AC == 1853) %>% pull(MIMS) %>% unlist(), linetype = 2) + 
  scale_y_continuous(limits = c(0, 65))
  
dat_acc_aggmedian %>% filter(AC == 1853) %>% pull(MIMS) %>% unlist()
ggplot(dat_acc_aggmedian, aes(x = AC, y = MIMS)) + 
  geom_point(size = 0.2, alpha = 0.2) + 
  geom_line() + 
  geom_vline(xintercept = 1853, linetype = 2) + 
  geom_hline(yintercept = dat_acc_aggmedian %>% filter(AC == 1853) %>% pull(MIMS) %>% unlist(), linetype = 2) + 
  scale_y_continuous(limits = c(0, 65))


c(
  mean(dat_acc$AC < 1853),
  
  mean(dat_acc$MIMS < 10.4956),
  mean(dat_acc$MIMS < 10.48177),
  
  mean(dat_acc$MIMS < 11.445)
)


