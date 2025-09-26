
rm(list = ls())
library(tidyverse)
library(cowplot)
library(ggsci)
library(lubridate)
library(scales)
library(latex2exp)
options(scipen=999)


# name levels
names_levels1 <- c("MIMS", "ENMO", "MAD", "AI")

# ------------------------------------------------------------------------------
# read precomputed data 

path_tmp <- paste0(here::here(), "/results/2022-04-23-mapping_MPE_MAPE_MdPE_MdAPE_mapminutelevel.rds")
# path_tmp <- paste0(here::here(), "/results/2021-07-30-mapping_MPE_MAPE_mapminutelevel.rds")
dat_acc_agg <- readRDS(path_tmp)


# ------------------------------------------------------------------------------
# MPE

tbl_participant_MPE <- 
  dat_acc_agg %>%
  select(starts_with("MPE_")) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
  ) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = paste0("MPE_", names_levels1))) %>%
  arrange(name)

tbl_participant_MPE_form <- 
  tbl_participant_MPE %>%
  mutate(
    val_mean_f   = sprintf("%.1f", val_mean),
    val_sd_f     = sprintf("%.1f", val_sd),
  ) %>%
  mutate(
    val_mean_sd_MPE = paste0(val_mean_f, " (", val_sd_f, ")")
  ) %>%
  select(name, val_mean_sd_MPE)
tbl_participant_MPE_form


# ------------------------------------------------------------------------------
# MAPE

tbl_participant_MAPE <- 
  dat_acc_agg %>%
  select(starts_with("MAPE_")) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
  ) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = paste0("MAPE_", names_levels1))) %>%
  arrange(name)

tbl_participant_MAPE_form <- 
  tbl_participant_MAPE %>%
  mutate(
    val_mean_f   = sprintf("%.1f", val_mean),
    val_sd_f     = sprintf("%.1f", val_sd),
  ) %>%
  mutate(
    val_mean_sd_MAPE = paste0(val_mean_f, " (", val_sd_f, ")")
  ) %>%
  select(name, val_mean_sd_MAPE)
tbl_participant_MAPE_form


# ------------------------------------------------------------------------------
# MdPE

tbl_participant_MdPE <- 
  dat_acc_agg %>%
  select(starts_with("MdPE_")) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
  ) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = paste0("MdPE_", names_levels1))) %>%
  arrange(name)

tbl_participant_MdPE_form <- 
  tbl_participant_MdPE %>%
  mutate(
    val_mean_f   = sprintf("%.1f", val_mean),
    val_sd_f     = sprintf("%.1f", val_sd),
  ) %>%
  mutate(
    val_mean_sd_MdPE = paste0(val_mean_f, " (", val_sd_f, ")")
  ) %>%
  select(name, val_mean_sd_MdPE)
tbl_participant_MdPE_form


# ------------------------------------------------------------------------------
# MdAPE

tbl_participant_MdAPE <- 
  dat_acc_agg %>%
  select(starts_with("MdAPE_")) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(
    val_mean   = mean(value),
    val_sd     = sd(value),
  ) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = paste0("MdAPE_", names_levels1))) %>%
  arrange(name)

tbl_participant_MdAPE_form <- 
  tbl_participant_MdAPE %>%
  mutate(
    val_mean_f   = sprintf("%.1f", val_mean),
    val_sd_f     = sprintf("%.1f", val_sd),
  ) %>%
  mutate(
    val_mean_sd_MdAPE = paste0(val_mean_f, " (", val_sd_f, ")")
  ) %>%
  select(name, val_mean_sd_MdAPE)
tbl_participant_MdAPE_form


# ------------------------------------------------------------------------------
# combine tables

tbl_out <- 
  cbind(
    tbl_participant_MPE_form,
    tbl_participant_MAPE_form[, 2],
    tbl_participant_MdPE_form[, 2],
    tbl_participant_MdAPE_form[, 2]
    ) %>%
  mutate(name = gsub("MPE_", "", name)) %>%
  as.data.frame()
tbl_out

View(tbl_out)
