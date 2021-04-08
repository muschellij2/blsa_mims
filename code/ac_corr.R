#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

#' @description 
#' This script takes raw output from ActiLife giving acceleration measurements 
#' [g] along three orthogonal axes and computes minute-level summary statistic: 
#' - MIMS
#' - AI
#' - MAD
#' - ENMO 
#' 
#' input files dir: /mats/
#' output files dir: /open_source_measures/
#' 
#' use:
#' qrsh -l mem_free=50G,h_vmem=50G,h_fsize=50G,h_stack=256M
#' cd /dcl01/smart/data/activity/blsa_mims
#' module load conda_R

# remotes::install_github("javybai/ActivityIndex")
# remotes::install_github("muschellij2/SummarizedActigraphy")
library(MIMSunit)
library(dplyr)
library(readr)
library(ActivityIndex)
library(SummarizedActigraphy)
message("Loaded packages.")
options(digits.secs = 3)

# source util functions
source(here::here("code/helper_functions.R"))

fnames = sort(list.files(path = here::here("mats"), full.names = TRUE, 
                         pattern = "[.]mat"))
outfiles = here::here("open_measures", paste0(
  sub("RAW[.]mat", "", basename(fnames)), "_AChat.rds"))

csvs = here::here("csv", sub("RAW[.]mat", "60sec.csv", basename(fnames)))

stopifnot(all(file.exists(csvs)))

df = tibble::tibble(ac = outfiles, csv = csvs)
df$correlation = df$correlation_pos = df$correlation_both_pos = NA
df$slope = df$intercept = NA
i = 1

for (i in seq(nrow(df))) {
  print(i)
  idf = df[i,]
  ac = readRDS(idf$ac) %>% 
    rename(time = HEADER_TIME_STAMP)
  x = read_acc_csv(idf$csv, only_xyz = FALSE)
  x = x$data
  x = x[, c("time", "vectormagnitude")]
  out = full_join(x, ac, by = "time")
  mod = lm(vectormagnitude ~ AC, data = out)
  df$slope[i] = coef(mod)[2]
  df$intercept[i] = coef(mod)[1]
  
  df$n_zero_ac[i] = sum(out$AC == 0)
  df$n_zero_vm[i] = sum(out$vectormagnitude == 0)
  df$correlation[i] = cor(out$vectormagnitude, out$AC, use = "pairwise.complete.obs")
  ind = which(out$AC > 0)
  df$correlation_pos[i] = cor(out$vectormagnitude[ind], out$AC[ind], use = "pairwise.complete.obs")
  ind = which(out$AC > 0 & out$vectormagnitude > 0)
  df$correlation_both_pos[i] = cor(out$vectormagnitude[ind],
                                   out$AC[ind], 
                                   use = "pairwise.complete.obs")
}


outfile = here::here("results", "ac_correlation.rds")
readr::write_rds(df, outfile)

