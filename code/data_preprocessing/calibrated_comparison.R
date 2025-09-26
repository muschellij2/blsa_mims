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
library(corrr)
library(ggplot2)
source(here::here("code/helper_functions.R"))

library(SummarizedActigraphy)
message("Loaded packages.")
options(digits.secs = 3)

# source util functions

fnames = list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat")
df = tibble::tibble(
  c_outfile = here::here("open_measures", 
                          paste0(sub("RAW[.]mat", "", basename(fnames)), 
                                 "_calibrated_OSM.rds")),
  outfile = here::here("open_source_measures", 
                        paste0(sub("RAW[.]mat", "", basename(fnames)), 
                               "_OSM.rds"))
)

df = df %>% 
  filter(file_exists(outfile) & file_exists(c_outfile)) 
iid = 1 


cmat_fname = here::here("results/calibration_correlation_matrices.rds")
cmatneg_fname = here::here("results/calibration_correlation_withneg_matrices.rds")
spear_fname = here::here("results/calibration_spearman_correlation_matrices.rds")
calib_fname = here::here("results/calibration_coefficients.rds")


stubs = sub("_OSM.*", "", basename(df$outfile))
cmat = vector(mode = "list", length = nrow(df)) 
calib_mat = vector(mode = "list", length = nrow(df)) 
names(cmat) = names(calib_mat) = stubs
cmat_withneg = spearman = cmat

for (iid in seq(nrow(df))) {
  idf = df[iid, ]
  i1 = readRDS(idf$outfile)
  i2 = readRDS(idf$c_outfile)
  stopifnot(nrow(i1) == nrow(i2))
  # stopifnot(!any(i1$MIMS_UNIT < 0),
  #           !any(i2$MIMS_UNIT < 0))
  calib = attr(i2, "calibration_values")
  stopifnot(length(calib) > 0)
  calib = calib[c("scale", "offset")]
  calib_mat[[iid]] = calib
  stopifnot(all(i1$HEADER_TIME_STAMP == i2$HEADER_TIME_STAMP))
  i1 = i1 %>% 
    select(-HEADER_TIME_STAMP)
  i2 = i2 %>% 
    select(-HEADER_TIME_STAMP)  
  cc = cor(i1, i2, use = "pairwise.complete")
  cmat_withneg[[iid]] = cc
  
  # 
  i1$MIMS_UNIT[i1$MIMS_UNIT < 0] = NA
  i2$MIMS_UNIT[i2$MIMS_UNIT < 0] = NA  
  cc = cor(i1, i2, use = "pairwise.complete")
  cmat[[iid]] = cc
  
  cc = cor(i1, i2, method = "spearman", use = "pairwise.complete")
  spearman[[iid]] = cc  
  print(iid)
}

readr::write_rds(cmat, 
                 file = cmat_fname, 
                 compress = "xz")


readr::write_rds(cmat_withneg, 
                 file = cmatneg_fname, 
                 compress = "xz")

readr::write_rds(spearman, 
                 file = spear_fname, 
                 compress = "xz")


readr::write_rds(calib_mat, 
                 file = calib_fname, 
                 compress = "xz")

cmat = readr::read_rds(cmat_fname)
cmat_withneg = readr::read_rds(cmatneg_fname)
spearman = readr::read_rds(spear_fname)
calib_mat = readr::read_rds(calib_fname)

mean_sd = function(x, na.rm = TRUE) {
  mn = mean(x, na.rm = na.rm)
  s = sd(x, na.rm = na.rm)
  sprintf("%03.3f (%03.3f)", round(mn, 3), round(s, 3))
}
median_iqr = function(x, na.rm = TRUE) {
  mn = median(x, na.rm = na.rm)
  q25 = quantile(x, na.rm = na.rm, probs = c(0.25, 0.75))
  sprintf("%03.3f (%03.3f - %03.3f)", round(mn, 3), round(q25[1], 3), round(q25[2], 3))
}
# weird mims: 493405WaTAS1E23150402 (2015-09-09)_OSM.rds
enmo = sapply(cmat, function(x) x['ENMO', 'ENMO_t'])
mad = sapply(cmat, function(x) x['MAD', 'MAD'])
mims = sapply(cmat, function(x) x['MIMS_UNIT', 'MIMS_UNIT'])
neg_mims = sapply(cmat_withneg, function(x) x['MIMS_UNIT', 'MIMS_UNIT'])
ai = sapply(cmat, function(x) x['AI', 'AI'])

mean_sd(ai)
median_iqr(ai)

mean_sd(mims)
median_iqr(mims)

mean_sd(mad)
median_iqr(mad)

offset = t(sapply(calib_mat, `[[`, "offset"))
colnames(offset) = SummarizedActigraphy::xyz
offset = tibble::as_tibble(offset)
offset$HEADER_TIME_STAMP = stubs
offset = SummarizedActigraphy::tidy_axes(offset) %>% 
  dplyr::rename(offset = value)
scaling = t(sapply(calib_mat, `[[`, "scale"))
colnames(scaling) = SummarizedActigraphy::xyz
scaling = tibble::as_tibble(scaling)
scaling$HEADER_TIME_STAMP = stubs
scaling = SummarizedActigraphy::tidy_axes(scaling) %>% 
  dplyr::rename(scale = value)

data = dplyr::full_join(offset, scaling)

scaling %>% group_by(axis) %>% summarise(mean(scale), sd(scale))
offset %>% group_by(axis) %>% summarise(mean(offset), sd(offset))

scaling %>% 
  ggplot(aes(x = scale)) + geom_histogram() + 
  facet_wrap( ~ axis, scales = "free_y", ncol = 1)

offset %>% 
  ggplot(aes(x = offset)) + geom_histogram() + 
  facet_wrap( ~ axis, scales = "free_y", ncol = 1)
  
data %>% 
  ggplot(aes(x = offset, y = scale)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  facet_wrap( ~ axis)


ind = data %>% 
  filter(axis == "Z") %>% 
  filter(scale == min(scale)) %>%
  pull(time)

cmat[[ind]]
spearman[[ind]]
