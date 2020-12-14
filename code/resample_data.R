library(MIMSunit)
library(dplyr)
library(readr)
library(ActivityIndex)
options(digits.secs = 3)

source(here::here("code/helper_functions.R"))

fnames = list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat")
gt3x = list.files(path = here::here("gt3x"), full.names = TRUE, pattern = "[.]gt3x")

df = tibble::tibble(
  mat_file = fnames,
  id = sub("RAW[.]mat", "", basename(mat_file)))
df = df %>% 
  mutate(
    gt3x_file = here::here("gt3x", paste0(id, ".gt3x.gz")),
    qc_file = here::here("qc", paste0(id, "_read.gt3x.txt")),
    qc_file2 = here::here("qc", paste0(id, "_AGread.txt"))
  )

df = df %>% 
  mutate(csv_file = here::here("open_measures", paste0(id, "_MIMS.csv.gz")),
         ac_file = here::here("csv", paste0(id, "60sec.csv.gz")),
         res_file = here::here("resampled", paste0(id, ".rds")))
rm(fnames)


df = df %>% 
  filter(grepl("^[0-9]", basename(gt3x_file)))


ifile =  as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifile) || ifile < 1) {
  ifile = 1
}
print(ifile)
fname = df$mat_file[ifile]
print(ifile)
print(fname)

outfile = df$res_file[ifile]

if (!file.exists(outfile)) {
  acc_df = read_acc_mat(fname)
  srate = acc_df$fs
  header = acc_df$hed
  dynamic_range =  get_dynamic_range(header)
  
  acc_df = acc_df$Xi
  acc_df = acc_df %>%
    select(HEADER_TIME_STAMP, X, Y, Z)
  stopifnot(!anyNA(acc_df))
  acc_df = as.data.frame(acc_df)
  noise_level = 0.03; k = 0.05; spar = 0.6
  
  resampled_data <- MIMSunit::extrapolate(acc_df, dynamic_range, noise_level, 
                                          k, spar)
  readr::write_rds(resampled_data, outfile, compress = "xz")
}
