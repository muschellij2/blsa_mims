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
         ac_file = here::here("csv", paste0(id, "60sec.csv.gz")))
rm(fnames)


df = df %>% 
  filter(grepl("^[0-9]", basename(gt3x_file)))

df = df %>% 
  filter(file.exists(gt3x_file))

ifile =  as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifile) || ifile < 1) {
  ifile = 1
}
print(ifile)
fname = df$mat_file[ifile]
print(ifile)
print(fname)

outfile = df$csv_file[ifile]

if (!file.exists(outfile)) {
  acc_df = read_acc_mat(fname)
  srate = acc_df$fs
  header = acc_df$hed
  dynamic_range =  get_dynamic_range(header)
  
  acc_df = acc_df$Xi
  acc_df = acc_df %>%
    select(HEADER_TIME_STAMP, X, Y, Z)
  
  out = calculate_measures(
    df = acc_df, 
    fix_zeros = TRUE, 
    dynamic_range = dynamic_range,
    calculate_mims = TRUE,
    verbose = TRUE)
  
  acc_df = acc_df %>%
    rename(Index = HEADER_TIME_STAMP)
  ai = computeActivityIndex(acc_df, sigma0 = 0, epoch = 60, hertz = srate)
  ai = ai %>% 
    rename(HEADER_TIME_STAMP = RecordNo)  
  acc_df = acc_df %>%
    rename(HEADER_TIME_STAMP = Index)
  
  # ai = quick_ai(df)
  
  mad = acc_df %>% 
    mutate(         
      r = sqrt(X^2 + Y^2 + Z^2),
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, "1 min")) %>% 
    group_by(HEADER_TIME_STAMP) %>% 
    mutate(abs_diff = abs(r - mean(r))) %>% 
    summarise(
      mean_r = mean(r),
      MAD = mean(abs_diff),
      MEDAD = median(abs_diff),
      SD  = sd(r)
    )
  su = function(x) sort(unique(x))
  stopifnot(isTRUE(
    all.equal(su(mad$HEADER_TIME_STAMP), su(ai$HEADER_TIME_STAMP))
  ))
  mad = full_join(mad, ai)
  # c1 = mad %>% 
  #   select(-HEADER_TIME_STAMP) %>% 
  #   corrr:::correlate()
  # rs = mad %>% 
  #   select(-HEADER_TIME_STAMP) 
  # rs = rowSums(rs > 0) > 0
  # c2 = mad[rs, ] %>% 
  #   select(-HEADER_TIME_STAMP) %>% 
  #   corrr:::correlate()
  
  # df = MIMSunit::import_actigraph_csv(csv_file, has_ts = FALSE)
  acc_df = as.data.frame(acc_df)
  mims = mims_unit(acc_df, dynamic_range = dynamic_range, epoch = "1 min")
  
  mims = full_join(mims, mad)
  
  readr::write_csv(mims, outfile)
  rm(mims)
}
