library(MIMSunit)
library(dplyr)
library(readr)
library(ActivityIndex)
options(digits.secs = 3)
source(here::here("code/helper_functions.R"))
user = Sys.info()[['user']]
if (user == "johnmuschelli") {
  # setwd("~/Johns Hopkins/Jacek Urbanek - BLSA Accelerometry/gt3x/")
  root_data_dir = "/Volumes/CT_DATA/mims_comparison/"
} else {
  root_data_dir = "~/mims_comparison"
}
data_dir = file.path(root_data_dir, "mats")

fnames = list.files(path = data_dir, full.names = TRUE, pattern = "[.]mat")
ifile =  as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifile) || ifile < 1) {
  ifile = 2
}
print(ifile)
fname = fnames[ifile]
print(ifile)
print(fname)

outfile = sub("RAW.mat", "_MIMS.csv.gz", fname)

if (!file.exists(outfile)) {
  df = read_acc_mat(fname)
  srate = df$fs
  header = df$hed
  dynamic_range =  get_dynamic_range(header)
  
  df = df$Xi
  df = df %>%
    select(HEADER_TIME_STAMP, X, Y, Z)
  
  df = df %>%
    rename(Index = HEADER_TIME_STAMP)
    
  ai = computeActivityIndex(df, sigma0 = 0, epoch = 60, hertz = srate)
  ai = ai %>% 
    rename(HEADER_TIME_STAMP = RecordNo)
  
  df = df %>%
    rename(HEADER_TIME_STAMP = Index)
  mad = df %>% 
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
  df = as.data.frame(df)
  mims = mims_unit(df, dynamic_range = dynamic_range, epoch = "1 min")

  mims = full_join(mims, mad)
  mims = full_join(mims, ai)
  
  readr::write_csv(mims, outfile)
  rm(mims)
}
