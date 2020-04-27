library(MIMSunit)
library(dplyr)
library(readr)
source(here::here("code/helper_functions.R"))

user = Sys.info()[['user']]
if (user == "johnmuschelli") {
  # setwd("~/Johns Hopkins/Jacek Urbanek - BLSA Accelerometry/gt3x/")
  setwd("/Volumes/CT_DATA/mims_comparison/mats/")
} else {
  setwd("~/gt3x")
}


fnames = list.files(pattern = "[.]mat")
fnames = list.files(pattern = "RAW.csv.gz")
ifile =  as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifile) || ifile < 1) {
  ifile = 2
}
print(ifile)
fname = fnames[ifile]
print(ifile)
print(fname)

outfile = sub("RAW", "_MIMS", fname)
if (!file.exists(outfile)) {
  csv_file = R.utils::gunzip(fname,
                             remove = FALSE, 
                             temporary = TRUE,
                             overwrite = TRUE)
  
  
  meta = import_actigraph_meta(fname)
  meta$gr = as.numeric(meta$gr)
  dynamic_range = c(-meta$gr, meta$gr)
  
  df = read_csv(csv_file,
                skip = 10)
  hdr = read_lines(csv_file,
                   n_max = 10)
  file.remove(csv_file)
  
  st = sub_thing(hdr, "Start Time")
  sd = sub_thing(hdr, "Start Date")
  srate = as.numeric(sub(".*at (\\d*) Hz.*", "\\1", hdr[1]))
  start_date = lubridate::mdy_hms(paste0(sd, " ", st))
  
  df$HEADER_TIME_STAMP = seq(0, nrow(df) - 1)/srate
  df$HEADER_TIME_STAMP = start_date + df$HEADER_TIME_STAMP
  
  df = df %>%
    rename(X = `Accelerometer X`,
           Y = `Accelerometer Y`,
           Z = `Accelerometer Z`) %>% 
    select(HEADER_TIME_STAMP, X, Y, Z)
  
  # df = MIMSunit::import_actigraph_csv(csv_file, has_ts = FALSE)
  df = as.data.frame(df)
  mims = mims_unit(df, dynamic_range = dynamic_range, epoch="1 min")
  
  readr::write_csv(mims, outfile)
}
