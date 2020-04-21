setwd("~/Johns Hopkins/Jacek Urbanek - BLSA Accelerometry/gt3x")
library(readr)
library(AGread)
library(lubridate)
library(dplyr)
options(digits.secs = 3)

sub_thing = function(hdr, string) {
  x = hdr[grepl(string, hdr)]
  x = gsub(string, "", x)
  x = trimws(x)
}

# fail 030311WaTAS1D46140223 (2015-10-30).gt3x.gz
ids = list.files(pattern = "RAW")
df = data.frame(
  csv = ids,
  gt3x = gsub("RAW[.]csv", ".gt3x", ids),
  stringsAsFactors = FALSE
)
ids = list.files(pattern = "[.]gt3x")
df = df[rowSums(!sapply(df, file.exists)) == 0,]
# stopifnot(all(ids %in% df$gt3x))

df = df[-grep("030311WaTAS1D46140223", df$csv),]
df = df[-seq(grep("071526WaTAS1D46140232", df$csv)), ]
# iid = grep("030311WaTAS1D46140223", df$csv)
iid = 1
for (iid in seq(nrow(df))) {
  print(iid)
  idf = df[iid, ]
  print(idf)
  # gt3x_file = R.utils::gunzip(idf$gt3x,
  #                             remove = FALSE, temporary = TRUE,
  #                             overwrite = TRUE)
  gt3x_file = idf$gt3x
  
  full_gt3x = AGread::read_gt3x(
    gt3x_file,
    include = c("METADATA", "EVENT", "ACTIVITY", "ACTIVITY2"),
    flag_idle_sleep = TRUE,
    verbose = TRUE,
    cleanup = TRUE)
  file.remove(gt3x_file)
  
  gt3x = full_gt3x$RAW
  attr(gt3x, "header") = full_gt3x$info
  gt3x = gt3x %>% 
    rename(time = "Timestamp")
  gt3x = gt3x %>%
    rename_all(.funs = function(x) gsub("Accelerometer_", "", x))
  tz(gt3x$time) = "UTC"
  
  # Read RAW CSV 
  csv_file = idf$csv
  # csv_file = R.utils::gunzip(idf$csv, remove = FALSE, temporary = TRUE)
  csv = read_csv(csv_file,
                 skip = 10)
  hdr = read_lines(csv_file,
                   n_max = 10)
  file.remove(csv_file)
  
  csv = csv %>%
    rename(X = `Accelerometer X`,
           Y = `Accelerometer Y`,
           Z = `Accelerometer Z`)
  
  st = sub_thing(hdr, "Start Time")
  sd = sub_thing(hdr, "Start Date")
  srate = as.numeric(sub(".*at (\\d*) Hz.*", "\\1", hdr[1]))
  start_date = lubridate::mdy_hms(paste0(sd, " ", st))
  
  csv$time = seq(0, nrow(csv) - 1)/srate
  csv$time = start_date + csv$time
  
  
  head(gt3x)
  
  stopifnot(nrow(gt3x) == nrow(csv))
  
  d = gt3x[, c("X", "Y", "Z")] - csv[, c("X", "Y", "Z")]
  stopifnot(all(d == 0))
  
  rm(full_gt3x)
  rm(csv)
  rm(d)
  rm(gt3x)
}
# stopifnot(isTRUE(all.equal(gt3x$time, csv$time)))
