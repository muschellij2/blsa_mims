# setwd("~/Johns Hopkins/Jacek Urbanek - BLSA Accelerometry/gt3x")
setwd("/Volumes/CT_DATA/mims_comparison/gt3x")
library(readr)
library(read.gt3x)
library(lubridate)
library(dplyr)
options(digits.secs = 3)
source("helper_functions.R")

# fail 030311WaTAS1D46140223 (2015-10-30).gt3x.gz
ids = list.files(pattern = "RAW")
df = data.frame(
  csv = ids,
  gt3x = gsub("RAW[.]csv", ".gt3x", ids),
  stringsAsFactors = FALSE
)
ids = list.files(pattern = "[.]gt3x")
df = df[rowSums(!sapply(df, file.exists)) == 0,]
# fe = sapply(df, function(x) {
#   sapply(x, function(r) {
#     suppressWarnings(
#       {
#         out = try({
#           file(r, "rb")
#         }, silent = TRUE)
#       })
#     bad = inherits(out, "try-error")
#     if (!bad) {
#       close(out)
#     }
#     !bad
#   })
# })
# df = df[ rowSums(fe) == 2, ]
# stopifnot(all(ids %in% df$gt3x))

df = df[!grepl("030311WaTAS1D46140223", df$csv),]
df = df[-seq(grep("071526WaTAS1D46140232", df$csv)),]
# iid = grep("030311WaTAS1D46140223", df$csv)
iid = 1
print(nrow(df))
for (iid in seq(nrow(df))) {
  print(iid)
  idf = df[iid, ]
  print(idf)
  ext = tools::file_ext(idf$gt3x)
  tmp_gt3x_file = gt3x_file = file.path(tempdir(), basename(idf$gt3x))
  if (!file.exists(gt3x_file)) {
    file.copy(idf$gt3x, gt3x_file)
    idf$gt3x = gt3x_file
  }
  if (ext == "gt3x") {
    gt3x_file = idf$gt3x
  } else {
    gt3x_file = R.utils::gunzip(gt3x_file,
                                remove = FALSE, temporary = TRUE,
                                overwrite = TRUE)
    file.remove(tmp_gt3x_file)
  }
  gt3x = read.gt3x::read.gt3x(
    gt3x_file,
    asDataFrame = TRUE)
  at = attributes(gt3x)

  gt3x = read.gt3x::read.gt3x(
    gt3x_file,
    asDataFrame = TRUE,
    imputeZeroes = TRUE)
  tz(gt3x$time) = "UTC"
  
  file.remove(gt3x_file)
  out_dir = file.path(tempdir(), 
                      sub("[.]gt3x.*", "", basename(gt3x_file)))
  if (dir.exists(out_dir)) {
    unlink(out_dir, recursive = TRUE)
  }
  out_dir = file.path(tempdir(), gsub("\\s", "", basename(out_dir)))
  if (dir.exists(out_dir)) {
    unlink(out_dir, recursive = TRUE)
  }  

  
  zgt3x = fix_zeros(gt3x)

  # Read RAW CSV 
  ext = tools::file_ext(idf$csv)
  tmp_csv_file = csv_file = file.path(tempdir(), basename(idf$csv))
  if (!file.exists(csv_file)) {
    file.copy(idf$csv, csv_file)
    idf$csv = csv_file
  }
  if (ext == "csv") {
    csv_file = idf$csv
  } else {
    csv_file = R.utils::gunzip(idf$csv,
                               remove = FALSE, 
                               temporary = TRUE,
                               overwrite = TRUE)
    file.remove(tmp_csv_file)
  }  
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
  
  format = sub(".*date format (.*) at.*", "\\1", hdr[1])
  if (format == "") {
    warning("No format for date in the header, using mdy")
    format = "mdy"
  } else {
    format = tolower(format)
    format = c(sapply(strsplit(format, "/"), substr, 1,1))
    format = paste(format, collapse = "")
  }
  all_formats = c("ydm", "dym", "ymd", "myd", "dmy", "mdy")
  stopifnot(format %in% all_formats)
  lubridate_func = paste0(format, "_hms")
  lubridate_func = getFromNamespace(lubridate_func, "lubridate")
  start_date = do.call(lubridate_func, args = list(paste0(sd, " ", st)))
  
  
  csv$time = seq(0, nrow(csv) - 1)/srate
  csv$time = start_date + csv$time


  head(gt3x)

  stopifnot(nrow(gt3x) == nrow(csv))
  stopifnot(nrow(zgt3x) == nrow(csv))

  dtime = zgt3x$time - csv$time
  
  
  d = zgt3x[, c("X", "Y", "Z")] - csv[, c("X", "Y", "Z")]
  stopifnot(all(d == 0))
}
# stopifnot(isTRUE(all.equal(gt3x$time, csv$time)))
