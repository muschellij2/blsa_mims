library(readr)
library(read.gt3x)
library(lubridate)
library(dplyr)
options(digits.secs = 3)
setwd(here::here())

# fail 030311WaTAS1D46140223 (2015-10-30).gt3x.gz
ids = list.files(path = "gt3x", pattern = ".gt3x",
                 full.names = TRUE)

ifile = ids[1]
df = data.frame(id = ids)
df$accel_max = df$accel_min = df$serial = df$device_type = NA
for (iid in seq_along(ids)) {
  ifile = ids[iid]
  print(iid)
  print(ifile)
  hdr = parse_gt3x_info(ifile)
  r = c(hdr$`Acceleration Min`, hdr$`Acceleration Max`)
  r = as.numeric(r)
  df$accel_max[iid] = hdr$`Acceleration Max`
  df$accel_min[iid] = hdr$`Acceleration Min`
  df$serial[iid] = hdr$`Serial Number`
  df$device_type[iid] = hdr$`Device Type`
  stopifnot(all(abs(r) ==8))
  file.remove(list.files(pattern = ".gt3x", path = tempdir(), full.names=TRUE))
}

df = tibble::as_tibble(df)

readr::write_rds(df, here::here("results", "device_info.rds"))
