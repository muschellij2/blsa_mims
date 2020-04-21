library(dplyr)
library(readr)
library(here)
source(here::here("helper_functions.R"))

setwd(here::here("csv"))

fnames = list.files(pattern = "60sec.csv.gz")

ifile = 1

for (ifile in seq_along(fnames)) {
  fname = fnames[ifile]
  print(ifile)
  print(fname)
  
  outfile = sub("60sec", "_nonwear", fname)
  # if (!file.exists(outfile)) {
  df = read_csv(fname,
                skip = 10)
  wear = WearNonWear(df$vectormagnitude)
  df$wear = wear > 0
  
  d = diff(df$timestamp)
  units(d) = "mins"
  d = as.numeric(d)
  stopifnot(isTRUE(all.equal(unique(d), 1)))
  readr::write_csv(df, outfile)
  # }
}
