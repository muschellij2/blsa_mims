library(readr)
options(digits.secs = 3)
source(here::here("code/helper_functions.R"))
data_dir = here::here("mats")

fnames = list.files(path = data_dir, full.names = TRUE, pattern = "[.]mat")
fname = fnames[1]
check_data = function(fname) {
  x = read_xi_dims(fname)
  result = x$nrow %% x$srate == 0
  if (!result) {
    message(fname)
  }
  result
}

names(fnames) = fnames
res = pbapply::pbsapply(fnames, check_data)
