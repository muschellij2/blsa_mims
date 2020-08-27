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
