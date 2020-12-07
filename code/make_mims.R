library(MIMSunit)
library(dplyr)
library(readr)
source(here::here("code/helper_functions.R"))


fnames = list.files(pattern = "RAW.csv.gz", path = here::here("gt3x"),
                    full.names = TRUE)
ifile =  as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifile) || ifile < 1) {
  ifile = 2
}
print(ifile)
fname = fnames[ifile]
print(ifile)
print(fname)

outfile = here::here("open_measures", sub("RAW", "_MIMS", basename(fname)))
if (!file.exists(outfile)) {
  csv_file = R.utils::gunzip(fname,
                             remove = FALSE, 
                             temporary = TRUE,
                             overwrite = TRUE)
  
  
  meta = import_actigraph_meta(fname)
  meta$gr = as.numeric(meta$gr)
  dynamic_range = c(-meta$gr, meta$gr)
  
  df = SummarizedActigraphy::read_acc_csv(csv_file)
  hdr = df$parsed_header
  df = df$data
  
  df = df %>%
    select(HEADER_TIME_STAMP = time, X, Y, Z)
  
  df = as.data.frame(df)
  mims = mims_unit(df, dynamic_range = dynamic_range, epoch="1 min")
  
  readr::write_csv(mims, outfile)
}
