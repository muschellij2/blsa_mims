library(dplyr)
library(readr)
library(read.gt3x)
options(digits.secs = 3)
source(here::here("code/helper_functions.R"))

fnames = list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat")
gt3x = list.files(path = here::here("gt3x"), full.names = TRUE, pattern = "[.]gt3x")
ifile =  as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifile) || ifile < 1) {
  # ifile = 29
  ifile = 1
}
df = tibble::tibble(
  mat_file = fnames,
  id = sub("RAW[.]mat", "", basename(mat_file)))
df = df %>% 
  mutate(gt3x_file = here::here("gt3x", paste0(id, ".gt3x.gz")),
         qc_file = here::here("qc", paste0(id, "_read.gt3x.txt")),
         qc_file2 = here::here("qc", paste0(id, "_AGread.txt"))
  )
rm(fnames)

df = df %>% 
  filter(grepl("^[0-9]", basename(gt3x_file)))
df = df %>% 
  filter(file.exists(gt3x_file))
# these shouldn't exist
setdiff(gt3x, df$gt3x_file)

# df = df %>%
# filter(file.exists(qc_file) & !file.exists(qc_file2))

# df = df %>% 
#   filter(!file.exists(qc_file))

# for (ifile in 1:nrow(df)) {
print(ifile)
idf = df[ifile,]
fname = idf$mat_file
print(fname)
gt3x_file = idf$gt3x_file
qc_file = idf$qc_file
qc_file2 = idf$qc_file2

if (!all(file.exists(qc_file, qc_file2))) {
  
  mat = read_acc_mat(fname)
  srate = mat$fs
  header = mat$hed
  dynamic_range =  get_dynamic_range(header)
  
  mat = mat$Xi
  mat = mat %>%
    select(time = HEADER_TIME_STAMP, X, Y, Z)
  if (anyNA(mat)) {
    next
  }
  
  gt3x = read.gt3x::read.gt3x(
    gt3x_file, verbose = FALSE, 
    asDataFrame = TRUE, 
    imputeZeroes = TRUE)
  
  gt3x = SummarizedActigraphy::fix_zeros(gt3x)
  
  head(gt3x)
  
  stopifnot(nrow(gt3x) == nrow(mat))
  
  dtime = gt3x$time - mat$time
  stopifnot(all(dtime == 0))
  
  xyz = c("X", "Y", "Z")
  d = gt3x[, xyz] - mat[, xyz]
  bad = rowSums(abs(d) > 0.001) > 0
  check = all(!bad)
  if (!check) {
    mat_bad = rowSums(mat[which(bad),xyz] == 0) == 3
    # all 0s
    # if (all(mat_bad)) {
    #   check = TRUE
    # }
  }
  rm(d)
  if (!check) {
    print(head(mat[bad, ]))
    print(head(gt3x[bad, ]))
  }
  # stopifnot(check)
  if (check) {
    writeLines("TRUE", qc_file)
  }
  
  rm(bad)
  rm(check)
  rm(dtime)
  
  rm(gt3x); gc(); gc()
  
  
  res = try({AGread::read_gt3x(gt3x_file, 
                               verbose = TRUE, 
                               include = c("METADATA", "EVENT", "ACTIVITY", "ACTIVITY2"),
                               flag_idle_sleep = FALSE)
  })
  if (!inherits(res, "try-error")) {
    res = res$RAW
    colnames(res) = sub("Accelerometer_", "", colnames(res))
    res = res %>% 
      rename(time = Timestamp)
    
    stopifnot(nrow(res) == nrow(mat))
    
    # res = SummarizedActigraphy::fix_zeros(res)
    
    d = res[, c("X", "Y", "Z")] - mat[, c("X", "Y", "Z")]
    bad = rowSums(abs(d) > 0.001) > 0
    rm(d)
    check = all(!bad)
    if (!check) {
      print(head(mat[bad, ]))
      print(head(res[bad, ]))
    }
    # stopifnot(check)
    if (check) {
      writeLines("TRUE", qc_file2)
    }
    rm(res)
  }
  
  rm(mat)
}
# }
