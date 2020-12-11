library(readr)
library(corrr)
library(dplyr)
library(ggplot2)
library(SummarizedActigraphy)

options(digits.secs = 3)
fnames = list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat")
gt3x = list.files(path = here::here("gt3x"), full.names = TRUE, pattern = "[.]gt3x")

df = tibble::tibble(
  mat_file = fnames,
  id = sub("RAW[.]mat", "", basename(mat_file)))
df = df %>% 
  mutate(
    gt3x_file = here::here("gt3x", paste0(id, ".gt3x.gz")),
    qc_file = here::here("qc", paste0(id, "_read.gt3x.txt")),
    qc_file2 = here::here("qc", paste0(id, "_AGread.txt"))
  )

df = df %>% 
  mutate(csv_file = here::here("open_measures", paste0(id, "_MIMS.csv.gz")),
         ac_file = here::here("csv", paste0(id, "60sec.csv.gz")))
rm(fnames)


df = df %>% 
  filter(grepl("^[0-9]", basename(gt3x_file)))

df = df %>% 
  filter(file.exists(gt3x_file))
df = df %>% 
  filter(file.exists(csv_file) & file.exists(ac_file))
df = df %>% 
  filter(file.exists(qc_file))

ac_file = df$ac_csv[1]
csv_file = df$csv_file[1]


corrs = pbapply::pblapply(df$csv_file, function(path) {
  acc_df = read_csv(path, progress = FALSE,
                    col_types = cols(
                      HEADER_TIME_STAMP = col_datetime(format = ""),
                      MIMS_UNIT = col_double(),
                      mean_r = col_double(),
                      MAD = col_double(),
                      MEDAD = col_double(),
                      SD = col_double(),
                      AI = col_double()
                    )) 
  stopifnot("AI" %in% colnames(acc_df))
  out = acc_df %>% 
    select(-HEADER_TIME_STAMP, -mean_r) %>% 
    correlate(use = "pairwise.complete.obs", quiet = TRUE,
              method = "pearson") %>% 
    # shave() %>% 
    stretch() %>% 
    filter(!is.na(r))
  out$path = path
  out
})

out = bind_rows(corrs)

g = out %>% 
  ggplot(aes(x = r)) + geom_histogram() + 
  facet_grid(x ~ y)
g

out %>% 
  group_by(x, y) %>% 
  summarise(
    n = sum(!is.na(r)),
    mean = mean(r),
    median = median(r)
  )
