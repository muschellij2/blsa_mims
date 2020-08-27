library(readr)
library(corrr)
library(dplyr)
library(ggplot2)
path = "/Volumes/CT_DATA/mims_comparison/mats/"
if (!dir.exists(path)) {
  path = "~/Desktop/mats"
}
x = list.files(pattern = "csv", path = path,  full.names = TRUE)

path = x[1]
corrs = pbapply::pblapply(x, function(path) {
  df = read_csv(path, progress = FALSE,
                col_types = cols(
                  HEADER_TIME_STAMP = col_datetime(format = ""),
                  MIMS_UNIT = col_double(),
                  mean_r = col_double(),
                  MAD = col_double(),
                  MEDAD = col_double(),
                  SD = col_double(),
                  AI = col_double()
                )) 
  stopifnot("AI" %in% colnames(df))
  out = df %>% 
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
