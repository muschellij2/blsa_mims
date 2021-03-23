library(read.gt3x)
library(pbapply)
setwd(here::here())

fname = '493408WaTAS1D46140290 (2019-01-10).gt3x.gz'
get_info = function(fname) {
  out_fname = read.gt3x:::unzip_zipped_gt3x(fname, cleanup = TRUE)
  remove = attr(out_fname, "remove")
  if (is.null(remove)) {
    remove = FALSE
  }
  if (remove) {
    on.exit({
      file.remove(out_fname)
    })
  }
  con = unz(out_fname, "info.txt")
  out = extract_gt3x_info(con)
  close(con)
  out
}

x = list.files(pattern = ".gt3x", path = "gt3x", full.names = TRUE)
names(x) = x
res = pbapply::pblapply(x, get_info)

readr::write_rds(res, here::here("results", "all_metadata.rds"), compress = "xz")

res = dplyr::bind_rows(lapply(res, as.data.frame), .id = "file")
readr::write_rds(res, here::here("results", "all_metadata_df.rds"), compress = "xz")
